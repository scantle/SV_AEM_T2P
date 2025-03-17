import matplotlib
matplotlib.use('TkAgg')
import seaborn as sns
import numpy as np
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import statsmodels.api as sm
from pathlib import Path
from tqdm import tqdm
from scipy.stats import norm, lognorm, entropy
from scipy.optimize import minimize
from sklearn.metrics import silhouette_score, normalized_mutual_info_score
from sklearn.neighbors import NearestNeighbors

from pykrige.ok import OrdinaryKriging
from kmodes.kprototypes import KPrototypes

import os
os.chdir("./03_Scripts/")
os.environ["OMP_NUM_THREADS"] = "2"

from sklearn.preprocessing import StandardScaler
from sklearn.metrics import pairwise_distances
from sklearn.cluster import AgglomerativeClustering, DBSCAN, SpectralClustering

import sys
sys.path.append('./')
from aem_read import read_xyz, aem_wide2long

# -------------------------------------------------------------------------------------------------------------------- #
# Settings
# -------------------------------------------------------------------------------------------------------------------- #
np.random.seed(1024)

# Directories
data_dir = Path('../01_Data/')
shp_dir = data_dir / 'shapefiles'
out_dir = Path('../05_Outputs')
plt_dir = Path('../04_Plots/')

# Files
aem_sharp_file = data_dir / 'SCI_Sharp_10_West_I01_MOD_inv.xyz'
aem_litho_file = data_dir / 'AEM_WELL_LITHOLOGY_csv_WO2_20220710_CVHM.csv'
wl_file = data_dir / 'WLs_Oct312021.csv'

# Shapefiles
aem_sharp_sv_file = shp_dir / 'aem_sv_Sharp_I01_MOD_inv_UTM10N_idwwl.shp'
aem_hqwells_file = shp_dir / 'aem_sv_HQ_LithologyWells_UTM10N.shp'

# -------------------------------------------------------------------------------------------------------------------- #
# Classes/Functions
# -------------------------------------------------------------------------------------------------------------------- #

def jensen_shannon_divergence(p, q):
    """Calculate the Jensen-Shannon Divergence between two probability distributions."""
    m = 0.5 * (p + q)
    return 0.5 * (entropy(p, m, base=2) + entropy(q, m, base=2))


class AEMCell(object):
    """ From Knight et al. (2018) Eq (2) """
    def __init__(self, rho_aem, rho_std, t_aem, t_clusters, dist):
        self.dist = dist
        self.rho_aem = rho_aem
        self.rho_std = rho_std
        self.rho_dist = norm(loc=rho_aem, scale=rho_std)
        self.clus_frac = t_clusters / t_aem

    @property
    def weight(self):
        return 1/self.rho_std**2

    def values_weighted(self):
        """ returns a row of A and b for Ax = b, with an extra value for w, 1/variance"""
        return self.clus_frac, 1/self.rho_aem, self.weight

def lognormal_neg_log_likelihood(params, data):
    shape, loc, scale = params
    if loc < 0:
        return np.inf  # Impose a penalty for negative loc values
    return -np.sum(lognorm.logpdf(data, s=shape, scale=scale, loc=loc))

def fit_lognormal_with_constraints(data):
    # Initial guesses for shape, loc, scale
    initial_params = [1, max(0, np.min(data) - 0.1 * np.min(data)), np.std(data)]

    # Constraint to ensure loc is non-negative
    cons = ({'type': 'ineq', 'fun': lambda x: x[1]})  # x[1] is loc

    # Perform the minimization with constraints
    result = minimize(lognormal_neg_log_likelihood, initial_params, args=(data,),
                      constraints=cons, method='SLSQP', options={'disp': False})

    if result.success:
        return result.x
    else:
        raise RuntimeError("Optimization failed: " + result.message)

# -------------------------------------------------------------------------------------------------------------------- #
# Main
# -------------------------------------------------------------------------------------------------------------------- #

# Read in data
aem_wide = read_xyz(aem_sharp_file, 26, x_col='UTMX', y_col='UTMY', delim_whitespace=True)
litho = pd.read_csv(aem_litho_file)
obs_wls = pd.read_csv(wl_file)

# Read in shapefiles
aem_shp = gpd.read_file(aem_sharp_sv_file)
aem_hqwells_shp = gpd.read_file(aem_hqwells_file)
aem_hqwells_shp.set_index('WELLINFOID', inplace=True)

# Get Water Levels at log locs
ok = OrdinaryKriging(obs_wls['UTM_x'], obs_wls['UTM_y'], obs_wls['DTW_m'],
                     variogram_model='spherical',
                     variogram_parameters=[42, 4500, 0.0], enable_plotting=False, nlags=30)
aem_shp['ok_dtw'], ok_ss = ok.execute('points', n_closest_points=18, backend='loop',
                           xpoints=aem_shp.geometry.x,
                           ypoints=aem_shp.geometry.y)
aem_shp['ok_wl'] = aem_shp['ELEVATION'] - aem_shp['ok_dtw']

# Join logs to AEM data based on nearest
aem_hqwells_shp = aem_hqwells_shp.sjoin_nearest(aem_shp, how='inner',
                                                rsuffix='aem',
                                                distance_col='dist')

# Subset AEM based on Scott Valley, make long version
aem_wide = aem_wide[aem_wide['LINE_NO'].isin(aem_shp['LINE_NO'])]
aem_long = aem_wide2long(aem_wide,
                         id_col_prefixes=['RHO_I', 'RHO_I_STD', 'SIGMA_I', 'DEP_TOP', 'DEP_BOT', 'THK', 'THK_STD', 'DEP_BOT_STD'],
                         line_col='LINE_NO')

# Calculate Elevations for well logs
litho['ELEV_TOP'] = litho['GROUND_SURFACE_ELEVATION_m'] - litho['LITH_TOP_DEPTH_m']
litho['ELEV_BOT'] = litho['GROUND_SURFACE_ELEVATION_m'] - litho['LITH_BOT_DEPTH_m']
litho['overlap'] = False
# -------------------------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------------------------- #
lith_use = litho.copy()
lith_use['UID'] = range(1, len(lith_use) + 1)

# Limit Distance
aem_wells_use = aem_hqwells_shp[aem_hqwells_shp['dist'] <= 500.0]  # aem_hqwells_shp.copy()

# Initialize an empty list to store the data
overlapping_data = []

# Iterate over each well log entry to find overlapping AEM data
for id, loc in tqdm(aem_wells_use.iterrows(), total=aem_wells_use.shape[0]):
    log = lith_use[lith_use['WELL_INFO_ID'] == id].copy()
    paem_long = aem_long[(aem_long['LINE_NO'] == loc['LINE_NO']) & (aem_long['FID'] == loc['FID'])]

    for j, pixel in paem_long.iterrows():
        if (pixel['ELEVATION'] - pixel['DEP_BOT']) > loc['ok_wl']: continue

        log['overlap'] = (log['LITH_BOT_DEPTH_m'] - pixel['DEP_TOP'] > 0) & (pixel['DEP_BOT'] - log['LITH_TOP_DEPTH_m'] >= 0)

        overlapping_logs = log[log['overlap']]

        for k, overlapped_log in overlapping_logs.iterrows():
            if overlapped_log['Texture'] == 'unknown': continue
            # Capture the overlapping information
            overlapping_data.append({
                'WELL_INFO_ID': id,
                'LINE_NO': loc['LINE_NO'],
                'FID': loc['FID'],
                'UID': overlapped_log['UID'],
                'Texture': overlapped_log['Texture'],
                'Texture_Qualifier': overlapped_log.get('Texture_Qualifier', None),
                'Primary_Texture_Modifier': overlapped_log.get('Primary_Texture_Modifier', None),
                'Secondary_Texture_Modifier': overlapped_log.get('Secondary_Texture_Modifier', None),
                'Classification': overlapped_log.get('Classification', None),
                'rho': pixel['RHO_I'],
                'rho_std': pixel['RHO_I_STD'],
                'ELEVATION': pixel['ELEVATION'],
                'DEP_TOP': pixel['DEP_TOP'],
                'DEP_BOT': pixel['DEP_BOT'],
                'dist': loc['dist']
            })

# Convert the list of dictionaries to a DataFrame
overlapping_df = pd.DataFrame(overlapping_data)

# Some forced classification
overlapping_df.loc[overlapping_df['Texture']=='shale','Classification']    = 'fine'
overlapping_df.loc[overlapping_df['Texture']=='top soil','Classification'] = 'fine'
overlapping_df.loc[overlapping_df['Texture']=='rock','Classification']     = 'coarse'
overlapping_df = overlapping_df.loc[overlapping_df['Texture']!='top soil']

# -------------------------------------------------------------------------------------------------------------------- #
# Great time to see the mess:
# overlapping_df.boxplot(column='rho', by='Texture')
# overlapping_df.boxplot(column='rho', by=['Texture','Primary_Texture_Modifier'])
#overlapping_df.boxplot(column='rho', by=['Texture','Primary_Texture_Modifier','Secondary_Texture_Modifier'])

# Data Exploration w/ Boxplots
# experiment = overlapping_df.copy()
# experiment.loc[experiment['Primary_Texture_Modifier'].isna(),'Primary_Texture_Modifier'] = 'none'
# experiment.loc[experiment['Secondary_Texture_Modifier'].isna(),'Secondary_Texture_Modifier'] = 'none'
# experiment.loc[experiment['Texture_Qualifier'].isna(),'Texture_Qualifier'] = 'none'
# experiment['combined'] = (experiment['Texture'] + ', ' +
#                                    experiment['Primary_Texture_Modifier'] + ', ' +
#                                    experiment['Secondary_Texture_Modifier'])
# experiment = experiment.sort_values('combined')
# # experiment.boxplot(column='rho', by=['Texture','Primary_Texture_Modifier','Secondary_Texture_Modifier'])
# # plt.xticks(rotation=45)
# sns.catplot(x='combined', y='rho', data=experiment, kind='boxen', color='white')
# sns.stripplot(experiment, x="combined", y="rho", size=4, color="orangered")
# plt.ylim((0,500))
# plt.xticks(rotation=90)  # Rotate the x-axis labels for readability
# plt.xlabel('Combined Categories - Texture, Primary Modifier, Secondary Modifier')
# plt.ylabel('Resistivity (rho)')
# plt.tight_layout()  # Adjust layout to make room for label rotation
# -------------------------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------------------------- #
# Cluster Analysis
columns_to_use = ['Texture', 'Classification','Primary_Texture_Modifier','rho']  #, 'rho', 'dist']
numeric_cols = ['rho']  #['rho','dist']
data = overlapping_df[columns_to_use]
nclus = (3,7)
nmeta = 5

# Encode categorical variables (assuming they are all object type)
data_encoded = pd.get_dummies(data.drop(numeric_cols, axis=1))

# Standardize the 'rho' column
scaler = StandardScaler()
data_encoded[numeric_cols] = scaler.fit_transform(data[numeric_cols])

data_matrix = data_encoded.values

# Indices of categorical columns (after one-hot encoding)
categorical_indices = range(data.drop(numeric_cols, axis=1).shape[1], data_encoded.shape[1])

# Do clustering, many times
num_runs = 25  #50
n_clalgs = 5

cluster_assignments = np.zeros((overlapping_df.shape[0], num_runs*n_clalgs))
for run in tqdm(range(num_runs)):
    base_idx = run * n_clalgs

    # k-prototypes
    kproto = KPrototypes(n_clusters=np.random.randint(*nclus), verbose=0, max_iter=20, random_state=run)
    clusters = kproto.fit_predict(data_matrix, categorical=list(categorical_indices))
    cluster_assignments[:, base_idx] = clusters

    # k-prototypes
    kproto = KPrototypes(n_clusters=np.random.randint(*nclus), init='Huang', verbose=0, max_iter=20, random_state=run)
    clusters = kproto.fit_predict(data_matrix, categorical=list(categorical_indices))
    cluster_assignments[:, base_idx + 1] = clusters

    # For Agglomerative Clustering, we compute a pairwise distance matrix
    dist_matrix = pairwise_distances(data_matrix)
    ag = AgglomerativeClustering(n_clusters=np.random.randint(*nclus), metric='precomputed', linkage='complete')
    cluster_assignments[:, base_idx + 2] = clusters

    # DBSCAN
    dbscan = DBSCAN(eps=1.5, min_samples=5)
    clusters = dbscan.fit_predict(data_matrix)
    # Handling noise by assigning them to a new cluster
    clusters[clusters == -1] = max(clusters.max() + 1, 0)
    cluster_assignments[:, base_idx + 3] = clusters

    # Spectral
    spectral = SpectralClustering(n_clusters=np.random.randint(*nclus), affinity='nearest_neighbors', random_state=run)
    clusters = spectral.fit_predict(data_matrix)
    cluster_assignments[:, base_idx + 4] = clusters

# Similarity Matrix to analyze cluster stability
similarity_matrix = np.zeros((overlapping_df.shape[0], overlapping_df.shape[0]))
for i in range(overlapping_df.shape[0]):
    for j in range(overlapping_df.shape[0]):
        similarity_matrix[i, j] = np.sum(cluster_assignments[i, :] == cluster_assignments[j, :]) / num_runs

# Consensus Clustering
meta_cluster = AgglomerativeClustering(n_clusters=nmeta, metric='precomputed', linkage='complete')
meta_cluster.fit(1 - similarity_matrix)  # Use dissimilarity

#-- Reorder Clusters low to high
overlapping_df['meta_cluster'] = meta_cluster.labels_
cluster_means = overlapping_df.groupby('meta_cluster')['rho'].mean()
cluster_order = cluster_means.rank(method='dense').astype(int)
overlapping_df['cluster'] = overlapping_df['meta_cluster'].map(cluster_order.to_dict())

#overlapping_df['cluster'] = overlapping_df['meta_cluster'].replace({0: 3, 1: 0, 2: 2, 3: 1, 4: 4})
# -------------------------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------------------------- #
# # Brief aside - what is the correct choice of eps for DBSCAN?
# k = 5 - 1  # k  should be min_samples - 1
#
# # Initialize the NearestNeighbors model
# nn = NearestNeighbors(n_neighbors=k + 1, algorithm='auto').fit(data_matrix)
#
# # Compute the distances of the k-th nearest neighbors for each point
# distances, indices = nn.kneighbors(data_matrix)
#
# # The distance to the k-th nearest neighbor
# kth_distances = distances[:, k]
#
# # Sort the distances
# sorted_kth_distances = np.sort(kth_distances)
#
# # Plot the k-distance plot
# plt.figure(figsize=(10, 6))
# plt.plot(sorted_kth_distances)
# plt.ylabel(f'Distance to {k}-th Nearest Neighbor')
# plt.xlabel('Points sorted by distance to {k}-th nearest neighbor')
# plt.title('k-Distance Plot')
# plt.grid(True)
# plt.show()
# -------------------------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------------------------- #
# Summary statistics by cluster
# with pd.option_context('display.max_rows', None, 'display.max_columns', None):
#     for i in range(kproto.n_clusters):
#         cluster_data = overlapping_df[overlapping_df['cluster'] == i]
#         print(f"Cluster {i}:")
#         print(cluster_data.describe(include='all'))

data_for_viz = data.copy()
data_for_viz['cluster'] = overlapping_df['cluster']

#sns.pairplot(data_for_viz, hue='cluster')

plt.style.use('default')
sns.violinplot(x='cluster', y='rho', data=data_for_viz)
plt.title('Violin Plot of rho by Cluster')
plt.savefig(plt_dir / '01_violin_plot_rho_by_cluster.png', dpi=300, bbox_inches='tight')

fig, axs = plt.subplots(nrows=len(columns_to_use), figsize=(10, 15))

for i, col in enumerate(columns_to_use):  # Skip 'rho'
    sns.countplot(x='cluster', hue=col, data=data_for_viz, ax=axs[i])
    axs[i].set_title(f'Counts of {col} by Cluster')
    axs[i].legend(title=col, bbox_to_anchor=(1.05, 1), loc='upper left')

plt.tight_layout()
plt.savefig(plt_dir / '01_count_plots_by_cluster.png', dpi=300, bbox_inches='tight')

# from sklearn.manifold import TSNE
#
# tsne = TSNE(n_components=2, verbose=1, perplexity=40, n_iter=300)
# tsne_results = tsne.fit_transform(data_encoded.drop('rho', axis=1))
#
# plt.figure(figsize=(8, 8))
# sns.scatterplot(x=tsne_results[:,0], y=tsne_results[:,1], hue=overlapping_df['cluster'], palette='viridis', alpha=0.5)
# plt.title('t-SNE Visualization of Clusters')

# Visualize Similarity Matrix (how stable are clusters?)
plt.figure(figsize=(10, 8))
sns.heatmap(similarity_matrix, cmap='viridis', xticklabels=False, yticklabels=False)
plt.title('Similarity Matrix Heatmap')
plt.xlabel('Clusters')
plt.ylabel('Clusters')
plt.savefig(plt_dir / '01_similarity_matrix_heatmap.png', dpi=300, bbox_inches='tight')

# -------------------------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------------------------- #

# Statistical measures

# Calculate the average similarity for each data point
average_similarity = np.mean(similarity_matrix, axis=1)
print("Average similarity score per data point:", average_similarity.mean())

# Assuming you have a way to convert 'data_matrix' back to its original space if needed for silhouette
# If 'data_matrix' itself can be directly used, then just use it as is
silhouette_avg = silhouette_score(data_matrix, meta_cluster.labels_)
print("Average silhouette score for consensus clustering:", silhouette_avg)

# CDF of the similarity scores
# Flatten the updated similarity matrix for CDF calculation
flat_similarity_scores = similarity_matrix[np.triu_indices_from(similarity_matrix, k=1)]
sorted_scores = np.sort(flat_similarity_scores)

# # Calculate the CDF values
# cdf = np.arange(1, len(sorted_scores)+1) / len(sorted_scores)
#
# # Plotting the CDF
# plt.figure(figsize=(8, 6))
# plt.plot(sorted_scores, cdf, marker='.', linestyle='none')
# plt.xlabel('Similarity Score')
# plt.ylabel('CDF')
# plt.title('CDF of Updated Similarity Scores')
# plt.grid(True)

# -------------------------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------------------------- #
# Build System of Equations

tlog_src = []
tlog_dis = []
tex_list = []

loc = aem_wells_use.iloc[2]
id = aem_wells_use.index[2]

for id, loc in tqdm(aem_wells_use.iterrows()):
    # Get log for this wellid
    log = lith_use[lith_use['WELL_INFO_ID'] == id].copy()

    # Get log corresponding to nearest AEM point data (from wide data, b/c slightly easier to work with)
    paem_long = aem_long[(aem_long['LINE_NO']==loc['LINE_NO']) & (aem_long['FID']==loc['FID'])]

    # Calc distance
    log_pixel_dist = loc

    # Loop over pixels below the water table
    for j, pixel in paem_long.iterrows():
        if (pixel['ELEVATION']-pixel['DEP_BOT']) > loc['ok_wl']:
            continue
        # Get litho overlaps
        log['overlap'] = (log['LITH_BOT_DEPTH_m'] - pixel['DEP_TOP'] > 0) & (pixel['DEP_BOT'] - log['LITH_TOP_DEPTH_m'] >= 0)
        if log['overlap'].any():
            thicks = np.zeros(nmeta)
            # Need to add up thickness for each cluster
            for k, intv in log[log.overlap].iterrows():
                if intv['Texture'] in ['top soil', 'unknown'] : continue
                cluster = overlapping_df.loc[overlapping_df.UID == intv.UID, 'cluster'].iloc[0]
                thick = min(pixel['DEP_BOT'], intv['LITH_BOT_DEPTH_m']) - max(pixel['DEP_TOP'], intv['LITH_TOP_DEPTH_m'])
                thicks[cluster-1] += thick
            # put in AEM cell class
            tex_list.append(AEMCell(rho_aem=pixel['RHO_I'],
                                    rho_std=pixel['RHO_I_STD'],
                                    t_aem=pixel['THK'],
                                    t_clusters=thicks,
                                    dist=loc['dist']))

# -------------------------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------------------------- #
# Initialize Matrix
rho_dict = {i:[] for i in range(0,nmeta)}
ensemble_size = len(tex_list)
a = np.zeros(ensemble_size*nmeta).reshape(ensemble_size, nmeta)
b = np.zeros(ensemble_size)
w = np.zeros(ensemble_size)

# Bootstrap Solve
for s in tqdm(range(0,1000)):
    ensemble = np.random.choice(tex_list, size=ensemble_size, replace=True)

    # Fill arrays
    for i, row in enumerate(ensemble):
        #a[i], b[i] = row.stoc_values()
        a[i], b[i], w[i] = row.values_weighted()

    # Solve
    #x = linalg.lstsq(a, b)
    #x, rnorm = nnls(a, b)
    model = sm.WLS(b, a, w)
    results = model.fit()
    x = results.params
    if sum(x < 0) > 0: continue  # skip invalid solutions

    # Save solution
    for i,val in enumerate(x):
        rho_dict[i].append(val**-1)

# -------------------------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------------------------- #

# Rename Clusters
cluster_names = ['1 - Fine-grained', '2 - Mixed Fine', '3 - Sand', '4 - Mixed Coarse', '5 - Very Coarse']
tex_names = [name.split('-')[1].strip().replace(' ','_') for name in cluster_names]

# Loop over data adding to histogram, fit and save dist
plt.style.use('seaborn-v0_8-colorblind')
hplt, hax = plt.subplots(figsize=(12, 8))
fit_dists = {}
hax.grid(which='both', linestyle='-', linewidth='0.5', color='lightgrey')
hist_patches = []
# order by median....

for tex in sorted(rho_dict.keys(), key=lambda k: np.median(rho_dict[k])):  # by median...

    bins = np.logspace(np.log10(np.min(rho_dict[tex])), np.log10(np.max(rho_dict[tex])), num=40)
    ptch = hax.hist(rho_dict[tex], bins=bins, alpha=0.5, density=True, zorder=2, label=cluster_names[tex])
    hist_patches.extend(ptch[2])

    if tex<4:
        shape, loc, scale = lognorm.fit(rho_dict[tex], floc=0)
    else:
        shape, loc, scale = fit_lognormal_with_constraints(rho_dict[tex])
    x = np.linspace(0, 500, 1000)
    y = lognorm.pdf(x, s=shape, loc=loc, scale=scale)
    plt.plot(x, y, zorder=2, color=ptch[2][0].get_facecolor())
    fit_dists[tex] = (shape, loc, scale)
    # For Later
    #print(tex, ptch[2][0].get_facecolor())
    print(tex, shape, loc, scale)
hax.set_xscale('log')
medians = [round(np.median(rho_dict[tex])) for tex in rho_dict.keys()]
newticks = [10,20,50,100,150,200,300,400]
plt.xticks(newticks, [f'{t}' for t in newticks])
hax.set_xlim(10,400)

hax.set_xlabel(r'Resistivity (log scale), $\rho$', fontsize=15)
hax.set_ylabel('Density', fontsize=15)
hax.legend(fontsize=13, title='Texture Clusters', title_fontsize=15)
hplt.tight_layout()
plt.savefig(plt_dir / '01_histogram_resistivity_clusters.png', dpi=300, bbox_inches='tight')
# -------------------------------------------------------------------------------------------------------------------- #

# Write distribution parameter file
#dist_df.to_csv(out_dir / 'lognorm_dist.par', sep='   ')
with open(out_dir / 'lognorm_dist_clustered.par', 'w') as f:
    f.write(f"{len(fit_dists.keys())}    # Number of texture classes\n")
    f.write(f"{'Texture':>15}{'Shape':>12}{'Location':>12}{'Scale':>12}\n")
    for tex in fit_dists.keys():
        f.write(f"{tex_names[tex]:15}{fit_dists[tex][0]:12.6f}{fit_dists[tex][1]:12.6f}{fit_dists[tex][2]:12.6f}\n")
# -------------------------------------------------------------------------------------------------------------------- #

# Compute std dev of bootstrap mean estimates from rho_dict
std_boot = {texture: np.std(rho_dict[texture]) for texture in rho_dict}

n_samples = 497  # Actual dataset size
z_score = 1.96  # 95% confidence interval

results = []
for tex, (shape, loc, scale) in fit_dists.items():
    bootstrap_values = np.sort(rho_dict[tex])  # Sort bootstrap estimates

    # Get 95% confidence bounds using percentiles
    mean_low = np.percentile(bootstrap_values, 2.5)
    mean_high = np.percentile(bootstrap_values, 97.5)

    # Compute bounds for scale parameter
    scale_min = (mean_low - loc) / np.exp((shape**2) / 2.0)
    scale_max = (mean_high - loc) / np.exp((shape**2) / 2.0)

    # Optionally expand range by ±10%
    scale_min *= 0.9
    scale_max *= 1.1

    results.append((tex, scale_min, scale_max))

# Print results
print(f"{'Texture':<15} {'Scale Min':>10} {'Scale Max':>10}")
for r in results:
    print(f"{r[0]:<15} {r[1]:10.2f} {r[2]:10.2f}")

with open(out_dir / 'lognorm_dist_clustered_scale_ranges.dat', 'w') as f:
    f.write(f"{'Texture':>15}{'ScaleMin':>12}{'ScaleMax':>12}\n")
    for i, r in enumerate(results):
        f.write(f"{tex_names[i]:15} {r[1]:10.2f} {r[2]:10.2f}\n")

# -------------------------------------------------------------------------------------------------------------------- #
