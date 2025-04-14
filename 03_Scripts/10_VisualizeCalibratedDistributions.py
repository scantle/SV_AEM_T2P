import matplotlib
matplotlib.use('TkAgg')
import json
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import lognorm

# Load rho_dict
with open('./05_Outputs/rho_dict.json', 'r') as f:
    rho_dict = json.load(f)

# Convert keys to int and values to np.array
rho_dict = {int(k): np.array(v) for k, v in rho_dict.items()}

# Load the new fitted lognormal parameters
fit_dists = {}
with open('./05_Outputs/lognorm_dist_clustered_calibrated.par', 'r') as f:
    lines = f.readlines()[2:]  # Skip header
    for line in lines:
        parts = line.split()
        texture = parts[0]
        shape, loc, scale = map(float, parts[1:])
        fit_dists[texture] = (shape, loc, scale)

# Assume this ordering and naming are still used
cluster_names = ['1 - Fine-grained', '2 - Mixed Fine', '3 - Sand', '4 - Mixed Coarse', '5 - Very Coarse']
tex_names = [name.split('-')[1].strip().replace(' ', '_') for name in cluster_names]

# Plot
plt.style.use('seaborn-v0_8-colorblind')
fig, ax = plt.subplots(figsize=(12, 8))

for i, tex in enumerate(sorted(rho_dict.keys(), key=lambda k: np.median(rho_dict[k]))):
    label = cluster_names[tex]
    values = rho_dict[tex]
    bins = np.logspace(np.log10(values.min()), np.log10(values.max()), num=40)
    _, _, patches = ax.hist(values, bins=bins, alpha=0.5, density=True, label=label)

    shape, loc, scale = fit_dists[tex_names[tex]]
    x = np.linspace(0.1, 500, 1000)
    y = lognorm.pdf(x, s=shape, loc=loc, scale=scale)
    ax.plot(x, y, color=patches[0].get_facecolor(), lw=2)

ax.set_xscale('log')
ax.set_xlabel(r'Resistivity (log scale), $\rho$', fontsize=15)
ax.set_ylabel('Density', fontsize=15)
ax.set_xlim(10, 400)
ax.legend(title='Texture Clusters', fontsize=13, title_fontsize=15)
ax.grid(which='both', linestyle='--', linewidth=0.5)
fig.tight_layout()
#plt.savefig('../04_Plots/02_calibrated_histograms.png', dpi=300, bbox_inches='tight')
plt.show()