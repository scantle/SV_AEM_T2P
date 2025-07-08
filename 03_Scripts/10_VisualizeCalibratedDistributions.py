import matplotlib
matplotlib.use('TkAgg')
import json
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
from scipy.stats import lognorm

# -------------------------------------------------------------------------------------------------------------------- #
# Settings
# -------------------------------------------------------------------------------------------------------------------- #

# Directories
out_dir = Path('./05_Outputs')
plt_dir = Path('./04_Plots/')
calb_dir = Path('./02_Models/SVIHM_MF_Calibrated/')

# Cluster colors
cc = ['#df263e', '#e37e26', '#e3c128', '#6da14d', '#5289db']

# -------------------------------------------------------------------------------------------------------------------- #
# Main
# -------------------------------------------------------------------------------------------------------------------- #

# Load rho_dict
with open(out_dir / 'rho_dict.json', 'r') as f:
    rho_dict = json.load(f)

# Convert keys to int and values to np.array
rho_dict = {int(k): np.array(v) for k, v in rho_dict.items()}

# Load the new fitted lognormal parameters
fit_dists = {}
with open(calb_dir / 'preproc/AEM2Texture.in', 'r') as f:
    lines = f.readlines()[5:]  # Skip header
    for line in lines:
        parts = line.split()
        texture = parts[0]
        shape, loc, scale = map(float, parts[1:])
        fit_dists[texture] = (shape, loc, scale)

# Assume this ordering and naming are still used
cluster_names = ['1 - Fine-grained', '2 - Mixed Fine', '3 - Sand', '4 - Mixed Coarse', '5 - Very Coarse']
tex_names = [name.split('-')[1].strip().replace(' ', '_') for name in cluster_names]

# Plot
sns.set_context("paper")
sns.set_style("whitegrid")       # y-axis gridlines
plt.style.use('seaborn-v0_8-colorblind')
fig, ax = plt.subplots(figsize=(12, 8))

for i, tex in enumerate(sorted(rho_dict.keys(), key=lambda k: np.median(rho_dict[k]))):
    label = cluster_names[tex]
    values = rho_dict[tex]
    bins = np.logspace(np.log10(values.min()), np.log10(values.max()), num=40)
    _, _, patches = ax.hist(values, bins=bins, alpha=0.5, density=True, label=label, color=cc[i])

    shape, loc, scale = fit_dists[tex_names[tex]]
    x = np.linspace(0.1, 500, 1000)
    y = lognorm.pdf(x, s=shape, loc=loc, scale=scale)
    ax.plot(x, y, color=cc[i], lw=2)

ax.set_xscale('log')
ax.set_xlabel(r'Resistivity (log scale), $\rho$', fontsize=15)
ax.set_ylabel('Density', fontsize=15)
ax.set_xlim(10, 400)
ax.legend(title='Texture Clusters', fontsize=13, title_fontsize=15)
ax.grid(which='both', linestyle='--', linewidth=0.5)
fig.tight_layout()
plt.savefig(plt_dir / '10_calibrated_histograms.png', dpi=300, bbox_inches='tight')
plt.show()