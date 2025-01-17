import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm, LinearSegmentedColormap
import pandas as pd
import numpy as np

cmap_lith = plt.get_cmap('tab20')

lithology = [
    'Conglomerate            ',
    'Granite                 ',
    'Rock                    ',
    'Sandstone               ',
    'Lahar                   ',
    'Lava                    ',
    'Volcanic Ash/Bentonite  ',
    'Gravel/Boulders         ',
    'Sand and Gravel         ',
    'Sand                    ',
    'Silty Sand              ',
    'Silty Clay              ',
    'Sandy Clay              ',
    'Silt                    ',
    'Roadfill and/or Topsoil ',
    'No Sample               ',
    'Clay                    ',
    'Shale                   ',
    'Siltstone               ',
    'Mudstone                ',
]
lithology = [l.strip() for l in lithology]
lithology = pd.Series([cmap_lith(i/len(lithology)) for i, l in enumerate(lithology)], index=lithology)

# https://stackoverflow.com/questions/18926031/how-to-extract-a-subset-of-a-colormap-as-a-new-colormap-in-matplotlib
def truncate_colormap(cmap, minval=0.0, maxval=1.0, n=100):
    if type(cmap) is str:
        cmap = plt.get_cmap(cmap)
    new_cmap = LinearSegmentedColormap.from_list(
        'trunc({n},{a:.2f},{b:.2f})'.format(n=cmap.name, a=minval, b=maxval),
        cmap(np.linspace(minval, maxval, n)))
    return new_cmap


rho_norm = LogNorm(5, 50)
rho_cmap = truncate_colormap('gist_ncar', 0.08, 0.83)

def plot_lith_legend(ax=None, width=0.3):
    ax.barh(range(len(lithology)), height=1, width=width, color=lithology.values)
    for i, l in enumerate(lithology.index):
        ax.text(width/2, i, l, ha='center', va='center') 
    ax.set_axis_off()


if __name__ == '__main__':
    plot_lith_colors = False
    if plot_lith_colors:
        fig, ax = plt.subplots(figsize=(3, 5))
        plot_lith_legend(ax, 0.3)
    
    plot_rho_cmap = False
    
    if plot_rho_cmap:        
        arr = np.linspace(0, 50, 100).reshape((10, 10))
        fig, ax = plt.subplots(ncols=2)

        cmap = plt.get_cmap('jet')
        new_cmap = truncate_colormap(cmap, 0.2, 0.8)
        ax[0].imshow(arr, interpolation='nearest', cmap='gist_ncar')
        ax[1].imshow(arr, interpolation='nearest', cmap=rho_cmap)


#         gradient = np.linspace(5, 1, 51)
#         gradient = np.vstack((gradient, gradient))
        
#         cmap_list = ['gist_ncar_r']
#         nrows = len(cmap_list)
#         figh = 0.35 + 0.15 + (nrows + (nrows - 1) * 0.1) * 0.22
#         fig, axs = plt.subplots(nrows=nrows + 1, figsize=(6.4, figh))
#         fig.subplots_adjust(top=1 - 0.35 / figh, bottom=0.15 / figh,
#                             left=0.2, right=0.99)
#         axs[0].set_title('colormaps', fontsize=14)

#         for ax, name in zip(axs, cmap_list):
#             ax.imshow(gradient, aspect='auto', cmap=plt.get_cmap(name))
#             ax.text(-0.01, 0.5, name, va='center', ha='right', fontsize=10,
#                     transform=ax.transAxes)
#         for ax in axs:
#             ax.set_axis_off()
