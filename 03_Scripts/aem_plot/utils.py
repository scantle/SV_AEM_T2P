"""
Some of these functions were originally written by Michael Ou of S.S. Papadopulos & Assoc.
"""

from matplotlib.patches import Rectangle
from matplotlib.collections import PatchCollection

def df2rectangles(df, x_col, y_col, xthk_col, ythk_col):
    return df.apply(lambda p: Rectangle((p[x_col], p[y_col]), p[xthk_col], p[ythk_col]), axis=1)


def plot_slice_rect(fig, ax, rect, values, cmap='viridis', norm=None, title=None,
                    xlim=None, ylim=None,
                    xlabel=None, ylabel=None,
                    colorbar_label=None, hide_xticks=False, clim: tuple = None):
    ax.clear()
    line_rects = PatchCollection(rect, cmap=cmap, norm=norm)
    line_rects.set_array(values)
    if clim is not None:
        line_rects.set_clim(clim)
    ax.add_collection(line_rects)
    ax.set(title=title,
           xlim=xlim,
           ylim=ylim,
           xlabel=xlabel,
           ylabel=ylabel)
    cb = fig.colorbar(line_rects, ax=ax, label=colorbar_label)
    if hide_xticks:
        ax.get_xaxis().set_ticklabels([])
    return line_rects, cb

def plot_slice_rect_doi(fig, ax, rect, values, doi_values=None, doi_alpha=0.45, cmap='viridis', norm=None,
                    title=None, xlim=None, ylim=None, xlabel=None, ylabel=None,
                    colorbar_label=None, hide_xticks=False, clim: tuple = None):
    """
    Plots rectangles with optional transparency below DOI.

    Parameters:
        fig: matplotlib figure object
        ax: matplotlib axis object
        rect: collection of rectangles
        values: values to color rectangles
        doi_values: array of DOI elevations, same length as `rect` (optional)
        doi_alpha: alpha value for rectangles below DOI
        cmap: colormap for coloring
        norm: normalization for colormap
        title: title for the axis
        xlim, ylim: limits for the x and y axes
        xlabel, ylabel: axis labels
        colorbar_label: label for the colorbar
        hide_xticks: whether to hide x-axis ticks
        clim: tuple for color limits
    Returns:
        line_rects, colorbar
    """
    ax.clear()

    # Create PatchCollection for the rectangles
    line_rects = PatchCollection(rect, cmap=cmap, norm=norm)
    line_rects.set_array(values)
    if clim is not None:
        line_rects.set_clim(clim)

    # Adjust alpha for rectangles below DOI if DOI values are provided
    if doi_values is not None:
        alphas = []
        for rect_obj, doi in zip(rect, doi_values):
            bottom = rect_obj.get_y()  # Bottom y-coordinate of the rectangle
            top = bottom + rect_obj.get_height()  # Top y-coordinate of the rectangle
            if top >= doi:  # Fully above DOI
                alphas.append(1.0)
            elif bottom <= doi:  # Fully below DOI
                alphas.append(doi_alpha)
            else:  # Partially intersecting DOI
                alpha = doi_alpha + (1 - doi_alpha) * (doi - bottom) / rect_obj.get_height()
                alphas.append(alpha)

        # Set alpha values for the PatchCollection
        line_rects.set_alpha(alphas)

    ax.add_collection(line_rects)

    # Set axis properties
    ax.set(title=title, xlim=xlim, ylim=ylim, xlabel=xlabel, ylabel=ylabel)

    # Add a colorbar
    cb = fig.colorbar(line_rects, ax=ax, label=colorbar_label)

    if hide_xticks:
        ax.get_xaxis().set_ticklabels([])

    return line_rects, cb

def plot_line_by_depth(ax, df, x_col, y_col, elev_col, fmt='r--', lw=0.9):
    return ax.plot(df[x_col], df[elev_col]-df[y_col], fmt, lw=lw)

def plot_wl(ax, df, x_col, y_col, elev_col, width_col=None, fmt='r--', lw=0.9, center=True):
    if center is True and width_col is not None:
        return ax.plot((df[x_col] + df[width_col]/2), df[y_col], fmt, lw=lw)
    else:
        return ax.plot(df[x_col], df[y_col], fmt, lw=lw)

def plot_pc_hist(ax, values, color='black', remove_box=True, title=None):
    ax.hist(values, range=(0, 1), color=color)
    if remove_box:
        ax.spines['top'].set_visible(False)
        ax.spines['right'].set_visible(False)
        ax.spines['bottom'].set_visible(False)
        ax.spines['left'].set_visible(False)
    if title is not None:
        ax.set_title(title)