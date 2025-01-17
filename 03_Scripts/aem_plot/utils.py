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

def plot_doi(ax, df, x_col, y_col, elev_col, fmt='r--', lw=0.9):
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