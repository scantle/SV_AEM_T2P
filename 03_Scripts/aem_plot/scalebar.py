
import matplotlib.text as mtext
import matplotlib.transforms as mtransforms
import numpy as np
from matplotlib import patheffects

class RotationAwareAnnotation2(mtext.Annotation):
    '''
    https://stackoverflow.com/questions/19907140/keeps-text-rotated-in-data-coordinate-system-after-resizing
    '''

    def __init__(self, s, xy, p, ax, pa=None, **kwargs):
        self.ax = ax
        self.p = p
        self.pa = pa or xy
        kwargs.update(rotation_mode=kwargs.get("rotation_mode", "anchor"))
        mtext.Annotation.__init__(self, s, xy, **kwargs)
        self.set_transform(mtransforms.IdentityTransform())
        if 'clip_on' in kwargs:
            self.set_clip_path(self.ax.patch)
        self.ax._add_text(self)

    def calc_angle(self):
        p = self.ax.transData.transform_point(self.p)
        pa = self.ax.transData.transform_point(self.pa)
        ang = np.arctan2(p[1]-pa[1], p[0]-pa[0])
        return np.rad2deg(ang)

    def _get_rotation(self):
        return self.calc_angle()

    def _set_rotation(self, rotation):
        pass

    _rotation = property(_get_rotation, _set_rotation)



def add_scalebar(ax, x, y, length, x1=None, y1=None, unit='miles', unit_factor=5280,
                linewidth=3, color='k', pad=5, zorder=9999, **kwargs):
    '''
    https://stackoverflow.com/questions/32333870/how-can-i-show-a-km-ruler-on-a-cartopy-matplotlib-plot
    '''

    if x1 is None or y1 is None:
        x1 = x + 1
        y1 = y

    len_ = ((x1 - x) ** 2 + (y1 - y) ** 2) ** 0.5
    length1 = length * unit_factor

    x0 = (x - x1) * length1 * 0.5 / len_ + x
    x2 = (x1 - x) * length1 * 0.5 / len_ + x

    y0 = (y - y1) * length1 * 0.5 / len_ + y
    y2 = (y1 - y) * length1 * 0.5 / len_ + y

    buffer = [patheffects.withStroke(linewidth=5, foreground="w")]
    ax.plot([x0, x2], [y0, y2], color=color, linewidth=linewidth, zorder=zorder, path_effects=buffer)

    buffer = [patheffects.withStroke(linewidth=3, foreground="w")]
    RotationAwareAnnotation2(str(length) + ' ' + str(unit), xy=(x, y), p=(x1, y1),
                            ax=ax, xytext=(0, pad), textcoords="offset points",
                            va="bottom", ha='center', zorder=zorder, path_effects=buffer, **kwargs)

def add_northarrow(ax, x, y):
    ax.text(x, y, u'\u25B2\nN', ha='center', va='top',
        path_effects=[patheffects.withStroke(linewidth=5, foreground="w")], )