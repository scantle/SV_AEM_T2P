import geopandas as gpd
from aem_plot.scalebar import add_scalebar, add_northarrow

class LinesMap(object):
    def __init__(self, ax, lines: gpd.GeoDataFrame, n_arrow_x, n_arrow_y, scalebar_x, scalebar_y,
                 points: gpd.GeoDataFrame=None, img=None, img_extent=None):
        self.line_gdf = lines
        self.line_gdf['c'] = 'grey'
        if img is not None:
            ax.imshow(img, extent=img_extent)
        self.line_gdf.plot(ax=ax, color=self.line_gdf['c'], lw=1)
        self.lines = ax.collections[0]
        ax.set_axis_off()
        if points is not None:
            self.point_gdf = points
            self.point_gdf['c'] = 'lightgrey'
            self.point_gdf.plot(ax=ax, color=self.point_gdf['c'], markersize=2)
            self.points = ax.collections[1]
        add_northarrow(ax, n_arrow_x, n_arrow_y)
        add_scalebar(ax, scalebar_x, scalebar_y, 2)
        ax.set_xlim(img_extent[0:2])
        ax.set_ylim(img_extent[2:4])

    def color_line(self, line, color='b', width=2):
        self.line_gdf['c'] = 'grey'
        self.line_gdf.loc[line, 'c'] = color
        self.line_gdf['w'] = 1
        self.line_gdf.loc[line, 'w'] = width
        self.lines.set_colors(self.line_gdf['c'])
        self.lines.set_linewidths(self.line_gdf['w'])

    def color_points(self, points, color='r', markersize=3):
        self.point_gdf['c'] = 'lightgrey'
        self.point_gdf.loc[points, 'c'] = color
        self.point_gdf['w'] = 1
        self.point_gdf.loc[points, 'mks'] = markersize
        self.points.set_color(self.point_gdf['c'])
        self.points.set_linewidths(self.point_gdf['mks'])
