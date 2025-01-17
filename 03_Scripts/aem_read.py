import pandas as pd
import numpy as np
import re

# -------------------------------------------------------------------------------------------------------------------- #

def get_data_columns(filename, line, delim_whitespace=False):
    with open(filename, 'r') as f:
        for i in range(0, line):
            f.readline()
        if delim_whitespace:
            header = f.readline().strip().strip('/').split()
        else:
            header = f.readline().strip().strip('/').split(',')
    return header

# -------------------------------------------------------------------------------------------------------------------- #

def line_dist(df, x_col, y_col):
    dist = np.sqrt((df[x_col] - df[x_col].iloc[0]) ** 2 + (df[y_col] - df[y_col].iloc[0]) ** 2) / 1000
    #dist *= np.sign(df[x_col] - df[x_col].iloc[0])  # Correct for direction
    return dist

# -------------------------------------------------------------------------------------------------------------------- #

def calc_line_geometry(df, x_col='Easting', y_col='Northing', max_len=100.0):
    # Check if start of line is further east than end of line
    # E.g., flight started on RIGHT side, thus we want to flip it to starts on LEFT
    if df.loc[df.index[0], x_col] > df.loc[df.index[-1], x_col]:    # Formerly max
        df = df.reindex(index=df.index[::-1]).set_index(df.index[::1])
    else:
        # Normal, no need to flip it
        pass
    xy_dists = df[[x_col, y_col]].diff().shift(-1)
    df['LINE_WIDTH'] = np.hypot(xy_dists.values[:,0], xy_dists.values[:,1])
    # Last width is estimated
    df.loc[df.index[-1],'LINE_WIDTH'] = df['LINE_WIDTH'].median()
    df['LINE_DIST'] = df['LINE_WIDTH'].shift(1).cumsum().fillna(0.0)
    # Correct for big gaps (interference) in points
    if max_len is not None:
        df.loc[df['LINE_WIDTH'] > max_len, 'LINE_WIDTH'] = df['LINE_WIDTH'].median()
    return df

# -------------------------------------------------------------------------------------------------------------------- #

def read_xyz(filepath, skiprows=0, x_col='East_M', y_col='North_M', line_col='LINE_NO', delim_whitespace=False):
    header = get_data_columns(filepath, skiprows, delim_whitespace)
    df = pd.read_csv(filepath,
                     skiprows=skiprows+1,
                     sep='\\s+',
                     header=None,
                     names=header,
                     na_values=9999)
    # Sort
    #df = df.sort_values(by=[line_col, x_col, y_col], axis=0)

    # Calculate line distances/widths
    df = df.groupby(line_col, group_keys=False).apply(calc_line_geometry, x_col, y_col)

    return df

# -------------------------------------------------------------------------------------------------------------------- #

def aem_wide2long(df, id_col_prefixes, dist_col='LINE_DIST', line_col='Line'):
    id_dict = {}
    # Separate columns into individual values, all value columns, and id columns
    for c in id_col_prefixes:
        id_dict[c] = [col for col in df.columns if col.startswith(c) and not col.startswith(c + '_STD')]
        id_dict['val_all'] = id_dict.setdefault('val_all', []) + id_dict[c]
    id_dict['ids'] = [col for col in df.columns if col not in id_dict['val_all']]
    # Longify
    ldf = None
    # Sort val columns by number of values - want most "points" first. Otherwise breaks df concatenation
    col_order = np.flip(np.argsort([len(id_dict[key]) for key in id_col_prefixes]))
    id_col_prefixes = [id_col_prefixes[i] for i in col_order]
    for c in id_col_prefixes:
        cdf = pd.melt(df, id_dict['ids'], id_dict[c], 'POINT', c)
        cdf.POINT = cdf.POINT.apply(lambda x: re.findall(r'\d+', x)[0]).astype(int)
        if ldf is None:
            ldf = cdf
        else:
            ldf = pd.concat([ldf, cdf[c]], axis=1)
    ldf = ldf.sort_values(by=[line_col, dist_col, 'POINT'], axis=0)
    # TODO - re-order columns before returning?
    return ldf

# -------------------------------------------------------------------------------------------------------------------- #
