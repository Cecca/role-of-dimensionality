#
# Invoke as python3 ann_benchmark/plotting/lid.py *-lid.txt


from pprint import pprint
import sys
import matplotlib
import matplotlib.collections as mcoll
# I get a crash without this line
matplotlib.use('Agg')
from plotnine import *
from plotnine.geoms.geom import geom
from plotnine.utils import to_rgba, make_line_segments, SIZE_FACTOR
import numpy as np
import pandas as pd
import seaborn as sns
from pandas.api.types import CategoricalDtype

from scipy.interpolate import interp1d

from plotnine.utils import groupby_apply, interleave, resolution
from plotnine.geoms.geom_polygon import geom_polygon
from plotnine.geoms.geom_path import geom_path
from plotnine.geoms.geom import geom
from plotnine.geoms.geom_violin import make_quantile_df


def lid(path):
    """Plot the distribution of the LID score from the given dataset"""
    data = pd.read_csv(path, sep=' ', names=['query', 'lid'])

    percentiles = np.percentile(data.lid, (5,50,95))
    # Use the default mapping, unless we sample things. In that case we will
    # add the weight to the mapping
    geom_density_mapping = aes()

    # if it's too big, sample it
    threshold = 200000
    num_to_keep = 1000
    if len(data) > threshold:
        to_keep_boundaries = np.sort(data.lid)[[num_to_keep,-num_to_keep]]
        to_keep = data[
            (data['lid'] <= to_keep_boundaries[0]) &\
            (data['lid'] >= to_keep_boundaries[1])
        ]
        sample_size = threshold
        print("The dataset is too big (",len(data)," points), sampling", sample_size, "points")
        print("Keeping all points out of the boundaries:", to_keep_boundaries)
        to_sample = data[
            (data['lid'] > to_keep_boundaries[0]) &\
            (data['lid'] < to_keep_boundaries[1])
        ]
        weight = len(to_sample) / sample_size
        sampled = to_sample.sample(sample_size)
        sampled['weight'] = weight
        to_keep['weight'] = 1
        data = pd.concat([to_keep, sampled])
        geom_density_mapping = aes(weight='weight')
        rugdata = pd.concat([
            sampled[sampled['lid'] < percentiles[2]],
            data[data['lid'] >= percentiles[2]]
        ])

    g = (ggplot(data, aes(x='lid'))
         + geom_density(mapping=geom_density_mapping,
                        color='red', fill='red', alpha=0.6)
         + geom_rug(data=rugdata)
         + geom_vline(xintercept=percentiles[0], linetype='dashed')
         + geom_vline(xintercept=percentiles[1], color='black')
         + geom_vline(xintercept=percentiles[2], linetype='dashed')
         + xlab('lid')
         + theme_bw()
         + theme(figure_size=(10,3)))

    g.draw()
    g.save('{}.png'.format(path), dpi=300)

  
class geom_comb(geom):
    DEFAULT_AES = {'alpha': 1, 'color': 'black', 'size': 0.5,
                   'linetype': 'solid'}
    DEFAULT_PARAMS = {'stat': 'identity', 'position': 'identity',
                      'na_rm': False, 'sides': 'bl', 'length': 0.03,
                      'draw_x': True, 'draw_y': True}
    legend_geom = 'path'

    @staticmethod
    def draw_group(data, panel_params, coord, ax, **params):
        data = coord.transform(data, panel_params)
        data['size'] *= SIZE_FACTOR

        has_x = 'x' in data.columns
        has_y = 'y' in data.columns

        if has_x or has_y:
            n = len(data)
        else:
            return

        rugs = []
        sides = params['sides']
        xmin, xmax = panel_params['x_range']
        ymin, ymax = panel_params['y_range']
        xheight = (xmax-xmin)*params['length']
        yheight = (ymax-ymin)*params['length']

        if params['draw_x']:
            x = np.repeat(data['x'].values, 2)
            y = interleave(data['y'], data['y'] + yheight)
            rugs.extend(make_line_segments(x, y, ispath=False))

        if params['draw_y']:
            x = interleave(data['x'], data['x'] + xheight)
            y = np.repeat(data['y'].values, 2)
            rugs.extend(make_line_segments(x, y, ispath=False))

        color = to_rgba(data['color'], data['alpha'])
        coll = mcoll.LineCollection(rugs,
                                    edgecolor=color,
                                    linewidth=data['size'],
                                    linestyle=data['linetype'],
                                    zorder=params['zorder'])
        ax.add_collection(coll)

def _make_quantile_df(data, draw_quantiles):
    """
    Return a dataframe with info needed to draw quantile segments.
    Modified so to draw quantiles just on half the violin
    """
    dens = data['density'].cumsum() / data['density'].sum()
    ecdf = interp1d(dens, data['y'], assume_sorted=True)
    ys = ecdf(draw_quantiles)

    # Get the violin bounds for the requested quantiles
    violin_xminvs = interp1d(data['y'], data['xminv'])(ys)
    violin_xmaxvs = interp1d(data['y'], data['x'])(ys)
    # violin_xmaxvs = np.repeat(np.array(1.0), len(ys)) # interp1d(data['y'], data['xmaxv'])(ys)

    data = pd.DataFrame({
        'x': interleave(violin_xminvs, violin_xmaxvs),
        'y': np.repeat(ys, 2),
        'group': np.repeat(np.arange(1, len(ys)+1), 2)})

    return data

 
class geom_half_violin(geom):
    DEFAULT_AES = {'alpha': 1, 'color': '#333333', 'fill': 'white',
                   'linetype': 'solid', 'size': 0.5, 'weight': 1}
    REQUIRED_AES = {'x', 'y'}
    DEFAULT_PARAMS = {'stat': 'ydensity', 'position': 'dodge',
                      'draw_quantiles': None, 'scale': 'area',
                      'trim': True, 'width': None, 'na_rm': False,
                      'flip': False}
    legend_geom = 'polygon'

    def setup_data(self, data):
        if 'width' not in data:
            if self.params['width']:
                data['width'] = self.params['width']
            else:
                data['width'] = resolution(data['x'], False) * 0.9

        def func(df):
            df['ymin'] = df['y'].min()
            df['ymax'] = df['y'].max()
            df['xmin'] = df['x'] - df['width']/2
            df['xmax'] = df['x'] + df['width']/2
            return df

        # This is a plyr::ddply
        data = groupby_apply(data, 'group', func)
        return data

    def draw_panel(self, data, panel_params, coord, ax, **params):
        quantiles = params['draw_quantiles']

        for _, df in data.groupby('group'):
            # Find the points for the line to go all the way around
            if params['flip']:
                df['xminv'] = df['x']
                df['xmaxv'] = (df['x'] + df['violinwidth'] *
                               (df['xmax'] - df['x']))
            else:
                df['xminv'] = (df['x'] - df['violinwidth'] *
                               (df['x'] - df['xmin']))
                df['xmaxv'] = df['x']

            n = len(df)
            polygon_df = pd.concat(
                [
                    df.sort_values('y'), 
                    df.sort_values('y', ascending=False)
                ],
                axis=0, ignore_index=True)

            _df = polygon_df.iloc
            _loc = polygon_df.columns.get_loc
            _df[:n, _loc('x')] = _df[:n, _loc('xminv')]
            _df[n:, _loc('x')] = _df[n:, _loc('xmaxv')]

            # Close the polygon: set first and last point the same
            polygon_df.loc[-1, :] = polygon_df.loc[0, :]

            # plot violin polygon
            geom_polygon.draw_group(polygon_df, panel_params,
                                    coord, ax, **params)

            if quantiles:
                # Get dataframe with quantile segments and that
                # with aesthetics then put them together
                # Each quantile segment is defined by 2 points and
                # they all get similar aesthetics
                aes_df = df.drop(['x', 'y', 'group'], axis=1)
                aes_df.reset_index(inplace=True)
                idx = [0] * 2 * len(quantiles)
                aes_df = aes_df.iloc[idx, :].reset_index(drop=True)
                segment_df = pd.concat(
                    [make_quantile_df(df, quantiles), aes_df],
                    axis=1)

                # plot quantile segments
                geom_path.draw_group(segment_df, panel_params, coord,
                                     ax, **params)


def lid2(data, output):

    # sort datasets by difficulty
    dataset_cats = data.groupby('dataset')[['lid']].max().reset_index().sort_values(by='lid')['dataset'].values
    dataset_cats = CategoricalDtype(dataset_cats, ordered=True)
    data['dataset'] = data['dataset'].astype(dataset_cats)

    g = (ggplot(data, aes(x='dataset', y='lid'))
	 + geom_half_violin(aes(weight='weight'),
                            flip=True,
                            scale='width',
                            draw_quantiles=[.25, .5, .75])
         + geom_comb(draw_y=False, length=-0.01)
         + coord_flip()
         + theme_bw()
         + theme(figure_size=(8,6)))
    g.save(output, dpi=300)


def load_dataset(path, keep_top=1000, max_elements=200000):
    data = pd.read_csv(path, sep=' ', names=['query', 'lid'])
    dataset_name = path[:-len("-lid.txt")]
    data['dataset'] = dataset_name
    data['weight'] = 1

    if len(data) > max_elements:
        print('Sampling dataset', dataset_name, 'which has', len(data), 'elements')
        data = data.sort_values(by='lid', ascending=False)
        to_keep = data.iloc[:keep_top]
        to_sample = data.iloc[keep_top:]
        sampled = to_sample.sample(max_elements)
        weight = len(data) / max_elements
        sampled['weight'] = weight
        data = pd.concat([to_keep, sampled])    

    return data


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("USAGE: python3 -m ann_benchmarks.plotting.lid LID_FILES...")
        sys.exit()
    datasets = []
    for path in sys.argv[1:]:
        data = load_dataset(path)
        datasets.append(data)
    data = pd.concat(datasets)
    lid2(data, 'lid.png')
