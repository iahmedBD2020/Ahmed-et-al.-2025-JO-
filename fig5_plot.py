#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov  6 16:57:02 2024

@author: skistiaqueahmed
"""

import numpy as np
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import pandas as pd
import seaborn as sns
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
import cartopy.io.shapereader as shpreader
import cartopy.feature as cfeature
import matplotlib.ticker as tck
from matplotlib.path import Path

# Create a figure with 3 subplots, ensuring the figure size meets the journal's requirements
fig, axs = plt.subplots(1, 3, figsize=(17.4, 23.4), dpi=600, subplot_kw={'projection': ccrs.PlateCarree()})

# Define the extent for all plots
extent = (126.0, 162.0, 26.0, 50.0)

# Define colors for each cluster manually
cluster_colors = {
    '1': 'red',
    '2': 'blue',
    '3': 'green',
    '4': 'orange',
    '5': 'purple',
    '6': 'brown',
    '7': 'pink',
    '8': 'cyan',
    '9': 'navy'
}

# Define markers for each cluster manually
cluster_markers = {
    '1': 'o',     
    '2': '^',     
    '3': '+',     
    '4': 'x',     
    '5': 'D',     
    '6': 'v',     
    '7': None,    
    '8': '*',     
    '9': 'h'      
}

# Create custom marker for cluster 7
verts = [
    (-0.2, -0.2), (0.2, -0.2), (0.2, 0.2), (-0.2, 0.2), (-0.2, -0.2),  
    (-0.2, -0.2), (0.2, 0.2),  
    (-0.2, 0.2), (0.2, -0.2)   
]
codes = [
    Path.MOVETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.CLOSEPOLY,
    Path.MOVETO, Path.LINETO,
    Path.MOVETO, Path.LINETO
]
cross_inside_square = Path(verts, codes)

def plot_clusters(ax, data_path, label):
    ax.set_extent(extent, ccrs.PlateCarree())
    shpfilename = shpreader.natural_earth(resolution='10m', category='cultural', name='admin_0_countries')
    reader = shpreader.Reader(shpfilename)
    countries = reader.records()
    
    for country in countries:
        if country.attributes['ADM0_A3'] == 'JPN':
            ax.add_geometries(country.geometry, ccrs.PlateCarree(),
                              facecolor='#A9A9A9',
                              label=country.attributes['ADM0_A3'], zorder=20)

    ax.coastlines(resolution='10m', linewidth=0.8, zorder=3)
    ax.add_feature(cfeature.LAND, color='#A9A9A9', zorder=2)
    ax.add_feature(cfeature.OCEAN, color='white', zorder=1)
    ax.add_feature(cfeature.COASTLINE, linewidth=0.8)

    gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True, linewidth=0, color='k', alpha=1.0, linestyle='--')
    gl.xlocator = tck.FixedLocator(np.arange(126, 162, 5))
    gl.ylocator = tck.FixedLocator(np.arange(26, 50, 5))
    gl.xformatter = LONGITUDE_FORMATTER
    gl.yformatter = LATITUDE_FORMATTER

    point_df = pd.read_csv(data_path)

    for cluster in sorted(point_df['Cluster'].unique(), key=int):
        subset = point_df[point_df['Cluster'] == cluster]
        if cluster == 7:
            ax.scatter(
                x=subset['Longitude'],
                y=subset['Latitude'],
                color=cluster_colors[str(cluster)],
                marker=cross_inside_square,
                edgecolor='k',
                alpha=1.0,
                label=str(cluster),
                zorder=25,
                transform=ccrs.PlateCarree()
            )
        else:
            sns.scatterplot(
                x=subset['Longitude'],
                y=subset['Latitude'],
                color=cluster_colors[str(cluster)],
                marker=cluster_markers[str(cluster)],
                edgecolor='k',
                alpha=1.0,
                label=str(cluster),
                zorder=25,
                ax=ax
            )

    legend = ax.legend(
        title='Cluster',
        bbox_to_anchor=(0.98, 0.02),
        loc='lower right',
        borderaxespad=0.,
        facecolor='white',
        edgecolor='black'
    )

    for handle in legend.legendHandles:
        handle.set_sizes([30])

    plt.setp(legend.get_title(), fontsize=12, fontweight='bold')
    plt.setp(legend.get_texts(), fontsize=10)
    ax.set_xticklabels(ax.get_xticklabels(), fontsize=12)
    ax.set_yticklabels(ax.get_yticklabels(), fontsize=12)
    ax.text(-0.11, 0.5, 'Latitude', va='bottom', ha='center', rotation='vertical', rotation_mode='anchor', fontsize=14, fontweight='bold', transform=ax.transAxes)
    ax.text(0.5, -0.15, 'Longitude', va='bottom', ha='center', rotation='horizontal', rotation_mode='anchor', fontsize=14, fontweight='bold', transform=ax.transAxes)
    ax.text(.25, 0.55, 'Sea of Japan', va='bottom', ha='center', rotation=0, color='#808080', fontsize=12, fontweight='bold', fontstyle='italic', transform=ax.transAxes)
    ax.text(.57, 0.3, 'Pacific Ocean', va='bottom', ha='center', rotation=0, color='#808080', fontsize=12, fontweight='bold', fontstyle='italic', transform=ax.transAxes)
    ax.text(.35, 0.37, 'Japan', va='bottom', ha='center', rotation=36, color='white', fontsize=12, fontweight='bold', fontstyle='italic', transform=ax.transAxes, zorder=25)
    
    # Place the subplot label (a), (b), or (c) in the top left corner
    ax.text(0.02, 0.98, label, va='top', ha='left', fontsize=16, fontweight='bold', transform=ax.transAxes)

    # Adjust the x and y labels with padding
    ax.set_xlabel('Longitude', fontsize=14, labelpad=20)
    ax.set_ylabel('Latitude', fontsize=14, labelpad=20)

# Plot each cluster data
plot_clusters(axs[0], '/Users/skistiaqueahmed/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Cluster analysis/Cluster_map/Without_ZA/Jaccard_new/Full_N_Final/Bucket_cluster_map_new.csv', '(a)')
plot_clusters(axs[1], '/Users/skistiaqueahmed/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Cluster analysis/Cluster_map/Without_ZA/Jaccard_new/Full_N_Final/Intake_cluster_map_new.csv', '(b)')
plot_clusters(axs[2], '/Users/skistiaqueahmed/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Cluster analysis/Cluster_map/Without_ZA/Jaccard_new/Full_N_Final/Niskin_cluster_map_new.csv', '(c)')

# Adjust layout and save the figure
plt.tight_layout()
plt.savefig('/Users/skistiaqueahmed/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Cluster analysis/Cluster_map/Without_ZA/Jaccard_new/Full_N_Final/Joint_figure/Combined_cluster_map3.png', format='png', dpi=600, bbox_inches='tight')

plt.show()
