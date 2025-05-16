#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed May 22 12:02:38 2024

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

xlabel = 'Longitude'
ylabel = 'Latitude'

# Increase the resolution by adjusting figure size and dpi
fig, ax = plt.subplots(figsize=(10, 6), dpi=600, subplot_kw={'projection': ccrs.PlateCarree()})
ax.set_extent((126.0, 162.0, 26.0, 50.0), ccrs.PlateCarree())

shpfilename = shpreader.natural_earth(resolution='10m', category='cultural', name='admin_0_countries')
reader = shpreader.Reader(shpfilename)
countries = reader.records()

for country in countries:
    if country.attributes['ADM0_A3'] == 'JPN':
        ax.add_geometries(country.geometry, ccrs.PlateCarree(),
                          facecolor='#A9A9A9',
                          label=country.attributes['ADM0_A3'], zorder=20)

ax.coastlines(resolution='10m', linewidth=0.8, zorder=3)

ax.add_feature(cfeature.LAND, color='#A9A9A9', zorder=2)  # Set land color
ax.add_feature(cfeature.OCEAN, color='white', zorder=1)  # Set ocean color to white
ax.add_feature(cfeature.COASTLINE, linewidth=0.8)

gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True, linewidth=0, color='k', alpha=1.0)
gl.xlocator = tck.FixedLocator(np.arange(126, 162, 5))
gl.ylocator = tck.FixedLocator(np.arange(26, 50, 5))
gl.xformatter = LONGITUDE_FORMATTER
gl.yformatter = LATITUDE_FORMATTER

point_df = pd.read_csv("/Users/skistiaqueahmed/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Map_by_cruise_83/Map_cruise_83.csv")

# Define colors for each cruise manually
cruise_colors = {
    'KS-22-15': '#FF5733',
    'KS-21-11': '#FFD700',
    'KS-21-12': '#7FFF00',
    'KS-21-3': '#00FFFF',
    'KS-21-8': '#8A2BE2',
    'KS-22-11': '#FF1493',
    'KS-21-24': '#008080',
    'KS-18-5': '#FF4500',
    'KH-23-3': '#00FF00'
}

sns.scatterplot(
    x=point_df['Longitude'],
    y=point_df['Latitude'],
    hue=point_df['Cruise'],
    palette=cruise_colors,
    edgecolor='k',
    alpha=1.0,
    zorder=25
)

legend = plt.legend(
    title='Cruise',
    bbox_to_anchor=(0.98, 0.02),
    loc='lower right',
    borderaxespad=0.,
    facecolor='white',  # Set legend background color to white
    edgecolor='black'   # Set legend edge color to black
)

for handle in legend.legendHandles:
    handle.set_sizes([30])  # Adjust the size here for smaller handles

plt.setp(legend.get_title(), fontsize='10', fontweight='bold')
plt.setp(legend.get_texts(), fontsize='8')
ax.set_xticklabels(ax.get_xticklabels(), fontsize=10)
ax.set_yticklabels(ax.get_yticklabels(), fontsize=10)
ax.text(-0.09, 0.55, 'Latitude', va='bottom', ha='center', rotation='vertical', rotation_mode='anchor', fontsize='12', fontweight='bold', transform=ax.transAxes)
ax.text(0.5, -0.1, 'Longitude', va='bottom', ha='center', rotation='horizontal', rotation_mode='anchor', fontsize='12', fontweight='bold', transform=ax.transAxes)
ax.text(.25, 0.55, 'Sea of Japan', va='bottom', ha='center', rotation=0, color='#808080', fontsize=10, fontweight='bold', fontstyle='italic', transform=ax.transAxes)
ax.text(.57, 0.3, 'Pacific Ocean', va='bottom', ha='center', rotation=00, color='#808080', fontsize=10, fontweight='bold', fontstyle='italic', transform=ax.transAxes)
ax.text(.37, 0.41, 'Japan', va='bottom', ha='center', rotation=30, color='white', fontsize=10, fontweight='bold', fontstyle='italic', transform=ax.transAxes, zorder=25)
plt.savefig("/Users/skistiaqueahmed/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Map_by_cruise_83/map_83_cruise.pdf", dpi=600)
plt.show()
