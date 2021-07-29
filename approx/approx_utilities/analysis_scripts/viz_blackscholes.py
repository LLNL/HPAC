#!/usr/bin/env python3
import argparse
from numpy.random import random
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import yaml
from yaml import CLoader
import argparse
import subprocess
import os
import shutil
import time
import re
import sys
import numpy as np
from pathlib import Path
import itertools
import math
import pandas as pd
import seaborn as sns
from matplotlib.lines import Line2D
import numpy as np
import scipy.stats

tex_fonts = {
# Use LaTeX to write all text
"text.usetex": True,
"font.family": "serif",
# Use 10pt font in plots, to match 10pt font in document
"axes.labelsize": 8,
"font.size": 8,
# Make the legend/label fonts a little smaller
"legend.fontsize": 6,
"xtick.labelsize": 8,
"ytick.labelsize": 8
}


def set_size(width, fraction=1):
    """Set figure dimensions to avoid scaling in LaTeX.
    Parameters
    ----------
    width: float
    Document textwidth or columnwidth in pts
    fraction: float, optional
    Fraction of the width which you wish the figure to occupy
    Returns
    -------
    fig_dim: tuple
    Dimensions of figure in inches
    """
    # Width of figure (in pts)
    fig_width_pt = width * fraction
    # Convert from pt to inches
    inches_per_pt = 1 / 72.27
    # Golden ratio to set aesthetic figure height
    # https://disq.us/p/2940ij3
    golden_ratio = (5**.5 - 1) / 2
    # Figure width in inches
    fig_width_in = fig_width_pt * inches_per_pt
    # Figure height in inches
    fig_height_in = fig_width_in * golden_ratio
    fig_dim = (fig_width_in, fig_height_in)
    return fig_dim

plt.rcParams.update(tex_fonts)

region_names = {}

def something(x):
    stdVal = x["Exec. Time"].std()
    avg = x["Exec. Time"].mean()
    length = len(x)

def mean_confidence_interval(data, confidence=0.95):
    a = 1.0 * np.array(data)
    n = len(a)
    m, se = np.mean(a), scipy.stats.sem(a)
    h = se * scipy.stats.t.ppf((1 + confidence) / 2., n-1)
    return m, m-h, m+h

def logMape(vals):
    tmp = vals + 1
    tmp = np.log10(tmp)
    return tmp

region_names["hotspot"] = { "OUTER_STENCIL" : "OUTER-ST-LOOP",
                            "PARALLEL_LOOP" : "PARALLEL-LOOP"
                        }

region_names["blackscholes"] = {    "CNDF_1" : "Cndf-1",
                                    "CNDF_2" : "Cndf-2",
                                    "ENTIRE" : "BlsEq",
                                    "exp"    : "Exp"
                                    }

region_names["CFD"] =  { "compute_flux_contributions" : "CFC",
                         "compute_flux" : "CC",
                        }

region_names["HPCCG"] =  {   "SPARSE_MV_INNER" : "SPMV-IN",
                             "SPARSE_MV_OUTER" : "SPMV-OUT",
                             "waxpby" : "WAXPBY",
                             "ddot" : "DOT",
                             "ddot_r2" : "R2"
                        }

region_names["kmeans"] =    {   "euclid_dist_2" : "ED",
                                "find_nearest_point" : "FN",
                            }

region_names["lavaMD"] = {
        "BODY" : "BODY",
        "ITERATE_PARTICLES_X" : "ITER-X",
        "ITERATE_NEIGHBORS" :   "ITE-N",
        "ITERATE_PARTICLES_Y" : "ITER-P",
        "NUMBER_BOXES" :        "ITER-B"
        }

region_names["leukocyte"] = {
        "find_min" : "FM",
        "update_cell_location" : "UCL",
        "find_diff" : "FD",
        "find_length" : "FL",
        "ellipseevolve" : "EE"
}


region_names["lulesh"] = {
        "CalcElemShapeFunctionDerivatives2":    "SH",
        "CombineDerivativesAndNormals" :        "NO",
        "CalcElemVolumeDerivative" :            "VO",
        "CalcElemFBHourglassForce" :            "HG",
        "CalcElemCharacteristicLength" :        "EL",
        "CalcElemVelocityGradient2" :           "VE",
        "CalcElemVolumeKinematics" :            "VK",
        "COMPUTEPRESSURE" :                     "CO",
        "CalcMonotonicQForElems" :              "EL",
        "CalcEnergyForElems" :                  "EL",
        "CalcSoundSpeedForElems" :              "CA",
        "CalcMonotonicQGradientsForElems" :     "MO",
        "CalcMonotonicQRegionForElems" :        "MO",
        "CalcVelocityForNodes" :                "VE"
        }

region_names["pFilter"]={
        "LIKELIHOOD" : "LIKELIHOOD",
        "WEIGHT_SUM" : "WEIGHT-SUM",
        "findIndex" : "FIND-INDEX",
        }

error_type = { "kmeans" : "MPP(%)",
               "CFD" : "MAPE(%)",
               "lulesh" : "MAPE(%)",
               "HPCCG" : "log(MAPE(%)+1)",
               "leukocyte" : "MAPE(error(%)",
               "pFilter" : "log(MAPE(%)+1)",
               "blackscholes" : "MAPE(%)",
               "lavaMD" : "log(MAPE(%)+1)",
               "hotspot" : "MAPE(%)",
             }


error_type_transform = { "kmeans" : None,
               "CFD" : None,
               "lulesh" : None,
               "HPCCG" : logMape,
               "leukocyte" : None,
               "pFilter" : logMape,
               "blackscholes" : None,
               "lavaMD" : logMape,
               "hotspot" : None
             }

perf_names ={ "iPerfo" : "IP",
              "fPerfo" : "FP",
              "sPerfo" : "SP",
              "lPerfo" : "LP",
              "rPerfo" : "RP",
              "TAF" : "TAF",
              "iACT" : "iACT"
            }

perf_grouped_names ={ "iPerfo" : "PERFO",
              "fPerfo" : "PERFO",
              "sPerfo" : "PERFO",
              "lPerfo" : "PERFO",
              "rPerfo" : "PERFO",
              "TAF" : "TAF",
              "iACT" : "iACT"
            }

def hexToRGB(h):
    return tuple(float(int(h[i:i+2], 16)/255.0) for i in (0, 2, 4))

def scatterPlotOnly(appName, data, output_file, x_name, y_name, hue_col, style_col, v_line=None, h_line=None, x_cut = None, y_cut = None):
    local_data = data.copy()
    if y_cut:
       local_data = local_data[local_data[y_name] < y_cut]
    if x_cut:
       local_data = local_data[local_data[x_name] < x_cut]

    local_data = local_data[local_data[x_name] < 30]

    if (len (local_data) == 0):
        return

    approxTechniques =  ['PERFO', 'TAF', 'iACT']
    regions = local_data[style_col].unique()

    approximation_colors = {}

    markers ={}
    allMarkers=[["d","none"], ["s","none"], ["v","none"]]

    for i, j in enumerate(approxTechniques):
        markers[j] = allMarkers[i]

    tmp_colors2 = sns.color_palette("YlOrBr",len(regions))
    colors ={}
    for c,s in zip(tmp_colors2, regions):
        colors[s] = c

    sizes=set_size(width=textWidth, fraction=0.5)
    fig, ax = plt.subplots(figsize=sizes)

    legend_regions = []
    for r in regions:
        legend_regions.append(Line2D([0], [0], marker='o', lw=0.2,  mec=colors[r],label=r, markerfacecolor=colors[r], linestyle='None'))

    legend_techniques = []
    for t in approxTechniques:
        legend_techniques.append(Line2D([0], [0], lw=0.2, marker=markers[t][0], color=colors[r], markerfacecolor="white",  mec="black", label=t, linestyle='None'))


    for r in regions:
        tmp_data = local_data[local_data[style_col] == r]
        for t in tmp_data[hue_col].unique():
            data= tmp_data[tmp_data[hue_col] == t]
            x= data[x_name].tolist()
            y= data[y_name].to_list()
            if ( markers[t][1] == "none"):
                ax.scatter(x, y, linewidth=0.2, s=4, color=colors[r], marker=markers[t][0], edgecolors=colors[r], facecolors='none')
            else:
                ax.scatter(x, y, linewidth=0.2, s=4, color=colors[r], marker=markers[t][0], edgecolors=colors[r])


#    plot = sns.scatterplot(ax = ax, data=local_data, x=x_name, y=y_name, hue=hue_col, style=style_col, alpha=0.4, palette=approximation_colors, hue_order=approx_tech_order,  style_order=marker_order)
    if ( v_line ):
        ax.axvline(v_line, color='gray', ls='--', lw=1.0)
    if ( h_line):
        ax.axhline(h_line, color='gray', ls='--', lw=1.0)
#    handles0, labels0 = ax.get_legend_handles_labels()
#    print(handles0, labels0)
#    cols = len(labels0) / 2
#    cols = int(cols)
#    plot.legend(handles0, labels0, ncol=cols, loc="lower right")
    y_label =  y_name.replace("%", "\%")
    ax.set( ylabel=y_label)
    ax.set( xlabel=x_name)
    legend1 = ax.legend(handles = legend_techniques, title="Technique", frameon=False, bbox_to_anchor=(0.73,1.0))
    ax.add_artist(legend1)
    legend2 = ax.legend(handles = legend_regions, title="Region", frameon= False)
#    plt.margins(x=0.0, y=0.0)
    plt.tight_layout()
#    plot.figure.savefig(output_file,bbox_inches='tight')
    ax.figure.savefig(output_file,bbox_inches='tight')
    plt.close()
#    handles0, labels0 = plot.get_legend_handles_labels()

print_names = {     "blackscholes" : "Blackscholes",
                    "CFD" : "CFD",
                    "HPCCG" : "HPCCG",
                    "hotspot" : "Hotspot",
                    "lavaMD" : "LavaMD",
                    "lulesh" : "Lulesh",
                    "kmeans" : "K-Means",
                    "pFilter" : "Particle-Filter",
                    "leukocyte" : "Leukocyte"
                    }


if (len(sys.argv) != 2):
    print ("%s input_file" %sys.argv[0])
    sys.exit(0)

textWidth=505.89

input_file = sys.argv[1]
df = pd.read_pickle(input_file)
appName = df["Application"].unique()[0]
rootOut = "sc_paper_plots"

if appName not in region_names:
    print("No information in region names")
    sys.exit(0)

if appName not in  print_names:
    print("No information in print names")
    sys.exit(0)

df["Ratio"] = df["Ratio"].astype(np.float64)
df["App Quality"] = df["App Quality"].astype(np.float64)
df["Exec. Time"] = df["Exec. Time"].astype(np.float64)
df["App Quality"] = df["App Quality"].astype(np.float64)
df["Ratio"] = df["Ratio"].astype(np.float64)
df["Num Threads"] = df["Num Threads"].astype(str)
approx = df

outputDirectory = "%s/" %(rootOut)
os.makedirs( outputDirectory, exist_ok=True )
outPrefix = outputDirectory
approx["Region"] = approx["Label"].map(region_names[appName])
approx[error_type[appName]] = approx["App Quality"] * 100.0
if ( error_type_transform[appName] ):
    approx[error_type[appName]] = error_type_transform[appName](approx[error_type[appName]])
approx["Technique"] = approx["Approx Technique"].map(perf_grouped_names)
if (error_type_transform[appName]):
    cut_qual = error_type_transform[appName](cut_qual)
scatterPlotOnly(print_names[appName], approx, outPrefix + appName + "_figure2.pdf" , "Speed Up", error_type[appName], "Technique", "Region",1.0,None, 2.0)
