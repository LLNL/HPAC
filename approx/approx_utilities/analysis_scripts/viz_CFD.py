#!/usr/bin/env python3
import argparse
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
    print(x["Num Threads"].unique(), "RSD", (stdVal)/avg *100, length)

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

region_names["blackscholes"] = {    "CNDF_1" : "CNDF-1",
                                    "CNDF_2" : "CNDF-2",
                                    "ENTIRE" : "ENTIRE",
                                    "exp"    : "EXP"
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

perf_names ={ "iPerfo" : "ini",
              "fPerfo" : "fini",
              "sPerfo" : "small",
              "lPerfo" : "large",
              "rPerfo" : "rand",
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

def scatterPlotPerfo(appName, data, output_file, x_name, y_name, hue_col, style_col, v_line=None, h_line=None, x_cut = None, y_cut = None):
    local_data = data.copy()
    if y_cut:
       local_data = local_data[local_data[y_name] < y_cut]

    if x_cut:
       local_data = local_data[local_data[x_name] > x_cut]

    approxTechniques =  ['large', 'small', 'rand', 'ini', 'fini']
    allMarkers=[["d","none"], ["s","none"], ["v","none"], ["+", "fill"], ["x", "fill"]]

    tmp_colors2 = sns.color_palette("mako",len(approxTechniques))
    markers ={}
    colors={}
    for i, j in enumerate(approxTechniques):
        markers[j] = allMarkers[i]
        colors[j] = tmp_colors2[i]

    sizes=set_size(width=textWidth, fraction=0.5)
    fig, ax = plt.subplots(figsize=sizes)

    legends = []
    for t in approxTechniques:
        legends.append(Line2D([0], [0], lw=0.8, marker=markers[t][0], color=colors[t], markerfacecolor="white",  mec=colors[t], label=t, linestyle='None'))

    for t in approxTechniques:
        data= local_data[local_data[hue_col] == t]
        x= data[x_name].tolist()
        y= data[y_name].to_list()
        if ( markers[t][1] == "none"):
            ax.scatter(x, y, linewidth=1,  color=colors[t], marker=markers[t][0], edgecolors=colors[t], facecolors='none')
        else:
            ax.scatter(x, y, linewidth=1,  color=colors[t], marker=markers[t][0], edgecolors=colors[t])


    if ( v_line ):
        ax.axvline(v_line, color='gray', ls='--', lw=1.0)
    if ( h_line):
        ax.axhline(h_line, color='gray', ls='--', lw=1.0)

    y_label =  y_name.replace("%", "\%")
    ax.set( ylabel=y_label)
    ax.set( xlabel=x_name)
    legend = ax.legend(handles = legends, title="Perforation Type", frameon= False)
    plt.tight_layout()
    ax.figure.savefig(output_file,bbox_inches='tight')
    plt.close()

def scatterPlotOnly(appName, data, output_file, x_name, y_name, hue_col, style_col, v_line=None, h_line=None, x_cut = None, y_cut = None):
    local_data = data.copy()
    if y_cut:
       local_data = local_data[local_data[y_name] < y_cut]

    if x_cut:
       local_data = local_data[local_data[x_name] > x_cut]

    local_data = local_data[local_data[x_name] < 30]

    if (len (local_data) == 0):
        print("Returning from ", data["Region"].unique())
        print(x_cut, y_cut)
        print(local_data)
        return

    approxTechniques =  ['PERFO', 'TAF', 'iACT']
    print(local_data[hue_col].unique())

    regions = local_data[style_col].unique()
    approximation_colors = {}
    markers ={}
    allMarkers=[["d","none"], ["s","none"], ["v","none"]]
    colors ={}
    tmp_colors2 = sns.color_palette("mako",len(approxTechniques))
    for i, j in enumerate(approxTechniques):
        markers[j] = allMarkers[i]
        colors[j] = tmp_colors2[i]

    sizes=set_size(width=textWidth, fraction=0.5)
    fig, ax = plt.subplots(figsize=sizes)

    legends = []
    for t in approxTechniques:
        legends.append(Line2D([0], [0], lw=0.8, marker=markers[t][0], color=colors[t], markerfacecolor="white",  mec=colors[t], label=t, linestyle='None'))


    for r in regions:
        tmp_data = local_data[local_data[style_col] == r]
    for t in approxTechniques:
        data= local_data[local_data[hue_col] == t]
        x= data[x_name].tolist()
        y= data[y_name].to_list()
        if ( markers[t][1] == "none"):
            ax.scatter(x, y, linewidth=1,  color=colors[t], marker=markers[t][0], edgecolors=colors[t], facecolors='none')
        else:
            ax.scatter(x, y, linewidth=1,  color=colors[t], marker=markers[t][0], edgecolors=colors[t])


    if ( v_line ):
        ax.axvline(v_line, color='gray', ls='--', lw=1.0)
    if ( h_line):
        ax.axhline(h_line, color='gray', ls='--', lw=1.0)

    y_label =  y_name.replace("%", "\%")
    ax.set( ylabel=y_label)
    ax.set( xlabel=x_name)
    legend = ax.legend(handles = legends, title="Technique", frameon= False)
    plt.tight_layout()
    ax.figure.savefig(output_file,bbox_inches='tight')
    plt.close()

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

df = pd.read_pickle(input_file)
pd.set_option('display.max_rows', df.shape[0]+1)
print(df.head())
#df = df.drop(columns=["Input File", "Date Time"], axis=0)
df["Ratio"] = df["Ratio"].astype(np.float64)
df["App Quality"] = df["App Quality"].astype(np.float64)
df["Exec. Time"] = df["Exec. Time"].astype(np.float64)
df["App Quality"] = df["App Quality"].astype(np.float64)
df["Ratio"] = df["Ratio"].astype(np.float64)
indexes = ["Application", "Num Threads", "Approx Technique", "Label", "Hyper Parameters"]
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
approx["Type"] = approx["Approx Technique"].map(perf_names)

if (error_type_transform[appName]):
    cut_qual = error_type_transform[appName](cut_qual)
cut_perf=1

approx = approx[(approx["Region"] == "CC")]

scatterPlotOnly(print_names[appName], approx, outPrefix + "CFD_figure3.pdf", "Speed Up", error_type[appName], "Technique", "Num Threads", cut_perf)
perfo = approx[approx["Technique"] == "PERFO"]
scatterPlotPerfo(print_names[appName], perfo, outPrefix + "CFD_figure4.pdf", "Speed Up", error_type[appName], "Type", "Num Threads", cut_perf, None, None, None)


