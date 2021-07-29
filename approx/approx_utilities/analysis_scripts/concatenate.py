import argparse
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
from string import Template
import glob

stdMean= []
def quantile_function(df, quant_low, quant_high, col):
    value = df.quantile(quant_low)[col]
    quantile_df = df[df[col] > value]
    value = df.quantile(quant_high)[col]
    quantile_df = quantile_df[quantile_df[col]< value]
    if (df[col].mean() != 0.0 and quantile_df[col].mean()!=0):
        stdMean.append([df[col].std() / df[col].mean(),quantile_df[col].std() / quantile_df[col].mean()])
    return quantile_df


directory = sys.argv[1]
application = sys.argv[2]

pattern= "%s/interm_results/%s_*_*.pkl" % (directory, application)
results= []
info=[]
labels={}
dropped = 0
for f in glob.glob(pattern):
    tmpDF = pd.read_pickle(f)
    tmpDF["Ratio"] = tmpDF["Ratio"].astype(np.float64)
    if "TAF" in f or "iACT" in f:
        for v in tmpDF["NUM THREADS"].unique():
            tmp = tmpDF[tmpDF["NUM THREADS"] == v]
            if tmp["Ratio"].mean() < 0.1:
                for i in tmp["Label"].unique():
                    if i == "accurate":
                        continue
                    if i not in info:
                        info.append((i,f))
                    if i not in labels:
                        labels[i] = []
                    if "TAF" in f and "TAF" not in labels[i]:
                        labels[i].append("TAF")
                    elif "iACT" in f and "iACT" not in labels[i]:
                        labels[i].append("iACT")
#    continue
    if "TAF" in f or "iACT" in f:
        tmp = len(tmpDF)
        tmpDF = tmpDF[tmpDF.Ratio != '0']
        dropped +=  tmp - len(tmpDF)
    results.append((tmpDF,f))

processed_final=[]
save = True
for df,fName in results:
    df.rename( columns ={"NUM THREADS": "Num Threads"},inplace=True)
    df = df.drop(columns=["Input File", "Date Time"], axis=0)
    df["App Quality"] = df["App Quality"].astype(np.float64)
    df["Exec. Time"] = df["Exec. Time"].astype(np.float64)
    df["App Quality"] = df["App Quality"].astype(np.float64)
    indexes = ["Application", "Num Threads", "Approx Technique", "Label", "Hyper Parameters"]
    df["Num Threads"] = df["Num Threads"].astype(str)
    pd.set_option('display.max_rows', df.shape[0]+1)
    df = df.groupby(indexes).apply(quantile_function, 0.25,0.75, "Exec. Time").reset_index(drop=True)
    df = df.groupby(indexes).mean().reset_index()

    approx = df[df["Approx Technique"] != "accurate"]
    accurate = df[df["Approx Technique"] == "accurate"]

    num_threads = approx["Num Threads"].unique()
    accurate = accurate[accurate['Num Threads'].isin(num_threads)]
    if len(approx["Num Threads"].unique())!= 3:
        sys.exit(0)
        print("Exiting From HEre")
        continue
    approx = approx.reset_index()
    approx = approx.set_index(['Num Threads'])
    accurate = accurate.set_index(['Num Threads'])
    approx["Norm. Time"] = approx["Exec. Time"] / accurate["Exec. Time"]
    approx["Speed Up"] = accurate["Exec. Time"] / approx["Exec. Time"]
    approx = approx.reset_index()
    processed_final.append(approx)

if save:
    df = pd.concat(processed_final)
    print(df)
    df.to_pickle("%s/%s.pkl" %(directory, application ))

