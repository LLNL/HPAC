#!/usr/bin/env python3
import pandas as pd
import sys

df = pd.read_pickle(sys.argv[1])
print(df.head())
