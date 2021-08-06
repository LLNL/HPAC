#!/usr/bin/env python3
#
# Copyright 2020 Lawrence Livermore National Security, LLC
# and project developers
#
# LLNL release number LLNL-CODE-XXXXXX
#
# Author: Konstantinos Parasyris, parasyris1@lln.gov
#
# SPDX-License-Identifier: "Apache-2.0 WITH LLVM-exception"
#
# See top-level files LICENSE and NOTICE for details.
#

import os
import subprocess
import sys

if "THRESHHOLD" in os.environ: 
    threshhold = float(os.environ["THRESHHOLD"])
else:
    threshhold = 0.000001

cmd = os.environ["APPROX_UTIL_ROOT"] + "/quality -a" + sys.argv[1] + " -t" + sys.argv[2] + " -m " + sys.argv[3]
p = subprocess.run( cmd, stderr=subprocess.PIPE, stdout=subprocess.PIPE, universal_newlines=True,  shell=True )
out = p.stdout
err = p.stderr

sys.stderr.write(err)
sys.stderr.write(out)

if len(err) != 0:
    print (err)
    print ("status:  fail")
    sys.exit(0)

if "QUALITY" not in out:
    print ("status:  fail")
else:
    vals=out.split(":")
    error = float(vals[1].rstrip())
    if error > threshhold:
        print ("status:   fail: ", error)
    else:
        print ("status:   pass")

