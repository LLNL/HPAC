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

import sys
import re
import os
import shutil
import re
import yaml
from yaml import CLoader
import pprint

approximate_techniques = [("iACT", "memo(in)"), ("TAF", "memo(out)"), ("sPerfo" , "perfo(small:__approx_step__)"), ("lPerfo", "perfo(large:__approx_step__)"), ("rPerfo", "perfo(rand:__approx_percentage__)"), ("iPerfo",  "perfo(init:__approx_percentage__)"), ("fPerfo", "perfo(fini:__approx_percentage__)") ]

IN_PATTERN='IN\((.*?)\)\s+'
OUT_PATTERN='OUT\((.*?)\)\s+'
INOUT_PATTERN='INOUT\((.*?)\)\s+'
LABEL_PATTERN='LABEL\(\"(.*?)\"\)\s+'
APPROX_TECH_PATTERN='APPROX_TECH\((.*?)\)\s+'

in_regex=re.compile(IN_PATTERN)
out_regex=re.compile(OUT_PATTERN)
inout_regex=re.compile(INOUT_PATTERN)
label_regex=re.compile(LABEL_PATTERN)
tech_regex = re.compile(APPROX_TECH_PATTERN)

def applyTechOnRegion(tech, r_tech):
    if tech == "iACT" and "MEMO_IN" in r_tech:
        return True
    elif tech == "TAF" and "MEMO_OUT" in r_tech:
        return True
    elif tech == "sPerfo" and "PERFO" in r_tech:
        return True;
    elif tech == "lPerfo" and "PERFO" in r_tech:
        return True;
    elif tech == "rPerfo" and "PERFO" in r_tech:
        return True;
    elif tech == "fPerfo" and "PERFO" in r_tech:
        return True;
    elif tech == "iPerfo" and "PERFO" in r_tech:
        return True;
    return False

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

# This is a brute force approach.
# And it only annotates a single file on each configuration. 
def create_and_configure_files(install_dir, root_dir, bench_desc, app_name, output_dir, src_dir, src_files, regions):
    created_directories = []
    if app_name not in bench_desc:
        eprint("should never happen")
        sys.exit(-1)

    bench_desc[app_name]["build_dir"] = output_dir
    bench_desc[app_name]["versions"] = {}

    for approx in approximate_techniques:
        bench_desc[app_name]["versions"][approx[0]] = {}
        for elem in regions:
            if not applyTechOnRegion(approx[0], elem[6]):
                continue
            bench_desc[app_name]["versions"][approx[0]][elem[5]] = {}
            output_name = approx[0] + "_" + elem[5]
            output_file = output_dir + "/" +  approx[0] + "_" + elem[5] + "/"
            os.makedirs( output_file , exist_ok=True)
            bench_desc[app_name]["versions"][approx[0]][elem[5]]["build"] = 'make ' + app_name + '_' + output_name + '.exe'
            bench_desc[app_name]["versions"][approx[0]][elem[5]]["clean"] = 'cmake -P CMakeFiles/' + app_name + '_' + output_name + '.exe' + '.dir/cmake_clean.cmake'
            bench_desc[app_name]["versions"][approx[0]][elem[5]]["exe"] = app_name + '_' + output_name + '.exe'
            created_directories.append(output_name)
            file_to_modify = elem[0]
            for src in src_files:
                configured_file = output_file + "/" + src
                if src == file_to_modify:
                    dest = open(configured_file, "w")
                    with open(src_dir+"/"+src, "r") as fd:
                        for num, line  in enumerate(fd,1):
                            if ( num == elem[1]):
                                if approx[0] in ["sPerfo", "lPerfo"]:
                                    dest.write("int __approx_step__ = approx_rt_get_step();\n")
                                elif approx[0] in ["rPerfo", "fPerfo", "iPerfo"]:
                                    dest.write("float __approx_percentage__ = approx_rt_get_percentage();\n")
                                dest.write("#pragma approx ")
                                dest.write(approx[1])
                                if approx[0] not in ["sPerfo", "lPerfo", "rPerfo", "iPerfo", "fPerfo"]:
                                    dest.write(elem[2] + " " + elem[3] + " " + elem[4] + " ")
                                    dest.write(" label(\"" +  elem[5] + "\")\n")
                                else:
                                    dest.write("\n")
                            else:
                                dest.write(line)
                    dest.close()
                else:
                    shutil.copy2(src_dir + "/" + src, configured_file)


    accurate_dir = output_dir + "/accurate/"
    os.makedirs( accurate_dir, exist_ok=True)
    for src in src_files:
        shutil.copy2(src_dir + "/" + src, accurate_dir)
    created_directories.append("accurate")
    bench_desc[app_name]["versions"]["accurate"] = {}
    bench_desc[app_name]["versions"]["accurate"]["exe"] =  app_name + '_accurate.exe'
    bench_desc[app_name]["versions"]["accurate"]["build"]  = 'make ' + app_name + '_accurate.exe'
    bench_desc[app_name]["versions"]["accurate"]["clean"] = 'cmake -P CMakeFiles/' + app_name + '_accurate.exe' + '.dir/cmake_clean.cmake'
    bench_desc[app_name]["root_dir"] = root_dir
    bench_desc[app_name]["project_dir"] = install_dir
    bench_desc[app_name]['results_dir'] = '%s/%s' % (root_dir, 'analysis_results')

    with open(app_name + ".yaml", "w") as f:
        yaml.dump(bench_desc,f, Dumper=yaml.CDumper, default_flow_style=False)


    return created_directories

def calculate_approx_regions(src_dir, src_files):
    regions = []
    for f in src_files:
        with open(src_dir+"/"+f, "r") as fd:
            eprint(f)
            for num, line  in enumerate(fd,1):
                if ("//@APPROX") in line:
                    inputs=""
                    outputs=""
                    inouts=""
                    label=""
                    approx_techs = []
                    if " INOUT" in line:
                        result=inout_regex.search(line)
                        inouts = "inout(" + result.group(1) + ")"
                    if " IN" in line:
                        result=in_regex.search(line)
                        if result != None:
                            inputs = "in(" + result.group(1) + ")"
                    if " OUT" in line:
                        result=out_regex.search(line)
                        outputs = "out(" + result.group(1) + ")"
                    if " LABEL" in line:
                        result = label_regex.search(line)
                        label = result.group(1)
                    if "APPROX_TECH" in line:
                        result = tech_regex.search(line)
                        if result != None:
                            approx_techs = result.group(1).split("|")
                            approx_techs = [v.replace(" ", "") for v in approx_techs]
                    regions.append( (f, num, inputs, outputs, inouts, label, approx_techs) )
    return regions

def main(args):
    install_dir = os.getcwd()
    root_dir =  args[0]
    output_dir = args[1]
    src_dir = args[2]
    app_name = args[3]
    src_files = args[4:]
    with open(src_dir + "/benchDescr.yaml", "r") as f:
        bench_desc = yaml.load(f, Loader=CLoader)

# The first thing I need to do 
# is to iterate through all the files
# and identify approx_regions
    regions = calculate_approx_regions(src_dir, src_files)
    configed_dirs = create_and_configure_files(install_dir, root_dir, bench_desc, app_name, output_dir, src_dir, src_files, regions)
    print(*configed_dirs)


if __name__ == "__main__":
    main(sys.argv[1:])
