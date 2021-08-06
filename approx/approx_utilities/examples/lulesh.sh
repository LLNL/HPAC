#!/bin/bash
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


if [[ "$#" -eq "0" ]] || [[ "$#" -gt "2" ]]; then
  echo "Command Line: $0 'experiment size (small|large)' 'vizualize(optional, 0->False(default), 1->True)'"
  exit
fi

exp_size=$1
repeat=8
visualize=false
if [[ "$#" -eq "2" ]] && [[ "$2" -eq "1" ]]; then 
  visualize=true
fi

if [[ $exp_size != "large" ]] && [[ "$exp_size" != "small" ]]; then
  echo "Size must be either small or large"
  exit
fi

if [[ "$exp_size" == "small" ]]; then
  repeat=3
fi

python ./benchmark.py -i build/lulesh.yaml -t "approx_space/$exp_size/approximate_techniques.yaml" -r $repeat -n 1,16,32 -b -l COMPUTEPRESSURE
python ./benchmark.py -i build/lulesh.yaml -t "approx_space/$exp_size/CalcElemCharacteristicLength.yaml" -r $repeat -n 1,16,32 -b -l CalcElemCharacteristicLength
python ./benchmark.py -i build/lulesh.yaml -t "approx_space/$exp_size/approximate_techniques.yaml" -r $repeat -n 1,16,32 -b -l CalcElemFBHourglassForce
python ./benchmark.py -i build/lulesh.yaml -t "approx_space/$exp_size/CalcElemShapeFunctionDerivatives2.yaml" -r $repeat -n 1,16,32 -b -l CalcElemShapeFunctionDerivatives2
python ./benchmark.py -i build/lulesh.yaml -t "approx_space/$exp_size/approximate_techniques.yaml" -r $repeat -n 1,16,32 -b -l CalcElemVelocityGradient2
python ./benchmark.py -i build/lulesh.yaml -t "approx_space/$exp_size/CalcElemVolumeDerivative.yaml" -r $repeat -n 1,16,32 -b -l CalcElemVolumeDerivative
python ./benchmark.py -i build/lulesh.yaml -t "approx_space/$exp_size/CalcElemVolumeKinematics.yaml" -r $repeat -n 1,16,32 -b -l CalcElemVolumeKinematics
python ./benchmark.py -i build/lulesh.yaml -t "approx_space/$exp_size/approximate_techniques.yaml" -r $repeat -n 1,16,32 -b -l CalcEnergyForElems
python ./benchmark.py -i build/lulesh.yaml -t "approx_space/$exp_size/approximate_techniques.yaml" -r $repeat -n 1,16,32 -b -l CalcMonotonicQForElems
python ./benchmark.py -i build/lulesh.yaml -t "approx_space/$exp_size/approximate_techniques.yaml" -r $repeat -n 1,16,32 -b -l CalcMonotonicQGradientsForElems
python ./benchmark.py -i build/lulesh.yaml -t "approx_space/$exp_size/approximate_techniques.yaml" -r $repeat -n 1,16,32 -b -l CalcMonotonicQRegionForElems
python ./benchmark.py -i build/lulesh.yaml -t "approx_space/$exp_size/approximate_techniques.yaml" -r $repeat -n 1,16,32 -b -l CalcSoundSpeedForElems
python ./benchmark.py -i build/lulesh.yaml -t "approx_space/$exp_size/approximate_techniques.yaml" -r $repeat -n 1,16,32 -b -l CalcVelocityForNodes
python ./benchmark.py -i build/lulesh.yaml -t "approx_space/$exp_size/CombineDerivativesAndNormals.yaml" -r $repeat -n 1,16,32 -b -l CombineDerivativesAndNormals
python analysis_scripts/concatenate.py analysis_results/ lulesh 

if [ "$visualize" == true ]; then
  python analysis_scripts/viz_lulesh.py analysis_results/lulesh.pkl
fi
