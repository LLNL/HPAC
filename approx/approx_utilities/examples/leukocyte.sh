#!/bin/bash

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

python ./benchmark.py -b -i build/leukocyte.yaml -t "approx_space/$exp_size/approximate_techniques.yaml" -r $repeat -n 1,16,32  
python analysis_scripts/concatenate.py analysis_results/ leukocyte

if [ "$visualize" == true ]; then
  python analysis_scripts/viz_leukocyte.py analysis_results/leukocyte.pkl
fi
