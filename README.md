# HPAC

HPAC is comprised of two components, namely the core and the harness. The core implements the approximate 
programming model. It extends the Clang/LLVM compiler and provides runtime support. The harness 
facilitates easy exploration of the approximate design space.

Currently, HPAC supports:

- Perforation:
   - ini: Drop the first iterations of a for loop.
   - fini: Drop the last iterations of a for loop.
   - random: Drop randomly iterations of a for loop.
   - small: Drop 1 iteration every N iterations.
   - large: Drop N-1 iterations every N iterations.
- Memoization:
   - iACT: Apply approximate memoization based on the inputs of a code region.
   - TAF: Apply approximate memoization based on the outputs of a code region.


## Build Requirements:

HPAC on a x86_64 system has been successfully built with the following dependencies: 
following free projects:
- Ninja 1.9.0
- CMAKE 3.14
- gcc 8.3.1
- python 3.7.6
- pandas 1.2.2
- numpy 1.20.1
- matplotlib 3.3.4
- seaborn-0.11.1
- yaml 5.4.1

## Build HPAC:

### Build HPAC Compiler Extensions

To build the HPAC compiler and all the infrastructure please execute the following commands:

```bash
git clone git@github.com:koparasy/HPAC.git
cd HPAC
./setup.sh 'PREFIX' 'NUM THREADS' 
```

The 'PREFIX' argument defines where to install all the HPAC related binaries and executables. The 'NUM THREADS' parameter
define how many threads should the installation use. The installation script performs the following actions:

1. Configures, builds and installs clang/LLVM/OpenMP including the approximation extensions.
2. Configures, builds and installs the approximation library. 
3. Creates a file, called 'hpac_env.sh', at the root of the project which should always be sourced before using HPAC.
4. Fetches the original applications of our evaluation.
5. Patches the applications with our extensions.
6. Configures and compiler the extended approximated applications.

The installation process can take a considerable amount of time. In our tested system, the complete 
installation took ~5 hours. 

# Performing Analysis using  HPAC.

1. To use the harness, change directory to the HPAC root directory and
source the hpac_env.shfile. The file was created during the installation.
If the file is already source this step can be skipped.

```bash
source hpac_env.sh
```

2. Change directory to the 'approx/approx_utilities/' directory, Then issue the following command:

```bash
cd approx/approx_utilities/
python ./benchmark.py -t 'APPROX SPACE' -i 'CONFIG FILE' -p 'APPLICATION NAME' -b -r N -n '1,2,4,8' 
```

This command will perform the space exploration for the application called "APPLICATION NAME". It
will read application configuration parameters from the file 'CONFIG FILE'. It will configure
the approximation space to be 'APPROX SPACE'. For, each exploration point it will execute with 
different number of threads. In this example,
it will explore the threads '1,2,4,8' and for each thread number it will execute 'N' experiments ('N' is defined 
with the '-r' option.

## Output

The HPAC harness executes the experiments and measures the execution time and the quality loss 
for different approximation techniques and parameters. All the results are stored into a 
"\*.pkl" files under the 'analysis_results/interm_results/' directory. To view the contents of "\*.pkl" file
please issue the command:

``` bash
python analysis_scripts/read.py analysis_results/interm_results/'FILE NAME'.pkl
```

Replace the 'FILE NAME' with the respective output file. 

The output of this command outputs, to stdout, the contents of the table with the following columns. 

|      | Application |  Threads | Technique | Input File | Label |  Parameters   | Exec. Time   | Quality   |
| ---- |----         |  ----    |   ----    |   ----     |  ---- |          ---- |----          |----       |

The values of each row will correspond to the measured values of a specific experiment. The file is stored in Pandas
format, and thus any data analysis technique can be applied easily on top of the data.

## Replicate Analysis of SC-21

To run the analysis for each of the benchmarks please execute the following commands:

| Benchmark Name | Command                                        | Time to perform small Experiments | Time to perform large Experiments |
| -------------- | ------                                         |--------------                     |--------------                     |
| Blackscholes   | ./examples/blackscholes.sh 'size' 'visualize'  |1.5h                               |15h                                |
| Particle Filter| ./examples/pFilter.sh 'size' 'visualize'       |1.5h                               |15h                                |
| LavaMD         | ./examples/lavaMD.sh 'size'  0                 |4h                                 |40h                                |
| K-Means        | ./examples/kmeans.sh 'size' 'visualize'        |3h                                 |30h                                |
| lulesh         | ./examples/lulesh.sh 'size' 'visualize'        |5h                                 |5h                                 |
| CFD            | ./examples/cfd.sh 'size' 'visualize'           |2h                                 |20h                                |
| HPCCG          | ./examples/hpccg.sh 'size' 'visualize'         |2h                                 |20h                                |
| Leukocyte      | ./examples/leukocyte.sh 'size' 'visualize'     |10h                                |100h                               |

The scripts use two parameters. The first parameter defines the size of the exploration. There are two options, 
namely 'large' and 'small'. The final results are obtained using the 'large' option. However, the 'large' experiments
may take days to finish. Therefore, there is a 'small' option which performs less experiments. The final outputs
should not change significantly. Finally, the second option is visualize, which can take the value of 0 (disable visualization)
or 1 (enable visualization). In the latter case, the visual outputs of the analysis are stored under the directory
'sc_paper_plots'. For example, if you execute:

```bash
./examples/blackscholes.sh 'small' 1
```

A figure called 'blackscholes_figure2.pdf' should be stored under the 'sc_paper_plots'. This figure 
is similar to Figure 2 of the paper submission.


## Contributing
To contribute to this repo please send a [pull
request](https://help.github.com/articles/using-pull-requests/) on the
develop branch of this repo.

## Authors

This code was created by Konstantinos Parasyris (parasyris1@llnl.gov) and Giorgis Georgakoudis,
(georgakoudis1@llnl.gov), assisted with design input from Harshitha Menon (gopalakrishn1@llnl.gov).


## License

This repo is distributed under the terms of the Apache License (Version
2.0) with LLVM exceptions. Other software that is part of this
repository may be under a different license, documented by the file
LICENSE in its sub-directory.

All new contributions to this repo must be under the Apache License (Version 2.0) with LLVM exceptions.

See files [LICENSE](LICENSE) and [NOTICE](NOTICE) for more information.

SPDX License Identifier: "Apache-2.0 WITH LLVM-exception"

LLNL-CODE- 825539
