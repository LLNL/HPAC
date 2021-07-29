#!/usr/bin/env python3

import argparse
from datetime import datetime
import itertools
import logging
import math
import numpy as np
import os
import pandas as pd
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time
import yaml
from yaml import CLoader
from string import Template

loggers = {}

verbosity_levels={
        'debug' : logging.DEBUG,
        'info' : logging.INFO,
        'warning' : logging.WARNING,
        'error' : logging.ERROR,
        'crtical' : logging.CRITICAL
}

def setup_logger(app_name, verbosity):
#def setup_logger(logger_name, log_file, level=logging.INFO):
    os.makedirs( 'logs/' , exist_ok=True )
    log_file = 'logs/' + app_name + '.log'
    l = logging.getLogger(app_name)
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    fileHandler = logging.FileHandler(log_file, mode='a')
    fileHandler.setFormatter(formatter)
    streamHandler = logging.StreamHandler()
    streamHandler.setFormatter(formatter)
    l.setLevel(verbosity)
    l.addHandler(fileHandler)
    l.addHandler(streamHandler)
    return logging.getLogger(app_name)

def get_directories(config, program, version, label):
    build_dir = config[program]['build_dir']
    root_dir = config[program]['root_dir']
#    root_dir = os.getcwd()
    if version != 'accurate':
        exe = build_dir + '/' + config[program]['versions'][version][label]['exe']
        build_cmd = config[program]['versions'][version][label]['build']
        clean_cmd= config[program]['versions'][version][label]['clean']
        bin_dir = root_dir + '/bin/' + program + '/' + version + '/' + label + '/'
        quality_dir = root_dir + '/quality/' + program + '/' + version + '/' + label + '/'
    else:
        exe = build_dir + '/' + config[program]['versions'][version]['exe']
        build_cmd = config[program]['versions'][version]['build']
        clean_cmd= config[program]['versions'][version]['clean']
        bin_dir = root_dir + '/bin/' + program + '/' + version + '/' + '/'
        quality_dir = root_dir + '/quality/' + program + '/' + version + '/' + 'pid_' + (str(os.getpid())) +  '/'
    loggers[program].debug('get_directories returns:%s %s %s %s %s %s' % (build_dir, exe, build_cmd, clean_cmd, bin_dir, quality_dir))
    return (build_dir, exe, build_cmd, clean_cmd, bin_dir, quality_dir)

def execute_command(program, bin_dir, cmd, times=1, patterns=None):
    results = []
    if patterns is None:
        patterns = []
    for i in range(0, times):
        t1 = time.perf_counter()
        try:
            p = subprocess.run( cmd, capture_output=True, cwd=bin_dir,  shell=True )
        except Exception as e:
            loggers[program].error('Cmd: %s Failed' % cmd)
        t2 = time.perf_counter()
        out = str(p.stdout.decode('utf-8'))
#        loggers[program].info('%s' %(out))
        err = str(p.stderr.decode('utf-8'))
        output = out + err
        match = []
        for p in patterns:
            items = re.search(p['pattern'],output)
            if items != None:
                loggers[program].debug('pattern: %s found' % p['pattern'])
                tmp = list(items.groups())
                tmp = [t for t in tmp if t]
                match += tmp[0:len(p['default'])]
            else:
                loggers[program].debug('pattern: %s NOT found' % p['pattern'])
                match += p['default']
        results.append(match)

    loggers[program].info('Execution Directory \'%s\'' % ( bin_dir))
    loggers[program].info('Executed \'%s\' Total times \'%d\'' % ( cmd, times))

    if ('OMP_PROC_BIND' in os.environ):
        loggers[program].info('Cores Where binded:%s' % (os.environ['OMP_PROC_BIND']))

    if ('OMP_NUM_THREADS' in os.environ):
        loggers[program].info('Cores Where binded:%s' % (os.environ['OMP_NUM_THREADS']))


    loggers[program].info('Executed \'%s\' Total times \'%d\'' % ( cmd, times))
    if ( 'EXECUTE_BOTH' in os.environ ):
        loggers[program].info('Executed Both %s' % ( os.environ['EXECUTE_BOTH']))
    else:
        loggers[program].info('Executed Both: No')

    return results

def transform(vals):
    res = [vals[0], vals[1], *vals[2], *vals[3]]
    return res

def  execute_version(program, df, approx_techniques, bin_dir, exec_cmd, quality_exe, performance_pattern, quality_pattern, region_quality_pattern, times):
    ranges=[]
    env_vars = []
    for r in approx_techniques['exports']:
        if 'progression' in approx_techniques['exports'][r]:
            if 'geometric' == approx_techniques['exports'][r]['progression']:
                (start, end, mult) = approx_techniques['exports'][r]['range']
                vals = []
                nextVal = start
                while nextVal < end:
                    vals.append(nextVal)
                    nextVal = nextVal * mult
                ranges.append(vals)
                env_vars.append(r)
            else:
                if approx_techniques['exports'][r]['type'] == 'float':
                    ranges.append(np.arange(*approx_techniques['exports'][r]['range']).tolist())
                elif approx_techniques['exports'][r]['type'] == 'int':
                    ranges.append(range(*approx_techniques['exports'][r]['range']))
                env_vars.append(r)
        else:
            if approx_techniques['exports'][r]['type'] == 'float':
                ranges.append(np.arange(*approx_techniques['exports'][r]['range']).tolist())
            elif approx_techniques['exports'][r]['type'] == 'int':
                ranges.append(range(*approx_techniques['exports'][r]['range']))
            env_vars.append(r)
    res = list(itertools.product(*ranges))
    backup = os.environ.copy()
    results = []

    for r in res:
        version_experiments = times
        os.environ.clear()
        os.environ.update(backup)
        environ_name=[]
        for index, val in enumerate(r):
            environ_name.append(env_vars[index] + '=' + str(val))
            os.environ[env_vars[index]] = str(val)
        # I am executing initially the performance eval.
        name =';'.join(environ_name)
        curr_env = df[df['Hyper Parameters'] == name]
        length = len(curr_env.index)
        #Check whether this configuration has already been executed.
        #If it has execute only the remaining iterations.
        if ( length >= times ):
            continue
        else:
            version_experiments = times-length

        loggers[program].info('Executing Performance Measurements %d times, environment is: %s ' % (version_experiments, name))
        performance = execute_command(program, bin_dir, exec_cmd, version_experiments, performance_pattern)
        # On a seperate execution I am also executing the
        # quality evaluation.
        # Some one should be able to select these seperate execution scenario via
        # different options in the run parameter.
        loggers[program].info('Executing Quality Measurements %d times, environment is: %s ' % (1, name))
        app_quality = execute_command(program, bin_dir, quality_exe, 1, quality_pattern)
        name = [name]* len(performance)
        app_quality = app_quality * len(performance)
        now = datetime.now()
        dateTime=[now]*len(performance)
        vals = list(map(transform, zip(name,dateTime, performance, app_quality)))
        results += vals
    os.environ.clear()
    os.environ.update(backup)
    return results

def run_and_evaluate(df, config, approx_techniques, program, version, label, times, num_threads):
    (build_dir, exe, build_cmd, clean_cmd, bin_dir, quality_dir) = get_directories(config, program, version, label)
    all_results = []
    for in_file in config[program]['input']:
        #Select only data related to this input file
        curr_df = df[df['Input File'] == in_file]
        cmd = config[program]['cmd']
        out_file = in_file + '.out'
        (_,acc_exe, *_, accurate_bin_dir , accurate_quality_dir) = get_directories(config, program, 'accurate', 'accurate')
        accurate_file = accurate_quality_dir + '/' + out_file
        #This code is only exected to get the 'golden' output file
        #it should not be executed in any other case
        #we will not keep any statistics out of it.
        if ( not os.path.exists(accurate_file) ):
            acc_cmd = config[program]['cmd']
            acc_cmd = acc_cmd.replace('<input>', in_file)
            acc_cmd = acc_cmd.replace('<output>', accurate_file)
            acc_exec_cmd = '%s %s' % (acc_exe,  acc_cmd)
            loggers[program].info('Golden output does not exist, going to execute it')
            loggers[program].info('Executing %s' % acc_exec_cmd)
            execute_command(program, accurate_bin_dir, acc_exec_cmd)

        test_file = quality_dir + '/' + out_file
        cmd = cmd.replace('<input>', in_file)
        cmd = cmd.replace('<output>', test_file)
        if 'kmeans' in program:
            exec_cmd = 'taskset -c 0-36 ' + exe + ' ' + cmd
        else:
            exec_cmd = exe + ' ' + cmd
        exec_quality= '%s/bin/quality -a %s -t %s -m %s' % (config[program]['project_dir'],accurate_file, test_file, config[program]['metric'])
        exec_pattern = [{'pattern' : config[program]['measure'], 'default' : [0]}, {'pattern' : 'APPROX:([0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)', 'default': [-1]}]
        quality_pattern = [{'pattern' : config[program]['quality_pattern'], 'default' : [0]}]
        region_quality_pattern = [{'pattern' : 'REGION_ERROR:([0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)', 'default': [-1]}]
        results = execute_version(program, curr_df[curr_df['Approx Technique'] == version], approx_techniques[version],bin_dir, exec_cmd, exec_quality, exec_pattern, quality_pattern, region_quality_pattern, times)
        if ( len(results) > 0 ):
            mul= len(results)
            all_results += [[a[0],a[1],a[2],a[3],a[4], *a[5]] for a in (zip([program]*mul, [num_threads]*mul, [version]*mul, [in_file]*mul, [label]*mul, results))]

    return all_results

def clean(config,program):
    repo_dir = './benchmarks/'
    build_dir = repo_dir + '/' + config[program]['build_dir']
    subprocess.run( config[program]['clean'], cwd=build_dir, shell=True)


def compile_and_install(config, program, version, label):
    (build_dir, exe, build_cmd, clean_cmd, bin_dir, quality_dir) = get_directories(config, program, version, label)
    os.makedirs( bin_dir, exist_ok=True )
    os.makedirs( quality_dir , exist_ok=True )
    loggers[program].info('Compiling...')

    if not os.path.isfile( exe ):
        loggers[program].info('Cleaning...')
        subprocess.run( clean_cmd, cwd=build_dir, shell=True)
        try:
            subprocess.run( build_cmd, cwd=build_dir, shell=True )
        except Exception as e:
            loggers[program].error('Cmd: %s Failed' % clean_cmd)

    loggers[program].info('Copying Executable')
    try:
        shutil.copy( exe , bin_dir)
    except Exception as e:
        loggers[program].error('Could not copy executable %s to %s)' % (exe, bin_dir))

    loggers[program].info('Copying Input Files')
    if 'copy' in config[program]:
        for copy in config[program]['copy']:
            if os.path.isdir( build_dir + '/' + copy ):
                try:
                    shutil.copytree( build_dir + '/' + copy, bin_dir + copy)
                except Exception as e:
                    loggers[program].error('Could not copy file  %s/%s to %s)' % (build_dir, copy, bin_dir + copy))
            else:
                try:
                    shutil.copy( build_dir + '/' + copy, bin_dir)
                except Exception as e:
                    loggers[program].error('Could not copy file  %s/%s to %s)' % (build_dir, copy, bin_dir))


    loggers[program].info('Symlinking Input Files')
    if 'soft_copy' in config[program]:
        for copy in config[program]['soft_copy']:
            if not os.path.isfile( bin_dir + copy ):
                try:
                    os.symlink( build_dir + '/' + copy, bin_dir + copy)
                except Exception as e:
                    loggers[program].error('Could not symlink file  %s/%s to %s)' % (build_dir, copy, bin_dir + copy))

def getOptimalParallelism(config, program):
    results_dir = config[program]['results_dir'] + '/interm_results/'
    file_name = '%s%s_accurate_accurate.pkl' % (results_dir, program)
    df = pd.read_pickle(file_name)
    df = df[df['Application'] ==  program]
    df = df[df['Approx Technique'] == 'accurate']
    df['NUM THREADS'] = df['NUM THREADS'].astype(int)
    df['Exec. Time'] = df['Exec. Time'].astype(np.float64)
    df = df[['NUM THREADS', 'Exec. Time']]
    df = df.reset_index(drop=True)
    df = df.groupby(['NUM THREADS']).mean().reset_index()
    best = df[df['Exec. Time'] == df['Exec. Time'].min()]
    bestT = best['NUM THREADS'].values
    return bestT[0]

# THIS IS HACK
def run_accurate(config, approx_techniques, program, version, label, times, num_threads):
    backupEnviron = os.environ.copy()
    os.environ['OMP_NUM_THREADS'] = str(num_threads)
    os.environ['OMP_PROC_BIND'] = 'true'

    cols = ['Application', 'NUM THREADS', 'Approx Technique', 'Input File', 'Label', 'Hyper Parameters', 'Date Time', 'Exec. Time', 'Ratio', 'App Quality']
    results = []
    results_dir = config[program]['results_dir'] + '/interm_results/'
    os.makedirs( results_dir, exist_ok=True )
    #Open DB for this application
    file_name = '%s%s_%s_%s.pkl' % (results_dir, program, label, version)
    if os.path.isfile(file_name):
        df = pd.read_pickle(file_name)
    else:
        df = pd.DataFrame(columns = cols)

    label='accurate'
    version='accurate'

    if label != 'all':
        tmp_df = df[df['NUM THREADS'] == num_threads]
        tmp_df = tmp_df[tmp_df['Application'] ==  program]
        tmp_df = tmp_df[tmp_df['Label'] == label]
        results = run_and_evaluate(tmp_df, config, approx_techniques, program, version, label, times, num_threads)
        current_results = pd.DataFrame(results, columns = cols)
        if not df.empty:
            df = pd.concat([current_results, df],ignore_index = True)
        else:
            df = current_results
        df.sort_values(by=['Application', 'Approx Technique', 'Input File', 'Label', 'Hyper Parameters', 'Date Time'],inplace=True)
        df.to_pickle(file_name)
    else:
        for l in config[program]['versions'][version]:
            tmp_df = df[df['NUM THREADS'] == num_threads]
            tmp_df = tmp_df[tmp_df['Application'] ==  program]
            tmp_df = tmp_df[tmp_df['Label'] == l]
            results = run_and_evaluate(tmp_df, config, approx_techniques, program, version, l, times, num_threads)
            current_results = pd.DataFrame(results, columns = cols)
            if not df.empty:
                df = pd.concat([current_results, df],ignore_index = True)
            else:
                df = current_results
            df.sort_values(by=['Application', 'Approx Technique', 'Input File', 'Label', 'Hyper Parameters', 'Date Time'],inplace=True)
            df.to_pickle(file_name)
    os.environ.clear()
    os.environ.update(backupEnviron)


def run(config, approx_techniques, program, version, label, times, num_threads):
    backupEnviron = os.environ.copy()
    os.environ['OMP_NUM_THREADS'] = str(num_threads)
    if 'kmeans' not in program:
        os.environ['OMP_PROC_BIND'] = 'true'

    if 'kmeans' in program and num_threads != 1:
        os.environ['OMP_PROC_BIND']= 'scatter'
        os.environ['OMP_PLACES'] = 'threads'

    cols = ['Application', 'NUM THREADS', 'Approx Technique', 'Input File', 'Label', 'Hyper Parameters', 'Date Time', 'Exec. Time', 'Ratio', 'App Quality']
    results = []
    results_dir = config[program]['results_dir'] + '/interm_results/'
    os.makedirs( results_dir, exist_ok=True )
    #Open DB for this application
    file_name = '%s%s_%s_%s.pkl' % (results_dir, program, label, version)
    if os.path.isfile(file_name):
        df = pd.read_pickle(file_name)
    else:
        df = pd.DataFrame(columns = cols)

    if label != 'all':
        tmp_df = df[df['NUM THREADS'] == num_threads]
        tmp_df = tmp_df[tmp_df['Application'] ==  program]
        tmp_df = tmp_df[tmp_df['Label'] == label]
        results = run_and_evaluate(tmp_df, config, approx_techniques, program, version, label, times, num_threads)
        current_results = pd.DataFrame(results, columns = cols)
        if not df.empty:
            df = pd.concat([current_results, df],ignore_index = True)
        else:
            df = current_results
        df.sort_values(by=['Application', 'Approx Technique', 'Input File', 'Label', 'Hyper Parameters', 'Date Time'],inplace=True)
        df.to_pickle(file_name)
    else:
        for l in config[program]['versions'][version]:
            tmp_df = df[df['NUM THREADS'] == num_threads]
            tmp_df = tmp_df[tmp_df['Application'] ==  program]
            tmp_df = tmp_df[tmp_df['Label'] == l]
            results = run_and_evaluate(tmp_df, config, approx_techniques, program, version, l, times, num_threads)
            current_results = pd.DataFrame(results, columns = cols)
            if not df.empty:
                df = pd.concat([current_results, df],ignore_index = True)
            else:
                df = current_results
            df.sort_values(by=['Application', 'Approx Technique', 'Input File', 'Label', 'Hyper Parameters', 'Date Time'],inplace=True)
            df.to_pickle(file_name)
    os.environ.clear()
    os.environ.update(backupEnviron)

def build(config, program, version, label):
    if label!= 'all':
        compile_and_install(config, program, version, label)
    else:
        for l in config[program]['versions'][version]:
            compile_and_install(config, program, version, l)

def getAvailableLabels(config, program, version, desiredLabels):
    if version == 'all':
        print('Please provide a specific application version')
        sys.exit(0)
    labels = []
    tmp = list(config[program]['versions'][version].keys())
    if  'all' in desiredLabels:
        return tmp
    return list(set (desiredLabels) & set(tmp))

def sbatch_execute(batchScript):
    cmd='sbatch -N 1 -n 36 %s' % (batchScript)
    p = subprocess.run( cmd, capture_output=True, shell=True )

def deploy(program, config_name, approx_technique_file, build, run, version, label, nThreads):
    time= '16:00:00'
    with open('batchfiles/template.batch', 'r') as myfile:
        data = myfile.read()
    if program == 'lulesh':
        specialCases = {
                'CalcElemShapeFunctionDerivatives2' :   'lulesh_approx_techniques/CalcElemShapeFunctionDerivatives2.yaml',
                'CombineDerivativesAndNormals' :        'lulesh_approx_techniques/CombineDerivativesAndNormals.yaml',
                'CalcElemVolumeDerivative':             'lulesh_approx_techniques/CalcElemVolumeDerivative.yaml',
                'CalcElemCharacteristicLength':         'lulesh_approx_techniques/CalcElemCharacteristicLength.yaml',
                'CalcElemVolumeKinematics':             'lulesh_approx_techniques/CalcElemVolumeKinematics.yaml'
                }
        if label in specialCases:
            approx_technique_file = specialCases[label]

    outName= '%s_%s_%s' % (program, version, label)
    template = Template(data)
    threads= ','.join([str(t) for t in nThreads])
    script = template.substitute(CONFIG=config_name, OUTNAME=outName, TECHNIQUES=approx_technique_file, RUN=run, THREADS=threads, VERSION=version, LABEL=label, TIME=time)
    name = 'batchfiles/tmp_' + outName + '.batch'
    with open(name, 'w') as outFile:
        outFile.write(script)
    sbatch_execute(name)


def main():
    parser = argparse.ArgumentParser(description='Benchmark programs for Approximate Computing')
    parser.add_argument('-i', '--input', dest='input', type=str, help='configuration YAML input file for programs', required=True)
    parser.add_argument('-t', '--techniques', dest='approx_techniques', type=str,
            help='configuration YAML of approximate techniques', required=True)
    parser.add_argument('-b', '--build', dest='build', action='store_true', help='build programs')
    parser.add_argument('-r', '--run', dest='run', type=int, help='run <repetitions>')
    parser.add_argument('-p', '--programs', dest='programs', type=str, nargs='+', help='programs to run from the config')
    parser.add_argument('-c', '--clean', dest='clean', action='store_true', help='clean directories')
    parser.add_argument('-a', '--approximate-version', dest='versions', type=str,  help='specific technique')
    parser.add_argument('-l', '--labels', dest='labels', type=str,  help='Specific code region')
    parser.add_argument('-o', '--output', dest='output', type=str,  help='Output log file name')
    parser.add_argument('-n', '--nThreads', dest='nThreads', type=str,  help='number of Threads')
    parser.add_argument('-d', '--deploy', dest='deploy', action='store_true' ,  help='deploy using sbatch')
    parser.add_argument('-V', '--verbosity', dest='verbosity', type=str.lower,
            choices=verbosity_levels.keys(),
            help='Verbosity Level',
            default='info')
    args = parser.parse_args()

    with open(args.input, 'r') as f:
        config = yaml.load(f, Loader=CLoader)

    with open(args.approx_techniques, 'r') as f:
        approx_techniques= yaml.load(f, Loader=CLoader)


    if args.programs == None:
        args.programs = config.keys()
    elif args.programs == ['all']:
        args.programs = config.keys()

    for app in args.programs:
        if args.output:
            loggers[app] = setup_logger(args.output, verbosity_levels[args.verbosity])
        else:
            loggers[app] = setup_logger(app, verbosity_levels[args.verbosity])

    if args.versions == None:
        args.versions = approx_techniques.keys()
        versions = list(args.versions)
    elif args.versions == ['all']:
        args.versions = approx_techniques.keys()
        versions = list(args.versions)
    else:
        versions = args.versions.split(',')

    nThreads = None
    if args.nThreads != None:
        nThreads= [ int(v) for v in args.nThreads.split(',')]

    error_msg=''
    for v in versions:
        if v not in approx_techniques.keys():
            error_msg += v +' ,'
    if error_msg != '' :
        print('These approximate techniques do not exist:' + error_msg[:-1])
        sys.exit()

    if 'accurate' not in versions:
        versions.append('accurate')

    if args.labels == None:
        labels=['all']
    else:
        labels = args.labels.split(',')

    print('programs', args.programs)
    print('versions', versions)
    print('labels', args.labels)
    print('build', args.build)
    print('run', args.run)

#   Check whether configurations scripts/parameters are correct
    should_exit = False
    for p in args.programs:
        if 'accurate' not in config[p]['versions']:
            print('Each application should have also an accurate version')
            should_exit = True
        if 'root_dir' not in config[p]:
            print('Root directory is not set by CMAKE')
            should_exit = True
        if 'project_dir' not in config[p]:
            print('project directory is not set by CMAKE')
            should_exit = True

        for v in versions:
            if  v != 'accurate' and v not in  approx_techniques.keys():
                print('Version: \'',v,'\' does not exist, will exit soon...')
                should_exit = True

    if (should_exit):
        print('Exiting ...')
        sys.exit(-1)

    print(versions)
    print(labels)

#  I need to check whether I need to build (before running build is neccessary). 
    scenaria = []
    if args.build or args.run:
        for p in args.programs:
            build(config, p, 'accurate', 'accurate')
            for v in versions:
                if v not in config[p]['versions']:
                    print(v)
                    continue
                for l in labels:
                  if (l != 'all') and (l not in config[p]['versions'][v]):
                    continue
                  build(config, p, v, l)
    if 'accurate' in versions:
        versions.remove('accurate')

# I will start executing. 
    if args.run:
        for p in args.programs:
            if nThreads == None:
                for t in [1, 2 , 4, 8, 16, 32]:
                    run(config, approx_techniques, p, 'accurate', 'accurate', args.run, t)
                optimal = getOptimalParallelism(config, p)
                nThreads = [1, optimal]
            vLabels = []
            for v in versions:
                vLabels= getAvailableLabels(config, p, v, labels)
                for l in vLabels:
                    if args.deploy:
                        deploy(p, args.input, args.approx_techniques, args.build, args.run, v, l, nThreads)
                    else:
                        for t in nThreads:
                            run_accurate(config, approx_techniques, p, v, l, args.run, t)
                            run(config, approx_techniques, p, v, l, args.run, t)

if __name__ =='__main__':
    main()


