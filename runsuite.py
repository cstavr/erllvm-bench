import os
from glob import glob
import subprocess
from optparse import OptionParser
import logging
import datetime
import shutil

formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger('BenchSuite')
logger.setLevel(logging.DEBUG)
ch = logging.StreamHandler()
ch.setFormatter(formatter)
fh = logging.FileHandler('bench.log')
fh.setFormatter(formatter)
logger.addHandler(ch)
logger.addHandler(fh)

PLATFORMS=['beam', 'hipe', 'erllvm', 'erjang']

def run_suite(options, args):
    base_dir = os.path.abspath(args[0])
    logger.info('-' * 80)
    logger.info('-' * 80)
    logger.info('Starting Suite with Base Directory %s', base_dir)
    logger.info('-' * 80)
    logger.info('-' * 80)

    try:
        categories = options.category.split(',')
    except AttributeError:
        categories = CATEGORIES
    logger.info('Categories: %s', categories)
    
    try:
        benchmarks = options.benchmarks.split(',')
    except AttributeError:
        benchmarks = 'all'
    
    try:
        platforms = options.platforms.split(',')
    except AttributeError:
        platforms = PLATFORMS

    platforms = find_platforms(base_dir, platforms)

    times = options.times
    size = options.size

    f = open('stdout', 'a')
    f.write('-' * 80+'\n')
    f.write('-' * 80 + '\n')
    f.write('Started: %s\n' % datetime.datetime.now())
    f.write('-' * 80+'\n')
    f.write('-' * 80 + '\n')
    f.close()

    logger.info('Benchmarks: %s', benchmarks)
    error = False
    compile_bench(platforms[platforms.keys()[0]], 'runner.erl')
    for cat in categories:
        logger.info('Starting %s ' % cat)
        logger.info('-' * 80)

        benchmarks = find_benchmarks(cat, benchmarks)
        logger.info('Benchmarks: %s', benchmarks)

        for b in benchmarks:
            for plat_name, plat_path in platforms.items():
                try:
                    # compile_bench(plat, 'runner.erl')
                    compile_bench(plat_path, b)
                except CompilationError as e:
                    logger.error('Could not compile %s with %s: %s', b, plat,
                                   e)
                    error = True
                    continue

                try:
                    run_bench(plat_name, plat_path, b, times, size)
                except RunningError as e:
                    logger.error('Could not run %s with %s: %s', b, plat, e)
                    error = True

    logger.info('Completed Suite')

    if not options.plot:
        return

    if error:
        logger.error('Not ploting result. An error occured during run')
        return

    collect_results(platforms.keys())

def find_platforms(base_dir, platforms_names):
    platforms = {}
    for p in platforms_names:
        path = base_dir + '/otp_' + p
        if os.path.exists(path):
            platforms[p] = path
        else:
            logger.error('Platform %s not found', path)
    return platforms


def find_benchmarks(category, benchmarks):
    path = 'src/' + category + '/'
    if benchmarks == 'all':
        benchmarks = glob(path + '*.erl')
    else:
        benchs = []
        for b in benchmarks:
            bench_path = path + b + '.erl'
            if os.path.exists(bench_path):
                benchs.append(bench_path)
            else:
                logger.error('Benchmark %s does not exist', bench_path)
    return benchmarks


def compile_bench(platform, name):
    logger.debug('Compiling benchmark %s with %s', name, platform)

    compiler = platform + '/bin/erlc'

    p = subprocess.Popen([compiler,'-o', 'ebin', name], stdout=subprocess.PIPE,
            stderr=subprocess.PIPE, bufsize=-1)
    output, errors = p.communicate()

    if p.returncode:
        raise CompilationError(output)
    else:
        return

def run_bench(plat_name, plat_path, name, times=1, size='small'):
    logger.debug('Running benchmark %s with %s', name, plat_path)
    if plat_name == 'erjang':
        erl = '/'.join(plat_path.split('/')[:-1]) + '/erjang/ej'
    else:
        erl = plat_path + '/bin/erl'

    bench_name, _ = os.path.basename(name).split('.')
     #-run init stop -noshell
    f = open('stdout', 'a')
    sep = 80 * '-' + '\n'
    f.write(sep)
    f.write('Output from %s:\n' % name)
    f.write(sep)
    f.flush()
    p = subprocess.Popen([erl, '-pa', 'ebin/', '.', '-noshell', '-run' ,'runner',
                         'main', bench_name, plat_name, str(times), size, '-run', 'init', 'stop'],
            stdout=f,
            stderr=f, bufsize=-1)
    output, errors = p.communicate()
    f.close()
    if p.returncode:
        raise RunningError(output)
    else:
        return


class CompilationError:
    def __init__(self, msg):
        self.msg = msg

    def __repr__(self):
        return self.msg


class RunningError:
    def __init__(self, msg):
        self.msg = msg

    def __repr__(self):
        if self.msg:
            return self.msg
        else:
            return ''

def collect_results(platforms):
    result_path = 'results/runtime_total.res'
    if os.path.exists(result_path):
        shutil.os.remove(result_path)
    res_file = open(result_path, 'a')
    times = {}
    for plat in platforms:
        f = open('results/runtime_' + plat +'.res', 'r')
        lines = f.readlines()
        times[plat] = {}
        for l in lines:
            bench, time = l.split(' ')
            times[plat][bench] = time.strip()

    times2 = {}
    benchmarks = times[platforms[0]]
    for bench in benchmarks:
        times2[bench] = {}
        for plat in platforms:
            times2[bench][plat] = times[plat][bench]

    line = ['#Name']
    for plat in PLATFORMS:
        try:
            line.append(plat)
        except KeyError:
            pass
    line = map(lambda x: x.ljust(20), line)
    res_file.write(''.join(line) + '\n')

    for bench in benchmarks:
        line = [bench]
        for plat in PLATFORMS:
            try:
                line.append(times2[bench][plat])
            except KeyError:
                pass
        line = map(lambda x: x.ljust(20), line)
        res_file.write(''.join(line) + '\n')
    res_file.close()





def parse_arguments():
    parser = OptionParser()
    parser.add_option("-c", "--category", dest="category",
                  default=None,
                  help="Run only these categories"),
    parser.add_option("-b", "--benchmarks",
                  dest="benchmarks", default=None,
                  help="Run only these benchmarks")
    parser.add_option("-n", "--num",
                  dest="times", default=1,
                  help="Number of times to loop")
    parser.add_option("-p", "--platforms",
                  dest="platforms", default=None,
                  help="Run only for these platforms")
    parser.add_option("-s", "--size",
                  dest="size", default='small',
                  help="Specify size of benchmark(small,medium,big)")
    parser.add_option("--no-plot",
                  action='store_false',
                  dest="plot", default=True,
                  help="Dot not collect results and plot diagrams")
    return parser.parse_args()


if __name__=='__main__':
    (options, args) = parse_arguments()
    run_suite(options, args)
