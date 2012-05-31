import os
import glob

SYS = ['beam', 'hipe', 'erllvm', 'erjang', 'erjang_1']

## MISC FUNCTIONS
def pretty_name(name):
    if name == 'beam':
        return 'Beam'
    if name == 'hipe':
        return 'HiPE'
    if name == 'erllvm':
        return 'ErLLVM'
    if name == 'erjang_1':
        return 'Erjang-1'
    if name == 'erjang':
        return 'Erjang'

def print_line(f, line):
    line = map(lambda x: str(x).ljust(15), line)
    line = ''.join(line)
    f.write(line + '\n')

def print_plot_info(f, cluster, mmax):
    f.write("# clustered graph example from Derek Bruening's CGO 2005 talk\n")
    cluster = "=cluster;%s\n" % cluster
    f.write(cluster + '\n')
    f.write("# green instead of gray since not planning on printing this\n")
    f.write("colors=grey5,med_blue,light_green,red,yellow\n")
    f.write("=arithmean\n")
    f.write("meanlabel=Average\n")
    f.write("=table\n")
    f.write("yformat=%g\n")
    f.write("max=%s\n" % str(mmax))
    f.write("ylabel=Runtime Speedup\n")
    f.write("xlabel=Benchmark\n")
    f.write("rotateby=-40\n")
    f.write("horizline=1\n")
    f.write("# stretch it out in x direction\n\n")


### MAIN


# Get results from files
results = {}
for name in SYS:
    results_ = {}
    f = open('results/runtime_'+name+'.res')
    lines = f.readlines()
    for line in lines:
        ll = line.rstrip().split('\t')
        bench_name, time= line.rstrip().split('\t')
        if not time:
            time = "0"
        results_[bench_name] = time
    results[name] = results_
    f.close()

# Assert all benchmarks run
for name in SYS[1:]:
    assert len(results[name]) == len(results[SYS[0]])

benchmarks = results['beam'].keys()
platforms = SYS

# Revert table
bench_results = {}
bench_results['shootout'] = {}
bench_results['misc'] = {}
for bench_name in benchmarks:
    fname1 = 'src/shootout/' + bench_name + '.erl'
    if os.path.exists(fname1):
        category = 'shootout'
    else:
        category = 'misc'
    bench_results[category][bench_name] = {}
    for name in platforms:
        bench_results[category][bench_name][name] = results[name][bench_name]

# Print merged results table
f = open('results/runtime_total.res', 'w')
print_line(f, ['#Name']+platforms)
for cat in ['shootout', 'misc']:
    for name in sorted(bench_results[cat].keys()):
        line = []
        for plat in platforms:
            value = bench_results[cat][name][plat]
            bench_results[cat][name][plat] = float(value)
            line.append(value)
        print_line(f, line)
f.close()



for cat in ['shootout', 'misc']:
    for drop_plat in platforms:
        rest_plat = list(platforms)
        rest_plat.remove(drop_plat)

        f_from = open('results/speed_from_'+drop_plat+'_'+cat+'.perf', 'w')
        f_to = open('results/speed_to_'+drop_plat+'_'+cat+'.perf', 'w')


        # Find max (!ugly)
        from_max = 0
        to_max = 0
        for name in sorted(bench_results[cat].keys()):
            for plat in rest_plat:
                from_val =  round(bench_results[cat][name][drop_plat] / bench_results[cat][name][plat], 3)
                to_val =  round(bench_results[cat][name][plat] / bench_results[cat][name][drop_plat], 3)
                if to_val > to_max:
                    to_max = to_val
                if from_val > from_max:
                    from_max = from_val

        from_cluster=[]
        to_cluster=[]
        for plat in rest_plat:
            from_cluster.append(pretty_name(drop_plat)+'/'+pretty_name(plat))
            to_cluster.append(pretty_name(plat)+'/'+pretty_name(drop_plat))

        # Print ploting info
        from_cluster = ';'.join(from_cluster)+'\n'
        to_cluster = ';'.join(to_cluster)+'\n'
        from_max = str(from_max + 1)
        to_max = str(to_max + 1)
        print_plot_info(f_from, from_cluster, from_max)
        print_plot_info(f_to, to_cluster, to_max)



        from_line = []
        to_line = []
        for plat in rest_plat:
            from_line.append(drop_plat + '/' + plat)
            to_line.append(plat + '/' + drop_plat)
        print_line(f_from, ['#Name'] + from_line)
        print_line(f_to, ['#Name'] + to_line)


        for name in sorted(bench_results[cat].keys()):
            from_line = [name]
            to_line = [name]
            for plat in rest_plat:
                from_val =  round(bench_results[cat][name][drop_plat] / bench_results[cat][name][plat], 3)
                to_val =  round(bench_results[cat][name][plat] / bench_results[cat][name][drop_plat], 3)
                from_line.append(from_val)
                to_line.append(to_val)


            print_line(f_from, from_line)
            print_line(f_to, to_line)


        f_from.close()
        f_to.close()


for f in glob.glob('results/*.perf'):
    basename = f.split('.')[0]
    os.system('./gnuplot_scripts/bargraph.pl ' + f + ' > ' + basename + '.eps')






