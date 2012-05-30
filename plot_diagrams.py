import os
import glob
SYS = ['beam', 'hipe', 'erllvm', 'erjang', 'erjang_1']

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

for name in SYS[1:]:
    assert len(results[name]) == len(results[SYS[0]])

benchmarks = results['beam'].keys()
platforms = SYS

bench_results = {}
for bench_name in benchmarks:
    bench_results[bench_name] = {}
    for name in platforms:
        bench_results[bench_name][name] = results[name][bench_name]

f = open('results/runtime_total.res', 'w')
line = 'Name'.ljust(15)
for plat in platforms:
    line += plat.ljust(15)
f.write('#'+line+'\n')

for name in benchmarks:
    line = name.ljust(15)
    for plat in platforms:
        line += (bench_results[name][plat]).ljust(15)
    f.write(line+'\n')
f.close()

mmax = 0
for plat in platforms:
    for bench in benchmarks:
        value = float(bench_results[bench][plat])
        bench_results[bench][plat] = value
        if value > mmax:
            mmax = value

foo = "# clustered graph example from Derek Bruening's CGO 2005 talk\n"
cluster = "=cluster;Beam;HiPE;ErLLVM;Erjang-1;Erjang-6i\n"
foo1 = \
"# green instead of gray since not planning on printing this\n"\
"colors=grey5,med_blue,light_green,red,yellow\n"\
"=arithmean\n"\
"meanlabel=Average\n"\
"=table\n"\
"yformat=%g\n"
mmax = "max= %s" % str(mmax)
foo3 =\
"""
ylabel=Runtime Speedup
xlabel=Benchmark
rotateby=-40
horizline=1
# stretch it out in x direction
# extraops=set label "5.66" at 14,4.2 right font "Times,10"



"""

for cat in ['shootout', 'misc']:
    for drop_plat in platforms:
        rest_plat = list(platforms)
        rest_plat.remove(drop_plat)
        f_from = open('results/speed_from_'+drop_plat+'_'+cat+'.perf', 'w')
        f_to = open('results/speed_to_'+drop_plat+'_'+cat+'.perf', 'w')

        from_max = 0
        to_max = 0
        for name in benchmarks:
            if cat == 'shootout':
                fname1 = 'src/shootout/'+name+'.erl'
                fname2 = ''
            else:
                fname1 = 'src/small/'+name+'.erl'
                fname2 = 'src/orbit-int/'+name+'.erl'
            if os.path.exists(fname1) or os.path.exists(fname2):
                for plat in rest_plat:
                    from_val =  round(bench_results[name][drop_plat] / bench_results[name][plat], 3)
                    to_val =  round(bench_results[name][plat] / bench_results[name][drop_plat], 3)

                    if to_val > to_max:
                        to_max = to_val
                    if from_val > from_max:
                        from_max = from_val

        from_cluster=[]
        to_cluster=[]
        for plat in rest_plat:
            from_cluster.append(pretty_name(drop_plat)+'/'+pretty_name(plat))
            to_cluster.append(pretty_name(plat)+'/'+pretty_name(drop_plat))

        from_cluster = '=cluster;'+';'.join(from_cluster)+'\n'
        to_cluster = '=cluster;'+';'.join(to_cluster)+'\n'
        from_max = 'max=%s' % str(from_max+1)
        to_max = 'max=%s' % str(to_max+1)

        f_from.write(foo+from_cluster+foo1+from_max+foo3)
        f_to.write(foo+to_cluster+foo1+to_max+foo3)

        from_line = '# Name'.ljust(15)
        to_line = '# Name'.ljust(15)
        for plat in rest_plat:
            from_line += (drop_plat+'/'+plat).ljust(15)
            to_line += (plat+'/'+drop_plat).ljust(15)
        f_from.write(from_line+'\n')
        f_to.write(to_line+'\n')

        for name in benchmarks:
            if cat == 'shootout':
                fname1 = 'src/shootout/'+name+'.erl'
                fname2 = ''
            else:
                fname1 = 'src/small/'+name+'.erl'
                fname2 = 'src/orbit-int/'+name+'.erl'
            if os.path.exists(fname1) or os.path.exists(fname2):
                from_line = name.ljust(15)
                to_line = name.ljust(15)
                for plat in rest_plat:
                    from_val =  round(bench_results[name][drop_plat] / bench_results[name][plat], 3)
                    to_val =  round(bench_results[name][plat] / bench_results[name][drop_plat], 3)
                    from_line += str(from_val).ljust(15)
                    to_line += str(to_val).ljust(15)


                f_from.write(from_line+'\n')
                f_to.write(to_line+'\n')


        f_from.close()
        f_to.close()


for f in glob.glob('results/*.perf'):
    basename = f.split('.')[0]
    os.system('./gnuplot_scripts/bargraph.pl ' + f + ' > ' + basename + '.eps')






