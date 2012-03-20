#!/bin/sh

## Executes all benchmarks
main ()
{
    echo "Running all benchmark classes..."

    ## If runtime/results.res exists rename it:
    if [ -e results/runtime.res ]; then
        NEW_RES=runtime.res-`date +"%y.%m.%d-%H:%M:%S"`
        mv results/runtime.res results/$NEW_RES
    fi

    ## Get failing
    if [ -r failing ]; then
        skipped=`cat failing`
    else
        skipped=
    fi

    for i in `seq 1 $2`; do
        echo "Iter $i:"
        ## Look for all available Classes to run
        for c in `find ebin/ -maxdepth 1 -mindepth 1 -type d`; do
            class=`basename $c`
            run_class $class
        done
        awk '{btl += $9 ;htl +=$11} END {print "Runtime BTL:", btl/NR, "Runtime HTL:", htl/NR, "lines", NR}' results/runtime.res
    done
}

run_class ()
{
    class=$1
    echo "   [Class] $class"
    for f in `ls ebin/$class/*.beam`; do
        bench=`basename $f .beam`
        ## Skip file if in failing
        skip="no"
        for s in $skipped; do
            if [ "$bench" = "$s" ]; then
                skip="yes"
                break
            fi
        done
        if [ "$skip" = "yes" ]; then
            continue
        fi
        ## Else run benchmark
        run_benchmark $bench $class
    done
}

run_benchmark ()
{
    echo "   --- $1"
    EBIN_DIRS=`find ebin/ -maxdepth 1 -mindepth 1 -type d`
    erl -pa ebin/ $EBIN_DIRS -noshell -s run_benchmark run $1 -s erlang halt
}

if [ $# -ne 2 ]; then
    echo "Usage: `basename $0` OTP_DIR ITERATIONS"
    exit 1
fi

main $1 $2
