#!/bin/sh

## Executes all benchmarks
run_all ()
{
    OTP_ROOT=$1
    ITERS=$2
    echo "Running all benchmark classes..."

    ## Look for all available Classes to run
    for c in `find ebin/ -maxdepth 1 -mindepth 1 -type d`; do
        CLASS=`basename $c`
        run_class $OTP_ROOT $CLASS
    done
}

run_class ()
{
    OTP_ROOT=$1
    CLASS=$2
    echo "   [Class] $CLASS"

    ## Get failing
    if [ -r failing ]; then
        skipped=`cat failing`
    else
        skipped=
    fi

    for f in `ls ebin/$CLASS/*.beam`; do
        BENCH=`basename $f .beam`
        ## Skip file if in failing
        SKIP="no"
        for s in $skipped; do
            if [ "$BENCH" = "$s" ]; then
                SKIP="yes"
                break
            fi
        done
        if [ "$SKIP" = "yes" ]; then
            continue
        fi
        ## Else run benchmark
        run_benchmark $OTP_ROOT $BENCH
    done
}

run_benchmark ()
{
    OTP_ROOT=$1
    BENCH=$2
    echo "   --- $BENCH"

    EBIN_DIRS=`find ebin/ -maxdepth 1 -mindepth 1 -type d`
    $OTP_ROOT/bin/erl -pa ebin/ $EBIN_DIRS -noshell -s run_benchmark run $BENCH -s erlang halt
}

main ()
{
    if [ $# -eq 2 ]; then
        RUN=run_all
        OTP_ROOT=$1
        ITERS=$2
    else
        if [ $# -eq 3 ]; then
            RUN=run_class
            OTP_ROOT=$1
            ITERS=$2
            CLASS=$3
        else
            echo "Usage: `basename $0` OTP_DIR ITERATIONS <CLASS>"
            exit 1
        fi
    fi

    ## Run $ITERS times:
    for i in `seq 1 $ITERS`; do
        ## If runtime/results.res exists rename it:
        if [ -e results/runtime.res ]; then
            NEW_RES=runtime.res-`date +"%y.%m.%d-%H:%M:%S"`
            mv results/runtime.res results/$NEW_RES
        fi

        echo "Iter $i/$ITERS:"
        $RUN $OTP_ROOT $CLASS
        awk '{btl += $9 ;htl +=$11} END {print "Runtime BTL:", btl/NR, "Runtime HTL:", htl/NR}' results/runtime.res
    done
}

main $1 $2 $3
