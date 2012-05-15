#!/usr/bin/env bash

## Erlc flags to be used in each case:
HIPE_FLAGS="+native +'{hipe,[{regalloc,coalescing},o2]}'"
ERLLVM_FLAGS="+native +'{hipe,[o2,to_llvm]}'"

## Executes all benchmarks
run_all ()
{
    OTP=$1
    COMP=$2
    DEBUG=$3
    echo "Running all benchmark classes..."

    ## Look for all available Classes to run
    for c in `find ebin/ -maxdepth 1 -mindepth 1 -type d`; do
        CLASS=`basename $c`
        run_class $OTP $CLASS $COMP $DEBUG
    done
}

run_class ()
{
    OTP=$1
    CLASS=$2
    COMP=$3
    DEBUG=$4
    echo "   [Class] $CLASS"

    ## Get failing
    if [ -r failing ]; then
        skipped=`cat failing`
    else
        skipped=
    fi

    ## Get boilerplate
    BOILERPLATE=src/$CLASS/boilerplate
    if [ -r  $BOILERPLATE ]; then
        skipped="$skipped `cat $BOILERPLATE`"
    fi

    for f in `ls ebin/$CLASS/*.beam`; do
        BENCH=`basename $f .beam`
        ## Skip file if in failing or boileprlate
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
        run_benchmark $OTP $BENCH $CLASS $COMP $DEBUG
    done
}

run_benchmark ()
{
    OTP=$1
    BENCH=$2
    CLASS=$3
    COMP=$4
    DEBUG=$5
    echo "   --- $BENCH"

    EBIN_DIRS=`find ebin/ -maxdepth 1 -mindepth 1 -type d`

    $OTP/bin/erl -pa ebin/ $EBIN_DIRS -noshell -s run_benchmark run \
        $BENCH $COMP -s erlang halt
}

collect_results ()
{
    echo "Collecting results..."

    echo "### Benchmark BEAM/ErLLVM HiPE/ErLLVM BEAM HiPE ErLLVM" \
        > results/runtime.res
    pr -m -t results/runtime_beam-err.res results/runtime_hipe-err.res \
        results/runtime_erllvm-err.res \
        | gawk '{print $1 "\t" $2/$4 "\t" $4/$6 "\t\t" $2 "\t" $4 "\t" $6}' \
        >> results/runtime.res

    ## Print average performance results of current execution:
    awk '{btl += $2; htl += $3} END {print "Runtime BTL:", btl/(NR-1), \
        "Runtime HTL:", htl/(NR-1)}' results/runtime.res
}

plot_diagram ()
{
    INPUT=$1
    HASH=`basename $INPUT .res`
    TMP_DIR=/dev/shm/erllvm-bench-diagrams
    SCRIPTS_DIR=gnuplot_scripts
    DIAGRAMS_DIR=diagrams
    TMP_PERF=$TMP_DIR/speedup.perf
    echo "Plotting results..."

    mkdir -p $TMP_DIR
    ## Copy speedup.perf template and append speedup results only:
    cp $SCRIPTS_DIR/speedup.perf $TMP_PERF
    cat results/$INPUT | awk '{print $1 "\t& " $2 "\t& " $3}' >> $TMP_PERF

    ## Create diagram in diagram:
    $SCRIPTS_DIR/bargraph.pl $TMP_PERF > $DIAGRAMS_DIR/$HASH.eps 2> /dev/null
    rm -rf $TMP_DIR
}

usage ()
{
    cat << EOF
Usage: $0 options OTP_ROOT

This script runs the benchmarks using the provided OTP directory (first
non-option argument) as root and creates the corresponding diagrams.

In the OTP directory provided there should be 3 subdirectories
including complete OTP installations:
  * otp_beam: This OTP is used to run BEAM stuff and all modules are
              in BEAM.
  * otp_hipe: This OTP is used to run HiPE stuff and is has been
              compiled with --enable-native-libs.
  * otp_erllvm: This OTP is used to run ErLLVM stuff and is has been
                compiled with --enable-native-libs and [to_llvm].

OPTIONS:
  -h    Show this message
  -a    Run all available benchmarks
  -c    Benchmark class to run
  -n    Number of iterations

Examples:
  1) $0 -c shootout -n 3 ~/git/otp
  2) $0 -a ~/git/otp
  3) $0 -a -n 5 ~/git/otp
EOF
}

main ()
{
    RUN=
    BENCH_CLASS=
    ITERATIONS=1
    DEBUG=0

    while getopts "hadn:c:" OPTION; do
        case $OPTION in
            h|\?)
                usage
                exit 0
                ;;
            a)
                RUN=run_all
                ;;
            c) ## Run *only* specified benchmark class:
                RUN=run_class
                BENCH_CLASS=$OPTARG
                ;;
            n)
                ITERATIONS=$OPTARG
                ;;
            d)
                DEBUG=1
                ;;
        esac
    done

    ## $1 is now the first non-option argument, $2 the second, etc
    shift $(($OPTIND - 1))
    OTP_ROOT=$1

    ## If RUN is not set something went wrong (probably the script was called
    ## with no args):
    if [ -z $RUN ]; then
        usage
        exit 1
    fi

    if [ $DEBUG -eq 1 ]; then
        cat << EOF
-- Debug info:
  Iter         = $ITERATIONS
  Run          = $RUN
  Class        = $BENCH_CLASS
  OTP          = $OTP_ROOT
  HiPE_FLAGS   = $HIPE_FLAGS
  ErLLVM_FLAGS = $ERLLVM_FLAGS
EOF
    fi

    # Run $ITERATIONS times:
    for i in `seq 1 $ITERATIONS`; do
        echo "Iter $i/$ITERATIONS:"

        for COMP in "beam" "hipe" "erllvm"; do
            ## Proper compile
            make clean > /dev/null
            echo "  Re-compiling with $COMP..."
            ## Use the appropriate ERLC flags
            ERL_CFLAGS=
            if [ "$COMP" = "hipe" ]; then
                ERL_CFLAGS=$HIPE_FLAGS
            fi
            if [ "$COMP" = "erllvm" ]; then
                ERL_CFLAGS=$ERLLVM_FLAGS
            fi
            make ERLC=$OTP_ROOT/otp_$COMP/bin/erlc ERL_COMPILE_FLAGS="$ERL_CFLAGS" \
                | pv -p > /dev/null

            ## Proper run
            echo "  Running $COMP..."
            $RUN $OTP_ROOT/otp_$COMP $BENCH_CLASS $COMP $DEBUG
        done

        ## Collect results in results/runtime.res:
        collect_results

        ## Plot results:
        plot_diagram runtime.res

        ## Backup all result files & diagrams to unique .res files:
        NEW_SUFFIX=`date +"%y.%m.%d-%H:%M:%S"`
        mv results/runtime.res results/runtime-$NEW_SUFFIX.res
        mv results/runtime_beam.res results/runtime_beam-$NEW_SUFFIX.res
        mv results/runtime_hipe.res results/runtime_hipe-$NEW_SUFFIX.res
        mv results/runtime_erllvm.res results/runtime_erllvm-$NEW_SUFFIX.res
        mv diagrams/runtime.eps diagrams/runtime-$NEW_SUFFIX.eps
    done
}

main $@
