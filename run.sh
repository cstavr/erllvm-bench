#!/usr/bin/env bash

## Executes all benchmarks
run_all ()
{
    OTP_ROOT=$1
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

calc ()
{
    echo $1 | bc -l
}

run_benchmark ()
{
    OTP_ROOT=$1
    OTP_ROOT_BEAM=$OTP_ROOT/otp_beam
    OTP_ROOT_HIPE=$OTP_ROOT/otp_hipe
    OTP_ROOT_ERLLVM=$OTP_ROOT/otp_erllvm
    BENCH=$2
    echo "   --- $BENCH"

    EBIN_DIRS=`find ebin/ -maxdepth 1 -mindepth 1 -type d`
    ## Run benchmarks and collect results (as floats!):
    BT_tmp=`$OTP_ROOT_BEAM/bin/erl -pa ebin/ $EBIN_DIRS -noshell -s run_benchmark run $BENCH beam -s erlang halt`
    BT=`calc $BT_tmp`
    HT_tmp=`$OTP_ROOT_HIPE/bin/erl -pa ebin/ $EBIN_DIRS -noshell -s run_benchmark run $BENCH hipe -s erlang halt`
    HT=`calc $HT_tmp`
    LT_tmp=`$OTP_ROOT_ERLLVM/bin/erl -pa ebin/ $EBIN_DIRS -noshell -s run_benchmark run $BENCH erllvm -s erlang halt`
    LT=`calc $LT_tmp`
    ## Print results to "resuls/runtime.res":
    printf "%-16s & %6.3f & %6.3f & %6.2f & %6.2f & %6.2f \\\\\\ \n" \
	$BENCH `calc $BT/$LT` `calc $HT/$LT` `calc $BT/1000` `calc $HT/1000` `calc $LT/1000` >> results/runtime.res
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
    ## Copy speedup.perf template and append results:
    cp $SCRIPTS_DIR/speedup.perf $TMP_PERF
    cat results/$INPUT >> $TMP_PERF
    ## Check that their are no unmet dependencies:
    echo -n "Checking for gnuplot..."
    command -v gnuplot > /dev/null 2>&1 || \
	{ echo "gnuplot is required but it's not installed. Aborting." >&2; exit 1; }
    echo " ok!"
    echo -n "Checking for fig2ps..."
    command -v gnuplot > /dev/null 2>&1 || \
	{ echo "fig2ps is required but it's not installed. Aborting." >&2; exit 1; }
    echo " ok!"
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
  Iter  = $ITERATIONS
  Run   = $RUN
  Class = $BENCH_CLASS
  OTP   = $OTP_ROOT
EOF
    fi

    ## Run $ITERATIONS times:
    for i in `seq 1 $ITERATIONS`; do
	echo "Iter $i/$ITERATIONS:"
	echo "### Benchmark BEAM/ErLLVM HiPE/ErLLVM BEAM HiPE ErLLVM" > results/runtime.res
	$RUN $OTP_ROOT $BENCH_CLASS
	awk '{btl += $3 ;htl += $5} END {print "Runtime BTL:", btl/NR, "Runtime HTL:", htl/NR}' \
	    results/runtime.res

	## Copy results to another .res file:
	NEW_RES=runtime-`date +"%y.%m.%d-%H:%M:%S"`.res
	mv results/runtime.res results/$NEW_RES

	plot_diagram $NEW_RES
    done
}

main $@