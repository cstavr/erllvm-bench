#!/bin/sh
# This script executes all .erl files in this directory (function
# test/0) and prints the runtime both for BEAM and native all tests
# with coalescing regalloc

if [ $# -ne 2 ]
then
  echo "Usage `basename $0` OTP_DIR Loops"
  echo "Example `basename $0` ~/code/otp 1"
  exit $E_BADARGS
fi

if test -n "$USER"; then
   USER=`whoami`
   export USER
fi

while test 1=1
do
    case "$1" in
     *--rts_opt*)
            shift
            rts_options=$1
            shift
            ;;
     *)
            break
            ;;
    esac
done
##
## OSH dir argument
##
OSH_DIR=$1

HIPE_RTS=$OSH_DIR/bin/erl

GREP="grep -i"
MSG_FILE=/tmp/hipe_bm_msg.$USER
RES_FILE=/tmp/hipe_bm_res.$USER

if test ! -x "$HIPE_RTS"; then
    echo "Can't execute $HIPE_RTS"
    echo "aborting..."
    echo "Can't execute $HIPE_RTS" >$MSG_FILE
    HOSTNAME=`hostname`
    echo "Aborted bm run on $HOSTNAME..." >> $MSG_FILE
    Mail -s "BM run aborted" $USER < $MSG_FILE
    rm -f $MSG_FILE    exit
fi

lockfile=lock.test
testdir=`pwd`

trap 'rm -f $testdir/$lockfile; exit 1' 1 2 15

if test -f $testdir/$lockfile; then
    cat <<EOF

  The lock file ./$lockfile exists.
  Probably bm is already running...
  If not, remove
        ./$lockfile
  and continue
========================================================================

EOF
  exit
else
   echo $$ > $lockfile
fi


if test -f "$RES_FILE"; then
  echo "There was an old $RES_FILE... removing"
  rm -f $RES_FILE
fi

make OTP_DIR=$OSH_DIR

echo %========================================================================
echo %====================== Running Benchmarks ==============================
echo %========================================================================
echo 'Results will be placed in results/runtime.res'

if [ ! -d results/ ]; then
  mkdir results/
fi

for i in `seq 1 $2`
do
  $HIPE_RTS -pa "." -noshell -s bm run
  cp results/runtime.res results/runtime_$i
  awk '{btl += $9 ;htl +=$11} END { print  "Runtime BTL: ", btl/NR, "Runtime HTL: ", htl/NR,"lines",NR }' results/runtime_$i >> results/runtime_$i
done

HOSTNAME=`hostname`

rm -f $RES_FILE
rm -f $lockfile

echo %========================================================================
