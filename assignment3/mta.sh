#!/bin/bash

# source the .bash_profile containing $NDK/$SDK with correct path for tools
source $HOME/.bash_profile

# mool compiler directory
MOOL="/Users/perezmunoz/Google Drive/AY2015-2016/Semester 1/CS4212 Compiler Design/assignment3/mool_compiler_backend_v2"

# mool testcases directory
TC=$MOOL/testcases

# android ndk directory (set such value in your environment)
NDK=/usr/local/Cellar/android-ndk/r10e/platforms/android-19/arch-arm/

# retrieve the input corresponding to a flag
# -m is the mOOL input file
# -o is whether the user wants to use the optmised version (boolean)
# need to do:
    # if opmitisation
    # then use ir3_to_ARM_optimised.ml file
    # else use ir3_to_ARM_non_opt.ml file
while getopts "m:o:" opts; do
        case $opts in
                m) moolfile=$OPTARG ;;
                o) optimisation=$OPTARG ;;
        esac
done

# generates the arm filename from the mool input file and stores out value in global out
arm=$moolfile".s"
out="out"

# generates the arm code from the mool file
"$MOOL"/mOOL_compiler "$TC"/$moolfile > "$TC"/$arm

# generates the bytecode from the arm file (extension .s)
$CC -o "$TC"/$out "$TC"/$arm --sysroot=$NDK

printf "Start data transfer from local to android device...\n"
# copy file/dir to device. -p is used to show the progress of the transfer
adb push -p "$TC"/$out /data/local/tmp
printf "End data transfer from local to android device...\n"

# console output begin
printf "\n==== android output ====\n\n"

# run the shell command (placed after shell)
adb shell /data/local/tmp/$out

# console output end
printf "\n==== android output ====\n\n"

# clean the environment
adb shell rm /data/local/tmp/$out
rm "$TC"/$out
