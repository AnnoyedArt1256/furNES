#!/bin/bash
if [ $# -eq 0 ]
then
    echo "No arguments supplied"
    echo "Make sure to run this command with an argument"
    echo "example: convert.sh test_file.fur"
else
python3 convert_to_asm.py $1
echo "converted .fur file to .asm!"
cd nsf
cl65 -d -vm -l nsf.lst -g -t nes -C nsf.cfg -m nsf.map -Ln nsf.lbl -o furNES-test.nsf nsf.asm
echo "compiled .nsf file at nsf/furNES-test.nsf"
cd ..
fi
