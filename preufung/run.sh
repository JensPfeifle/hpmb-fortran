#!/bin/bash
threads=1
export OMP_NUM_THREADS=$threads
echo "Starting ./main with $threads thread..."
time (./main > pool.out)
python viewer.py