#!/bin/bash
echo "Note: one thread is fastest due to overhead..."
read -p "How many threads?  " threads
export OMP_NUM_THREADS=$threads
echo "Starting ./main with $threads threads..."
time (./main > pool.out)
echo "View results with 'python viewer.py?'"
select yn in "Yes" "No"; do
    case $yn in
        Yes ) python viewer.py ; break;;
        No ) exit;;
    esac
done
