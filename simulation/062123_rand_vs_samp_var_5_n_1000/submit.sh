#!/bin/bash
# Job name:
#SBATCH --job-name=rand_vs_samp
#
# Partition:
#SBATCH --partition=low
#
# Wall clock limit ('0' for unlimited):
#SBATCH --time=168:00:00
#
# Number of nodes for use case:
#SBATCH --nodes=2
#SBATCH --ntasks=2
#SBATCH --cpus-per-task=32
#
# Mail type:
#SBATCH --mail-type=all
#
# Mail user:
#SBATCH --mail-user=sky.qiu@berkeley.edu

R CMD BATCH --no-save run_rand.R run_rand.Rout &
R CMD BATCH --no-save run_samp.R run_samp.Rout &

wait
