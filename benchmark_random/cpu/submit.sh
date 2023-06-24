#!/bin/bash
# Job name:
#SBATCH --job-name=cpu
#
# Partition:
#SBATCH --partition=low
#
# Wall clock limit ('0' for unlimited):
#SBATCH --time=168:00:00
#
# Number of nodes for use case:
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#
# Mail type:
#SBATCH --mail-type=all
#
# Mail user:
#SBATCH --mail-user=sky.qiu@berkeley.edu

R CMD BATCH --no-save random.R random.Rout
