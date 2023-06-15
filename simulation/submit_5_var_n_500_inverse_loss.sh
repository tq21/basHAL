#!/bin/bash
# Job name:
#SBATCH --job-name=job_1
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

R CMD BATCH --no-save 5_var_n_500_inverse_loss.R 5_var_n_500_inverse_loss.Rout
