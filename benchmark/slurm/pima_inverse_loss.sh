#!/bin/bash
# Job name:
#SBATCH --job-name=pima_inverse_loss
#
# Partition:
#SBATCH --partition=high
#
# Wall clock limit ('0' for unlimited):
#SBATCH --time=168:00:00
#
# Number of nodes for use case:
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#
# Mail type:
#SBATCH --mail-type=all
#
# Mail user:
#SBATCH --mail-user=sky.qiu@berkeley.edu

R CMD BATCH --no-save ../pima_inverse_loss.R logs/pima_inverse_loss.Rout
