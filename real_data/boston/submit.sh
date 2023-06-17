#!/bin/bash
# Job name:
#SBATCH --job-name=boston
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

R CMD BATCH --no-save inverse_loss.R inverse_loss.Rout &
R CMD BATCH --no-save double_weight_v3.R double_weight_v3.Rout
