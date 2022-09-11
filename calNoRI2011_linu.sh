#!/bin/bash
#SBATCH --account=def-tgan
#SBATCH --ntasks=1               # number of MPI processes
#SBATCH --mem-per-cpu=16384M      # memory; default unit is megabytes
#SBATCH --time=10-0000           # time (DD-HHMM)
module load nixpkgs/16.09
module load gcc/7.3.0
module load netcdf/4.6.1
module load r/3.6.1
module load expat/2.2.6
module load gdal/3.0.1
module load udunits/2.2.26
cd $PBS_O_WORKDIR
cd /scratch/xtan/Elsa_1/Semi2/Tasks
echo "Current working directory is `pwd`"
echo "Running on hostname `hostname`"
echo "Starting run at: `date`"
R --vanilla < calNoRI2011_linu.R

echo "Program diffuse finished with exit code $? at: `date`"
