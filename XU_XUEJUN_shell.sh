#!/bin/bash
#----------------------------------------------------
# Example SLURM job script to run OpenMP applications
# on TACC's Maverick system.
#----------------------------------------------------
#SBATCH -J me397_job_name       			# Job name
#SBATCH -o me397_job_name.out   			# Name of stdout output file(%j expands to jobId)
#SBATCH -p gpu                   			# Submit to the 'normal' or 'development' queue
#SBATCH -N 1                        		# Total number of nodes requested 
#SBATCH -n 20                        		# Total number of mpi tasks requested
#SBATCH -t 4:00:00                 			# Run time (hh:mm:ss) - 4 hours
#SBATCH -A ME397M-Applied-Engin         	# 'ME397M-Applied-Engin' is the name of the project with the Maverick allocation
#SBATCH --xuejunxu@utexas.edu	            # email address to use
#SBATCH --mail-type=begin  	   				# email me when the job starts
#SBATCH --mail-type=end    	   				# email me when the job finishes

module load intel/15.0.3
module load mvapich2/2.1
module load Rstats

Rscript XU_XUEJUN_Rscript.R

