#!/bin/bash
#$ -j y
#$ -o log_load_results
#$ -N load_results

/usr3/graduate/ashiklom/.singularity/Rscript scripts/load_results.R
