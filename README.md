# Instruction to reproduce the results in "paper title" published in Precision Agriculture

This supplementary material contains the R codes to replicate the data simulation and estimation procedure of this study.
Unzip the file to your local folder, and open the R project file TrialDesignEcon.prj. 
Run the following R code files in the Codes folder by order:

0_spatial_measure_ranking.R
This file computes the spatial property measures and select the high-ranked and low-ranked trial designs used for the simulation. 

1_prepare_data.R
This file generates the simulated on-farm precision experiment data.

2_model_estimation.R
This file estimates the site-specific yield responses based on simulated data, and creates economic evaluations of the trial designs.

3_figures.R
This file creates the diagrams to show the economic evaluation results of this simulation study.