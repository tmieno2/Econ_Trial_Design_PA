# Instruction to reproduce the results in "paper title" published in Precision Agriculture

This repository contains the R codes to replicate the figures, tables, and the results of the simulation analysis conducted in the article. Please follow the steps below:

1. Clone the repository to your computer
2. Open the R project file Econ_Trial_Design.Rproj 
3. Run the following files in the `Codes` folder in the following order:
   + 0_spatial_measure_ranking.R: This file computes the spatial property measures and select the high-ranked and low-ranked trial designs used for the simulation. 
   + 1_prepare_data.R: This file generates the simulated on-farm precision experiment data.
   + 2_model_estimation.R: This file estimates the site-specific yield responses based on simulated data, and creates economic evaluations of the trial designs.
   + 3_figures.R: This file creates the diagrams to show the economic evaluation results of this simulation study.