

#' Post Simulation Economic Analysis


rm(list=ls())

#/*=================================================*/
#' # Settings
#/*=================================================*/

#=== Packages ===#
library(sp)
library(spatialreg)
library(gstat)
library(GWmodel)
library(scam)
library(mgcv)       # gam()
library(parallel)
library(spdep)
library(mgcv)
library(dplyr)
library(ggplot2)
library(magrittr)
library(data.table)
library(sf)
library(here)
library(magic)
library(tictoc)
library(tidyverse)
library(stringr)

#=== Set working directory ===#
setwd(here())

#=== Load functions ===#
fs::dir_ls(here("Codes", "Functions"), full.names = TRUE) %>%
    lapply(., function(x) source(x))




# /*===========================================================
#' # combine field_parameters
# /*===========================================================

#------ load simulation results data ------#
field_parameters_1 <- readRDS(file=here("Data/field_parameters_200.rds"))
field_parameters_2 <- readRDS(file=here("Data/more/field_parameters_800.rds"))

#------ combine data for simulation scenarios ------#
for(i in 1:nrow(field_parameters_1)){
    df1 <- field_parameters_1[i,] %>% pull(field_pars) %>% 
        .[[1]]
    df2 <- field_parameters_2[i,] %>% pull(field_pars) %>% 
        .[[1]] %>% 
        .[, sim := sim + 200]
    df <- rbind(df1, df2)
    
    field_parameters_1[i,]$field_pars[[1]] <- df
    
}

#------ save combined simulation results data ------#
field_parameters <- field_parameters_1
saveRDS(field_parameters, here("Data/mc_sim_results.rds"))




# /*===========================================================
#' # combine mc_sim_results
# /*===========================================================

#------ load simulation results data ------#
mc_sim_results_1 <- readRDS(file=here("Data/mc_sim_results_200.rds"))
mc_sim_results_2 <- readRDS(file=here("Data/more/mc_sim_results_800.rds"))

#------ combine data for simulation scenarios ------#
for(i in 1:nrow(mc_sim_results_1)){
    df1 <- mc_sim_results_1[i,]$sim_results[[1]]
    df2 <- mc_sim_results_2[i,]$sim_results[[1]]
    df <- rbind(df1, df2)
    df$sim <- 1:nrow(df)
    
    mc_sim_results_1[i,]$sim_results[[1]] <- df
    
}

#------ save combined simulation results data ------#
mc_sim_results <- mc_sim_results_1
saveRDS(mc_sim_results, here("Data/mc_sim_results.rds"))



# /*===========================================================
#' # combine spatial_measure_results
# /*===========================================================

#------ load simulation results data ------#
spatial_measure_results_1 <- readRDS(file=here("Data/spatial_measure_results_200.rds"))
spatial_measure_results_2 <- readRDS(file=here("Data/more/spatial_measure_results_800.rds"))

#------ combine data for simulation scenarios ------#
for(i in 1:nrow(spatial_measure_results_1)){
    df1 <- spatial_measure_results_1[i,]$spatial_results[[1]]
    df2 <- spatial_measure_results_2[i,]$spatial_results[[1]]
    df <- rbind(df1, df2)
    df$sim <- 1:nrow(df)
    
    spatial_measure_results_1[i,]$spatial_results[[1]] <- df
    
}

#------ save combined simulation results data ------#
spatial_measure_results <- spatial_measure_results_1
saveRDS(spatial_measure_results, here("Data/spatial_measure_results.rds"))










