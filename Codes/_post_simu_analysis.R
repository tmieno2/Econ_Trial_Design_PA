

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
#' # MC simulation
# /*===========================================================
tic()
mc_sim_results <-  
    lapply(
        1:nrow(field_with_design),
        function(x) run_mc_sim(x, field_with_design)
    ) %>%
    rbindlist()
toc()

#* save the field parameters
saveRDS(mc_sim_results, here("Data", "mc_sim_results.rds"))




# /*===========================================================
#' # Get spatial measures
# /*===========================================================
# field_with_design <- readRDS(here("Data", "field_with_design.rds"))
# 
# tic()
# spatial_measure_results <-
#     lapply(
#         1:nrow(field_with_design),
#         function(x) run_spatial_measures(x, field_with_design)
#     ) %>%
#     rbindlist()
# toc()
# saveRDS(spatial_measure_results, here("Data", "spatial_measure_results.rds"))


#'     What results are generated:
#'     1. gwr coefficients estimates (at aunit level)
#'             [scenario, sim, aunit_id, b0_hat, b1_hat, b2_hat, ...]
#'     2. spatial measures (field level)
#'             [scenario, sim, MST, moran, accidental corr, ...]
#'     3. simulated N treatment rates (cell level)
#'             [scenario, sim, cell_id, N, Nid]


################################################################################
###
###                                 Load Data                                ###
###
################################################################################

#=== load generated parameters ===#
field_data <- readRDS(here("Data/field_data.rds"))
field_with_design <- readRDS(here("Data/field_with_design.rds"))
field_parameters <- readRDS(here("Data/field_parameters.rds"))

#=== load simulation results data ===#
mc_sim_results <- readRDS(file=here("Data/mc_sim_results.rds"))

#=== profit data tibble ===#
pi_tib <- tibble(field_col = field_parameters$field_col) %>% 
    rowwise() %>% 
    mutate(pi_data = list(nrow(field_parameters))) %>% 
    print()

#=== Loop over simulation scenarios ===#
for(i in 1:nrow(field_parameters)){
    
    #=== field sizes ===#
    field_col_i <- field_parameters[i, "field_col"] %>% unlist()
    
    #=== data ===#
    cell_data <- field_parameters[i,] %>% pull(field_pars) %>% 
        .[[1]]
    field_dt <- field_parameters[i,] %>% pull(field_sf) %>% 
        .[[1]] %>% data.table()
    cell_data <- cell_data[field_dt[, .(cell_id, aunit_id)], on = "cell_id"]
    
    #=== simulation results data ===#
    est_data_all <- mc_sim_results[field_col==field_col_i,]
    
    
    
################################################################################
###
###                         Profitability Analysis                           ###
###
################################################################################
    
    
    # -------------------
    #  Loop over designs
    # -------------------
    pi_data_ls <- list()
    for(ds in est_data_all$design_name){
        
        # --------------------------
        #  Response estimation data
        # --------------------------
        est_data <- est_data_all %>% filter(design_name==ds) %>% pull(sim_results) %>%
            .[[1]] %>% 
            unnest(mc_results) %>% 
            data.table() %>% 
            .[, design := ds]
        
        #=== price scenarios ===#
        pCorn <- est_data_all %>% filter(design_name==ds) %>% pull(pCorn)
        pN <- est_data_all %>% filter(design_name==ds) %>% pull(pN)
        
        
        # -----------------
        # Merge data
        # -----------------
        #=== merge data: true parameters (cell) + estimation (aunit)
        merge_data <- cell_data[est_data, on = c("sim", "aunit_id"),
                                allow.cartesian=TRUE]
        
        
        # -----------------
        # Economic analysis
        # -----------------
        econ_data <- merge_data %>% 
            #--- True optima ---#
            .[,opt_N := (pN/pCorn-b1)/(2*b2)] %>%
            .[,opt_N := pmin(Nk, opt_N)] %>%
            .[,opt_N := pmax(0, opt_N)] %>%
            .[,yield_opt := gen_yield_QP(b0,b1,b2,Nk,opt_N)] %>%
            .[,pi_opt := pCorn*yield_opt - pN*opt_N] %>%
            #--- GWR: regular ---#
            .[,yield_gwr_1 := gen_yield_QP(b0,b1,b2,Nk,opt_N_gwr_1)] %>%
            .[,pi_gwr_1 := pCorn*yield_gwr_1 - pN*opt_N_gwr_1] %>%
            .[,pi_gwr_1 := pi_gwr_1 - pi_opt] %>% 
            #--- GWR: constant N2 coef ---#
            .[,yield_gwr_2 := gen_yield_QP(b0,b1,b2,Nk,opt_N_gwr_2)] %>%
            .[,pi_gwr_2 := pCorn*yield_gwr_2 - pN*opt_N_gwr_2] %>%
            .[,pi_gwr_2 := pi_gwr_2 - pi_opt] %>% 
            #--- BRF ---#
            .[,yield_brf := gen_yield_QP(b0,b1,b2,Nk,opt_N_brf)] %>%
            .[,pi_brf := pCorn*yield_brf - pN*opt_N_brf] %>%
            .[,pi_brf := pi_brf - pi_opt] %>% 
            print()
        
        
        # -------------------
        # Field level profit
        # -------------------
        pi_data_ls[[ds]] <- econ_data %>%
            .[, lapply(.SD, mean), by=.(sim), 
              .SDcols=c("pi_gwr_1", "pi_gwr_2", "pi_brf")] %>% 
            .[, design := ds] %>% 
            print()   
    }
    
    pi_tib$pi_data[[i]] <- rbindlist(pi_data_ls)
    
}


# -----------------------------
# profit data of all scenarios
# -----------------------------
pi_data <- pi_tib %>% unnest(pi_data) %>% data.table() %>% print()

saveRDS(pi_data, here('Results/pi_data.rds'))






