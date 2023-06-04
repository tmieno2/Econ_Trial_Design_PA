

#' Model Estimation and Economic Analysis

#'     What results are generated:
#'     1. gwr coefficients estimates (at aunit level)
#'             [scenario, sim, aunit_id, b0_hat, b1_hat, b2_hat, ...]
#'     2. spatial measures (field level)
#'             [scenario, sim, MST, moran, accidental corr, ...]
#'     3. simulated N treatment rates (cell level)
#'             [scenario, sim, cell_id, N, Nid]



rm(list = ls())

# /*=================================================*/
#' # Settings
# /*=================================================*/

# === Packages ===#
library(sp)
library(spdep)
library(spatialreg)
library(gstat)
library(sf)
library(GWmodel)
library(grf)
library(glmnet)
library(scam)
library(parallel)
library(mgcv)
library(dplyr)
library(ggplot2)
library(magrittr)
library(data.table)
library(here)
library(magic)
library(tictoc)
library(tidyverse)
library(stringr)

# === Set working directory ===#
setwd(here())

# === Load functions ===#
fs::dir_ls(here("Codes/Functions"), full.names = TRUE) %>%
  lapply(., function(x) source(x))


################################################################################
###
###                             Model Estimation                             ###
###
################################################################################

# === load simulated reg data ===#
field_with_design <- readRDS(here("Data/field_with_design.rds"))


# /*=================================================*/
#' # Model estimation based on MC sim data
# /*=================================================*/

#* range of simulations
r1 = 0
r2 = 1000

#* estimate yield responses and EONRs
tic()
mc_sim_results <-
  lapply(
    1:nrow(field_with_design),
    function(x) run_mc_sim(x, field_with_design)
  ) %>%
  rbindlist()
toc()

#* save the field parameters
saveRDS(mc_sim_results, here("Results", 
                             paste0("mc_sim_results_", r1+1, "-", r2, ".rds")))




################################################################################
###
###                         Profitability Analysis                           ###
###
################################################################################

# === load field data ===#
field_data <- readRDS(here("Data/field_data.rds"))

# === load true parameters ===#
cell_data <- readRDS(here("Data/field_parameters.rds")) %>%
  rowwise() %>%
  # === match cell_id and aunit_id ===#
  mutate(
    field_pars = list(
      data.table(field_sf)[, .(cell_id, aunit_id)] %>%
        field_pars[., on = "cell_id"] %>%
        # === drop unused parameters to save space ===#
        .[, plateau := NULL] %>%
        .[, m_error := NULL] %>%
        .[, N_error := NULL]
    )
  ) %>%
  dplyr::select(field_col, field_pars)


## -------------------------------------
## Re-organize estimation results data
## -------------------------------------
est_data <-
  mc_sim_results %>%
  dplyr::select(field_col, design_name, sim_results) %>%
  rowwise() %>%
  mutate(
    sim_results = list(
      unnest(sim_results, cols = mc_results) %>%
        # === drop unused estimation data columns ===#
          dplyr::select(-c(b0_hat_1, b0_hat_2, b1_hat_1, b1_hat_2, b2_hat_1, b2_hat_2))
    )
  ) %>%
  mutate(sim_results = list(sim_results %>% nest_by(pCorn, pN))) %>%
  unnest(sim_results) %>%
  rowwise()



## -----------------------------
## Calculate profits
## -----------------------------

pi_data_ls <- list()
for (i in 1:nrow(est_data)) {
    # === true pars data for loop i ===#
    cell_data_i <-
        cell_data %>%
        filter(field_col == est_data[i, ]$field_col) %>%
        pull(field_pars) %>% .[[1]]
    
    pi_data_ls[[i]] <- est_data[i, ] %>%
        rowwise() %>% 
        # === merge est data with true pars ===#
        mutate(
            data = list(
                merge(
                    # cell_data %>% filter(field_col == field_col) %>% pull(field_pars) %>% .[[1]],
                    cell_data_i,
                    data.table(data),
                    by = c("sim", "aunit_id")
                )
            )
        ) %>%
        # === cell level profits ===#
        mutate(
            data = list(
                data %>%
                    #--- True cell-level EONR ---#
                    .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
                    .[, opt_N := pmin(Nk, opt_N)] %>%
                    .[, opt_N := pmax(0, opt_N)] %>%
                    #--- True optimal profit ---#
                    .[, yield := gen_yield_QP(b0, b1, b2, Nk, opt_N)] %>%
                    .[, pi_opt := pCorn * yield - pN * opt_N] %>%
                    #--- scam ---#
                    .[, yield := gen_yield_QP(b0, b1, b2, Nk, opt_N_scam)] %>%
                    .[, pi := pCorn * yield - pN * opt_N_scam] %>%
                    .[, pi_scam := pi - pi_opt] %>%
                    #--- GWR: regular ---#
                    .[, yield := gen_yield_QP(b0, b1, b2, Nk, opt_N_gwr_1)] %>%
                    .[, pi := pCorn * yield - pN * opt_N_gwr_1] %>%
                    .[, pi_gwr_1 := pi - pi_opt] %>%
                    #--- GWR: constant N2 coef ---#
                    .[, yield := gen_yield_QP(b0, b1, b2, Nk, opt_N_gwr_2)] %>%
                    .[, pi := pCorn * yield - pN * opt_N_gwr_2] %>%
                    .[, pi_gwr_2 := pi - pi_opt] %>%
                    #--- MA_CF ---#
                    .[, yield := gen_yield_QP(b0, b1, b2, Nk, opt_N_macf)] %>%
                    .[, pi := pCorn * yield - pN * opt_N_macf] %>%
                    .[, pi_macf := pi - pi_opt] %>%
                    #--- BRF ---#
                    .[, yield := gen_yield_QP(b0, b1, b2, Nk, opt_N_brf)] %>%
                    .[, pi := pCorn * yield - pN * opt_N_brf] %>%
                    .[, pi_brf := pi - pi_opt]
            )
        ) %>%
        # === Field level profit ===#
        mutate(
            data = list(
                data[, lapply(.SD, mean),
                     by = .(sim),
                     .SDcols = c(
                         "pi_scam", "pi_gwr_1", "pi_gwr_2",
                         "pi_macf", "pi_brf"
                     )
                ]
            )
        )
}

pi_data <- rbindlist(pi_data_ls)
object.size(pi_data) / 1024^2

pi_data <- rbindlist(pi_data_ls) %>%
  # === convert to data table format ===#
  unnest(data) %>%
  data.table()
object.size(pi_data) / 1024^2



saveRDS(pi_data, here("Results/pi_data.rds"))



# /*===========================================================
#' # Get spatial measures
# /*===========================================================
tic()
spatial_measure_results <-
    lapply(
        1:nrow(field_with_design),
        function(x) run_spatial_measures(x, field_with_design)
    ) %>%
    rbindlist()
toc()
saveRDS(spatial_measure_results, here("Results/spatial_measure_results.rds"))



