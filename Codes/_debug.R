

#*     Debug codes using a single simulation

rm(list=ls())

## ==============================================================
##                          Preparation                        =
## ==============================================================

# === Packages ===#
library(sp)
library(spdep)
library(spatialreg)
library(sf)
library(raster)
library(data.table)
library(tidyverse)
library(dplyr)
library(magrittr)
library(gstat)
library(GWmodel)
library(grf)
library(mgcv)         # gam()
library(magic)
library(stringr)
library(ggplot2)
library(tictoc)
library(here)
options(stringsAsFactors = FALSE)

# === Set working directory ===#
setwd(here())

# === load functions ===#
#* source all the functions in the Functions folder  
fs::dir_ls(here("GitControlled/Codes/Functions"), full.names = TRUE) %>%
    lapply(., function(x) source(x))


# /*===========================================================
#' # Create a field
# /*===========================================================
#! Define field and plot characteristics here

#! Set the experiment parameters
plot_length = 12 # the length of a plot (in number of cells)
plot_width = 3 # the width of a plot (in number of cells)
cell_buffer = 1
aunit_length = 2 # the length of an analysis unit (in number of cells)
aunit_width = 3 # the width of an analysis unit (in number of cells)
cell = 6 # the length of a cell in meter
#* how wide the field is
field_col = 120 # the number of cell columns
#* how tall the field is
field_row = 72 # the number of row columns
sp_range = 600
gstat_model = "Sph"
#* prices
pCorn = 0.197 # $/kg 
pN = 0.882     # $/kg

#! Field map data
field_sf = make_field(
            field_col = field_col,
            field_row = field_row,
            aunit_length = aunit_length,
            aunit_width = aunit_width,
            cell = cell,
            cell_buffer = cell_buffer)



# /*===========================================================
#' # Add trial design layout by design
# /*===========================================================

design_layout = make_design_layout(plot_length, field_col)
design_name_ls <- design_layout$design_name

#! Pick a design
ds <- 4
design <- design_name_ls[ds]
plot_length <- design_layout$plot_length[ds]
cols_plot_in_block <- design_layout$cols_plot_in_block[ds]
rows_plot_in_block <- design_layout$rows_plot_in_block[ds]

# design layout
plot_block_id_data = gen_plot_block_ids(
            field_sf = field_sf,
            plot_length = plot_length,
            plot_width = plot_width,
            cols_plot_in_block = cols_plot_in_block,
            rows_plot_in_block = rows_plot_in_block,
            cell_buffer = cell_buffer
        )
field_sf = left_join(field_sf, plot_block_id_data, by = "cell_id")



# /*===========================================================
#' # Generate parameters
# /*===========================================================
cell_data <- gen_field_pars(
    sp_range = sp_range,
    gstat_model = gstat_model,
    field_sf = field_sf,
    nsim = 1)



# /*===========================================================
#' # Create regression data
# /*===========================================================
#* create regression data for each trial specification 
reg_data <- gen_reg_data_single(field_sf, cell_data)





# /*===========================================================
#' # MC simulation
# /*===========================================================

field_with_design <- readRDS(here("Shared/Data/field_with_design.rds"))

{
    i = 11

    w_data <- field_with_design[i, ]
    sim_results <-
        #* read the right regression data for this experimental setting from the data_file_name
        readRDS(here(w_data$data_file_name)) %>%
        dplyr::select(reg_data, pCorn_ls, pN_ls) %>%
        unnest(reg_data) %>%
        rowwise()
    
    data = sim_results$data[[1]]
    pCorn_ls = sim_results$pCorn_ls[[1]]
    pN_ls = sim_results$pN_ls[[1]]
    sim = sim_results$sim[[1]]
    N_levels = sim_results$N_levels[[1]]
    design = w_data$design_name
}



mc_sim_results <- mc_simulate(
    data = data,
    pCorn_ls = pCorn_ls,
    pN_ls = pN_ls,
    sim = 1,
    N_levels = unique(data$Ntg) %>% .[order(.)],
    record_print = design
)



# /*===========================================================
#' # Get spatial measures
# /*===========================================================
sb_results_plot = get_spatial_measures_plot(
    data = reg_data,
    field_sf = field_sf,
    plot_width = plot_width, 
    plot_length = 12,
    design_name = design,
    sim = 1,
    N_levels = unique(reg_data$Ntg) %>% .[order(.)]
)

sb_results_plot


#*******************************************************************************

### compare with previously calculated spatial measures
old_SB_df <- readRDS(here('Results/SB_df.rds'))
old_SB_df

### new results
# mean_data <- gdata %>%
#     select(design, profit, explain_df$variable) %>% 
#     #=== take average
#     .[, lapply(.SD, mean), by = .(design)] %>% 
#     print()



#*******************************************************************************



