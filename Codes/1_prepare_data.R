## ==============================================================
##                          Preparation                        =
## ==============================================================

rm(list = ls())

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
library(mgcv) # gam()
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
fs::dir_ls(here("Codes/Functions"), full.names = TRUE) %>%
  lapply(., function(x) source(x))


# /*===========================================================
#' # Create a field
# /*===========================================================
# ! Define field and plot characteristics here

field_data <-
  # ! This is where you set the experiment parameters
  CJ(
    plot_length = 12, # the length of a plot (in number of cells)
    plot_width = 3, # the width of a plot (in number of cells)
    cell_buffer = 1,
    aunit_length = 2, # the length of an analysis unit (in number of cells)
    aunit_width = 3, # the width of an analysis unit (in number of cells)
    cell = 6, # the length of a cell in meter
    #* how wide the field is
    field_col = c(72, 144), # the number of cell columns
    #* how tall the field is
    field_row = 72, # the number of row columns
    sp_range = 600,
    gstat_model = "Sph"
  ) %>%
  rowwise() %>%
  mutate(
    field_sf = list(
      make_field(
        field_col = field_col,
        field_row = field_row,
        aunit_length = aunit_length,
        aunit_width = aunit_width,
        cell = cell,
        cell_buffer = cell_buffer
      )
    ),
    #* price ratio scenarios
    pCorn = 7 / 25.4, # $/kg
    pRatio_ls = list(
        readRDS(here("Data/price_ratio_data.rds")) %>% 
            pull(pRatio) %>% 
            quantile(c(0.05, 0.5, 0.95), na.rm=TRUE) %>% 
            round(3)
    )
  )


#* save the fields
saveRDS(field_data, here("Data/field_data.rds"))

# /*===========================================================
#' # Add trial design layout by design
# /*===========================================================

field_with_design <-
  field_data %>%
  rowwise() %>%
  mutate(design_layout = list(
    make_design_layout(plot_length, field_col)
  )) %>%
  dplyr::select(-plot_length) %>%
  unnest(cols = "design_layout") %>%
  rowwise() %>%
  mutate(plot_block_id_data = list(
    gen_plot_block_ids(
      field_sf = field_sf,
      plot_length = plot_length,
      plot_width = plot_width,
      cols_plot_in_block = cols_plot_in_block,
      rows_plot_in_block = rows_plot_in_block,
      cell_buffer = cell_buffer
    )
  )) %>%
  mutate(field_sf = list(
    left_join(field_sf, plot_block_id_data, by = "cell_id")
  )) %>%
  dplyr::select(-plot_block_id_data) %>%
  mutate(
    data_file_name =
      paste0(
        "Data/",
        stringr::str_replace_all(design_name, " ", ""),
        "_", field_col, ".rds"
      )
  )

#* save the fields with designs
saveRDS(field_with_design, here("Data/field_with_design.rds"))

# /*===========================================================
#' # Generate true parameters
# /*===========================================================

field_parameters <-
  readRDS(here("Data/field_data.rds")) %>%
  mutate(field_pars = list(
    gen_field_pars(
      sp_range = sp_range,
      gstat_model = gstat_model,
      field_sf = field_sf,
      nsim = 1000
    )
  ))

#* save the field parameters
saveRDS(field_parameters, here("Data/field_parameters.rds"))


# /*===========================================================
#' # Create regression data
# /*===========================================================
# ! updated in field_with_design$data_file_name

lapply(
  1:nrow(field_with_design),
  function(x) gen_reg_data(x, field_with_design, field_parameters)
)
