

#'   Global settings of the simulations


###########################################################
###
###                    Field Settings                   ###
###
###########################################################

#' cell: basic unit
#' aunit: unit of data analysis
#' plot: unit for N application
#' block: group of N plots


# ~~~~~~~~~~~~~~
# size of cell
# ~~~~~~~~~~~~~~
cell <- 6 # in meters

# ~~~~~~~~~~~~~~
# size of aunit
# ~~~~~~~~~~~~~~
cols_cell_in_aunit <- 2 # cols of cells in aunit
rows_cell_in_aunit <- 3 # rows of cells in aunit


# ~~~~~~~~~~~~~~
# size of plot
# ~~~~~~~~~~~~~~

# === plot length list ===#
plot_len_ls <- c(12) # in cells

# === plot width ===#
rows_cell_in_plot <- 3

# === buffer zone size ===#
cell_buffer <- 1


# ~~~~~~~~~~~~
# field size (full field)
# ~~~~~~~~~~~~
field_col <- 120 # field columns in cells
field_row <- 72 # field rows in cells

# === field dimensions in meters ===#
field_lgth_meter <- cell * field_col
field_wdth_meter <- cell * field_row





###########################################################
###
###                     Trial Designs                   ###
###
###########################################################

# === plot length (in cells) ===#
cols_cell_in_plot <- plot_len_ls[1]

# === table of design layout settings ===#
source("./Codes/Modules/design_layout_table.R")

# === list of designs
design_name_ls <- design_layout_table$design_name

# === Latin Square Matrices ===#
Latin_ls <- readRDS(file = paste0("./Data/Latin_ls.rds"))





###########################################################
###
###                     Other Settings                  ###
###
###########################################################

# ~~~~~~~~~~~~~~~~~~~~
# Spatial field
# ~~~~~~~~~~~~~~~~~~~~

# === variagram range ===#
range_ls <- c(200, 400, 600) # in meters

# === error psill ===#
psill_ls <- c(0.002, 0.015, 0.028)
#' roughly,
#' 0.002 means 500 sd,
#' 0.015 means 1300 sd,
#' 0.028 means 2000 sd


# ~~~~~~~~~~~~~~~~~~~~
# Economic parameters
# ~~~~~~~~~~~~~~~~~~~~

# === prices: corn ($/kg), N ($/kg) ===#
price_table <- data.table(
    pCorn = round(c(3.5, 3.5, 5) * 200 / 5080, digits = 3),
    pN = round(c(0.4, 0.6, 0.4) / 0.453592, digits = 3)
)

# === fixed cost ===#
fixed_cost <- 550 * 2.471



# ~~~~~~~~~~~~~~~~~~~~~~~
# Number of simulations
# ~~~~~~~~~~~~~~~~~~~~~~~
Num_Sim <- 10