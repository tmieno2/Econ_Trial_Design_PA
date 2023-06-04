
#' The spatial balance measurements for all the designs
#' - for random designs, the measurement is a random realization


#/*=================================================*/
#' #                    Preparation
#/*=================================================*/
rm(list=ls())
#=== Packages ===#
library(sf)
library(sp)
library(raster)
library(gstat)
library(GWmodel)
library(scam)
library(mgcv)
library(data.table)
library(magrittr)
library(ggplot2)
library(parallel)
library(spdep)
library(dplyr)
library(tidyverse)
options(stringsAsFactors = FALSE)

#=== Set working directory ===#
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")



###################################
###								              ###
### 		Create Field		        ###
###								              ###
###################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load configurations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source('./Codes/config_setting.R')

#~~~~~~~~~~~~~~~~~~~~~~
# Create basic field sf
#~~~~~~~~~~~~~~~~~~~~~~
# matrix -> raster -> sp -> sf
Mf <- matrix(1:field.cell, nrow=field.row, ncol=field.col, byrow=TRUE)
R <- raster(Mf)
extent(R) <- extent(0, field.lgth, 0, field.wdth)
names(R) <- "cid"
field_sf <- as(R,'SpatialPolygonsDataFrame') %>%
	st_as_sf()

#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create plot and block ids
#~~~~~~~~~~~~~~~~~~~~~~~~~~
#=== use the pre-defined id-generating function
source('./Codes/fn_gen_field_ids.R')
all_fields <- lapply(design_name_ls,function(x) gen_field_ids(field_sf,x))
names(all_fields) <- design_name_ls



###############################################
###							                            ###
### 		Spatial Balance Measurements		    ###
###							                            ###
###############################################

# #=== ED for each design ===#
# ED <- data.table(design=design_name_ls)
# GR <- data.table(design=design_name_ls)
# NB <- data.table(design=design_name_ls)
# NB_list <- vector("list", length(design_name_ls)); names(NB_list) <- design_name_ls
# MI_global <- data.table(design=design_name_ls)
# MI_local <- vector("list", length(design_name_ls)); names(MI_local) <- design_name_ls

SB_df <- data.frame(design=design_name_ls,
                    MSTmin=NA,
                    GR_row=NA,
                    GR_col=NA,
                    NB_gini=NA,
                    moran_I=NA,
                    van_Es_var=NA)

for(ds in 1:length(design_name_ls)){
  
	temp_design <- design_name_ls[ds]
	
	#~~~~~~~~~~~~~~~~~~~~
	# field layout
	#~~~~~~~~~~~~~~~~~~~~
	field <- all_fields[[temp_design]]
	data <- cbind(field,field %>% st_centroid %>% st_coordinates) %>%
	  data.table()
	
	
	#/*~~~~~~~~~~~~~~~~~~~~~~*/
	#'  Assign N
	#/*~~~~~~~~~~~~~~~~~~~~~~*/
	#=== define experimental N treatment levels ===#
	N_levels <- 1:6
	
	#=== count number of treatment blocks ===#
	block_num <- data[,block_id] %>% unique() %>% length()

	#=== N design ===#
	source('./Codes/fn_N_design.R')
	N_design <- fn_N_design(N_levels, block_num, temp_design)

	#=== merge N treatments to field data ===#
	data %<>% N_design[.,on=c('block_id','plot_in_block_id')] 

	#=== cell-level N mapping (sf) ===#
	f <- data[, list(cid, N)] %>%
	  left_join(field, ., by='cid') %>%
	  .[, c('cid', 'plot_id', 'plot_row_id', 'plot_col_id', 'N')]

	#=== plot-level N polygons ===#
	Af <- aggregate(f, by=list(f$plot_id), FUN=mean) %>%
	  as_Spatial()
	# plot(Af, axes=T)
	# text(Af, labels=Af$N)
	# plot(st_as_sf(Af['N']))
	
	#=== Calculate all spatial balance measures ===#
	source('./Codes/fn_spatial_balance.R')
	S <- fn_spatial_balance(Af)

	#=== output return ===#
	SB_df[ds, 'MSTmin'] <- S$MSTmin
	SB_df[ds, 'GR_row'] <- S$GR_row
	SB_df[ds, 'GR_col'] <- S$GR_col
	SB_df[ds, 'NB_gini'] <- S$NB_gini
	SB_df[ds, 'moran_I'] <- S$moran_I
	SB_df[ds, 'van_Es_var'] <- S$van_Es_var

}

saveRDS(SB_df,
		paste0('./Results/SB_df.rds'))


############################################################






