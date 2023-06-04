# /*===========================================================
#' # Spatial Properties Measures
# /*===========================================================
#' Create spatial measures:
#'  - MST, GR, NB, Moran, SB
#'  - local correlation, N variation




###================================
###
###         aunit level         ===
###
###================================

#=== calculate spatial measures at aunit level (i.e., use aunit as plot)

get_spatial_measures_aunit <- function(sim, data, design_name, field_sf, 
                                 aunit_length, aunit_width, N_levels){
    
    print(paste(design_name, " sim = ", sim, sep=","))
    
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    #'  regression data in sp
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    reg_data_sp <- 
        data %>%
        rowwise() %>% 
        #=== use aunit as plot
        mutate(plot_row_id = Y/(6*aunit_width) +0.5,
               plot_col_id = X/(6*aunit_length) + 1,
               plot_id = aunit_id
        ) %>% 
        #=== preserve coordinates
        mutate(lon = X,
               lat = Y) %>%
        st_as_sf(coords = c("lon", "lat")) %>%
        as("Spatial")
    
    
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    #'  Spatial propertiy measures
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    
    SB_df <- fn_spatial_measure(N_shp = reg_data_sp)
    
    
    
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    #'  Accidental correlation (error ~ N)
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    
    # === moving window calculation ===#
    corr_e_N <- get_local_e_N_correlation(
        polyshp = reg_data_sp,
        window_cols = 36,              # number of cells
        window_rows = 36
    )
    
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    #'  Local N Variation
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    
    # === moving window calculation ===#
    variation_N <- get_local_N_variation(
        polyshp = reg_data_sp,
        window_cols = 36,              # number of cells
        window_rows = 36
    )
    
    # ==> save those spatial measure results in field level data table
    
    
    # ---------------------------
    #'  Return results
    # ---------------------------
    SB_df["corr_e_N"] <- corr_e_N
    SB_df["variation_N"] <- variation_N
    
    return(SB_df)
}
  



###===============================
###
###         plot level         ===
###
###===============================

#=== calculate spatial measures at plot level

get_spatial_measures_plot <- function(sim, data, design_name, N_levels,
                                      field_sf, plot_width, plot_length){
    
    print(paste(design_name, " sim = ", sim, sep=","))
    
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    #'  regression data in sp
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    f <- left_join(field_sf, 
                   data[,.(aunit_id, yield, yield_error, Nid, N)], 
                   by = "aunit_id") %>% 
        data.table() %>% 
        #=== reset plot length for strip designs
        .[, plot_row_id := ceiling(row_id / plot_width)] %>%
        .[, plot_col_id := ceiling(col_id / plot_length)] %>%
        .[, plot_id := plot_col_id + (plot_row_id - 1) * max(plot_col_id)] %>% 
        #=== back to sf ===#
        st_as_sf()
    #=== aggregate to plot-level polygons ===#
    N_shp <- aggregate(f, by=list(f$plot_id), FUN=mean, na.rm=TRUE) %>% 
        as("Spatial")
    
    
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    #'  Spatial property measures
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    SB_df <- fn_spatial_measure(N_shp = N_shp)
    
    
    
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    #'  Accidental correlation (error ~ N)
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    
    # === moving window calculation ===#
    corr_e_N <- get_local_e_N_correlation(
        polyshp = N_shp,
        window_cols = plot_length*2,              # number of cells
        window_rows = plot_width*6
    )
    
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    #'  Local N Variation
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    
    # === moving window calculation ===#
    variation_N <- get_local_N_variation(
        polyshp = N_shp,
        window_cols = plot_length*2,              # number of cells
        window_rows = plot_width*6
    )
    
    #>>> save those spatial measure results in field level data table
    
    
    # ---------------------------
    #'  Return results
    # ---------------------------
    SB_df["corr_e_N"] <- corr_e_N
    SB_df["variation_N"] <- variation_N
    
    return(SB_df)
}









