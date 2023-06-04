
# ---------------------------------------------------
#  function to compute spatial measure for Latin i 
# ---------------------------------------------------

fn_spatial_measure_latin <- function(i, field){
    
    #' assign experimental input rate (Nid)
    N_design <- data.table(
        block_id = rep(1:block_num, each = 36),
        plot_in_block_id = rep(1:36, block_num),
        N = N_levels[rep(c(t(Latin_ls[[i]])), times = block_num)]
    ) %>% 
        .[, Nid := as.numeric((as.factor(N)))]
    
    # cell level shapefile
    f <- field[N_design, on = c("block_id", "plot_in_block_id")] %>% 
        #===remove unnecessary columns to speed up aggregation later===#
        .[, .(X, Y, plot_id, plot_row_id, plot_col_id, N, Nid, geometry)] %>% 
        #=== back to sf ===#
        st_as_sf()
    
    #=== aggregate to plot-level polygons ===#
    Af <- aggregate(f, by=list(f$plot_id), FUN=mean, na.rm=TRUE) %>% 
        as("Spatial")
    
    
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    #'  Spatial property measures
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    SB_df <- fn_spatial_measure(N_shp = Af)
    
    
    
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    #'  Local N Variation
    # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    
    # === moving window calculation ===#
    variation_N <- get_local_N_variation(
        polyshp = Af,
        window_cols = plot_length*2,              # number of cells
        window_rows = plot_width*6
    )
    
    SB_df["variation_N"] <- variation_N
    
    SB_df$id <- i
    print(paste0("design ", i))
    
    # return results
    return(SB_df)
}