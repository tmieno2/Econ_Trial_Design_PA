
  # === if strip design, reset plot length ===#
  if (str_detect(temp_design, "Strip")) {
    f <-
      data.table(f) %>%
      # === plot id ===#
      .[, plot_row_id := ceiling(row_id / plot_width)] %>%
      .[, plot_col_id := ceiling(col_id / plot_length)] %>%
      .[, plot_id := plot_col_id + (plot_row_id - 1) * max(plot_col_id)] %>%
      st_as_sf()
  }


  # === plot-level N polygons ===#
  Af <- aggregate(f, by = list(f$plot_id), FUN = mean)
  Af <- cbind(Af, Af %>% st_centroid() %>% st_coordinates()) %>%
    as_Spatial()

  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #'  Spatial Balance Measures
  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #' Compute the spatial property measures for all designs
  #' - fixed designs: only need one-time computation
  #' - random designs: need to compute for each simulation

  if (str_detect(temp_design, "Random")) {

    # === Calculate all spatial balance measures ===#
    S <- fn_spatial_measure(N.shp = Af)

    # === output return ===#
    MSTmin <- S$MSTmin
    GR_row <- S$GR_row
    GR_col <- S$GR_col
    NB_gini <- S$NB_gini
    moran_I <- S$moran_I
    van_Es_var <- S$van_Es_var
  } else {
    MSTmin <- SB_df[design == temp_design, MSTmin]
    GR_row <- SB_df[design == temp_design, GR_row]
    GR_col <- SB_df[design == temp_design, GR_col]
    NB_gini <- SB_df[design == temp_design, NB_gini]
    moran_I <- SB_df[design == temp_design, moran_I]
    van_Es_var <- SB_df[design == temp_design, van_Es_var]
  }


  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #'  Accidental correlation (error ~ N)
  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

  # === moving window calculation ===#
  corr_e_N <- get_local_e_N_correlation(
    polyshp = f,
    window_cols = 36,
    window_rows = 36
  )

  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #'  Local N Variation
  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

  # === moving window calculation ===#
  variation_N <- get_local_N_variation(
    polyshp = Af,
    window_cols = 36,
    window_rows = 36
  )

  # ==> save those spatial measure results in field level data table




