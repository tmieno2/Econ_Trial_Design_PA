



### =======================
### Local N Variation:
### =======================

# polyshp: SpatialPolygonsDataFrame

get_local_N_variation <- function(polyshp, window_cols, window_rows) {

  # === cell size is not set in the global env ===#
  cell <-  6
  
  # === moving window size:
  w_c <- window_cols * cell # in meters
  w_r <- window_rows * cell # in meters

  # === calculate local N variation:
  variation_N <- polyshp %>%
      data.frame() %>%
      data.table() %>% 
      # === divide field into non-overlapping windows:
      .[, w_row_id := ceiling(Y / w_r)] %>%
      .[, w_col_id := ceiling(X / w_c)] %>%
      # === N standard deviation by window
      .[, .(variation_N = sd(N, na.rm = TRUE)), 
        by = .(w_row_id, w_col_id)] %>% 
      # === average local correlation
      .[, .(variation_N = mean(abs(variation_N)))]
  
  # === return value:
  return(variation_N)
  
  
  # # old-style for loop
  # variation_N <- c()
  # for (xx in seq(min(polyshp$X), max(polyshp$X), by = w_c)) {
  #   for (yy in seq(min(polyshp$Y), max(polyshp$Y), by = w_r)) {
  #     f_window <- polyshp %>%
  #       data.frame() %>%
  #       data.table() %>%
  #       .[X >= xx & X < xx + w_c, ] %>%
  #       .[Y >= yy & Y < yy + w_r, ]
  #     variation_N <- c(
  #       variation_N,
  #       sd(f_window$N)
  #     )
  #     print(c(xx,yy,nrow(f_window)))
  #   }
  # }
  # # average local N variation
  # return(mean(abs(variation_N)))
}