



## =======================
## Accidental correlation (error ~ N)
## =======================

# polyshp: SpatialPolygonsDataFrame

get_local_e_N_correlation <- function(polyshp, window_cols, window_rows) {

  # === cell size is not set in the global env ===#
  cell <-  6
  
  # === moving window size:
  w_c <- window_cols * cell # in meters
  w_r <- window_rows * cell # in meters

  # === calculate local yield_error~N correlation:
  corr_e_N <- polyshp %>%
      data.frame() %>%
      data.table() %>% 
      # === divide field into non-overlapping windows:
      .[, w_row_id := ceiling(Y / w_r)] %>%
      .[, w_col_id := ceiling(X / w_c)] %>%
      # === yield_error~N correlation by window
      .[, .(corr_e_N = cor(yield_error, N, use="complete.obs")), 
        by = .(w_row_id, w_col_id)] %>% 
      # === average local correlation
      .[, .(corr_e_N = mean(abs(corr_e_N)))] %>% 
      print()
  
  # === return value:
  return(corr_e_N)

  

  # # old-style for loop
  # corr_e_N <- c()
  # for (xx in seq( min(polyshp$X), max(polyshp$X), by = w_c)) {
  #   for (yy in seq( min(polyshp$Y), max(polyshp$Y), by = w_r)) {
  #     f_window <- polyshp %>%
  #       data.frame() %>%
  #       data.table() %>%
  #       .[X >= xx & X < xx + w_c, ] %>%
  #       .[Y >= yy & Y < yy + w_r, ]
  #     corr_e_N <- c(corr_e_N, cor(f_window$yield_error, f_window$N, use="complete.obs"))
  #     print(c(xx,yy,nrow(f_window)))
  #   }
  # }
  # # average local e~N correlation
  # return(mean(abs(corr_e_N)))
}

