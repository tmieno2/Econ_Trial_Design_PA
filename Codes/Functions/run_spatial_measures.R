

run_spatial_measures <- function(i, field_with_design) {

  #! things to do  
  #* prepare scenario i data for spatial measure calculation

  w_data <- field_with_design[i, ]
  
  #! if random design, need to run the measure calculation for each simulation
  if(str_detect(w_data$design_name, "Random")){
      spatial_results <-
          #* read the right regression data for this experimental setting from the data_file_name
          readRDS(here(w_data$data_file_name)) %>%
          dplyr::select(reg_data, design_name, 
                        aunit_width, aunit_length, plot_width, plot_length) %>%
          unnest(reg_data) %>%
          rowwise() %>%
          mutate(field_sf = w_data$field_sf) %>% 
          #! spatial measures calculation
          mutate(
              sb_results_plot = 
                  list(
                      get_spatial_measures_plot(
                          data = data,
                          design_name = design_name,
                          field_sf = field_sf,
                          plot_width = plot_width, 
                          plot_length = 12,
                          sim = sim,
                          N_levels = N_levels
                      ))
          ) %>%
          dplyr::select(sim, sb_results_plot) 
  } else {
      #! if fixed pattern design, only need to run the measure calculation one time
      spatial_results_one <-
          #* read the right regression data for this experimental setting from the data_file_name
          readRDS(here(w_data$data_file_name)) %>%
          dplyr::select(reg_data, design_name, 
                        aunit_width, aunit_length, plot_width, plot_length) %>%
          unnest(reg_data) %>%
          #===only work on one simulation===#
          .[1,] %>% 
          rowwise() %>%
          mutate(field_sf = w_data$field_sf) %>% 
          #! spatial measures calculation
          mutate(
              sb_results_plot = 
                  list(
                      get_spatial_measures_plot(
                          data = data,
                          design_name = design_name,
                          field_sf = field_sf,
                          plot_width = plot_width, 
                          plot_length = 12,
                          sim = sim,
                          N_levels = N_levels
                      ))
          ) %>%
          dplyr::select(sim, sb_results_plot)
      #===replicate for all simulations===#
      n <- readRDS(here(w_data$data_file_name)) %>% pull(reg_data) %>% .[[1]] %>% nrow()
      spatial_results <- spatial_results_one[rep(1, times = n), ]
      spatial_results$sim <- 1:n
          
  }
  
  results_return <- 
    w_data %>%
    mutate(spatial_results = list(spatial_results))

  return(results_return)

}

