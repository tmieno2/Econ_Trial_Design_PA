gen_reg_data_single <- function(i, field_with_design, field_parameters) {


  # === load cell level data (coef, error) ===#
  cell_data <- cell_data
  field <- field_sf %>% data.table()

  # /*+++++++++++++++++++++++++++++++++++
  #' # Define the levels of experimental input rate by simulation id
  # /*+++++++++++++++++++++++++++++++++++
  N_levels_data <-
    cell_data[, .(sim, Nk)] %>%
    .[Nk > 300, Nk := 300] %>%
    .[, .(N_levels = list(
      c(
        min(Nk) - 20,
        quantile(Nk, prob = 0.2),
        quantile(Nk, prob = 0.4),
        quantile(Nk, prob = 0.6),
        quantile(Nk, prob = 0.8),
        max(Nk) + 20
      ) %>%
      pmax(0, .) %>%
      round()
    )), by = sim]

  # /*+++++++++++++++++++++++++++++++++++
  #' # Assign input rate
  # /*+++++++++++++++++++++++++++++++++++
  #* the number of blocks
  block_num <- unique(field$block_id) %>% length()

  #* assign N rates
  N_levels <- N_levels_data[1, N_levels][[1]]
  n_assign_data <- assign_input_rate(
    N_levels = N_levels,
    block_num = block_num,
    design = design
  )
    
  

  reg_data <-
    field[cell_data, on = "cell_id"] %>%
    n_assign_data[., on = c("block_id", "plot_in_block_id")] %>%
    #* used for calculating spatial measures
    .[, Nid := as.numeric((as.factor(N))), by = sim] %>%
    #* target N rate for mapping (before application error is added)
    .[, Ntg := N] %>%
    #===add cell-level N application noise===#
    .[, N := N * (1 + N_error * 0.1)] %>%
    .[N < 0, N := 0] %>%
    .[, N2 := N^2] %>%
    #* deterministic yield
    .[, det_yield := gen_yield_QP(b0, b1, b2, Nk, N)] %>%
    #* create yield errors 
    .[, mean_det_yield := mean(det_yield), by = sim] %>%
    .[, yield_error := mean_det_yield * m_error] %>%
    .[, yield := det_yield + yield_error]  %>%
    #* remove observations in the buffer zone 
    .[buffer == 0, ] %>%
    #* aggregate the data by analysis unit
    .[,
      lapply(.SD, mean),
      by = .(sim, aunit_id),
      .SDcols = c("yield", "yield_error", "Nid", "Ntg", "N", "N2", 
                  "b0", "b1", "b2", "Nk", "X", "Y")
    ] 

  return(reg_data)

}
