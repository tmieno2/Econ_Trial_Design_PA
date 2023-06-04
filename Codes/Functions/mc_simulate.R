# /*===========================================================
#' # MC simulations
# /*===========================================================
# ! Run GWR and BRF and do economic analysis
# ! buffer zone data are dropped from analysis

mc_simulate <- function(data, pCorn, pRatio_ls, sim, N_levels, record_print) {
  print(paste0(record_print, ", sim = ", sim))

  #* price scenario data
  price_ls <- CJ(pCorn = pCorn, 
                 pRatio = pRatio_ls) %>% 
      .[, pN := pCorn * pRatio_ls ] 


  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### SCAM
  # /*~~~~~~~~~~~~~~~~~~~~~~*/

  # === scam regression ===#
  scam_res <- gam(yield ~ s(N, k = 5) + s(X, k = 6) + s(Y, k = 6) + ti(X, Y, k = 6),
    data = data
  )

  data_scam <- data.table(
    N = seq(min(N_levels), max(N_levels), by = 1)
  ) %>%
    .[, X := data[1, ] %>% pull(X)] %>%
    .[, Y := data[1, ] %>% pull(Y)] %>%
    .[, yhat := predict(scam_res, newdata = .)] %>%
    # ===profits under different prices===#
    cbind(
      .[rep(1:nrow(.), times = nrow(price_ls)), ],
      price_ls[rep(1:nrow(price_ls), each = nrow(.)), ]
    ) %>%
    .[, pi_hat := pCorn * yhat - pN * N] %>%
    .[, .SD[which.max(pi_hat)], by = .(pCorn, pN)] %>%
    .[, opt_N_scam := N] %>%
    .[, c("pCorn", "pN", "opt_N_scam")]


  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### GWR estimate
  # /*~~~~~~~~~~~~~~~~~~~~~~*/

  # === regression data in sp ===#
  reg_data_sp <-
    data %>%
    st_as_sf(coords = c("X", "Y")) %>%
    as("Spatial")

  # === GWR estimation ===#
  tic()
  gwr_beta <-
    estimate_GWR(
      reg_data_sp = reg_data_sp,
      N_levels = N_levels,
      price_ls = price_ls
    )
  toc()

  # ! Note: bandwidth is selected as statistically optimal
  # ! note: buffer zone data are dropped from analysis



  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### MA_CF model (with true parameters)
  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  tic()
  # === load reg data ===#
  Y <- data[, yield]
  W_f <- as.factor(data[, Ntg])
  X <- data[, c("b0", "b1", "Nk"), with = FALSE]

  macf_tau <-
    grf::multi_arm_causal_forest(
      X, Y, W_f
    )

  data_macf <-
    predict(
      macf_tau,
      newdata = X
    )[[1]][, , 1] %>%
    data.table() %>%
    .[, aunit_id := data[, aunit_id]] %>%
    melt(id.var = "aunit_id") %>%
    .[, c("N_high", "N_low") := tstrsplit(variable, " - ", fixed = TRUE)] %>%
    .[, N_dif := as.numeric(N_high) - as.numeric(N_low)] %>%
    # ===duplicate data for different price scenarios===#
    cbind(
      .[rep(1:nrow(.), times = nrow(price_ls)), ],
      price_ls[rep(1:nrow(price_ls), each = nrow(.)), ]
    ) %>%
    # ===eonr===#
    .[, pi_hat := pCorn * value - pN * N_dif] %>%
    .[, .SD[which.max(pi_hat)], by = .(pCorn, pN, aunit_id)] %>%
    #* if the max profit is negative
    .[pi_hat < 0, N_high := N_levels[1]] %>%
    .[, opt_N_macf := as.numeric(N_high)] %>%
    .[, c("pCorn", "pN", "aunit_id", "opt_N_macf")]
  toc()


  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Boosted Random Forest (with true parameters)
  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

  tic()
  # === use perfect info ===#
  X <- data[, c("N", "b0", "b1", "Nk"), with = FALSE] %>% data.frame()
  Y <- data[, yield]
  BRF_temp <- boosted_regression_forest(
    X = X,
    Y = Y,
    # min.node.size = 10,
    num.trees = 1000
  )

  N_seq <- data.table(N = seq(min(N_levels), max(N_levels), by = 1))
  data_brf <-
    copy(data) %>%
    .[, c("aunit_id", "b0", "b1", "Nk", "yield"), with = FALSE] %>%
    expand_grid_df(., N_seq) %>%
    .[, yield_hat := predict(BRF_temp,
      newdata = .[, c("N", "b0", "b1", "Nk"), with = FALSE]
    )] %>%
    expand_grid_df(., price_ls) %>%
    .[, pi_hat := pCorn * yield_hat - pN * N] %>%
    .[, .SD[which.max(pi_hat)], by = .(pCorn, pN, aunit_id)] %>%
    .[, opt_N_brf := N] %>%
    .[, c("pCorn", "pN", "aunit_id", "opt_N_brf")]
  toc()

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Results data
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  est_beta <- data_scam %>%
    .[gwr_beta, on = c("pCorn", "pN")] %>%
    .[data_macf, on = c("pCorn", "pN", "aunit_id")] %>%
    .[data_brf, on = c("pCorn", "pN", "aunit_id")]

  return(est_beta)
}