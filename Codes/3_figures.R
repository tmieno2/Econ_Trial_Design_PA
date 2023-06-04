

#' Graphs of trial design simulation results
#'


rm(list = ls())

# === Packages ===#
library(sf)
library(raster)
library(ggplot2)
library(data.table)
library(magrittr)
library(viridis)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidytext)
library(ggh4x)
library(here)
theme_set(theme_bw())

#=== Set working directory ===#
setwd(here())



#' /*=========================================================================*/
#' /*                         Load and Prepare Data                           */
#' /*=========================================================================*/

# -----------------
# load results data
# -----------------
pi_data <- readRDS(file = here("Results", "pi_data.rds"))
field_with_design <- readRDS(here("Data", "field_with_design.rds"))


#=== abbreviation of design name ===#
abrv_df <- data.table(design_name = pi_data$design_name) %>% 
    unique() %>% 
    .[design_name=="Strip Completely Random", design_abrv := "StrCompRd"] %>% 
    .[design_name=="Strip Randomized Block", design_abrv := "StrBlockRd"] %>% 
    .[design_name=="Strip Fixed Block Best", design_abrv := "StrPatHi"] %>% 
    .[design_name=="Strip Fixed Block Worst", design_abrv := "StrPatLo"] %>% 
    .[design_name=="Completely Random", design_abrv := "GridCompRd"] %>% 
    .[design_name=="Randomized Block", design_abrv := "GridBlockRd"] %>% 
    .[design_name=="Fixed Block Best", design_abrv := "GridPatHi"] %>%
    .[design_name=="Fixed Block Worst", design_abrv := "GridPatLo"] %>%
    .[design_name=="Latin Square Best", design_abrv := "LatSqHi"] %>% 
    .[design_name=="Latin Square Worst", design_abrv := "LatSqLo"] %>% 
    .[design_name=="Latin Square LimJump", design_abrv := "LatSqRtJump"] %>% 
    .[design_name=="Cascade Plot", design_abrv := "Cascade"] %>% 
    .[design_name=="Wave", design_abrv := "Wave"] 
    


# ----------------
# data preparation
# ----------------
pi_data <- pi_data %>%
    #=== remove scam results ===#
    .[, pi_scam := NULL] %>% 
    #=== remove GWR 1 results ===#
    .[, pi_gwr_1 := NULL] %>% 
    #=== Wide to Long: melt() ===#
    melt(id.vars = c("field_col", "design_name", "pCorn", "pN", "sim")) %>%
    data.table() %>%
    #=== label model names ===#
    .[variable=="pi_gwr_2", model := "GWR"] %>%
    .[variable=="pi_macf", model := "CF"] %>%
    .[variable=="pi_brf", model := "BRF"] %>%
    .[, model := factor(model,
                        levels = c("GWR", "BRF", "CF"))] %>% 
    #=== rename ===#
    setnames("value", "profit") %>% 
    .[, .(field_col, design_name, pCorn, pN, sim, model, profit)] %>% 
    #=== abbreviated name of designs ===#
    .[abrv_df, on = "design_name"] %>% 
    #=== order of designs ===#
    .[, design_name := factor(design_name, levels = abrv_df$design_name)] %>% 
    .[, design_abrv := factor(design_abrv, levels = abrv_df$design_abrv)] %>% 
    #=== subgroups of designs ===#
    .[design_name %in% c("Strip Completely Random", 
                         "Strip Randomized Block",
                         "Strip Fixed Block Worst",
                         "Strip Fixed Block Best"), subgroup := "strip"] %>% 
    .[design_name %in% c("Completely Random", 
                         "Randomized Block",
                         "Fixed Block Best", 
                         "Fixed Block Worst"), subgroup := "block"] %>% 
    .[design_name %in% c("Latin Square Worst",
                         "Latin Square Best",
                         "Latin Square LimJump"), subgroup := "latin"] %>% 
    .[design_name %in% c( "Cascade Plot",
                          "Wave"), subgroup := "smooth"] %>% 
    #=== retrieve price ratio ===#
    .[, pRatio := (pN / pCorn) %>% round(2) ] %>% 
    .[, pLabel := factor(pRatio, 
                         levels = str_sort(unique(pRatio), numeric = TRUE))] %>% 
    #=== use design_abrv in figures ===#
    .[, design := design_abrv] %>% 
    print()

  
# --------------------
# field size scenario
# --------------------

f_size <- 144


#' /*=========================================================================*/
#' /*                               Boxplot                                   */
#' /*=========================================================================*/

# ----------------
# boxplot by model
# ----------------
# loop over price scenarios
for(p in unique(pi_data$pRatio)){
    gdata <- pi_data[field_col==f_size,] %>%
        .[pRatio==p, ] %>% 
        #=== convert to profit loss
        .[, profit := (-1)*profit]

    source(here("GitControlled/Codes/Modules",
                "figure_boxplot_grouped_v2_loss.R"))
    ggsave(file = here("Graph/boxplot",
                       paste0("profits_boxplot_", f_size, "_", p, ".png")),
           height = 6, width = 10)
}

# -----------------------------------------------
#  pairwise profit differences to aid discussion
# -----------------------------------------------
gdata <- pi_data %>%
    .[field_col==f_size,] %>%
    .[, .(profit = mean(profit)),
      by=c("field_col", "design_abrv", "model", "pRatio")] %>%
    print()
design_list <- abrv_df$design_abrv
for(p in unique(gdata$pRatio)){
    for(md in unique(gdata$model)){
        pair_df <- data.frame()
        for(i in 1:length(design_list)){
            for(j in 1:length(design_list)){
                pi <- gdata[design_abrv==design_list[i]&model==md&pRatio==p, profit]
                pj <- gdata[design_abrv==design_list[j]&model==md&pRatio==p, profit]
                pair_df[design_list[i], design_list[j]] <-
                    as.character(round(pi-pj, 2))
            }
        }
        pair_df[is.na(pair_df)] <- ""
        write.csv(pair_df, here(paste0('Graph/tables/profit_diff_', p, '_', md,'.csv')))
    }
}

#' /*=========================================================================*/
#' /*                       Overall Ranking of Designs                        */
#' /*=========================================================================*/


#------ rank design by all-price-and-model mean profit, at full field ------#
design_level <- pi_data %>%
    .[field_col==f_size,] %>% 
    .[, .(profit = mean(profit)), 
      by = c("design")] %>%
    .[order(-profit), ] %>%
    .$design


#--- mean and sd of profit ---#
mean_data <- pi_data %>%
    .[, .(profit = mean(profit),
          profit_sd = sd(profit)
    ),
    by=c("field_col", "pLabel", "design", "model")] %>%
    #=== re-calibrate profit by LJ = 0 ===#
    .[, profit := profit - profit[design=="LatSqRtJump"],
      by=c("field_col", "pLabel", "model")] %>% 
    print()


#--- graphing data ---#
gdata <- mean_data[field_col==f_size,] %>% 
    .[, design := factor(design, 
                         levels = design_level)]

# -------------------------------
#  mean profit
# -------------------------------
source(here("GitControlled/Codes/Modules",
            "figure_mean_line_all_prices.R"))
ggsave(file = here("Graph/mean_line", 
                   paste0("profits_line_", f_size, "_by_model.png")),
       height=4, width=8)


# -------------------------------
# standard deviation of profit
# -------------------------------
source(here("GitControlled/Codes/Modules",
            "figure_sd_line_all_prices.R"))
ggsave(file = here("Graph/mean_line", 
                   paste0("sd_line_", f_size, "_by_model.png")),
       height=4, width=8)

#=== sd table ===#
sd_data <- mean_data %>%
    .[, profit_sd := round(profit_sd, 2)] %>%
    .[field_col==f_size,] %>%
    #--- Long to Wide: dcast()
    dcast(pLabel+design~model, value.var="profit_sd") %>%
    data.table() %>%
    #--- order
    .[order(`pLabel`, `GWR`),] %>%
    print()
write.csv(sd_data, here('Graph/mean_line/sd_table.csv'))


# -------------------------
# extreme values of profit
# -------------------------
# set <-40 $/ha as extremely low profit
# extreme_percent <- pi_data[, .(count = sum(profit<(-40)),
#                                nsim = length(sim)),
#                            by=.(field_col, design, model)] %>%
#     .[, extreme_percent := round(count/nsim*100, 2)] %>%
#     #--- Long to Wide: dcast()
#     dcast(field_col+design~model, value.var="extreme_percent") %>%
#     data.table() %>%
#     .[order(field_col, GWR),] %>%
#     .[field_col==144,] %>%
#     print()
# write.csv(extreme_percent, here('Graph/mean_line/extreme_percent.csv'))



#' /*=========================================================================*/
#' /*                           Possible Explanations                         */
#' /*=========================================================================*/

# ---------------------------
# load spatial measures data
# ---------------------------
sm_df <- readRDS( here("Results/spatial_measure_results.rds") ) %>%
    dplyr::select(field_col, design_name, spatial_results) %>%
    rowwise() %>%
    mutate(spatial_results =
               list(
                   spatial_results %>%
                       dplyr::select(sim, sb_results_plot) %>%
                       rowwise() %>%
                       unnest(sb_results_plot)
               )
    ) %>%
    unnest(spatial_results) %>%
    data.table() %>%
    #===reverse spatial unbalance measure (van_Es_var)===#
    .[, van_Es_var := (-1)*van_Es_var] %>% 
    #===combine GR_col and GR_row===#
    .[, GR := GR_col+GR_row] %>%
    .[, GR_col := NULL] %>%
    .[, GR_row := NULL] %>%
    #===remove NB_gini===#
    .[, NB_gini := NULL] %>%
    print()
sm_data <- pi_data[sm_df, on = .(field_col, design_name, sim)]


# ----------------------------------
# All explaining statistic measures
# ----------------------------------
explain_df <- data.table(
    variable=c('MSTmin', 'van_Es_var',
               'moran_I', 'GR', 'variation_N', 'corr_e_N'),
    varname=c('Evenness of distribution',
              'Spatial balance',
              'Morans I',
              'Gradation',
              'Local N variation',
              'Accidental correlation') ) %>%
    .[, varname:=factor(varname, levels=varname)]

#=== Correlation between measures
var_ls <- explain_df$variable
corr_df <- cor(sm_df[, ..var_ls])
rownames(corr_df) <- explain_df$varname
colnames(corr_df) <- explain_df$varname
print(corr_df)


# --------------------------
# Explanation across designs
# --------------------------
for(m in c("GWR", "BRF", "CF")){
    gdata <- sm_data %>%
        .[field_col==144,] %>%
        .[model == m, ] %>%
        .[pRatio == 6.56, ] %>% 
        #.[!design_name %in% c("Wave", "Cascade Plot"),]
        #=== convert to profit loss
        .[, profit := (-1)*profit]
    source(here("GitControlled/Codes/Modules/figure_explain_across_designs_loss.R")) %>% print()
    ggsave(file = here("Graph/explain",
                       paste0("explain_across_designs_", m, ".png")),
           height=7.5,width=6.5,dpi=300)
}



# --------------------------
# Explanation within design (Completely Random)
# --------------------------
# gdata <- pi_data %>%
#     .[field_col==144,] %>%
#     .[model == "GWR", ] %>%
#     .[design=="Completely Random",]
# source(here("Codes/Modules/figure_explain_within_design.R")) %>% print()
# ggsave(file = here('Graph/explain/explain_within_design_gwr.png'),
#        height=6,width=8,dpi=300)




#' /*=========================================================================*/
#' /*                            Field Layout Map                             */
#' /*=========================================================================*/
#` Simulated field layout with spatial unit definitions`

# # load field sf and parameter data
# field_sf <- field_with_design %>%
#     filter(field_col==144) %>%
#     pull(field_sf) %>%
#     .[[1]]
# 
# # ------------------
# # Old-fashion plot()
# # ------------------
# png(file=here('Graph/map/field_layout.png'),
#     height=7.2, width=14.4, units="in", res=300)
# source(here("Codes/Modules/figure_field_layout_sp.R"))
# dev.off()
# 
# # ----------------
# # geom_sf() method
# # ----------------
# source(here("Codes/Modules/figure_field_layout_sf.R"))




#' /*=========================================================================*/
#' /*                        True Field Spatial Map                           */
#' /*=========================================================================*/

#=== load field sf and parameter data ===#
field_parameters <- readRDS(here("Data/field_parameters.rds"))
field_with_design <- readRDS(here("Data/field_with_design.rds"))
mc_sim_results <- readRDS(here("Results", 
                               paste0("mc_sim_results.rds")))

#=== which simulation round to graph ===#
i_sim = 7
f_size = 144


#=== cell-level sf data ===#
w_data <- field_parameters %>%
    filter(field_col == f_size) %>% 
    rowwise() %>% 
    mutate(
        field_sf = list(
            left_join(field_sf, 
                      field_pars[sim==i_sim,],
                      by = "cell_id")
        )
    )

field_cell <- w_data$field_sf[[1]]

#=== field parameter map ===#
for(par_name in c("b1", "b2", "Nk", "b0")){
    
    #--- spatial map:
    source(here("GitControlled/Codes/Modules/figure_par_map.R")) %>% print()
    ggsave(file=here("Graph/map", paste0("true_pars_",par_name,".png") ),
           height=3.1, width=7.2)
    
    #--- histogram map:
    source(here("GitControlled/Codes/Modules/figure_par_hist.R")) %>% print()
    ggsave(file=here("Graph/map", paste0("hist_true_pars_",par_name,".png") ),
           height=4, width=7.2)
}

#=== true EONR map ===#
field_cell <- data.table(field_cell) %>% 
    #--- N-corn price ratio of 6.56
    .[, EONR := -(b1 - 6.56) / (2 * b2)] %>% 
    st_as_sf
#--- spatial map:
par_name <- "EONR"
source(here("GitControlled/Codes/Modules/figure_par_map.R")) %>% print()
ggsave(file=here("Graph/map", paste0("true_pars_",par_name,".png") ),
       height=3.1, width=7.2)



#' /*=========================================================================*/
#' /*                               N Rate Map                                */
#' /*=========================================================================*/

# load trial design data
w_data <- field_with_design %>%
    filter(field_col == f_size) %>%
    data.table() %>%
    merge(., abrv_df, by = "design_name") %>%
    .[, design := factor(design_abrv,
                         levels = abrv_df$design_abrv)]

# ----------------------------------
#  individual design map
# ----------------------------------
for(i in 1:nrow(w_data)){
    
    source(here("GitControlled", "Codes", "Modules", "figure_map_Nrate_single.R"))
    
}

# ----------------------------------
# data of all designs combined
# ----------------------------------

data_ls <- list()

for(i in 1:nrow(w_data)){
    
    field_sf <- w_data[i, ]$field_sf[[1]] %>%
        .[, c("X", "Y", "cell_id", "aunit_id", "buffer")]
    reg_data <- readRDS(w_data[i, ]$data_file_name) %>%
        pull(reg_data)%>%
        .[[1]] %>%
        .[33]  # pick a random simulation
    data <- reg_data$data[[1]]
    N_levels <- reg_data$N_levels[[1]]
    data <-
        data %>%
        .[, Nid := as.numeric(as.factor(Nid))] %>%
        .[, Ntg := N_levels[Nid]]
    data_ls[[i]] <-
        left_join(field_sf, data[, .(aunit_id, Nid, Ntg)], by = "aunit_id") %>%
        data.table() %>%
        .[, Ntg := factor(Ntg)] %>%
        .[buffer == 1, Ntg := "buffer"] %>%
        .[, Ntg := factor(Ntg, levels = c("buffer", as.character(levels(Ntg)[-7])))] %>%
        .[, .(cell_id, aunit_id, Nid, Ntg)] %>% 
        .[, design_abrv := w_data[i, ]$design_abrv]
}
data <- rbindlist(data_ls) %>% 
    .[, design_abrv := factor(design_abrv, levels = abrv_df$design_abrv)]
field_sf <- w_data[1, ]$field_sf[[1]] %>%
    .[, c("cell_id", "aunit_id")]
field_sf_pool <- left_join(field_sf, data, by = c("cell_id", "aunit_id"))

# ----------------------------------
# combined Nrate map for all designs
# ----------------------------------
ggplot() +
    geom_sf(data = field_sf_pool, aes(fill = (Ntg), color = Ntg), size = 0) +
    scale_fill_grey(name = bquote("N Trial Rate (kg"~ha^{-1}~")" ), start = 1, end = 0) +
    scale_color_grey(name = bquote("N Trial Rate (kg"~ha^{-1}~")" ), start = 1, end = 0) +
    guides(fill = guide_legend(nrow = 1)) +
    facet_wrap(~ design_abrv, ncol = 4) +
    theme_void() +
    theme(
        text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, size = 24),
        strip.text.x = element_text(size = 18,
                                    margin = margin(0,0,0.1,0, "cm")),
        legend.position = c(0.625, 0.125),
        legend.title = element_text(size = 18, face = "bold"),
        legend.key.size = unit(0.6, "cm"),
        legend.key = element_rect(colour = "black"),
        legend.text = element_text(size = 18)
    )
ggsave(file=here("Graph/map/combine_Nrate_map_abrv.png"),
       height=8.35, width=14)


#' /*=========================================================================*/
#' /*                               Predicted EONR                            */
#' /*=========================================================================*/

#=== which trial design to use ===#
ds = "Latin Square LimJump"

#=== estimated EONR data ===#
eonr_df <- mc_sim_results %>% 
    filter(field_col == f_size) %>% 
    filter(design_name == ds) %>% 
    pull(sim_results) %>% 
    .[[1]] %>% 
    data.table() %>% 
    .[sim == i_sim, ] %>% 
    pull(mc_results) %>% 
    .[[1]] %>% 
    #--- use medium N price ===#
    .[, pN_select := median(pN)] %>% 
    .[pN==pN_select, ] %>% 
    #--- which eonr to use
    .[, .(aunit_id, opt_N_gwr_1)] %>% 
    setnames("opt_N_gwr_1", "eonr")

#---merge to field sf---
eonr_sf <- eonr_df %>% 
    #---merge to field sf---
    left_join(field_cell[, c("aunit_id")], ., by = c("aunit_id"))

#=== mapping of EONR ===#
gdata_sf <- eonr_sf %>% 
    data.table() %>% 
    .[!is.na(eonr), ] %>% 
    st_as_sf()
source(here("GitControlled/Codes/Modules/map_eonr.R"))
ggsave(file = here("Graph/map/pred_eonr.png"),
       height=6.6,width=14.4)


#' /*=========================================================================*/
#' /*                     Simulated (observed) Yield Map                      */
#' /*=========================================================================*/

#=== which trial design to use ===#
ds = "Latin Square LimJump"

#=== load the design layout data ===#
field_design <- field_with_design %>% 
    filter(field_col == f_size) %>% 
    filter(design_name == ds) %>% 
    pull(field_sf) %>% 
    .[[1]]
#* unmark buffer zone
field_design$buffer <- 0

#=== generate yield data ===#
yield_obs <- gen_reg_data_sim_i(design = ds, 
                                field_cell = field_cell, 
                                field_design = field_design) 

#=== mapping of obs yield ===#
gdata_sf <- yield_obs %>% 
    #---merge to field sf---
    left_join(field_cell[, c("aunit_id")], ., by = c("aunit_id"))
source(here("GitControlled/Codes/Modules/map_obs_yield.R"))
ggsave(file = here("Graph/map/obs_yield.png"),
       height=6.6,width=14.4)


