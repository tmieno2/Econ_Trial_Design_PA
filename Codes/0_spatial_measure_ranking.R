
#' Select the high-ranked and low-ranked designs
#' within each design type of:
#'   - patterned strip
#'   - patterned grid
#'   - Latin square
#' 
#' The selection procedure is:
#'   1. List all possible designs in the specific type;
#'   2. Calculate the spatial property measures (spatial balance, evenness, etc.);
#'   3. Get the average rank of each design over all measures.
#' 
#' Note: 
#'   The field layouts are dependent on the `field_with_design` and `field_sf` 
#'   data sets which should be generated later. This is a lazy compromise, 
#'   which skip the trouble of repeating the constructions of those data again here.
#'   


rm(list=ls())

#/*=================================================*/
#' # Settings
#/*=================================================*/

#=== Packages ===#
library(sp)
library(spatialreg)
library(gstat)
library(GWmodel)
library(scam)
library(mgcv)
library(parallel)
library(spdep)
library(mgcv)
library(dplyr)
library(ggplot2)
library(magrittr)
library(data.table)
library(sf)
library(here)
library(magic)
library(tictoc)
library(tidyverse)
library(stringr)

#=== Set working directory ===#
setwd(here())

#=== Load functions ===#
#* source all the functions in the Functions folder  
fs::dir_ls(here("Codes", "Functions"), full.names = TRUE) %>%
    lapply(., function(x) source(x))

#=== Load simulated data ===#
field_data <- readRDS(here("Data/field_data.rds"))
field_with_design <- readRDS(here("Data/field_with_design.rds"))

plot_width = field_data$plot_width[1]
plot_length = 12

#=== Result data table ===#
select_design <- data.table(
    variable = character(),
    value = numeric(),
    remove = character(),
    id = numeric(),
    design = character(),
    select = character()
)


                        ###################################
                        ###                             ###
                        ###     Fixed Strip Design      ###
                        ###                             ###
                        ###################################

## ==============================================================
##                      Single block design                     =
## ==============================================================

# ---------------------------
#  All possible strip designs
# ---------------------------
#*  Generate all permutations of strip orders
library(combinat)
Strip_ls <- permn(c(1:6))
length(Strip_ls)

#*  Trial design layout (ids of units)
dataset_strip <- field_with_design %>% 
    filter(field_col==144, design_name=="Strip Randomized Block")
field <- dataset_strip %>% pull(field_sf) %>% .[[1]] %>% data.table()

#* the number of blocks
block_num <- unique(field$block_id) %>% length()
N_levels <- 1:6

# ------------------------------
#  Calculate Efficiency Measures
# ------------------------------
#* loop calculation over 720 designs
tic()
spatial_measure_strip <-
    lapply(
        1:length(Strip_ls),
        function(x) fn_spatial_measure_strip(x, field = field)
    ) %>%
    rbindlist()
toc()
saveRDS(spatial_measure_strip, here("Results","spatial_measure_strip.rds"))


## ==========================================================
##                      Twin block design                   =
## ==========================================================

# ---------------------------
#  All possible strip designs
# ---------------------------
#*  All permutations of twin-block strip orders
library(combinat)
Strip_ls <- permn(c(1:6))
length(Strip_ls)

#*  Trial design layout (ids of units)
dataset_strip2 <- field_with_design %>% 
    filter(field_col==144, design_name=="Strip Fixed Block Worst")
field <- dataset_strip2 %>% pull(field_sf) %>% .[[1]] %>% data.table()

#* the number of blocks
block_num <- unique(field$block_id) %>% length()
N_levels <- 1:6

# ------------------------------
#  Calculate Efficiency Measures
# ------------------------------
#* loop calculation over 720 designs
tic()
spatial_measure_strip2 <-
    lapply(
        1:length(Strip_ls),
        function(x) fn_spatial_measure_strip2(x, field = field)
    ) %>%
    rbindlist()
toc()
saveRDS(spatial_measure_strip2, here("Results","spatial_measure_strip2.rds"))


## ============================
##    Ranking of strip designs
## ============================

#=== single-block strip designs ===#
spatial_measure_strip <- readRDS(here("Results", "spatial_measure_strip.rds")) %>% 
    .[, GR := GR_row + GR_col] %>% 
    .[, GR_row := NULL] %>% 
    .[, GR_col := NULL] %>% 
    #=== value reset: small value means good ===#
    .[, MST_neg := MSTmin*(-1)] %>% 
    .[, MSTmin := NULL] %>% 
    .[, variation_neg := variation_N*(-1)] %>% 
    .[, variation_N := NULL]

{
    spatial_measure_strip %>% summary()
    
    # only 2 measures vary
    spatial_measure_strip %>% 
        melt(id.vars = c("id")) %>% 
        ggplot(data = .) +
        geom_histogram(aes(x = value)) +
        facet_wrap(~ variable, ncol = 2, scales = "free")
    
    # remove not varying measures
    spatial_measure_strip <- 
        spatial_measure_strip %>% 
        .[, MST_neg := NULL] %>% 
        .[, NB_gini := NULL] %>% 
        .[, van_Es_var := NULL] %>% 
        .[, variation_neg := NULL] 
}

#=== twin-strip designs ===#
spatial_measure_strip2 <- readRDS(here("Results", "spatial_measure_strip2.rds")) %>% 
    .[, GR := GR_row + GR_col] %>% 
    .[, GR_row := NULL] %>% 
    .[, GR_col := NULL] %>% 
    #=== value reset: small value means good ===#
    .[, MST_neg := MSTmin*(-1)] %>% 
    .[, MSTmin := NULL] %>% 
    .[, variation_neg := variation_N*(-1)] %>% 
    .[, variation_N := NULL]

{
    spatial_measure_strip2 %>% summary()
    
    # same, also only 2 measures vary
    spatial_measure_strip2 %>% 
        melt(id.vars = c("id")) %>% 
        ggplot(data = .) +
        geom_histogram(aes(x = value)) +
        facet_wrap(~ variable, ncol = 2, scales = "free")
    
    # remove not-varying measures
    spatial_measure_strip2 <- 
        spatial_measure_strip2 %>% 
        .[, MST_neg := NULL] %>% 
        .[, NB_gini := NULL] %>% 
        .[, van_Es_var := NULL] %>% 
        .[, variation_neg := NULL] 
}

#=== merge single and twin designs ===#
spatial_measure_strip <- 
    rbind(
        spatial_measure_strip[, id := id + 1000],
        spatial_measure_strip2[, id := id + 2000]
    )

#=== ranking ===#
df <- 
    spatial_measure_strip %>% 
    melt(id.vars = c("id")) %>% 
    .[, ranking := rank(value, ties.method = "min"), by = .(variable)] %>% 
    .[, mean_ranking := mean(ranking), by = .(id)] %>% 
    print()

# ---------------------
#  best strip
# ---------------------
source(here("Codes", "Modules", "rank_strip_best.R"))
ggsave(p_all, file=here("Graph","design_select", "rank_strip_best.png"),
       height=2, width=6.5)

#* best Latin selected for simulation
select_design <- rbind(select_design,
                       select_df %>% .[, design:="strip"] %>% .[, select:="best"])
select_design_id <- select_design %>% 
    .[, .(id = mean(id)), by = .(design, remove, select)] %>% 
    .[order(design, remove, select), ] %>% 
    print()

#* strip matrix with best ranking
select_design_id[design=="strip" &
                     remove=="none" &
                     select=="best",
                 id] %>% `-`(1000) %>% 
    Strip_ls[[.]] %>% 
    saveRDS(here("Results", "strip_matrix_best.rds"))

# ---------------------
#  worst strip
# ---------------------
source(here("Codes", "Modules", "rank_strip_worst.R"))
ggsave(p_all, file=here("Graph","design_select", "rank_strip_worst.png"),
       height=2, width=6.5)

#* worst Latin selected for simulation
select_design <- rbind(select_design,
                       select_df %>% .[, design:="strip"] %>% .[, select:="worst"])
select_design_id <- select_design %>% 
    .[, .(id = mean(id)), by = .(design, remove, select)] %>% 
    .[order(design, remove, select), ] %>% 
    print()

#* strip matrix with worst ranking 
select_design_id[design=="strip" &
                     remove=="none" &
                     select=="worst",
                 id] %>% `-`(2000) %>% 
    Strip_ls[[.]] %>% 
    saveRDS(here("Results", "strip_matrix_worst.rds"))



                        ###################################
                        ###                             ###
                        ###     Fixed Block Design      ###
                        ###                             ###
                        ###################################

# -----------------------------------
#  All possible 6-plot orders in a block
# -----------------------------------
#*  Generate all permutations of strip orders
library(combinat)
FB_ls <- permn(c(1:6))
length(FB_ls)

#*  Trial design layout (ids of units)
dataset_FB <- field_with_design %>% 
    filter(field_col==144, design_name=="Randomized Block")
field <- dataset_FB %>% pull(field_sf) %>% .[[1]] %>% data.table()

#* the number of blocks
block_num <- unique(field$block_id) %>% length()
N_levels <- 1:6

# ------------------------------
#  Calculate Efficiency Measures
# ------------------------------
#* loop calculation over 720 designs
tic()
spatial_measure_FB <-
    lapply(
        1:length(FB_ls),
        function(x) fn_spatial_measure_FB(x, field = field)
    ) %>%
    rbindlist()
toc()
saveRDS(spatial_measure_FB, here("Results","spatial_measure_FB.rds"))


## ============================
##    Selection by Ranking
## ============================

spatial_measure <- readRDS(here("Shared/Results/spatial_measure_FB.rds")) %>% 
    .[, GR := GR_row + GR_col] %>% 
    .[, GR_row := NULL] %>% 
    .[, GR_col := NULL] %>% 
    #=== value reset: small value means good ===#
    .[, MST_neg := MSTmin*(-1)] %>% 
    .[, MSTmin := NULL] %>% 
    .[, variation_neg := variation_N*(-1)] %>% 
    .[, variation_N := NULL] 

{
    spatial_measure %>% summary()
    
    # same as strip designs, only 2 measures vary
    spatial_measure %>% 
        melt(id.vars = c("id")) %>% 
        ggplot(data = .) +
        geom_histogram(aes(x = value)) +
        facet_wrap(~ variable, ncol = 2, scales = "free")
    
    # remove not varying measures
    spatial_measure <- 
        spatial_measure %>% 
        .[, MST_neg := NULL] %>% 
        .[, NB_gini := NULL] %>% 
        .[, van_Es_var := NULL] %>% 
        .[, variation_neg := NULL] 
}

#=== ranking ===#
df <- 
    spatial_measure %>% 
    melt(id.vars = c("id")) %>% 
    .[, ranking := rank(value, ties.method = "min"), by = .(variable)] %>% 
    .[, mean_ranking := mean(ranking), by = .(id)] %>% 
    print()

# ---------------------
#  best FB
# ---------------------
#same ranking module as the strip
source(here("Codes", "Modules", "rank_strip_best.R"))
ggsave(p_all, file=here("Graph","design_select", "rank_FB_best.png"),
       height=2, width=6.5)

#* best FB selected for simulation
select_design <- rbind(select_design,
                       select_df %>% .[, design:="FB"] %>% .[, select:="best"])
select_design_id <- select_design %>% 
    .[, .(id = mean(id)), by = .(design, remove, select)] %>% 
    .[order(design, remove, select), ] %>% 
    print()

#* FB matrix with best ranking
select_design_id[design=="FB" &
                     remove=="none" &
                     select=="best",
                 id] %>% 
    Strip_ls[[.]] %>% 
    saveRDS(here("Results", "FB_matrix_best.rds"))

# ---------------------
#  worst FB
# ---------------------
#same ranking module as the strip
source(here("Codes", "Modules", "rank_strip_worst.R"))
ggsave(p_all, file=here("Graph","design_select", "rank_FB_worst.png"),
       height=2, width=6.5)

#* worst FB selected for simulation
select_design <- rbind(select_design,
                       select_df %>% .[, design:="FB"] %>% .[, select:="worst"])
select_design_id <- select_design %>% 
    .[, .(id = mean(id)), by = .(design, remove, select)] %>% 
    .[order(design, remove, select), ] %>% 
    print()

#* FB matrix with worst ranking
select_design_id[design=="FB" &
                     remove=="none" &
                     select=="worst",
                 id] %>% 
    Strip_ls[[.]] %>% 
    saveRDS(here("Results", "FB_matrix_worst.rds"))




                        ###################################
                        ###                             ###
                        ###     Latin Square Design     ###
                        ###                             ###
                        ###################################

# -----------------------------------
#  All possible Latin square designs
# -----------------------------------
#* Load pre-generated list of Latin squares (297,341 Latins)
Latin_ls <- readRDS(here("Shared/Data/Latin_ls.rds"))
length(Latin_ls)

#* Load trial design layout (ids of units)
dataset_latin <- field_with_design %>% 
    filter(field_col==144, design_name=="Latin Square Best")
field <- dataset_latin %>% pull(field_sf) %>% .[[1]] %>% data.table()

#* the number of blocks
block_num <- unique(field$block_id) %>% length()
N_levels <- 1:6

# ------------------------------
#  Calculate Efficiency Measures
# ------------------------------
#* loop calculation over 297,341 Latins (took 160 hours)
# tic()
# spatial_measure_latin <-
#     lapply(
#         1:length(Latin_ls),
#         function(x) fn_spatial_measure_latin(x, field = field)
#     ) %>%
#     rbindlist()
# toc()
# saveRDS(spatial_measure_latin, here("Results","spatial_measure_latin.rds"))

#* Note: Latin square with jump limit is index = 297338


## ============================
##    Selection by Ranking
## ============================

spatial_measure_latin <- readRDS(here("Shared/Results/spatial_measure_latin.rds")) %>% 
    .[, GR := GR_row + GR_col] %>% 
    .[, GR_row := NULL] %>% 
    .[, GR_col := NULL] %>% 
    #=== value reset: small value means good ===#
    .[, MST_neg := MSTmin*(-1)] %>% 
    .[, MSTmin := NULL] %>% 
    .[, variation_neg := variation_N*(-1)] %>% 
    .[, variation_N := NULL] %>% 
    .[, variation_neg := NULL] 

{
    spatial_measure_latin %>% 
        melt(id.vars = c("id")) %>% 
        ggplot(data = .) +
        geom_histogram(aes(x = value)) +
        facet_wrap(~ variable, ncol = 2, scales = "free")
}

#=== ranking ===#
df <- 
    spatial_measure_latin %>% 
    melt(id.vars = c("id")) %>% 
    .[, ranking := rank(value, ties.method = "min"), by = .(variable)] %>% 
    .[, mean_ranking := mean(ranking), by = .(id)] %>% 
    print()

{
    df[mean_ranking==min(mean_ranking),]
    # ranking is a bad idea; the 1st and 10000st may be very close, or even the same.
    df[ranking<3,]
    
    # min max ranking
    df %>% 
        .[, max_ranking := max(ranking), by = .(id)] %>% 
        .[max_ranking==min(max_ranking), ] %>% 
        print()
    
    # NB is most negatively related to the rest measures
    # the correlation matrix
    cor(spatial_measure_latin[, id := NULL])
}


# ------------------------------------
#  Overall Best-ranked Latin square
# ------------------------------------
source(here("Codes", "Modules", "rank_latin_best.R"))

#* compare all selection strategy in one graph
library(ggpubr)
ggarrange(p_all,
          p_NB_gini, 
          p_moran_I,
          p_van_Es_var,
          p_GR, 
          p_MST_neg,
          ncol = 1)
ggsave(file=here("Graph","design_select", paste0("rank_latin_best.png")),
       height=8, width=10)

#* best Latin selected for simulation
select_design <- rbind(select_design,
                       select_df_comb %>% .[, design:="latin"] %>% .[, select:="best"])
select_design_id <- select_design %>% 
    .[, .(id = mean(id)), by = .(design, remove, select)] %>% 
    .[order(design, remove, select), ] %>% 
    print()

#* best Latin matrix (ranking excluded NB_gini, 110119)
select_design_id[design=="latin" &
                     remove=="NB_gini" &
                     select=="best",
                 id] %>% 
    Latin_ls[[.]] %>% 
    saveRDS(here("Results", "latin_matrix_best.rds"))

# --------------------------------------
#  Overall Worst-ranked Latin square
# --------------------------------------
source(here("Codes", "Modules", "rank_latin_worst.R"))

#* compare all selection strategy in one graph
library(ggpubr)
ggarrange(p_all,
          p_NB_gini, 
          p_moran_I,
          p_van_Es_var,
          p_GR, 
          p_MST_neg,
          ncol = 1)
ggsave(file=here("Graph","design_select", "rank_latin_worst.png"),
       height=8, width=10)

#* worst Latin selected in simulation (use cascade Latin 297340)
select_design <- rbind(select_design,
                       select_df_comb %>% .[, design:="latin"] %>% .[, select:="worst"])
select_design_id <- select_design %>% 
    .[, .(id = mean(id)), by = .(design, remove, select)] %>% 
    .[order(design, remove, select), ] %>% 
    print()

#* use cascade Latin 297340 as the worst (remove none from ranking)
select_design_id[design=="latin" &
                     remove=="none" &
                     select=="worst",
                 id] %>% 
    Latin_ls[[.]] %>% 
    saveRDS(here("Results", "latin_matrix_worst.rds"))

#* Note:
#*   In fact, it seems 163953 (NB removed) is even worse than cascade, as its 
#*   NB is already very bad anyway, and its Moran's I is much worse than cascade.




                ###############################################
                ###                                         ###
                ###     Jump-Limited Latin Square Design    ###
                ###                                         ###
                ###############################################

#* From the row perspective there is essentially only one jump-limited (<= 2) 
#* Latin square. (The other one is a mirror.) But the vertical order of the rows
#* can have lots of possibilities.
#* The six rows in order of first element value is
LJ.matrix <- matrix(c(
    1, 3, 5, 6, 4, 2,
    2, 1, 3, 5, 6, 4,
    3, 5, 6, 4, 2, 1,
    4, 2, 1, 3, 5, 6,
    5, 6, 4, 2, 1, 3,
    6, 4, 2, 1, 3, 5
),
nrow = 6, ncol = 6, byrow = TRUE
)

# ---------------------------
#  All possible LJ designs
# ---------------------------
#*  All permutations of LJ row orders
library(combinat)
LJ_order_ls <- permn(c(1:6))
length(LJ_order_ls)

#* List of all LJ matrices
Latin_ls <- list()
for(i in 1:length(LJ_order_ls)){
    Latin_ls[[i]] <- LJ.matrix[LJ_order_ls[[i]], ]
}

#*  Trial design layout (ids of units)
dataset_latin <- field_with_design %>% 
    filter(field_col==144, design_name=="Latin Square LimJump")
field <- dataset_latin %>% pull(field_sf) %>% .[[1]] %>% data.table()

#* the number of blocks
block_num <- unique(field$block_id) %>% length()
N_levels <- 1:6

# ------------------------------
#  Calculate Efficiency Measures
# ------------------------------
#* loop calculation over 720 designs
tic()
spatial_measure <-
    lapply(
        1:length(Latin_ls),
        function(x) fn_spatial_measure_latin(x, field = field)
    ) %>%
    rbindlist()
toc()
saveRDS(spatial_measure, here("Results","spatial_measure_LJ.rds"))


## ============================
##    Selection by Ranking
## ============================

spatial_measure <- readRDS(here("Shared/Results/spatial_measure_LJ.rds")) %>% 
    .[, GR := GR_row + GR_col] %>% 
    .[, GR_row := NULL] %>% 
    .[, GR_col := NULL] %>% 
    #=== value reset: small value means good ===#
    .[, MST_neg := MSTmin*(-1)] %>% 
    .[, MSTmin := NULL] %>% 
    .[, variation_neg := variation_N*(-1)] %>% 
    .[, variation_N := NULL] %>% 
    .[, variation_neg := NULL] 

{
    spatial_measure %>% 
        melt(id.vars = c("id")) %>% 
        ggplot(data = .) +
        geom_histogram(aes(x = value)) +
        facet_wrap(~ variable, ncol = 2, scales = "free")
}

#=== ranking ===#
df <- 
    spatial_measure %>% 
    melt(id.vars = c("id")) %>% 
    .[, ranking := rank(value, ties.method = "min"), by = .(variable)] %>% 
    .[, mean_ranking := mean(ranking), by = .(id)] %>% 
    print()


# ------------------------------------
#  Best-ranked LJ
# ------------------------------------
source(here("Codes", "Modules", "rank_latin_best.R"))

#* compare all selection strategy in one graph
library(ggpubr)
ggarrange(p_all,
          p_NB_gini, 
          p_moran_I,
          p_van_Es_var,
          p_GR, 
          p_MST_neg,
          ncol = 1)
ggsave(file=here("Graph","design_select", paste0("rank_LJ_best.png")),
       height=8, width=10)

#* best Latin selected for simulation
select_design <- rbind(select_design,
                       select_df_comb %>% .[, design:="LJ"] %>% .[, select:="best"])
select_design_id <- select_design %>% 
    .[, .(id = mean(id)), by = .(design, remove, select)] %>% 
    .[order(design, remove, select), ] %>% 
    print()

#* best LJ matrix (ranking excluded NB_gini)
select_design_id[design=="LJ" &
                     remove=="NB_gini" &
                     select=="best",
                 id] %>% 
    Latin_ls[[.]] %>% 
    saveRDS(here("Results", "LJ_matrix_best.rds"))
#* Note:
#*   There is no clear winner in the LJ ranking. The NB-excluded one actually
#*   has very low NB ranking, and its GR and van_Es_var rankings are also not top.
#*   But moran_I and MST rankings are even lower for the other results. Relatively
#*   speaking, NB-excluded result is the over-all best.

# --------------------------------------
#  Worst-ranked LJ
# --------------------------------------
#* don't need the worst LJ

#===========================================================================================




