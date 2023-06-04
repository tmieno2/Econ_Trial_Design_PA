


#' The "error variance bias" is mainly caused by blocking, not
#' randomization!

rm(list = ls())

# /*=================================================*/
#' # Settings
# /*=================================================*/

# === Packages ===#
library(sf)
library(dplyr)
library(ggplot2)
library(magrittr)
library(data.table)
library(here)
library(tictoc)
library(tidyverse)

# === Set working directory ===#
setwd(here())


# === load simulated field data ===#
field_with_design <- readRDS(here("Shared/Data/field_with_design.rds"))
wdata <- filter(field_with_design, field_col==144)

#* range of simulations
r1 = 0
r2 = 100

#* aunit level data
j = 6
print(wdata[j, "design_name"])
sim_data <- readRDS(here(wdata[j, "data_file_name"])) %>%
    dplyr::select(reg_data) %>%
    unnest(reg_data) %>%
    filter(sim > r1 & sim <= r2) %>%
    rowwise()


# === design layout data ===#
#* 6-plot blocking layout
field_id <- wdata[j, "field_sf"][[1]][[1]] %>% data.table() %>% 
    dplyr::select(aunit_id, plot_id, block_id)
n_plots <- length(unique(field_id$plot_id))
n_blocks <- length(unique(field_id$block_id))

# === error variance bias calculation ===#
ssx_ls <- list()
sse_ls <- list()

for(i in 1:100){
    
    #* hypothetical uniformity trial data: yield_error as yield
    df <- sim_data$data[[i-r1]] %>%
        dplyr::select(aunit_id, yield_error, Nid) %>%
        #=== aggregate into plot-level data ===#
        left_join(field_id, by = "aunit_id") %>%
        .[, .(yield_error = mean(yield_error)),
          by = .(block_id, plot_id, Nid)] %>%
        #=== change column names ===#
        setnames(c("plot_id", "yield_error", "Nid"),
                 c("id", "y", "x"))
    
    #* completely randomized design 
    df1 <- copy(df) %>%
        # re-create random trial rates
        .[, x := sample(rep(1:6, n_blocks), n_plots, replace = F)] %>% 
        .[, ygm := mean(y)] %>%
        .[, ym := mean(y), by = .(x)] %>% 
        .[, SSX := (ym - ygm)^2] %>% 
        .[, SSE := (y - ym)^2]
    
    #* randomized block design
    df2 <- copy(df) %>%
        .[, ygm := mean(y)] %>%
        .[, ym := mean(y), by = .(x)] %>% 
        .[, SSX := (ym - ygm)^2] %>% 
        .[, SSE := (y - ym)^2]
    
    #* patternized block design
    df3 <- copy(df) %>%
        .[, x := c(3,5,1,4,2,6), by = block_id] %>% 
        .[, ygm := mean(y)] %>%
        .[, ym := mean(y), by = .(x)] %>% 
        .[, SSX := (ym - ygm)^2] %>% 
        .[, SSE := (y - ym)^2]
    
    #* randomized block with re-shuffled block ids
    df4 <- copy(df) %>%
        #=== randomly re-shuffle block ids
        #=== that breaks the spatial pattern of data
        .[, block_id := sample(df$block_id, n_plots, replace = F)] %>% 
        .[, x := sample(1:6, 6, replace=F), by = block_id] %>% 
        .[, ygm := mean(y)] %>%
        .[, ym := mean(y), by = .(x)] %>% 
        .[, SSX := (ym - ygm)^2] %>% 
        .[, SSE := (y - ym)^2]
    

    # treatment sum of squares
    ssx_ls[[i]] <- data.table(
        CompRand = sum(df1$SSX), 
        RandBlock = sum(df2$SSX),
        PatternBlock = sum(df3$SSX),
        ReSpatialBlock = sum(df4$SSX)
        )

    # error sum of squares
    sse_ls[[i]] <- data.table(
        CompRand = sum(df1$SSE), 
        RandBlock = sum(df2$SSE),
        PatternBlock = sum(df3$SSE),
        ReSpatialBlock = sum(df4$SSE)
    )
    

}

ssx_df <- rbindlist(ssx_ls)
sse_df <- rbindlist(sse_ls)
colMeans(ssx_df)
colMeans(sse_df)

#-> SSX is much lower for blocking designs. The randomized and systematic
#    blocking designs result in similar SSX.
#   So non-randomization is not an issue at all. Blocking is the problem!!!




