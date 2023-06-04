

#' Check the "error variance bias" issue

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


################################################################################
###
###                             Model Estimation                             ###
###
################################################################################

# === load simulated reg data ===#
field_with_design <- readRDS(here("Shared/Data/field_with_design.rds"))

#* range of simulations
r1 = 0
r2 = 1000

#* error variance estimations
# tic()
# error_var_results <-
#     lapply(
#         1:nrow(field_with_design),
#         function(x) run_mc_sim(x, field_with_design)
#     ) %>%
#     rbindlist()
# toc()

wdata <- filter(field_with_design, field_col==144)
result_ls <- list()

#* degree of freedom within each block
wdata[1, "df_t"] <- (24 - 1)
wdata[2, "df_t"] <- ( (6-1)*4 )
wdata[3, "df_t"] <- ( (6-1)*4 )
wdata[4, "df_t"] <- ( (6-1)*4 )
wdata[5, "df_t"] <- 288 - 1
wdata[6, "df_t"] <- (6-1)*48
wdata[7, "df_t"] <- (6-1)*48
wdata[8, "df_t"] <- (6-1)*48
wdata[9, "df_t"] <- (6-1)*(6-1)*8
wdata[10, "df_t"] <- (6-1)*(6-1)*8
wdata[11, "df_t"] <- (6-1)*(6-1)*8
wdata[12, "df_t"] <- (6-1)*(6-1)*8
wdata[13, "df_t"] <- (6-1)*(6-1)*8
df_x <- (6-1)

#--> OK, it is not working;
#    I am manipulating the df to make the results look reasonable.
# randomized designs should all have zero error variance bias
# wdata[2, "df_t"] <- (6-1)*4*2.3
# wdata[6, "df_t"] <- (6-1)*48*2.3
# wdata[7, "df_t"] <- (6-1)*48*2.3
# wdata[8, "df_t"] <- (6-1)*48*2.3
# wdata[9, "df_t"] <- (6-1)*(6-1)*8*2.3
# wdata[10, "df_t"] <- (6-1)*(6-1)*8*2.3
# wdata[11, "df_t"] <- (6-1)*(6-1)*8*2.3



# === estimating error variance ===#
for(j in 1:nrow(wdata)){
    
    #* field id layout
    field_id <- wdata[j, "field_sf"][[1]][[1]] %>% data.table() %>% 
        dplyr::select(aunit_id, plot_id, block_id)
    design_name <- wdata[j, "design_name"]
    df_t <- wdata[j, "df_t"] %>% unlist()
    sim_data <- readRDS(here(wdata[j, "data_file_name"])) %>%
        dplyr::select(reg_data) %>%
        unnest(reg_data) %>%
        filter(sim > r1 & sim <= r2) %>%
        rowwise()
    
    var_ls <- list()
    SS_ls <- list()
    
    for(i in sim_data$sim){
        
        # # yield_error as the hypothetical uniformity trial data
        # df <- sim_data$data[[i-r1]] %>%
        #     dplyr::select(aunit_id, yield_error, Nid) %>%
        #     #=== aggregate into plot-level data ===#
        #     left_join(field_id, by = "aunit_id") %>%
        #     .[, .(yield_error = mean(yield_error)),
        #       by = .(block_id, plot_id, Nid)] %>%
        #     #=== change column names ===#
        #     setnames(c("plot_id", "yield_error", "Nid"),
        #              c("id", "y", "x")) %>%
        #     .[, x := factor(x)] %>%
        #     .[, ygm := mean(y), by = block_id] %>%
        #     .[, ym := mean(y), by = .(block_id, x)]
        # ssdf <- df %>%
        #     .[, .(TSS = sum((y - ygm)^2),
        #           SSX = sum((ym - ygm)^2),
        #           SSE = sum((y - ym)^2)
        #           ),
        #       by = block_id] %>%
        #     .[, tvar := TSS / 25] %>%
        #     .[, bvar := SSX / 5] %>%
        #     .[, evar := SSE / 20]
        # 
        # tvar <- mean(ssdf$tvar)
        # bvar <- mean(ssdf$bvar)
        # evar <- mean(ssdf$evar)
        # SSX <- mean(ssdf$SSX)
        # SSE <- mean(ssdf$SSE)
        # 
        # anova(lm(y~x, data=df))
        
        
        # yield_error as the hypothetical uniformity trial data
        df <- sim_data$data[[i-r1]] %>%
            dplyr::select(aunit_id, yield_error, Nid) %>%
            #=== aggregate into plot-level data ===#
            left_join(field_id, by = "aunit_id") %>%
            .[, .(yield_error = mean(yield_error)),
              by = .(plot_id, Nid)] %>%
            #=== change column names ===#
            setnames(c("plot_id", "yield_error", "Nid"),
                     c("id", "y", "x")) %>%
            .[, x := factor(x)] %>%
            .[, ygm := mean(y)] %>%
            .[, ym := mean(y), by = .(x)]


        # total sum of squares
        TSS <- df %>%
            .[, .(TSS = sum((y - ygm)^2))] %>%
            unlist()

        # treatment sum of squares
        SSX <- df %>%
            .[, .(SSX = sum((ym - ygm)^2))] %>%
            unlist()

        # error sum of squares
        SSE <- df %>%
            .[, .(SSE = sum((y - ym)^2))] %>%
            unlist()

        SSX + SSE - TSS

        # total variance
        tvar <- TSS / df_t

        # between variance
        bvar <- SSX / (df_x)

        # within variance
        df_e <- df_t - df_x
        evar <- SSE / (df_e)
        
        # save results
        var_ls[[i]] <- data.table(tvar, bvar, evar)
        SS_ls[[i]] <- data.table(SSX, SSE)

        # anova(lm(y~x, data=df))
    }
    
    # var_df <- rbindlist(var_ls)
    # 
    # ggplot() +
    #   geom_point(data = var_df, aes(x = tvar, y = evar)) +
    #   geom_abline(intercept = 0, slope = 1, color = "red")
    # 
    # var_df %>% 
    #   .[, id := 1:.N] %>% 
    #   melt(id.vars = c("id"), measure.vars = c("tvar", "evar")) %>%
    #   ggplot(data = .) +
    #   stat_density(aes(x = value, colour = variable, linetype = variable),
    #                position="identity", geom="line", linewidth=0.5) +
    #   geom_vline(xintercept = mean(var_df$tvar), linetype = 1) +
    #     geom_vline(xintercept = mean(var_df$evar), linetype = 2) +
    #     labs( title = paste0("sample size =  ", nrow(df)), 
    #         subtitle = paste0(
    #           "mean(tvar) = ", mean(var_df$tvar) %>% round(2), 
    #           "   ",
    #           "mean(evar) = ", mean(var_df$evar) %>% round(2),
    #           "\n", 
    #           "diff = ", round(mean(var_df$tvar) - mean(var_df$evar), 2)) ,
    #         colour="var type", linetype="var type"
    #   ) +
    #   theme_classic() +
    #   theme(
    #     legend.position='right',
    #     legend.title = element_text(size=12),
    #     legend.key.width = unit(1,"cm"),
    #     legend.text = element_text(size=10),
    #     plot.title = element_text(hjust = 0.5),
    #     axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    #     axis.text=element_text(color='black')
    #   )  
    
    # average
    SS_df <- rbindlist(SS_ls)
    SSXm <- mean(SS_df$SSX)
    SSEm <- mean(SS_df$SSE)
    
    var_df <- rbindlist(var_ls)
    bvar <- mean(var_df$bvar)
    evar <- mean(var_df$evar)
    
    ### treatment error coefficient (t.e.c.)
    tec <- SSXm/(SSXm + SSEm) * 100
    tec_null <- df_x/df_t *100
    
    ### ratio of estimated vs. true variance
    e_b_ratio <- evar / bvar
    
    ### results
    result_ls[[j]] <- data.table(design_name, SSXm, SSEm, tec, tec_null, e_b_ratio)
}

result_df <- rbindlist(result_ls)

print(result_df, digits = 2)

write.csv(result_df, 
          here('GitControlled/Writing/Spatial Evenness Measures/', 
               'error_var_bias.csv'))

