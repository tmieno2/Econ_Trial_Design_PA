



#' #' /*=========================================================================*/
#' #' /*                           Best and Worst Designs                        */
#' #' /*=========================================================================*/
#' 
#' # -------------------------------------
#' # best/worse design in each simulation 
#' # -------------------------------------
#' pi_data_2 <- copy(pi_data)  %>%
#'     #===find max profit
#'     .[, vmax:=max(profit), by=.(field_col, model, sim)] %>%
#'     #===find min profit 
#'     .[, vmin:=min(profit), by=.(field_col, model, sim)] %>%
#'     .[order(field_col, model, sim, design),] %>%
#'     print()
#' best_data <- pi_data_2 %>%
#'     .[profit==vmax, .(field_col, model, sim, design)] %>%
#'     .[, perform:='Most Profitable'] %>%
#'     #===randomly select in ties
#'     .[sample(nrow(.)),] %>%
#'     unique(., by=c("field_col", "model", "sim")) %>%
#'     print()
#' worst_data <- pi_data_2 %>%
#'     .[profit==vmin, .(field_col, model, sim, design)] %>%
#'     .[, perform:='Least Profitable'] %>%
#'     #===randomly select in ties
#'     .[sample(nrow(.)),] %>%
#'     unique(., by=c("field_col", "model", "sim")) %>%
#'     print()
#' best_worst_data <- rbind(best_data, worst_data) %>%
#'     .[order(field_col, model, sim),] %>%
#'     #===generate label variables
#'     .[, perform:=factor(perform, levels=c('Most Profitable','Least Profitable'))] %>%
#'     print()
#' 
#' #=== percentage values
#' gdata <- best_worst_data %>%
#'     .[, design:=factor(design, levels=design_level)] %>%
#'     .[, .(pcnt=.N/1000), by=.(field_col, model, design, perform)] %>%
#'     .[order(model, perform, design),] %>%
#'     print()
#' 
#' 
#' #********* scenario: range=600, fieldsize=full *********#
#' 
#' # -------------------------------------
#' # stacked bar plot
#' # -------------------------------------
#' best_worst_data %>%
#'     .[field_col==144,] %>% 
#'     .[, design:=factor(design, levels=design_level)] %>%
#'     ggplot(data=., aes(x=model, fill=design)) +
#'     geom_bar(position = "fill", color='black', size=0.25) +
#'     # scale_fill_manual(values = c("white","grey60","grey30","grey0")) +
#'     facet_wrap(~perform) +
#'     scale_fill_discrete(name='Design') +
#'     xlab("") +
#'     ylab("Percent") +
#'     theme(
#'         legend.position='right',
#'         legend.key.size = unit(0.4, 'cm'),
#'         legend.text = element_text(size=10),
#'         axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
#'         axis.text=element_text(color='black')
#'     ) 
#' ggsave(file = here('Graph/pairwise/best_worst.png'),
#'        height=7.5,width=6.5)
#' 
#' # -------------------------------------
#' # pie chart
#' # -------------------------------------
#' library(plotly)
#' 
#' for(md in unique(gdata$model)){
#'     fig <- 
#'         gdata %>% 
#'         .[field_col==144,] %>% 
#'         .[model==md, ] %>% 
#'         .[perform=="Most Profitable", ] %>% 
#'         plot_ly(data = ., 
#'                 labels = ~design, 
#'                 values = ~pcnt, 
#'                 type = 'pie',
#'                 textposition = 'outside',
#'                 textinfo = 'label+percent') %>%
#'         layout(title = '',
#'                showlegend = FALSE,
#'                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#'                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#'     save_image(fig, 
#'                here(paste0('Graph/pairwise/best_prct_piechart_', md,'.png')))
#'               
#' }
#' 
#' #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' {
#'     #' /*================================================*/
#'     #' /*        Correlation(beta, N)                    */
#'     #' /*================================================*/
#'     
#'     # -----------------
#'     # load results data
#'     # -----------------
#'     all_results <- readRDS(file = "./Results/cor_beta_N.rds")
#'     
#'     # ----------------
#'     # data preparation
#'     # ----------------
#'     pi_data <- all_results %>%
#'         copy() %>%
#'         # ===relative profits
#'         .[, pi_gwr := pi_gwr - pi_opt] %>%
#'         # ===keep columns
#'         .[, .(design, sim, pi_gwr, cor_b1_N, cor_b2_N, cor_Nk_N, cor_pl_N)] %>%
#'         # === Wide to Long: melt()
#'         melt(id.vars = c("design", "sim", "pi_gwr")) %>%
#'         data.table() %>%
#'         # ===generate label variables
#'         .[variable == "cor_b1_N", application := "cor(b1, N)"] %>%
#'         .[variable == "cor_b2_N", application := "cor(b2, N)"] %>%
#'         .[variable == "cor_Nk_N", application := "cor(Nk, N)"] %>%
#'         .[variable == "cor_pl_N", application := "cor(plateau, N)"] %>%
#'         print()
#'     
#'     # -----------------
#'     # correlation plot
#'     # -----------------
#'     library(ggrepel)
#'     
#'     # ===pooled===#
#'     cor_df <- pi_data %>%
#'         .[, .(rho = cor(pi_gwr, value)), by = .(direction, application)] %>%
#'         .[, label := paste0("cor(pi,cor)=", round(rho, 3))] %>%
#'         print()
#'     ggplot() +
#'         geom_point(data = pi_data, aes(x = value, y = pi_gwr, color = design), size = 1) +
#'         geom_smooth(
#'             data = pi_data, aes(x = value, y = pi_gwr), method = "lm",
#'             formula = y ~ x + I(x^2)
#'         ) +
#'         geom_text(data = cor_df, aes(x = 0, y = 0, label = label)) +
#'         facet_grid(direction ~ application, scales = "free") +
#'         ggtitle("All Designs Pooled") +
#'         xlab("Correlation") +
#'         ylab("Profit Relative to True Optimal ($/ha)") +
#'         theme_bw()
#'     ggsave(
#'         file = paste0("./Graph/cor/cor_beta_N.png"),
#'         height = 8.5, width = 6.5
#'     )
#'     
#'     # ===by design===#
#'     design_ls <- unique(pi_data$design)
#'     for (temp_design in design_ls) {
#'         pi_data_temp <- pi_data %>%
#'             .[design == temp_design, ]
#'         cor_df <- pi_data_temp %>%
#'             .[, .(rho = cor(pi_gwr, value)), by = .(direction, application)] %>%
#'             .[, label := paste0("cor(pi,cor)=", round(rho, 3))] %>%
#'             print()
#'         ggplot() +
#'             geom_point(
#'                 data = pi_data_temp,
#'                 aes(x = value, y = pi_gwr, color = direction), size = 1
#'             ) +
#'             geom_smooth(
#'                 data = pi_data_temp, aes(x = value, y = pi_gwr, color = direction), method = "lm",
#'                 formula = y ~ x + I(x^2)
#'             ) +
#'             geom_text(data = cor_df, aes(x = 0, y = 0, label = label)) +
#'             facet_grid(application ~ ., scales = "free") +
#'             ggtitle(temp_design) +
#'             xlab("Correlation") +
#'             ylab("Profit Relative to True Optimal ($/ha)") +
#'             theme_bw()
#'         ggsave(
#'             file = paste0("./Graph/cor/cor_beta_N_", temp_design, ".png"),
#'             height = 8.5, width = 6.5
#'         )
#'     }
#'     
#'     
#'     ggplot() +
#'         geom_point(
#'             data = pi_data_temp,
#'             aes(x = value, y = pi_gwr, color = direction), size = 1
#'         ) +
#'         # geom_smooth(data=pi_data, aes(x=value, y=pi_gwr), method="lm", formula = y ~ x) +
#'         geom_smooth(data = pi_data_temp, aes(x = value, y = pi_gwr, color = direction), method = "lm") +
#'         geom_text(data = cor_df, aes(x = 0, y = 0, label = label)) +
#'         # facet_grid(direction~application, scales = "free") +
#'         ggtitle(temp_design) +
#'         xlab("Correlation") +
#'         ylab("Profit Relative to True Optimal ($/ha)") +
#'         theme_bw()
#'     
#'     
#'     # -----------------
#'     # linear fields
#'     # -----------------
#'     
#'     east_west <- readRDS(file = "./Results/cor_beta_N_east_west.rds") %>%
#'         .[, direction := "east_west"]
#'     north_south <- readRDS(file = "./Results/cor_beta_N_north_south.rds") %>%
#'         .[, direction := "north_south"]
#'     pi_data <- rbind(east_west, north_south) %>%
#'         # ===relative profits
#'         .[, pi_gwr := pi_gwr - pi_opt] %>%
#'         # ===keep columns
#'         .[, .(
#'             direction, design, sim, pi_gwr, cor_b1_N, cor_b2_N, cor_Nk_N, cor_pl_N,
#'             cor_e_N, cor_ye_N
#'         )] %>%
#'         # === take average
#'         # .[, lapply(.SD, mean), by=.(direction,design)] %>%
#'         # === Wide to Long: melt()
#'         melt(id.vars = c("direction", "design", "sim", "pi_gwr")) %>%
#'         data.table() %>%
#'         # ===generate label variables
#'         .[variable == "cor_b1_N", application := "cor(b1, N)"] %>%
#'         .[variable == "cor_b2_N", application := "cor(b2, N)"] %>%
#'         .[variable == "cor_Nk_N", application := "cor(Nk, N)"] %>%
#'         .[variable == "cor_pl_N", application := "cor(plateau, N)"] %>%
#'         .[variable == "cor_e_N", application := "cor(m_error, N)"] %>%
#'         .[variable == "cor_ye_N", application := "cor(yerror, N)"] %>%
#'         # ===strip designs only
#'         .[design %in% c("Fixed Strip Grad", "Fixed Strip Fluc", "Fixed Strip Latin", "Random Strip"), ] %>%
#'         print()
#'     
#'     
#'     ggplot(data = pi_data, aes(x = value, y = pi_gwr)) +
#'         geom_point(shape = 21, colour = "black", fill = "white", size = 6, stroke = 1) +
#'         # geom_smooth(data=pi_data, aes(x=value, y=pi_gwr), method="lm", formula = y ~ x) +
#'         # geom_text(data=cor_df, aes(x=0, y=0, label=label)) +
#'         geom_text_repel(aes(label = design), size = 2.5, color = "red") +
#'         facet_grid(direction ~ application, scales = "free") +
#'         ggtitle("Spatially Linear Change of Error Terms") +
#'         xlab("Correlation") +
#'         ylab("Profit Relative to True Optimal ($/ha)") +
#'         theme_bw()
#'     ggsave(
#'         file = paste0("./Graph/cor/cor_beta_N_linear_error_strip.png"),
#'         height = 6.5, width = 9.5
#'     )
#'     
#'     
#'     # ===by design===#
#'     design_ls <- unique(pi_data$design)
#'     for (temp_design in design_ls) {
#'         pi_data_temp <- pi_data %>%
#'             .[design == temp_design, ] %>%
#'             .[application %in% c("cor(yerror, N)"), ]
#'         cor_df <- pi_data_temp %>%
#'             .[, .(rho = cor(pi_gwr, value)), by = .(direction, application)] %>%
#'             .[, label := paste0("cor(pi,cor)=", round(rho, 3))] %>%
#'             print()
#'         ggplot() +
#'             geom_point(
#'                 data = pi_data_temp,
#'                 aes(x = value, y = pi_gwr, color = direction), size = 1
#'             ) +
#'             # geom_smooth(data=pi_data, aes(x=value, y=pi_gwr), method="lm", formula = y ~ x) +
#'             geom_smooth(data = pi_data_temp, aes(x = value, y = pi_gwr, color = direction), method = "lm") +
#'             geom_text(data = cor_df, aes(x = 0, y = 0, label = label)) +
#'             # facet_grid(direction~application, scales = "free") +
#'             ggtitle(temp_design) +
#'             xlab("Correlation") +
#'             ylab("Profit Relative to True Optimal ($/ha)") +
#'             theme_bw()
#'         ggsave(
#'             file = paste0("./Graph/cor/cor_error_N_", temp_design, ".png"),
#'             height = 8.5, width = 6.5
#'         )
#'     }
#' }



#' #' /*=========================================================================*/
#' #' /*                           Distribution of Profit                        */
#' #' /*=========================================================================*/
#' 
#' #=== a specific price scenario and estimation model ===#
#' p = unique(pi_data$pRatio)[2]
#' m = "GWR 1"
#' gdata <- pi_data[field_col==f_size,] %>%
#'     .[pRatio==p, ] %>%
#'     .[model==m]
#' 
#' 
#' # -----------------------------
#' # all designs stacked together
#' # -----------------------------
#' 
#' #--- kernel density lines ---#
#' xlow <- (-150)
#' xhigh <- (-25)
#' source(here("GitControlled/Codes/Modules/figure_kernel_dist.R"))
#' ggsave(file = here("Shared/Graph/kernel_dist",
#'                    paste0("kernel_dist_", p, "_", m,".png")),
#'        height=4,width=7.5)
#' 
#' #--- kernel density with shade ---#
#' # xlow <- (-150)
#' # xhigh <- (-25)
#' # source(here("GitControlled/Codes/Modules/figure_kernel_dist_shade.R"))
#' # ggsave(file = here("Shared/Graph/kernel_dist",
#' #                    paste0("kernel_dist_shade", p, "_", m,".png")),
#' #        height=4,width=7.5)
#' 
#' 
#' # ------------------
#' #  selected designs
#' # ------------------
#' 
#' #--- group 1: strip designs ---#
#' gdata_i <- gdata %>%
#'     .[design %in% c("SR", "SRB", "SFH", "SFL"), ] %>% 
#'     .[, design := factor(design, levels = c("SR", "SRB", "SFH", "SFL"))]
#' size_vector <- c("SR" = 2, "SRB" = 2, 
#'                  "SFH" = 1, "SFL" = 1)
#' source(here("GitControlled/Codes/Modules/figure_kernel_dist_select.R"))
#' 
#' #--- group 2: block designs ---#
#' gdata_i <- gdata %>%
#'     .[design %in% c("R", "RB", "FB"), ] %>% 
#'     .[, design := factor(design, levels = c("R", "RB", "FB"))]
#' size_vector <- c("R" = 1, "RB" = 2, 
#'                  "FB" = 1)
#' source(here("GitControlled/Codes/Modules/figure_kernel_dist_select.R"))
#' 
#' #--- group 3: Latin square designs ---#
#' gdata_i <- gdata %>%
#'     .[design %in% c("LJ", "LH", "LL", "C", "W"), ] %>% 
#'     .[, design := factor(design, levels = c("LJ", "LH", "LL", "C", "W"))]
#' size_vector <- c(c("LJ" = 2, "LH" = 2, "LL" = 1, "C" = 0.5, "W" = 0.5))
#' source(here("GitControlled/Codes/Modules/figure_kernel_dist_select.R"))
#' 
#' 
#' #--- combined figure ---#
#' gdata <- rbind(gdata0, gdata1, gdata2, gdata3) %>%
#'     .[, group := factor(group, levels = c("All","Top","Medium","Bottom"))]
#' ggsave(file = here("Shared/Graph/kernel_dist",
#'                    paste0("kernel_dist_select_", p, "_", m,".png")),
#'        height=7.5,width=8.5)
#' 
#' 
#' 
#' /*=========================================================================*/
#' /*                           Pairwise Comparison                           */
#' /*=========================================================================*/
#' design_level <- pi_data %>%
#'     .[field_col==144,] %>% 
#'     .[model == "GWR", ] %>%
#'     .[, .(profit = mean(profit)), by=c("design")] %>%
#'     .[order(-profit), ] %>%
#'     .$design
#' 
#' # ------------------------
#' # pairwise kernel density
#' # ------------------------
#' 
#' #=== pairs of designs to compare ===#
#' pair_df <- rbind(
#'     data.table(
#'         pair = 1,
#'         design = c("Alternate Block", "Latin Square Fixed",
#'                    "Latin Square Random", "Fixed Strip Fluc 2")),
#'     data.table(
#'         pair = 2,
#'         design = c("Alternate Block", "Latin Square Cascade","Randomized Block", 
#'                    "Fixed Strip Fluc 1", "Checkerboard", "Random Strip")),
#'     data.table(
#'         pair = 3,
#'         design = c("Alternate Block", "Completely Random",
#'                    "Fixed Strip Grad", "Cascade Plot", "Wave"))
#' )
#' 
#' #=== pairing designs (base: Alternate Block design) ===#
#' xlow <- (-80); xhigh <- 0
#' for(i in unique(pair_df$pair)){
#'     gdata <- pi_data %>% 
#'         .[field_col==144,] %>% 
#'         .[model=="GWR", ] %>% 
#'         .[pair_df[pair==i], on="design"] 
#'     #=== profit kernel comparisons ===#
#'     source(here("Codes/Modules/figure_kernel_dist_shade.R"))
#'     ggsave(file = here(
#'         paste0('Graph/pairwise/profits_dist_pair', i,'.png')
#'     ),
#'     height=4.5,width=6.5)
#' }
#' 
#' 
#' 
#' # ---------------------------
#' # pairwise profit difference
#' # ---------------------------
#' 
#' #=== difference between designs ===#
#' diff_data <- pi_data %>%
#'     #=== Long to Wide: dcast()
#'     dcast(field_col+model+sim~design, value.var="profit") %>%
#'     #=== pairwise difference
#'     .[, `Alternate Block - Latin Square Fixed` := `Alternate Block` - `Latin Square Fixed`] %>%
#'     .[, `Alternate Block - Latin Square Random` := `Alternate Block` - `Latin Square Random`] %>%
#'     .[, `Alternate Block - Latin Square Cascade` := `Alternate Block` - `Latin Square Cascade`] %>%
#'     .[, `Alternate Block - Fixed Block` := `Alternate Block` - `Fixed Block`] %>%
#'     .[, `Alternate Block - Fixed Strip Fluc` := `Alternate Block` - `Fixed Strip Fluc`] %>%
#'     .[, `Alternate Block - Randomized Block` := `Alternate Block` - `Randomized Block`] %>%
#'     .[, `Alternate Block - Random Strip` := `Alternate Block` - `Random Strip`] %>%
#'     .[, `Alternate Block - Completely Random` := `Alternate Block` - `Completely Random`] %>%
#'     .[, `Alternate Block - Cascade Plot` := `Alternate Block` - `Cascade Plot`] %>%
#'     .[, `Alternate Block - Fixed Strip Grad` := `Alternate Block` - `Fixed Strip Grad`] %>%
#'     .[, `Alternate Block - Wave` := `Alternate Block` - `Wave`] %>%
#'     #=== remove design columns ===#
#'     .[, !..design_level] %>%
#'     data.table() %>%
#'     #=== back to Long again
#'     melt(id.vars=c('field_col','model','sim')) %>%
#'     print()
#' 
#' #=== base scenario: range==600, field size=full
#' gdata <- diff_data %>% 
#'     .[field_col==144,]
#' label_data <- data.table(
#'     model = c("GWR", "BRF"),
#'     minx=c(-20, -20),
#'     maxx=c(50, 50),
#'     largerx=c(3, 3),
#'     largery=c(0.02, 0.02),
#'     lowerx=c(-15, -15),
#'     lowery=c(0.02, 0.02)
#' )
#' for(i in 1:2){
#'     source(here("Codes/Modules/figure_diff_dist.R"))
#'     ggsave(file = here(
#'         paste0('Graph/pairwise/profits_diff_', label_data[i, model],'.png')
#'     ),
#'     height=7.5, width=7.5)
#' }
#' 
#' 
#' # -------------------------------
#' #  difference table: percentages
#' # -------------------------------
#' for(md in unique(gdata$model)){
#' gdata <- pi_data %>% 
#'     .[field_col==144,] %>% 
#'     .[model == md, ] %>% 
#'     .[, design := factor(design, levels=design_level)] %>% 
#'     .[order(design), ]
#' 
#' pair_df <- data.frame()
#' for(i in 1:length(design_level)){
#'     for(j in 1:length(design_level)){
#'         pi <- gdata[design==design_level[i], profit]
#'         pj <- gdata[design==design_level[j], profit]
#'         pair_df[design_level[i], design_level[j]] <- 
#'             as.character(round(sum(pi>pj)/length(pi), 3)*100)
#'     }
#' }
#' pair_df[is.na(pair_df)] <- ""
#' write.csv(pair_df,
#'           here(paste0('Graph/tables/pair_df_', md,'.csv')))
#' }
#' 
# # -------------------------------------------
# #  difference table: absolute profits
# # -------------------------------------------
# gdata <- pi_data %>%
#     .[field_col==144,] %>%
#     .[, .(profit = mean(profit)),
#       by=c("field_col", "design_abrv", "model")] %>%
#     print()
# 
# for(md in unique(gdata$model)){
#     pair_df <- data.frame()
#     for(i in 1:length(design_level)){
#         for(j in 1:length(design_level)){
#             pi <- gdata[design==design_level[i]&model==md, profit]
#             pj <- gdata[design==design_level[j]&model==md, profit]
#             pair_df[design_level[i], design_level[j]] <-
#                 as.character(round(pi-pj, 2))
#         }
#     }
#     pair_df[is.na(pair_df)] <- ""
#     write.csv(pair_df, here(paste0('Graph/tables/profit_diff_', md,'.csv')))
# }
#' # -------------------------------------------
#' #  table: significance of profit difference
#' # -------------------------------------------
#' for(md in unique(gdata$model)){
#'     
#'     gdata <- pi_data %>%
#'         .[field_col==144,] %>% 
#'         .[model==md, ] %>% 
#'         print()
#'     
#'     pair_df <- data.frame()
#'     
#'     for(i in 1:length(design_level)){
#'         for(j in 1:length(design_level)){
#'             pi <- gdata[design==design_level[i], profit]
#'             pj <- gdata[design==design_level[j], profit]
#'             pv <- t.test(pi, pj, paired = TRUE)
#'             
#'             pair_df[design_level[i], design_level[j]] <- 
#'                 round(pv$estimate, 2)
#'             pair_df[as.character(i), design_level[j]] <- 
#'                 round(pv$p.value, 3)*(-1)
#'         }
#'     }
#'     pair_df[is.na(pair_df)] <- ""
#'     
#'     write.csv(pair_df,
#'               here(paste0('Graph/tables/profit_diff_ttest_', md,'.csv')))
#' }
#' 
#' #====== selected designs ======#
#' gdata <- pi_data %>%
#'     .[field_col==144,] %>% 
#'     .[model=="GWR", ] 
#' pi <- gdata[design=="Alternate Block", profit]
#' pj <- gdata[design=="Latin Square Fixed", profit]
#' t.test(pi, pj, paired = TRUE)