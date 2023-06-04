

#------ mean value ------#
mean_data <- gdata %>%
    .[, .(profit = mean(profit),
          profit_sd = sd(profit),
          IQR = quantile(profit, 0.75) - quantile(profit, 0.25),
          Q25 = quantile(profit, 0.25),
          Q75 = quantile(profit, 0.75),
          profit_min = min(profit),
          profit_max = max(profit)
    ), 
    by = c("field_col", "design", "model", "subgroup")] %>%
    .[, whisk_down := pmax(Q25 - 1.5*IQR, profit_min)] %>% 
    .[, whisk_up := pmin(Q75 + 1.5 * IQR, profit_max)] %>% 
    .[, box_length := whisk_up - whisk_down] %>% 
    print()

#------ range of profit values ------#
# axis_range <- gdata %>% 
#     .[, .(y_min = min(profit),
#           y_max = max(profit)
#           ),
#           by = c("field_col", "model", "design", "subgroup")] %>% 
#     .[model=="GWR 1", y_min := -220] %>% 
#     .[model=="GWR 2", y_min := -80] %>% 
#     .[model=="MA_CF", y_min := -80] %>% 
#     .[model=="BRF", y_min := -140] %>% 
#     melt(id.var = c("field_col", "model", "design", "subgroup")) %>% 
#     setnames("value", "profit")
y_range <- list(
    `GWR` = scale_y_continuous(
        limits = c(mean_data[model=="GWR", min(whisk_down)], 
                   mean_data[model=="GWR", max(whisk_up) + 0.27*max(box_length)])),
    `BRF` = scale_y_continuous(
        limits = c(mean_data[model=="BRF", min(whisk_down)], 
                   mean_data[model=="BRF", max(whisk_up) + 0.27*max(box_length)])),
    `CF` = scale_y_continuous(
        limits = c(mean_data[model=="CF", min(whisk_down)], 
                   mean_data[model=="CF", max(whisk_up + 0.27*max(box_length))]))
    )

#------ boxplot ------#
my_dodge <- position_dodge(width = 0)
my_width <- 0.45
ggplot(data = gdata, 
       aes(x = reorder(design, desc(design)), y = profit, 
           fill = subgroup)) +
    stat_boxplot(geom = "errorbar", width = my_width, position = my_dodge) +
    geom_boxplot(fatten = NULL, position = my_dodge, width = my_width, outlier.shape = NA) +
    stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                 width = my_width, linetype = "solid") +
    geom_text(data = mean_data, color = "black",
              aes(x = design, y = whisk_up,
                  label = paste0(round(profit, 2), " (", round(profit_sd, 2),")")),
              position = my_dodge, angle = 90,
              hjust = -0.2, vjust = 0.5, size = 3.5 ) +
    facet_wrap(~ model, ncol = 3, strip.position = "bottom", scales = "free") +
    ylab( bquote("Profit Loss ($ "~Ha^{-1}~")") ) +
    xlab("") +
    # ggtitle( paste0("Price Ratio = ", p)) +
    scale_x_discrete(position = "top", 
                     limits = c(abrv_df$design_abrv[1:4],
                                "Blank_1",
                                abrv_df$design_abrv[5:8],
                                "Blank_2",
                                abrv_df$design_abrv[9:11],
                                "Blank_3",
                                abrv_df$design_abrv[12:13]),
                     labels = c("Blank_1" = "",
                                "Blank_2" = "",
                                "Blank_3" = "")) +
    # geom_blank(data = axis_range, aes(y = profit)) +
    facetted_pos_scales(y = y_range) +
    theme_bw() +
    theme(
        text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position='none',
        legend.title = element_text(size = 13),
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(size = 13),
        strip.text.x = element_text(size = 13),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5, size = 13),
        axis.text = element_text(color='black')
    ) 
