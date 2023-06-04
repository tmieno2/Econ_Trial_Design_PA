

#------ mean value ------#
mean_data <- gdata %>%
    .[, .(profit = mean(profit),
          profit_sd = sd(profit),
          IQR = quantile(profit, 0.75) - quantile(profit, 0.25),
          profit_low = quantile(profit, 0.25),
          profit_high = quantile(profit, 0.75)
    ), 
    by=c("field_col", "design", "pLabel", "subgroup")] %>%
    .[, profit_low := profit_low - 1.5*IQR] %>% 
    .[, profit_high := profit_high + 1.5*IQR] %>% 
    print()

#------ range of profit values ------#
value_ls <- -seq(0, -gdata[,min(profit)/5] %>% ceiling()*5, by = 10)
yaxis_min <- gdata$profit %>% min()

#------ boxplot ------#
my_dodge <- position_dodge(width = 0)
my_width <- 0.45
ggplot(data = gdata, 
       aes(x = reorder(design, desc(design)), y = profit, 
           fill = subgroup)) +
    stat_boxplot(geom = "errorbar", width = my_width, position = my_dodge) +
    geom_boxplot(fatten = NULL, position = my_dodge, width = my_width, outlier.shape = NA) +
    stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                 width = my_width, linetype = "solid") +
    geom_text(data = mean_data, color = "black",
              aes(x = design, y = profit_low-2,
                  label = paste0(round(profit, 2), " (", round(profit_sd, 2),")")),
              position = my_dodge, angle = 90,
              hjust = 1.0, vjust = 0.5, size = 3 ) +
    facet_wrap(~pLabel, ncol = 3, strip.position = "right", scales = "free") +
    ylab('Average Profit Relative to True Optimal ($/ha)') +
    xlab('') +
    ggtitle( m ) +
    scale_x_discrete(position = "top",
                     limits = c(abrv_df$design_abrv[1:4],
                                "A",
                                abrv_df$design_abrv[5:7],
                                "B",
                                abrv_df$design_abrv[8:10],
                                "C",
                                abrv_df$design_abrv[11:12]),
                     labels = c("A" = "",
                                "B" = "",
                                "C" = "")) +
    # scale_y_continuous(expand = c(0, 0), breaks = value_ls, label = value_ls,
    #                    limits = c(yaxis_min + 0, 0)) +
    theme_bw() +
    theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position='none',
        legend.title = element_text(size=12),
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(size=10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text=element_text(color='black')
    ) 
