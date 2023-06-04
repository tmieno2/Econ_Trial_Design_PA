


#------ rank design by profit within each model group ------#
gdata <- gdata %>% 
    .[order(model, profit), ] %>% 
    .[, id:=1:.N] %>% 
    .[, model := factor(model, levels = c("GWR", "BRF"))]


#------ range of profit values ------#
value_ls <- -seq(0,-gdata[,min(profit)/5] %>% ceiling()*5,by=5)
yaxis_min <- gdata$profit_low %>% min() - 5


#------ bar plot ------#
ggplot(data = gdata, 
       aes(x = id, y = profit, fill = design)) +
    geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
    geom_errorbar(
        aes(ymin = profit_low, ymax = profit_high, 
            group = design, shape = model), 
        width = 0.25, position = pdodge) +
    coord_flip() +
    facet_grid(model~., scales = "free_y") +
    geom_text(aes(x = id, y = profit_low,
                  label = paste0(round(profit, 2), " (", round(profit_sd, 2),")")),
              position = position_dodge2(0.9, reverse = TRUE), 
              hjust = 1.2, size = 3) +
    geom_text(aes(x = id, y = 0, label = design),
              position = position_dodge2(0.9, reverse = TRUE), 
              hjust = 1.1, size = 3
    ) +
    ylab('Average Profit Relative to True Optimal ($/ha)') +
    xlab('') +
    scale_y_continuous(breaks = value_ls, label = value_ls, 
                       limits = c(yaxis_min-5, 0)) +
    theme_classic() +
    theme(
        legend.position='none',
        legend.title = element_text(size=12),
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(size=10),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text=element_text(color='black')
    )  

    

