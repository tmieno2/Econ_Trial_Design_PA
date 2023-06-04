


#------ rank design by gwr profit ------#
design_level <- gdata %>%
    .[model == "GWR", ] %>%
    .[order(profit), ] %>%
    .$design
gdata <- gdata[, design := factor(design, levels=design_level)]

#------ rank design by profit within each model group ------#
gdata <- gdata %>% 
    .[order(model, profit), ] %>% 
    .[, id:=1:.N] %>% 
    .[, model := factor(model, levels = c("GWR", "BRF"))]

#------ range of profit values ------#
value_ls <- -seq(0, -gdata[,min(profit_low)/5] %>% ceiling()*5, by=5)
yaxis_min <- gdata$profit_low %>% min() - 10

#------ bar plot ------#
pdodge <- position_dodge(.9)
ggplot(data = gdata,
       aes(x = id, y = profit, group = design, shape = model)) +
    geom_point(position = pdodge, size = 4) +
    geom_errorbar(
        aes(ymin = profit_low, ymax = profit_high, group = design, shape = model), 
                  width = 0.25, position = pdodge) +
    facet_grid(model~., scales = "free_y") +
    geom_text(aes(x = id, y = profit_low-1, 
                  label = paste0(round(profit, 2), " (", round(profit_sd, 2),")")),
          position = pdodge, angle = 0, 
          hjust = 1.1, size = 3 ) +
    geom_text(aes(x = id, y = profit_high, label = design),
              position = pdodge, angle = 0, 
              hjust = -0.1, size = 3 ) +
    scale_x_discrete(position = "bottom") +
    coord_flip() +
    ylab('Average Profit Relative to True Optimal ($/ha)') +
    xlab('') +
    scale_y_continuous(breaks = value_ls, label = value_ls, 
                       limits = c(yaxis_min-5, 5)) +
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


