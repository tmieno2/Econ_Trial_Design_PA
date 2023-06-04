


#------ rank design by gwr profit ------#
design_level <- gdata %>%
    .[model == "GWR 1", ] %>%
    .[order(profit), ] %>%
    .$design
gdata <- gdata[, design := factor(design, levels=design_level)]

#------ rank design by profit within each model group ------#
gdata <- gdata %>% 
    .[order(model, profit), ] %>% 
    .[, id := factor(1:.N)] %>% 
    .[, model := factor(model, levels = c("GWR 1", "GWR 2", "BRF"))]

#------ range of profit values ------#
value_ls <- -seq(0, -gdata[,min(profit_low)/5] %>% ceiling()*5, by = 10)
yaxis_min <- gdata$profit_low %>% min() - 30

#------ bar plot ------#
pdodge <- position_dodge(.9)
ggplot(data = gdata,
       aes(x = id, y = profit)) +
    geom_point(position = pdodge, size = 2, shape = 20) +
    geom_errorbar(
        aes(ymin = profit_low, ymax = profit_high), 
                  width = 0.25, position = pdodge) +
    geom_text(aes(x = id, y = profit_low-1, 
                  label = paste0(round(profit, 2), " (", round(profit_sd, 2),")")),
          position = pdodge, angle = 0, 
          hjust = 1.1, size = 3 ) +
    scale_x_discrete(position = "top", breaks = gdata$id, labels = gdata$design) +
    facet_wrap(~model, ncol = 1, strip.position = "left", scales = "free_y") +
    coord_flip() +
    ylab('Average Profit Relative to True Optimal ($/ha)') +
    xlab('') +
    scale_y_continuous(expand = c(0, 0), breaks = value_ls, label = value_ls, 
                       limits = c(yaxis_min-5, 0)) +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position='none',
        legend.title = element_text(size=12),
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(size=10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text=element_text(color='black')
    ) 

