


#------ rank design by gwr profit ------#
design_level <- gdata %>%
    .[model == "GWR", ] %>%
    .[order(-profit), ] %>%
    .$design
gdata <- gdata[, design := factor(design, levels=design_level)]

#------ range of profit values ------#
value_ls <- -seq(0,-gdata[,min(profit)/5] %>% ceiling()*5,by=5)


#------ bar plot ------#
ggplot(data = gdata, 
       aes(x = model, y = profit, fill = design)) +
    geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
    scale_x_discrete(position = "top") +
    coord_flip() +
    geom_text(aes(label = round(profit, 2)),
              position = position_dodge2(0.9, reverse = TRUE), 
              hjust = -0.1, size = 3) +
    geom_text(aes(x = model, y = 0, label = design),
              position = position_dodge2(0.9, reverse = TRUE), 
              hjust = 1.1, size = 3
    ) +
    ylab('Average Profit Relative to True Optimal ($/ha)') +
    xlab('') +
    scale_fill_discrete(name = "Design") +
    scale_y_continuous(breaks = value_ls, label = value_ls) +
    theme(
        legend.position='none',
        legend.title = element_text(size=12),
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(size=10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text=element_text(color='black')
    )  


