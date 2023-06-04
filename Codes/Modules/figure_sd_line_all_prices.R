

#------ points connected by line ------#
ggplot(data = gdata,
       aes(x = design, y = profit_sd, group = pLabel)) +
    geom_point(aes(color = pLabel, shape = pLabel), size = 2) +
    geom_line(aes(color = pLabel), size = 0.5) +
    facet_wrap(~ model, ncol = 3, strip.position = "bottom", scales = "free") +
    ylab( bquote("Standard Deviation of Profit ($ "~Ha^{-1}~")") ) +
    xlab("") +
    scale_x_discrete(position = "top") +
    scale_colour_discrete("Price Ratio") +
    scale_shape_discrete("Price Ratio") +
    guides(fill = guide_legend(nrow = 1)) +
    theme_bw() +
    theme(
        # panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        text = element_text(family = "serif"),
        legend.position='bottom',
        legend.title = element_text(size=12),
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
        axis.title.x = element_text(vjust = -2),
        axis.text=element_text(color='black')
    )


