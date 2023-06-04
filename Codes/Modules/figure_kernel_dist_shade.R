
#* Kernel density distribution of profit, with shaded area:

#------ rank design by gwr profit ------#
design_level <- gdata %>%
    .[, .(profit = mean(profit)), by=c("field_col", "design", "model")] %>%
    .[order(-profit), ] %>%
    .$design
gdata <- gdata[, design := factor(design, levels=design_level)]
    

#------ stack plot ------#
ggplot() +
    geom_density(data = gdata, 
                 aes(x = profit, colour = design, linetype = design, fill = design),
                 size=0.5, alpha = 0.2) +
    facet_grid(model~., scales = "free_y") +
    coord_cartesian(xlim = c(xlow, xhigh) ) +
    xlab('Profit Relative to True Optimal ($/ha)') +
    ylab("Density") +
    scale_color_discrete(name = "Design") +
    scale_fill_discrete(name = "Design") +
    scale_linetype_discrete(name = "Design") +
    theme_classic() +
    theme(
        legend.position='right',
        legend.title = element_text(size=12),
        legend.key.width = unit(1,"cm"),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text=element_text(color='black')
    )


