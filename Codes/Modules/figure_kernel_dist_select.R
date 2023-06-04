
#* Kernel density distribution of profit:

#------ stack plot ------#
ggplot() +
    stat_density(data = gdata_i, 
                 aes(x = profit, colour = design, linetype = design, size = design),
                 position="identity", geom="line") +
    facet_grid(model~., scales = "free_y") +
    coord_cartesian(xlim = c(xlow, xhigh) ) +
    xlab('Profit Relative to True Optimal ($/ha)') +
    ylab("Density") +
    labs(colour="Design", linetype="Design", size = "Design") +
    scale_size_manual(values = size_vector) +
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
