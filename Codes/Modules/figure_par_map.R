
ggplot() +
    geom_sf(data = field_cell, aes(fill = get(par_name)), color = NA) +
    scale_fill_gradientn(colours = rev(viridis(100))) +
    theme_void() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = "right",
        legend.title = element_text(size = 18, face = "bold"),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size = 18)
    ) +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 12,
                                 title = "",
                                 title.position = "top", title.vjust = 1
    ))
