
#* break color values
gdata_sf$eonr_fc <- cut(gdata_sf$eonr, 
                        quantile(gdata_sf$eonr, seq(0,1, by=0.25)),
                        include.lowest = TRUE
                        )

#* plot yield map
ggplot() +
    geom_sf(data = gdata_sf, aes(fill = (eonr_fc), color = eonr_fc), size = 0) +
    scale_fill_grey(name = "N rate (kg/ha)", start = 1, end = 0) +
    scale_color_grey(name = "N rate (kg/ha)", start = 1, end = 0) +
    guides(fill = guide_legend(nrow = 1)) +
    theme_void() +
    theme(
        text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, size = 24),
        strip.text.x = element_text(size = 18,
                                    margin = margin(0,0,0.1,0, "cm")),
        legend.position = "bottom",
        legend.title = element_text(size = 18, face = "bold"),
        legend.key.size = unit(0.6, "cm"),
        legend.key = element_rect(colour = "black"),
        legend.text = element_text(size = 18)
    )
