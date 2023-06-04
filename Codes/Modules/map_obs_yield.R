


#* aggregate to aunit level
gdata_sf <- 
    aggregate(gdata_sf, by = list(gdata_sf$aunit_id), FUN = mean)


#* plot yield map
ggplot() +
    geom_sf(data = gdata_sf, aes(fill = yield), color = NA) +
    scale_fill_gradientn(colours = c("red2", "yellow", "green4")) +
    theme_void() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = "right",
        legend.title = element_text(size = 18, face = "bold"),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size = 18)
    ) +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 20,
                                 title = "(kg/ha)",
                                 title.position = "top", title.vjust = 4
    ))



