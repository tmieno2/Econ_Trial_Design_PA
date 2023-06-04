

# ----------------------------------
# prepare sf data with all variables
# ----------------------------------
design_name <- w_data[i,]$design_name
field_sf <- w_data[i, ]$field_sf[[1]]
reg_data <- w_data[i, ]$data_file_name %>% 
    readRDS() %>% 
    pull(reg_data)%>%
    .[[1]] %>%
    .[1] # pick a random simulation
data <- reg_data$data[[1]]
N_levels <- reg_data$N_levels[[1]]
data <- 
    data %>%
    .[, Nid := as.numeric(as.factor(Nid))] %>%
    .[, Ntg := N_levels[Nid]]
f <-
    left_join(field_sf, data[, .(aunit_id, Nid, Ntg)], by = "aunit_id") %>%
    data.table() %>%
    .[, Ntg := factor(Ntg)] %>%
    .[buffer == 1, Ntg := "buffer"] %>%
    .[, Ntg := factor(Ntg, levels = c("buffer", as.character(levels(Ntg)[-7])))] %>%
    st_as_sf()


# ----------------------------------
# illustration map of N trial design
# ----------------------------------
plot_sf <- aggregate(f[, c("plot_id", "Nid")], by=list(f$plot_id), 
                     FUN=mean, na.rm=TRUE) 
block_sf <- aggregate(f[, c("block_id", "Nid")], by=list(f$block_id), 
                      FUN=mean, na.rm=TRUE) 
plot_text_sf <- st_centroid(plot_sf)
block_text_sf <- st_centroid(block_sf)
ggplot() +
    geom_sf(data = plot_sf, fill = NA, size = 0.3, linetype = "43") +
    geom_sf(data = block_sf, fill = NA, size = 2, linetype = "solid") +
    geom_sf_text(data = plot_text_sf, aes(label = Nid), size = 4) +
    theme_void()
ggsave(file=here("Graph","map", paste0("design_",design_name,".png")),
       height=8, width=14.4)


# ----------------------------------
# experimental N rates map
# ----------------------------------
ggplot() +
    geom_sf(data = f, aes(fill = (Ntg)), size = 0.3) +
    # scale_fill_viridis_d(name = "N rate (kg/ha)", direction = -1) +
    scale_fill_grey(name = "N rate (kg/ha)", start = 1, end = 0) +
    guides(fill = guide_legend(nrow = 1)) +
    theme_void() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = "bottom",
        legend.title = element_text(size = 18, face = "bold"),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size = 18)
    )
ggsave(file=here("Graph","map", paste0("Nrate_",design_name,".png")),
       height=8, width=14.4)


# ------------------------------------
# experimental N rates map with blocks
# ------------------------------------
ggplot() +
    geom_sf(data = f, aes(fill = (Ntg)), size = 0.3) +
    geom_sf(data = block_sf, color = "black", fill = NA, size = 2, 
            linetype = "solid") +
    geom_sf_text(data = block_text_sf, size = 10, color = "black",
                 aes(label = paste0("block ", block_id))) +
    # scale_fill_viridis_d(name = "N rate (kg/ha)", direction = -1) +
    scale_fill_grey(name = "N rate (kg/ha)", start = 1, end = 0) +
    guides(fill = guide_legend(nrow = 1)) +
    theme_void() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = "bottom",
        legend.title = element_text(size = 18, face = "bold"),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size = 18)
    )
ggsave(file=here("Graph","map", paste0("Nrate_",design_name,"_blocked.png")),
       height=8, width=14.4)

