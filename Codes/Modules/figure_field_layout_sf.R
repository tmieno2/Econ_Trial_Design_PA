

#'/*=====================================================================*/
#'
#'/*        Mapping Field Layout and Spatial Units: sf Method			 */
#' 
#'/*=====================================================================*/

# starting from the field sf data: field_sf
field_sf

plot_sf <-
    field_sf %>%
    nest_by(plot_id) %>%
    mutate(data = list(
        st_union(data)
    )) %>%
    unnest() %>%
    st_as_sf()

block_sf <-
    field_sf %>%
    nest_by(block_id) %>%
    mutate(data = list(
        st_union(data)
    )) %>%
    unnest() %>%
    st_as_sf()

block_text_sf <- st_centroid(block_sf)

#* Field: plots and blocks
g_field <-
    ggplot() +
    geom_sf(data = plot_sf, size = 0.2, fill = NA) +
    geom_sf(data = filter(plot_sf, plot_id == 264), fill = "red", alpha = 0.5) +
    geom_sf(data = block_sf, fill = NA, size = 1.5) +
    annotate("text", x = 830, y = 74, label = "plot", color = "red", size = 5) +
    geom_segment(
        aes(x = 830, xend = 830, y = 30, yend = -80),
        arrow = arrow(length = unit(0.5, "cm")),
        size = 1,
        color = "red"
    ) +
    geom_sf_text(
        data = block_text_sf,
        aes(label = paste0("block ", block_id)),
        size = 5
    ) +
    theme_void() +
    ggtitle("Panel (a): Plots and blocks in an experimental field")

## inside a plot
plot_sf_focus <- filter(field_sf, plot_id == 1)

subplot_sf <-
    plot_sf_focus %>%
    nest_by(aunit_id, buffer) %>%
    mutate(data = list(
        st_union(data)
    )) %>%
    unnest() %>%
    st_as_sf() %>%
    mutate(label = ifelse(
        buffer == 1,
        "buffer",
        paste0("subplot-", aunit_id - 1)
    )) %>%
    mutate(buf_or_not = ifelse(label == "buffer", "buffer", "subplot"))

subplot_text_sf <- st_centroid(subplot_sf)

site_sf <- plot_sf_focus[7, ]

g_inside_plot <-
    ggplot() +
    geom_sf(data = plot_sf_focus, size = 0.2, fill = NA) +
    geom_sf_text(data = subplot_text_sf, aes(label = label), size = 4) +
    geom_sf(data = subplot_sf, size = 1.2, fill = NA) +
    geom_sf(data = site_sf, fill = "red", alpha = 0.3) +
    annotate("text", x = 39, y = 429, label = "site") +
    scale_fill_discrete(name = "") +
    theme_void() +
    theme(
        legend.position = "bottom",
        legend.text = element_text(size = 12)
    ) +
    ggtitle("Panel (b): Subplots, buffers, and sites in a single plot")

