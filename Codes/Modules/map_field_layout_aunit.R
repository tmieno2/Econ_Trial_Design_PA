

# load cell-level field sf data
field_sf <- field_with_design %>%
    filter(field_col==144) %>%
    pull(field_sf) %>%
    .[[13]]
field_sf


#* aunit map
aunit_sf <-
    field_sf %>%
    nest_by(block_id, aunit_id, buffer) %>%
    mutate(
        data = list(st_union(data))
    ) %>%
    unnest(cols = c(data)) %>%
    st_as_sf()
aunit_sf

#* block map
block_sf <-
    field_sf %>%
    nest_by(block_id) %>%
    mutate(
        data = list(st_union(data))
    ) %>%
    unnest(cols = c(data)) %>%
    st_as_sf()
block_sf

ggplot() +
    geom_sf(data = aunit_sf, aes(fill = factor(buffer)), size = 0) +
    geom_sf(data = block_sf, fill = NA, linewidth = 2) +
    geom_sf_text(data = aunit_sf, aes(label = aunit_id), size = 3, angle = 90) +
    theme_void() +
    ggtitle("Wave design") +
    theme(
        plot.title = element_text(hjust = 0.5)
    )



