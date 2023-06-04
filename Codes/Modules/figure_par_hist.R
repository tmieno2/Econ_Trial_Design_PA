ggplot() +
    geom_histogram(data = field_cell, aes(x = get(par_name)),
                   color = "black", fill = "white", bins = 100) +
    xlab("") +
    ylab("Frequency") +
    theme_classic() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = "right",
        legend.title = element_text(size = 18, face = "bold"),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size = 18)
    )