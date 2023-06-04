

# ----------------------------------
#  Average worst rank of the two measures 
# ----------------------------------
df %>% 
    .[, mean_ranking := mean(ranking), by = .(id)] %>% 
    .[mean_ranking==max(mean_ranking), ] %>% 
    print()
# id of selected design
select_id <- df %>% 
    .[, mean_ranking := mean(ranking), by = .(id)] %>% 
    .[mean_ranking==max(mean_ranking), ] %>% 
    .[, unique(id)] %>% 
    .[1]
select_df <- data.table(
    variable = unique(df$variable),
    value = df[id==select_id, value],
    remove = "none",
    id = select_id
)
p_all <- 
    ggplot() +
    geom_histogram(data = df, aes(x = value, fill = variable)) +
    geom_vline(data = select_df, aes(xintercept = value), color = "red") +
    scale_fill_manual(values=rep("grey30", 5)) +
    facet_wrap(~ variable, nrow = 1, scales = "free") +
    xlab("") +
    ylab('Count') +
    theme_bw() +
    theme(
        legend.position = 'none',
        axis.text = element_text(color='black', size=7)
    )
p_all



