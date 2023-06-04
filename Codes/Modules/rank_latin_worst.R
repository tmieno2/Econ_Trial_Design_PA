

# ----------------------------------
#  Average rank of all measures 
# ----------------------------------
df %>% 
    .[, mean_ranking := mean(ranking), by = .(id)] %>% 
    .[mean_ranking==max(mean_ranking), ] %>% 
    print()

#*****************************************************************
# again, ranking is not the most appropriate approach to select best/worst designs
# latin 297340 (cascade latin) has 3 measures far worse than latin 144072,
# but the average ranking of 144072 is worst. I don't like it.
# Here I set 297340 (cascade) as the overall worst
#*****************************************************************
select_id <- 297340
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


# ----------------------------------
#  Exclude one measure 
# ----------------------------------
df_comb <- copy(df[, remove := "None"])
select_df_comb <- copy(select_df)
for(m in df$variable %>% unique()){
    df %>% 
        .[variable!=m, ] %>% 
        .[, mean_ranking := mean(ranking), by = .(id)] %>% 
        .[mean_ranking==max(mean_ranking), ] %>% 
        print()
    df_comb <- rbind(df_comb, df[, remove := m])
    
    select_id <- df %>% 
        .[variable!=m, ] %>% 
        .[, mean_ranking := mean(ranking), by = .(id)] %>% 
        .[mean_ranking==max(mean_ranking), ] %>% 
        .[, unique(id)] %>% 
        .[1]
    select_df <- data.table(
        variable = unique(df$variable),
        value = df[id==select_id, value],
        remove = m,
        id = select_id
    )
    select_df_comb <- rbind(select_df_comb, select_df)
    color_value <- rep("grey30", 5)
    color_value[which(df$variable %>% unique() == m)] <- "grey70"
    assign(paste0("p_", m), 
           ggplot() +
               geom_histogram(data = df, aes(x = value, fill = variable)) +
               geom_vline(data = select_df, aes(xintercept = value), color = "red") +
               scale_fill_manual(values=color_value) +
               facet_wrap(~ variable, nrow = 1, scales = "free") +
               xlab("") +
               ylab('Count') +
               theme_bw() +
               theme(
                   legend.position = 'none',
                   axis.text = element_text(color='black', size=7)
               )
    )
}
select_df_comb %>% 
    .[, .(id = mean(id)), by = .(remove)] %>% 
    print()
