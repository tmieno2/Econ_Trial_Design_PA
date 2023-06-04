

#' Draw figure of relationship between design economic performance and
#' possible explaining statistical measures of design
#' 


#=== prepare data ===#
mean_data <- gdata %>%
    dplyr::select(design, profit, explain_df$variable) %>% 
    #=== take average
    .[, lapply(.SD, mean), by = .(design)] %>% 
    #=== wide to Long: melt()
    melt(id.vars = c('design', 'profit')) %>%
    data.table() %>%
    #===generate label variables
    .[explain_df, on='variable'] %>%
    print()


#===nice plot===#
library(ggrepel)
ggplot(data = mean_data, aes(x = value, y = profit)) +
    geom_point(shape = 21, colour = "black", fill = "white", size = 3, stroke = 1) +
    geom_text_repel(aes(label = design), size = 2.5, color = "red") +
    facet_wrap(~varname, ncol = 2, scales = "free") +
    scale_y_reverse() +
    xlab("") +
    ylab( bquote("Average Profit Shortfall ($ "~Ha^{-1}~")") ) +
    theme_bw() +
    theme(
        legend.position = 'none',
        legend.title = element_text(size=12),
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(size=10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text = element_text(color='black', size=7)
    ) 

