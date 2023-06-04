

#' Draw figure of relationship between design economic performance and
#' possible explaining statistical measures of design



#=== prepare data ===#
g_data <- gdata %>%
    dplyr::select(design, profit, explain_df$variable) %>% 
  #=== wide to Long: melt()
  melt(id.vars = c('design', 'profit')) %>%
  data.table() %>%
  #===generate label variables
  .[explain_df, on='variable'] %>%
  print()


#===nice plot===#
ggplot(data = g_data) +
  geom_point(aes(x = value, y = profit), size = 0.5) +
  geom_smooth(aes(x = value, y = profit), method="lm", formula = y ~ x+I(x^2)) +
  facet_wrap(~varname, ncol = 3, scales = "free") +
  xlab("") +
  ylab('Profit Relative to True Optimal ($/ha)') +
  theme_bw() +
    theme(
        legend.position = 'none',
        legend.title = element_text(size=12),
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(size=10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text = element_text(color='black', size=7)
    ) 



