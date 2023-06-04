

#=== kernel curve data ===#
den_data <- gdata %>%
  .[model==label_data[i, model], ] %>% 
    .[, .(x=density(value)$x,
          y=density(value)$y),
      by=.(field_col,model,variable)] %>%
    print()

#=== percentage sum data ===#
sum_data <- gdata %>%
  .[model==label_data[i, model], ] %>% 
  .[, .(larger=sum(value>0)/length(value)), 
      by=.(model, field_col, variable)] %>%
    .[, larger_prct:=paste0(100*round(larger,2),'%')] %>%
    .[, lower_prct:=paste0(100*round(1-larger,2),'%')] %>%
    print()

ggplot()+
    geom_line(data=den_data, aes(x,y)) +
    geom_area(data=subset(den_data,x>0), aes(x,y), fill="red", alpha=0.5) +
    geom_area(data=subset(den_data,x<0), aes(x,y), fill="blue", alpha=0.5) +
    facet_wrap(~variable, ncol=3) +
    geom_text(data=sum_data, aes(x=label_data[i,largerx], y=label_data[i,largery], 
                                 label=larger_prct), 
              hjust=0, size=4) +
    geom_text(data=sum_data, aes(x=label_data[i,lowerx], y=label_data[i,lowery], 
                                 label=lower_prct), 
              hjust=0, size=4) +
    xlim(label_data[i,minx],label_data[i,maxx]) +
    xlab('Profit Difference between Two Designs ($/ha)') +
    ylab('Density') +
    ggtitle(label_data[i, model]) +
    theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text=element_text(color='black')
    )


