
make_design_layout <-  function(plot_length, field_col) {

  design_layout_table <- 
    tribble(
      ~ design_name, ~ plot_length, ~ cols_plot_in_block, ~ rows_plot_in_block,
      "Strip Completely Random", field_col, 1, 1,
      "Strip Randomized Block", field_col, 1, 6,
      "Strip Fixed Block Best", field_col, 1, 6,
      "Strip Fixed Block Worst", field_col, 1, 12,
      "Completely Random", plot_length, 1, 1,
      "Randomized Block", plot_length, 2, 3,
      "Fixed Block Best", plot_length, 2, 3,
      "Fixed Block Worst", plot_length, 2, 3,
      "Latin Square Best", plot_length, 6, 6,
      "Latin Square Worst", plot_length, 6, 6,
      "Latin Square LimJump", plot_length, 6, 6,
      "Cascade Plot", plot_length, 12, 12,
      "Wave", plot_length, 10, 10,
    ) %>%
    data.table()
  
  return(design_layout_table)

}
