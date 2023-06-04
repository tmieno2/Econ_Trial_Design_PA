


#'/*=================================================*/
#'
#'/*                Field Layout				     */
#' 
#'/*=================================================*/

#' after getting the field sf and parameter data ready

#------ extract field parameters
{
    cell <- field_with_design$cell[1]
    aunit_width <- field_with_design$aunit_width[1]
    aunit_length <- field_with_design$aunit_length[1]
    plot_width <- field_with_design$plot_width[1]
    plot_length <- field_with_design$plot_length[1]
    cols_plot_in_block <- field_with_design$cols_plot_in_block[1]
    rows_plot_in_block <- field_with_design$rows_plot_in_block[1]
    field_col <- field_with_design$field_col[1]
    field_row <- field_with_design$field_row[1]
}

#------ sf to sp
field <- as_Spatial(field_sf)

#------ coloring plots as white & gray
field$even <- (field$plot_row_id + field$plot_col_id)%%2 == 0
field$color <- "white"
field$color[field$even] <- "grey90"

#------ manually draw sp polygons
par(mar=c(0,0,0,0))

#=== field map ===#
plot(field, border="grey60", col=field$color, lwd=0.1)

#=== block label ===#
block_length_m <- cell*plot_length*cols_plot_in_block
block_width_m <- cell*plot_width*rows_plot_in_block
x0 <- 0
y0 <- field_row*cell - block_width_m
x_coord <- c(0, 0, block_length_m, block_length_m, 0) + x0
y_coord <- c(0, block_width_m, block_width_m, 0, 0) + y0
p1 <- Polygons(list(Polygon(cbind(x_coord, y_coord))), 1)
sps = SpatialPolygons(list(p1))
plot(sps, add=T, border="black", lwd=2, lty=1)
text(x0 + block_length_m*0.4, y0 - block_width_m*0.4, 
     labels="Block", adj=c(0,0), cex=4)

#=== plot label ===#
plot_length_m <- cell*plot_length
plot_width_m <- cell*plot_width
x0 <- plot_length_m*3
y0 <- field_row*cell - plot_width_m*3
x_coord <- c(0, 0, plot_length_m, plot_length_m, 0) + x0
y_coord <- c(0, plot_width_m, plot_width_m, 0, 0) + y0
p1 <- Polygons(list(Polygon(cbind(x_coord, y_coord))), 1)
sps = SpatialPolygons(list(p1))
plot(sps, add=T, border="black", lwd=2, lty=1)
text(x0 + plot_length_m*0.2, y0 - plot_width_m*1.5, 
     labels="Plot", adj=c(0,0), cex=2)

#=== subplot label ===#
subplot_length_m <- cell*aunit_length
subplot_width_m <- cell*aunit_width
x0 <- plot_length_m*2
y0 <- field_row*cell - plot_width_m*3
x_coord <- c(0, 0, subplot_length_m, subplot_length_m, 0) + x0
y_coord <- c(0, subplot_width_m, subplot_width_m, 0, 0) + y0
p1 <- Polygons(list(Polygon(cbind(x_coord, y_coord))), 1)
sps = SpatialPolygons(list(p1))
plot(sps, add=T, border="black", lwd=2, lty=1)
text(x0 - plot_length_m*0.3, y0 - plot_width_m*1.5, 
     labels="Subplot", adj=c(0,0), cex=2)

#=== cell label ===#
x0 <- plot_length_m
y0 <- field_row*cell - plot_width_m*3
x_coord <- c(0, 0, cell, cell, 0) + x0
y_coord <- c(0, cell, cell, 0, 0) + y0
p1 <- Polygons(list(Polygon(cbind(x_coord, y_coord))), 1)
sps = SpatialPolygons(list(p1))
plot(sps, add=T, border="black", lwd=2, lty=1)
text(x0 - plot_length_m*0.2, y0 - plot_width_m*1.5, 
     labels="Cell", adj=c(0,0), cex=2)

