

create_plots <- function(df) {
  gg_di = ggplot(df, aes(x, y)) +
    geom_raster(aes(fill=value)) +
    scale_fill_gradientn(colours = colour_scale)
}


create_snapshots <- function(nz_plot, file_num) {
  plot_gg(nz_plot,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
          zoom = 0.55, phi = 30)
  
  render_snapshot(filename = "timeseries/nzdi%03d.png")
  
  snapshottaken = TRUE
}


