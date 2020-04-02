

create_plots <- function(df) {
  gg_di = ggplot(df, aes(x, y)) +
    geom_raster(aes(fill=value)) +
    scale_fill_gradientn(colours = colour_scale)
}


create_snapshots_old <- function(nz_plot, file_num) {
  plot_gg(nz_plot,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1132,700),
          zoom = 0.55, phi = 30)
  
  Sys.sleep(60)
  
  render_snapshot(filename = sprintf("timeseries/nzdi%03d.png", file_num), title_text = date_title)
  
  snapshottaken = TRUE
  
  if(rgl::rgl.cur() != 0) {
    rgl::rgl.close()
  }
  gc()
  
  return(snapshottaken)
}


create_snapshots <- function(file, plot_title) {
  
  nc_dat <- nc_open(file)
  ndvi.array <- ncvar_get(nc_dat) 
  nc_close(nc_dat) 
  nc_df <- ndvi.array %>% reshape2::melt() %>% 
    rename(x = Var2,
           y = Var1) %>% 
    mutate(value = ifelse(!is.nan(value), value, NA))
  rm(ndvi.array)
  
  p1 <- create_plots(nc_df)
  
  rm(nc_df)

  plot_gg(p1,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1132,700),
          zoom = 0.55, phi = 30)
  
  Sys.sleep(60)
  
  render_snapshot(filename = paste0("timeseries/", plot_title, ".png"), title_text = plot_title)

  if(rgl::rgl.cur() != 0) {
    rgl::rgl.close()
  }
  gc()
  
  snapshottaken = TRUE
  snapshottaken
}


