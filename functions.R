

#' create the 2d plot that will feed into rayshader to create the 3d plot
#'
#' @param df tidy gathered dataframe of the drought index data
#'
#' @return ggplot object
create_plots <- function(df) {
  
  # create the colour scale based on a max drought index value of 2
  # scale goes up in 0.25
  no_breaks <- 2/0.25
  colour_scale <- rep("#F3F3F3", no_breaks)
  colour_scale[length(colour_scale)] <- "#D45972"
  colour_scale[length(colour_scale) - 1] <- "#F57F6E"
  colour_scale[length(colour_scale) - 2] <- "#FDB480"
  colour_scale[length(colour_scale) - 3] <- "#FEDD95"
  colour_scale[length(colour_scale) - 4] <- "#FFFD7D"
  
  ggplot(df, aes(x, y)) +
    geom_raster(aes(fill=value)) +
    scale_fill_gradientn(colours = colour_scale)
}


#' load nc data. create the 3d plot, and save a snapshot
#'
#' @param file filename of the .nc file
#' @param plot_title title to add to the snapshot
#'
#' @return
create_snapshots <- function(file, plot_title) {

  # open the file
  nc_dat <- nc_open(file)
  # obtain the array of data from the nc file
  ndvi.array <- ncvar_get(nc_dat) 
  # close the nc file
  nc_close(nc_dat) 
  # turn the array into a tidy gathered dataframe
  nc_df <- ndvi.array %>% reshape2::melt() %>% 
    rename(x = Var2,
           y = Var1) %>% 
    mutate(value = ifelse(!is.nan(value), value, NA))
  
  # clean up no longer required array
  rm(ndvi.array)
  
  # call the `create_plots()` function to create the 2d ggplot
  p1 <- create_plots(nc_df)
  
  # clean up no longer required dataframe
  rm(nc_df)

  # pass the 2d ggplot into rayshader to create the 3d rendered image
  plot_gg(p1,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1132,700),
          zoom = 0.55, phi = 30)
  
  # after the rgl window is created, wait 1 minute before taking the snapshot (to allow the image to fully settle)
  Sys.sleep(60)
  
  # snapshot and save the 3d image
  render_snapshot(filename = paste0("timelapse_images/", plot_title, ".png"), title_text = plot_title)

  # in case the rgl window did not close, close it
  if(rgl::rgl.cur() != 0) {
    rgl::rgl.close()
  }
  
  # do some garbage collection
  gc()
  
  # return success
  snapshottaken = TRUE
  snapshottaken
}


