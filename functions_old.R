
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