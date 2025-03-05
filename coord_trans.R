coord.trans <- function(coord,epsg.in,epsg.out){
  library(terra)
  
  if(epsg.in == 4326){
    epsg.in <- "+proj=longlat"
  }else{
    epsg.in <- paste0("EPSG:",epsg.in)
  }
  
  if(epsg.out == 4326){
    epsg.out <- "+proj=longlat"
  }else{
    epsg.out <- paste0("EPSG:",epsg.out)
  }
  ii <- complete.cases(coord)
  coord1 <- coord[ii,]
  x <- coord1[,1]
  y <- coord1[,2]
  xy <- cbind(x, y)
  colnames(xy) <- c('x', 'y')
  xy <- terra::vect(xy, "points")
  terra::crs(xy) <- epsg.in
  new.xy <- terra::project(xy, epsg.out)
  new.xy <- terra::geom(new.xy)
  new.xy.df <- as.data.frame(new.xy[, 3:4])
  return(new.xy.df)
}