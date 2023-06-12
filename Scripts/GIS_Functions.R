################################################################################
# GIS Functions used for analysis and generations of figures in the paper.     #
# entitled: "Searching for the earliest archaeological record: Insights from   #
#           the chimpanzee material landscapes.                                #                                                                            
#                                                                              #  
# Author: Jonathan S. Reeves                                                   #
# Date: 06.12.2023                                                             #  
################################################################################


hillshade_terra <- function(dem, df){
  require(terra)
  slope <- terra::terrain(dem, v="slope", unit= "radians")
  aspect <-terra::terrain(dem, v="aspect", unit= "radians")
  hs <- shade(slope,aspect, angle = 45)
  if(df==TRUE){
    hs <- as.data.frame(hs, xy = TRUE)
  }
  return(hs)
}

start_ggmap <- function(key =  "AIzaSyBA6RKAWOz9LnZN7axbs7C6wyvLKTFHygg"){
  require(ggmap)
  ggmap::register_google(key = key)
}

ggmap_bbox <- function(map, xCRS = 32629) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), xCRS))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox["ymin"]
  attr(map, "bb")$ll.lon <- bbox["xmin"]
  attr(map, "bb")$ur.lat <- bbox["ymax"]
  attr(map, "bb")$ur.lon <- bbox["xmax"]
  map
}

gghillshade <- function(dem, 
                        elev_ramp = "utah_1", 
                        elev_label = "Elev. (m.)",
                        alpha = .7 ){
  
  require(tidyterra)
  require(ggnewscale)
  dem_df <- as.data.frame(x = dem, xy = TRUE)
  hs_df <- hillshade_terra(dem, df = TRUE)
  plot <- ggplot() + 
    geom_raster(data=hs_df, 
                aes(x=x,y=y, fill = hillshade), 
                inherit.aes = FALSE, show.legend = FALSE) +
    scale_fill_gradient(low = "black", high = "white")+
    ggnewscale::new_scale_fill()+
    geom_raster(data = dem_df, 
                aes(x=x,y=y, fill = dem_df[,3]),
                alpha = alpha,
                inherit.aes = TRUE)+
    scale_fill_hypso_c(palette = elev_ramp, 
                       guide = guide_colorbar(title = elev_label))
    
  return(plot)
}
