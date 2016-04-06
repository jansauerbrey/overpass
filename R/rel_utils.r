# handles making of a Lines list from the way elements
# NOTE: ways can be polygons; need to figure that out
process_osm_rels <- function(doc, osm_nodes) {

  # get all the centers
  tmp <- xml_attrs(xml_find_all(doc, "//center"))
  relations <- as.data.frame(t(do.call(cbind, tmp)), stringsAsFactors=FALSE)
  relations <- relations[, c("lon", "lat")]
  
  # need numeric lon/lat
  mutate(nodes, lon=as.numeric(lon), lat=as.numeric(lat))

}
#osm_nodes_to_sptsdf <- function(osm_nodes) {
#  df <- data.frame(filter(osm_nodes, -lon, -lat))
#  spdf <- SpatialPointsDataFrame(as.matrix(osm_nodes[, c("lon", "lat")]), df)
#  spdf
#}
