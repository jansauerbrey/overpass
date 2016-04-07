# handles making of a Lines list from the way elements
# NOTE: ways can be polygons; need to figure that out
process_osm_relations <- function(doc) {

  # get all the relations
  tmp <- xml_attrs(xml_find_all(doc, "//relation"))
  relations <- as.data.frame(t(do.call(cbind, tmp)), stringsAsFactors=FALSE)
  relations <- relations[,"id"]
  
  tmp <- xml_attrs(xml_find_all(doc, "//center"))
  centers <- as.data.frame(t(do.call(cbind, tmp)), stringsAsFactors=FALSE)
  relations <- cbind(relations, centers[, c("lon", "lat")])
  
  # need numeric lon/lat
  mutate(relations, lon=as.numeric(lon), lat=as.numeric(lat))

}
#osm_nodes_to_sptsdf <- function(osm_nodes) {
#  df <- data.frame(filter(osm_nodes, -lon, -lat))
#  spdf <- SpatialPointsDataFrame(as.matrix(osm_nodes[, c("lon", "lat")]), df)
#  spdf
#}
