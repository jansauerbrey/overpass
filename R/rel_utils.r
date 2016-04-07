# find all the nodes and store them in a data frame
process_osm_relations <- function(doc) {

  # efficiently find all node attributes (lat/lon)
  # and get them into a data frame
  tmp <- xml_attrs(xml_find_all(doc, "//relation"))
  relations <- as.data.frame(t(do.call(cbind, tmp)), stringsAsFactors=FALSE)
  relations <- relations[, c("id")]
  
  tmp <- xml_attrs(xml_find_all(doc, "//center"))
  centers <- as.data.frame(t(do.call(cbind, tmp)), stringsAsFactors=FALSE)
  relations <- cbind(relations, centers[, c("lon", "lat")])
  colnames(relations) = c("id", "lon", "lat")
  relations$id = as.character(relations$id)
  
  # find all the nodes with tags
  relations_with_tags <- tryCatch(xml_find_all(doc, "//relation[child::tag]"),
                              errror=function(err){ return(list(0)) })

  # if there are any, add the tag key/value to make a wide data frame
  if (length(relations_with_tags) > 0) {
    bind_rows(lapply(relations_with_tags, function(x) {
      v <- xml_attr(xml_find_all(x, "tag"), "v")
      names(v) <- xml_attr(xml_find_all(x, "tag"), "k")
      pts <- cbind.data.frame(id=xml_attr(x, "id"), t(v),
                              stringsAsFactors=FALSE)
    })) -> relation_attrs
    print(relation_attrs)
    print("relations")
    print(relations)
    relations <- left_join(relations, relation_attrs, by="id")
  }

  # need numeric lon/lat
  mutate(relations, lon=as.numeric(lon), lat=as.numeric(lat))
  
}
#osm_nodes_to_sptsdf <- function(osm_nodes) {
#  df <- data.frame(filter(osm_nodes, -lon, -lat))
#  spdf <- SpatialPointsDataFrame(as.matrix(osm_nodes[, c("lon", "lat")]), df)
#  spdf
#}
