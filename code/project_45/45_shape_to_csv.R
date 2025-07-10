library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

file_rdL <- "net/Countries/IS/RailrdL.shp"
file_rdC <- "net/Countries/IS/RailrdC.shp"
name_deges <-  "edges_IS.csv"
name_stations <- "stations_IS.csv"

# Load railway lines
railways_raw <- st_read(file_rdL)


get_line_endpoints <- function(line) {
  coords <- st_coordinates(line)
  list(start = coords[1, 1:2], end = coords[nrow(coords), 1:2])
}


endpoints <- railways_raw %>%
  st_geometry() %>%
  map(get_line_endpoints)


points_list <- map(endpoints, function(pt) {
  data.frame(
    lat = c(pt$start[2], pt$end[2]),
    lon = c(pt$start[1], pt$end[1])
  )
})

points_df <- bind_rows(points_list)

# Remove duplicates
nodes_unique <- points_df %>%
  distinct(lat, lon) %>%
  mutate(nodeID = row_number())


find_node_id <- function(lat, lon) {
  nodes_unique %>%
    filter(abs(lat - !!lat) < 1e-8, abs(lon - !!lon) < 1e-8) %>%
    pull(nodeID)
}

# Build edges
edges <- map_dfr(endpoints, function(pt) {
  from_id <- find_node_id(pt$start[2], pt$start[1])
  to_id <- find_node_id(pt$end[2], pt$end[1])
  if (length(from_id) == 1 && length(to_id) == 1) {
    data.frame(
      nodeID_from = from_id,
      nodeID_to = to_id
    )
  } else {
    NULL
  }
})


edges <- edges %>%
  rowwise() %>%
  mutate(edge_key = paste(sort(c(nodeID_from, nodeID_to)), collapse = "-")) %>%
  distinct(edge_key, .keep_all = TRUE) %>%
  select(nodeID_from, nodeID_to)


nodes <- nodes_unique %>%
  select(nodeID, lat, lon)

# Load stations
stations_raw <- st_read(file_rdC)


stations_coords <- st_coordinates(stations_raw)
stations_data <- stations_raw %>%
  st_drop_geometry() %>%
  mutate(lat = stations_coords[,2], lon = stations_coords[,1])


stations_with_nodes <- stations_data %>%
  rowwise() %>%
  mutate(nodeID = find_node_id(lat, lon)) %>%
  filter(!is.na(nodeID))

# Build stations 
stations <- nodes %>%
  left_join(
    stations_with_nodes %>% select(nodeID, nodeLabel = NAMN1, country_ISO3 = ICC),
    by = "nodeID"
  ) %>%
  rename(latitude = lat, longitude = lon)

# View final results
#head(nodes)
#head(edges)
#head(stations)


# Save to CSV
write.csv(stations, name_stations, row.names = FALSE)
write.csv(edges, name_deges, row.names = FALSE)






