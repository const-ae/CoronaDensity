# Make map of Germany
library(tmap)
library(sf)

shape_kreise <- st_read("data/vg1000_2019-01-01.gk3.shape.ebenen/vg1000_ebenen/VG1000_KRS.shp")
shape_kreise

tm_shape(shape_kreise) +
  tm_polygons()


# Load Population density
library(tidyverse)

population_dens_raw <- read_lines("data/population_density_raw.tsv")
filtered_lines <- population_dens_raw[str_detect(population_dens_raw, "^\\d{5}")]
tf <- tempfile()
write_lines(paste0(filtered_lines, collapse = "\n"), tf)
pop_density <- read_tsv(tf, col_names = c("ID", "Type", "Name", "NUTS3", "Area", "Pop_total", "Pop_male", "Pop_female", "Pop_persqkm")) %>%
  dplyr::select(- c(Pop_male, Pop_female, Pop_persqkm)) %>%
  mutate(Area = str_remove_all(Area, "\\s") %>% readr::parse_double(locale = locale(decimal_mark = ","))) %>%
  mutate(Pop_total = str_remove_all(Pop_total, "\\s") %>% readr::parse_integer()) %>%
  separate(Name, into = c("Name", "Feature"), sep = ", ")



as_tibble(shape_kreise) %>%
  dplyr::select(ID = RS, Type = BEZ, Name = GEN) %>%
  full_join(pop_density, by = c("ID"))

# Plot of pop density
shape_kreise %>%
  mutate(ID = RS) %>%
  full_join(pop_density, by = c("ID")) %>%
  tm_shape() +
    tm_polygons("Pop_total")


pop_density %>%
  mutate(dots = round(Pop_total / 1e5)) %>%
  pull(dots) %>% sum


random_points_within_shape <- function(shape, n){
  res <- st_multipoint(matrix(numeric(2 * n), nrow = n, ncol = 2))
  bbox_shape <- st_bbox(shape)
  for(i in seq_len(n)){
    is_within <- FALSE
    while(!is_within){
      prop <- st_point(c(runif(n = 1, min = bbox_shape["xmin"], max = bbox_shape["xmax"]),
                         runif(n = 1, min = bbox_shape["ymin"], max = bbox_shape["ymax"])))
      is_within <- c(st_contains(shape, prop, sparse = FALSE))
    }
    res[i, ] <- prop
  }
  res <- st_sfc(res)
  st_crs(res) <- st_crs(shape)
  res
}


merged_data <- shape_kreise %>%
  mutate(ID = RS) %>%
  full_join(pop_density, by = c("ID")) %>%
  # head(n = 3) %>%
  mutate(dots = round(Pop_total / 5e4)) %>%
  rowwise() %>%
  mutate(points = random_points_within_shape(geometry, dots)) 
  
tm_shape(merged_data$geometry) +
    tm_polygons(col = "white", border.alpha = 0.3) +
  tm_shape(merged_data$points) +
    tm_dots(size = 0.1) 
  

