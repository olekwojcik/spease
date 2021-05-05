#' F Function
#'
#' This function does the F Function.
#' @param sf_object An sf object.
#' @keywords spatial
#' @export


f_function <- function(sf_object, ...){
  #First generating random points
  bb_studyregion = sf::st_bbox(sf_object) # the study region's bounds
  random_df = tibble(
    x = runif(n = length(sf_object), min = bb_studyregion[1], max = bb_studyregion[3]),
    y = runif(n = length(sf_object), min = bb_studyregion[2], max = bb_studyregion[4])
  )
  random_points = random_df %>%
    st_as_sf(coords = c("x", "y")) %>% # set coordinates
    st_set_crs(st_crs(sf_object)) # set geographic CRS
  #prepping for the F-function formula
  dt <- st_distance(random_points, sf_object$geometry)
  dm <- as.matrix(dt)
  distances <- apply(dm, 1, min, na.rm=TRUE)
  max_dist <- max(distances)
  distances_df <- data.frame(distances = distances)
  d <- seq(from = 0, to = max_dist, by = max_dist/100)
  props <- d %>%
    map_dbl(.f = function(.){
      #print(distances_df$distances > .)
      mutate(distances_df,
             true = case_when(distances < . ~ 1,
                              distances >= . ~ 0)) %>%
        summarize(prop = mean(true)) %>%
        as.double()
    })
  f_df <- data.frame(distance = d,
                     prop = props)
  ggplot(data = f_df,
         mapping = aes(x = distance,
                       y = prop)) +
    geom_line(color = "steelblue") +
    theme_minimal() +
    labs(title = "F-Function")
}
