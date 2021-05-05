#' G Function
#'
#' This function does the F Function.
#' @param sf_object An sf object.
#' @keywords spatial
#' @export


g_function <- function(sf_object, ...){
  g <- st_distance(sf_object)
  gm <- as.matrix(g)
  diag(gm) <- NA
  distances <- apply(gm, 1, min, na.rm=TRUE)
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
  g_df <- data.frame(distance = d,
                     prop = props)
  ggplot(data = g_df,
         mapping = aes(x = distance,
                       y = prop)) +
    geom_line(color = "steelblue") +
    theme_minimal() +
    labs(title = "G-Function")
}
