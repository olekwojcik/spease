#' K Function
#'
#' This function does the K Function.
#' @name k_function
#' @param sf_object An sf object.
#' @keywords spatial
#' @export


k_function <- function(sf_object, ...){
  pol <-  st_as_sfc(st_bbox(sf_object))
  sfarea <- as.numeric(st_area(pol))
  sfdens <- as.numeric(sfarea/nrow(sf_object))

  #distance
  d <- distm(st_coordinates(sf_object),st_coordinates(sf_object), fun=distHaversine)
  #Applying formula from class
  dist <- seq(1, 25000, 100)
  Kd <- sapply(dist, function(x) sum(d < x)) # takes a while
  Kd <- Kd / (length(Kd) * sfdens)
  K_df <- data.frame(distance = dist, Kd=Kd)
  ggplot(data = K_df,
         mapping = aes(x = distance,
                       y = Kd)) +
    geom_line(color = "steelblue") +
    theme_minimal() +
    labs(title = "K-Function")
}
