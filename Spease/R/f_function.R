#' F Function
#'
#' This function implements the F function, similar to the G function, but it is instead the cumulative frequency of distances from points randomly selected within the study region to the nearest event. (Based off of *Geographic Information Analysis* by O’Sullivan and Unwin)
#' @param sf_object An sf object
#' @param return_df Logical, TRUE if function should return dataframe, FALSE to return ggplot object. Defaults to FALSE.
#' @param return_length Numeric, length of dataframe that will be returned. Defaults to 100.
#' @param ... Inherited from st_distance
#' @importFrom  magrittr %>%
#' @keywords spatial
#' @export


f_function <- function(sf_object, return_df = FALSE, return_length = 100, ...){
  
  if(class(sf_object) != c("sf", "tbl_df", "tbl", "data.frame")){
    stop("sf_object is class ",
         class(sf_object))
  }
  
  if(class(return_df) != "logical"){
    stop("return_df is class ",
         class(return_df))
  }
  
  if(class(return_length) != "numeric" & class(return_length) != "integer"){
    stop("return_length is class ",
         class(return_length))
  }
  #First generating random points
  bb_studyregion = st_bbox(sf_object) # the study region's bounds
  random_df = tibble(
    x = runif(n = length(sf_object), min = bb_studyregion[1], max = bb_studyregion[3]),
    y = runif(n = length(sf_object), min = bb_studyregion[2], max = bb_studyregion[4])
  )
  random_points = random_df %>% 
    st_as_sf(coords = c("x", "y")) %>% # set coordinates
    st_set_crs(st_crs(sf_object)) # set geographic CRS 
  #prepping for the F-function formula
  dt <- st_distance(random_points, sf_object$geometry, ...)
  dm <- as.matrix(dt)
  distances <- apply(dm, 1, min, na.rm=TRUE)
  max_dist <- max(distances)
  distances_df <- data.frame(distances = distances)
  d <- seq(from = 0, to = max_dist, by = max_dist/return_length)
  props <- d %>%
    map_dbl(.f = function(.){
      #print(distances_df$distances > .)
      dplyr::mutate(distances_df,
                    true = dplyr::case_when(distances < . ~ 1,
                                            distances >= . ~ 0)) %>%
        dplyr::summarize(prop = mean(true)) %>%
        as.double()
    })
  f_df <- data.frame(distance = d,
                     prop = props)
  if(return_df == TRUE){
    return(f_df)
  }
  
  if(return_df == FALSE){
    ggplot2::ggplot(data = f_df,
                    mapping = ggplot2::aes(x = distance,
                                           y = prop)) +
      ggplot2::geom_line(color = "steelblue") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "F-Function")
  }
  
}
