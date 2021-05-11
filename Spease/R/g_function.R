#' G Function
#'
#' This function implements the G function, or the "refined nearest neighbor", the cumulative frequency distribution of the nearest neighbor distances. (Based off of *Geographic Information Analysis* by O’Sullivan and Unwin)
#' @param sf_object An sf dataframe object.
#' @param return_df Logical, TRUE if function should return dataframe, FALSE to return ggplot object. Defaults to FALSE.
#' @param return_length Numeric, length of dataframe that will be returned. Defaults to 100.
#' @param ... Inherited from st_distance
#' @keywords spatial
#' @export


g_function <- function(sf_object,
                       return_df = FALSE,
                       return_length = 100, ...){
  
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
  
  g <- st_distance(sf_object, ...)
  gm <- as.matrix(g)
  diag(gm) <- NA
  distances <- apply(gm, 1, min, na.rm=TRUE)
  max_dist <- max(distances)
  distances_df <- data.frame(distances = distances)
  d <- seq(from = 0, to = max_dist, by = max_dist/return_length)
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
  
  if(return_df == TRUE){
    return(g_df)
  }
  
  if(return_df == FALSE){
    ggplot(data = g_df,
           mapping = aes(x = distance,
                         y = prop)) +
      geom_line(color = "steelblue") +
      theme_minimal() +
      labs(title = "G-Function")
    
  }
  
}
