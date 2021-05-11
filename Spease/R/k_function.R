#' K Function
#'
#' This function implements the K Function, which takes the mean of the number of events f each circle of radius distance around each event, and divides it by the study area density. (Based off of *Geographic Information Analysis* by O’Sullivan and Unwin)
#' @name k_function
#' @param sf_object An sf object.
#' @param return_df Logical, TRUE if function should return dataframe, FALSE to return ggplot object. Defaults to FALSE.
#' @param return_length Numeric, length of dataframe that will be returned. Defaults to 100.
#' @keywords spatial
#' @export


k_function <- function(sf_object, return_df = FALSE, return_length = 100){
  
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
  
  pol <-  st_as_sfc(st_bbox(sf_object))
  sfarea <- as.numeric(st_area(pol))
  sfdens <- as.numeric(sfarea/nrow(sf_object))
  
  #distance
  d <- distm(st_coordinates(sf_object),st_coordinates(sf_object), fun=distHaversine)
  #Applying formula from class
  dist <- seq(1, 25000, return_length)
  Kd <- sapply(dist, function(x) sum(d < x)) # takes a while
  Kd <- Kd / (length(Kd) * sfdens)
  K_df <- data.frame(distance = dist, Kd=Kd)
  
  if(return_df == TRUE){
    return(K_df)
  }
  
  if(return_df == FALSE){
    ggplot(data = K_df,
           mapping = aes(x = distance,
                         y = Kd)) +
      geom_line(color = "steelblue") +
      theme_minimal() +
      labs(title = "K-Function") 
    
  }
  
}
