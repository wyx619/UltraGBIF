#' @title Detect threads and optimize them (internal)
#' @name usecores
#' @description Input a positive number and return optimized treads requirement.
#' @param x Threads requirement, a positive real number, default is 4
#'
#' @details If a positive integer is input,
#' return a positive integer not exceeding the total number of threads.
#' If a non-integer positive number is input, automatically calculate and return a reasonable number of threads.
#'
#' @return A positive real number of treads
#'
#'
#'
#' @export
usecores<-function(x)
{
  total=parallel::detectCores()
  if (!is.numeric(x))
  {stop("input must be numeric")}

  if (x > total)
  {stop("more than all available threads!")}

  if (x >= 1)
  {message(paste0(round(x),"/",total," ","threads used"))
    return(round(x))}

  if (x <= 0)
  {stop("illegal !!!")}

  if (0 < x && x < 1)
  {message(paste0(round(total*x)),"/",total," ","threads used")
    return(round(total*x))}

}
