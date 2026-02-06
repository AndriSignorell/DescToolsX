
#' Convert Degrees to Radians and Vice Versa
#' 
#' Convert degrees to radians (and back again). 
#' 
#' @name conv_angle
#' @aliases DegToRad RadToDeg
#' 
#' @param deg a vector of angles in degrees. 
#' @param rad a vector of angles in radians. 
#' 
#' @return DegToRad returns a vector of the same length as \code{deg} with the
#' angles in radians.\cr RadToDeg returns a vector of the same length as
#' \code{rad} with the angles in degrees. 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @keywords arith
#' @examples
#' 
#' degToRad(c(90,180,270))
#' radToDeg( c(0.5,1,2) * pi)
#' 


#' @rdname conv_angle
#' @export
degToRad <- function(deg) deg * pi /180


#' @rdname conv_angle
#' @export
radToDeg <- function(rad) rad * 180 / pi

