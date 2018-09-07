#' Production Unit
#'
#' @description
#' Build, set value of the Production Unit(i.e. PV or WindTurbine)
#' @usage
#' model <- production$new()
#'
#' model$setvalue(generation,timestep)
#'
#' @format
#' An object of R6Class
#' @examples
#' search `EMPC` on github, find more examples
#' @import R6
#' @export

production <- R6::R6Class(classname = "production",
                          public = list(
                            parameters = list(
                              generation  =  NA,
                              timestep  =  NA
                            ),
                            generation = NA,
                            setvalue = function(generation,timestep){
                              self$parameters$generation <- generation
                              self$parameters$timestep <- timestep
                            }
                          ),
                          private = list())

