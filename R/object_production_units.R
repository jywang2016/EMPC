#' Production Unit
#'
#' @description
#' Build, set value of the Production Unit(i.e. PV or WindTurbine)
#' @examples
#' #model <- production$new()
#'
#' #model$setvalue(generation,timestep)
#'
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

