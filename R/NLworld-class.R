################################################################################
#' The \code{NLworld} class
#'
#' Behaves the same as a \code{RasterLayer} object except:
#'
#' A \code{NLworld} is a grid composed of squared patches of resolution 1.
#' Patches have two coordinates \code{pxcor} and \code{pycor}.
#' \code{pxcor} and \code{pycor} are always integer.
#' \code{pxcor} and \code{pycor} represent the raster coordinates in the bottom left
#' corner of the patches.
#' When creating a \code{NLworld}, the extent of the \code{NLworld} is of
#' \code{xmin = minPxcor}, \code{xmax = maxPxcor}, \code{ymin = minPycor}, and
#' \code{ymax = maxPycor}. The number of patches is equal to
#' \code{((maxPxcor - minPxcor) + 1) * ((maxPycor - minPycor) + 1)}.
#' \code{pxcor} can be seen as a column number and \code{pycor} can be seen as a
#' row number but increasing as you move up. However, \code{pxcor} and \code{pycor}
#' can be negative if there are patches to the left or below the patch \code{[0,0]}.
#' The use of \code{[]} to extract \code{Raster*} cells by row and column numbers has
#' been redefined to extract \code{NLworld} patches using the patches' coordinates
#' \code{[pxcor,pyxor]}.
#'
#' @inheritParams RasterLayer
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @author Sarah Bauduin, Eliot McIntire, and Alex Chubaty
#' @exportClass NLworld
#'
setClass(
  "NLworld",
  contains = c("RasterLayer"),
  representation (
    minPxcor = "numeric",
    maxPxcor = "numeric",
    minPycor = "numeric",
    maxPycor = "numeric",
    pxcor = "numeric",
    pycor = "numeric"
  )
)


################################################################################
#' The \code{NLworldStack} class
#'
#' Behaves the same as a \code{RasterStack} object except that it is a collection
#' of \code{NLworld} objects.
#'
#' @inheritParams RasterStack
#'
#' @author Sarah Bauduin
#' @exportClass NLworldStack
#'
setClass(
  "NLworldStack",
  contains = "RasterStack"
)


################################################################################
#' Creating a \code{NLworldStack}
#'
#' Stacking several \code{NLworld} together to obtain a single \code{NlworldStack}
#' which contains different layers for the different \code{NLworld} values.
#'
#' @param ... \code{NLworld} objects.
#'
#' @return A \code{NLworldStack} object with the \code{NLworld} stacked as layers.
#'
#' @examples
#' # Create 2 worlds with the default settings but different values.
#' w1 <- createNLworld()
#' w2 <- createNLworld()
#' w1[] <- runif(n = 1089)
#' w2[] <- runif(n = 1089)
#' # Stack the 2 world together.
#' w3 <- NLstack(w1, w2)
#' plot(w3)
#'
#' @export
#' @importFrom raster addLayer
#' @docType methods
#' @rdname NLstack
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLstack",
  signature = "...",
  function(...) {
    standardGeneric("NLstack")
  })

#' @export
#' @importFrom SpaDES objectNames
#' @rdname NLstack
setMethod(
  "NLstack",
  signature = "NLworld",
  definition = function(...) {

    dots <- list(...)

    # Get object names:
    layerNames <- objectNames("NLstack")
    if(any(duplicated(sapply(layerNames, function(x) x$objs)))) {
      stop("Please use unique layer names")
    }

    worldStack <- new("NLworldStack")

    for (i in seq_along(dots)) {
      world <- dots[[i]]
      if(world@data@names == ""){
        world@data@names <- list(as.name(layerNames[[i]]$objs))  # name the layer with the object name
      }
      worldStack <- addLayer(worldStack, world)
    }
    return(worldStack)
  }
)

