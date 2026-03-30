##' @name helper functions (previous/following stratum)
##' @rdname funs
##'
##' @title Two helpers to select stratum connected to a flow
##'
##' @param x usually the .data pronoun
##' @param value ="stratum", the value that should be returned, e.g. "stratum" or "x"
##'
##' @return value mapped to the left (from) or right (to) side of the alluvium.
##' @importFrom rlang .data
NULL

##' @rdname funs
##' @examples
##' # stratum_to(.data)
##' @export
stratum_to <- function (x=.data, value="stratum") sapply(x[["alluvium"]], function(val) {
  x[[value]][which(x[["alluvium"]]==val & x[["flow"]]=="to")[1]]
})

##' @rdname funs
##' @examples
##' # stratum_from(.data)
##' @export
stratum_from <- function (x, value="stratum") sapply(x[["alluvium"]], function(val) {
  x[[value]][which(x[["alluvium"]]==val & x[["flow"]]=="from")[1]]
})
