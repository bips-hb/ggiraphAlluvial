#' @md
#'
#' @keywords internal
#' @importFrom ggiraph girafe
"_PACKAGE"
#'
#' Access internal objects from ggiraph
#'
#' Safely retrieves non-exported objects from the \pkg{ggiraph} namespace.
#' This is used to extend ggiraph functionality while avoiding direct use
#' of \code{:::}, which triggers \code{R CMD check} notes.
#'
#' @param name Character string. Name of the internal ggiraph object.
#'
#' @return The requested object from the ggiraph namespace.
#'
#' @details
#' This function is a thin wrapper around \code{getFromNamespace()}.
#' It performs a check to ensure the requested object exists and
#' provides a clear error message otherwise.
#'
#' @keywords internal
ggiraph_internal <- function(name) {
  ns <- asNamespace("ggiraph")
  if (!exists(name, envir = ns, inherits = FALSE)) {
    stop(
      "Internal ggiraph object '", name, "' is not available. ",
      "Please install a compatible version of ggiraph.",
      call. = FALSE
    )
  }
  get(name, envir = ns, inherits = FALSE)
}

#' Access internal objects from ggalluvial
#'
#' Safely retrieves non-exported objects from the \pkg{ggalluvial} namespace.
#'
#' @param name Character string. Name of the internal ggalluvial object.
#'
#' @return The requested object from the ggalluvial namespace.
#'
#' @details
#' This helper is used to access ggalluvial internals without relying
#' on \code{:::}. It wraps \code{getFromNamespace()} and performs a
#' safety check.
#'
#' @keywords internal
ggalluvial_internal <- function(name) {
  ns <- asNamespace("ggalluvial")
  if (!exists(name, envir = ns, inherits = FALSE)) {
    stop(
      "Internal ggalluvial object '", name, "' is not available. ",
      "Please install a compatible version of ggalluvial.",
      call. = FALSE
    )
  }
  get(name, envir = ns, inherits = FALSE)
}
