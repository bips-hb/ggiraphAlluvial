#' @md
#'
#' @title Strata at axes
#'
#' @description
#' This geometry is based on [ggalluvial::geom_stratum()].
#' See the documentation for those functions for more details.
#'
#' @param ... arguments passed to base function,
#' plus any of the interactive_parameters.
#' @examples
#' # add interactive contours to a ggplot -------
#' library(ggplot2)
#' library(ggiraph)
#' #...
#' #x <- girafe(ggobj = p)
#' if (interactive()) print(x)
#' @seealso [ggiraph::girafe()]
#' @export
geom_stratum_interactive <- function(...) ggiraph:::layer_interactive(ggalluvial::geom_stratum, ...)

#' @title ggproto class for ggiraph
#'
#' @description
#' ggproto class for ggiraph.
#'
#' geom_stratum_interactive returns a layer that contains a GeomInteractiveStratum
#' object. The GeomStratum object is responsible for rendering the data in the plot.
#' @format NULL
#' @usage NULL
#' @export
GeomInteractiveStratum <- ggplot2::ggproto(
  "GeomInteractiveStratum",
  ggalluvial::GeomStratum,

  required_aes = c("x", "y", "ymin", "ymax"),

  default_aes = ggiraph:::add_default_interactive_aes(ggalluvial::GeomStratum),
  parameters = ggiraph:::interactive_geom_parameters,

  setup_data = function(data, params) {

    width <- params$width
    if (is.null(width)) width <- 1/3

    transform(data,
              xmin = x - width / 2,
              xmax = x + width / 2,
              check.names = FALSE)
  },

  draw_key = ggiraph:::interactive_geom_draw_key,
  draw_panel = function(self, data, panel_params, coord, width=1/3, .ipar = IPAR_NAMES) {

    # taken from GeomRect and ggalluvial:GeomStratum

    strat_aes <- setdiff(
      names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
    )

    # construct polygon grobs
    polys <- lapply(split(data, seq_len(nrow(data))), function(row) {

      poly <- ggalluvial:::rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
      aes <- as.data.frame(row[strat_aes],
                           stringsAsFactors = FALSE)[rep(1, 5), ]

      GeomPolygon$draw_panel(cbind(poly, aes, group = 1), panel_params, coord)
    })

    # combine polygon grobs
    grob <- do.call(grid::grobTree, polys)
    grob$name <- grid::grobName(grob, "geom_stratum")
              coords <- coord$transform(data, panel_params)
    ggiraph:::add_interactive_attrs(grob, coords, ipar = .ipar)
  }
)
