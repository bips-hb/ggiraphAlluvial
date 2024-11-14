#' @md
#'
#' @title Flows between lodes or strata
#'
#' @description
#' This geometry is based on [geom_flow()].
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
#' @seealso [girafe()]
#' @export
geom_flow_interactive <- function(...) {
  ggiraph:::layer_interactive(ggalluvial::geom_flow, ...)
}
#
# geom_flow_interactive <- function(...) {
#   ggiraph:::layer_interactive(
#     function (mapping = NULL, data = NULL, stat = "flow", position = "identity",
#               width = 1/3, knot.pos = 1/4, knot.prop = TRUE, curve_type = NULL,
#               curve_range = NULL, segments = NULL, aes.flow = "forward",
#               na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...)
#     {
#       browser()
#       aes.flow <- match.arg(aes.flow, c("forward", "backward"))
#       layer(geom = GeomFlow, mapping = mapping, data = data, stat = stat,
#             position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#             params = list(width = width, knot.pos = knot.pos, knot.prop = knot.prop,
#                           curve_type = curve_type, curve_range = curve_range,
#                           segments = segments, aes.flow = aes.flow, na.rm = na.rm,
#                           ...))
#     },
#     ...)
# }

#' @title ggproto class for ggiraph
#'
#' @description
#' ggproto class for ggiraph.
#'
#' geom_flow_interactive returns a layer that contains a GeomInteractiveFlow
#' object. The GeomFlow object is responsible for rendering the data in the plot.
#' @format NULL
#' @usage NULL
#' @export
GeomInteractiveFlow <- ggplot2::ggproto(
  "GeomInteractiveFlow", ggplot2::Geom,

  required_aes = c("x", "y", "ymin", "ymax"),

  default_aes = ggiraph:::add_default_interactive_aes(ggalluvial::GeomFlow),
  parameters = ggiraph:::interactive_geom_parameters,

  #setup_params = function(data, params) {
  #  #if (is.function(params$tooltip)) {
  #  #  params$tooltip <- params$tooltip(data$stratum)
  #  #}
  #  #params ### probably this did not work because params should not have the parameter afterwards .... could be done in setup_data acc to manual??? how?
  #  lapply(params, function(x) if(is.function(x)) return(x(data$stratum)) else x) # nonsense when using after_stat not necessary?
  #},

  setup_data = function(data, params) {
    width <- params$width
    if (is.null(width)) width <- 1/3

    knot.pos <- params$knot.pos
    if (is.null(knot.pos)) knot.pos <- 1/4

    # positioning parameters
    transform(data,
              xmin = x - width / 2,
              xmax = x + width / 2,
              knot.pos = knot.pos,
              check.names=F)
  },

  draw_panel = function(self, data, panel_params, coord,
                        width = 1/3, aes.flow = "forward",
                        knot.pos = 1/4, knot.prop = TRUE,
                        curve_type = NULL, curve_range = NULL,
                        segments = NULL, outline.type = "both",      .ipar=IPAR_NAMES) { #outline.type = "both" or "full"
    # parameter defaults
    if (is.null(curve_type)) curve_type <- ggalluvial:::ggalluvial_opt("curve_type")
    if (is.null(curve_range)) curve_range <- ggalluvial:::ggalluvial_opt("curve_range")
    if (is.null(segments)) segments <- ggalluvial:::ggalluvial_opt("segments")

    # exclude one-sided flows
    data <- data[stats::complete.cases(data), ]

    # adjoin data with itself by alluvia along adjacent axes
    flow_pos <- intersect(names(data), c("x", "xmin", "xmax",
                                         "width", "knot.pos",
                                         "y", "ymin", "ymax"))
    flow_aes <- intersect(names(data), c("linewidth", "size", "linetype",
                                         "colour", "fill", "alpha", .ipar )) # names(ggiraph:::add_default_interactive_aes(ggalluvial::GeomFlow))   ))
     #   join_later <- intersect(names(data),.ipar)
    flow_fore <- if (aes.flow != "backward") flow_aes else NULL#join_later#NULL
    flow_back <- if (aes.flow != "forward") flow_aes else NULL#join_later#NULL
    data <- self_adjoin(
      data = data, key = "x", by = "alluvium",
      link = flow_pos,
      keep.x = flow_fore, keep.y = flow_back,
      suffix = c(".0", ".1")
    )
       ##############
       # for(x in join_later) {
       #   data <- data %>% tidyr::unite(!!x, starts_with(x), sep = "->")
       # }
    ########################

    # aesthetics (in prescribed order)
    aesthetics <- intersect(ggalluvial:::.color_diff_aesthetics, names(data))
    # arrange data by aesthetics for consistent (reverse) z-ordering
    data <- data[do.call(order, lapply(
      data[, c("step", aesthetics)],
      function(x) factor(x, levels = unique(x))
    )), ]

    # construct x-spline grobs
    grobs <- lapply(split(data, seq_len(nrow(data))), function(row) {

      # path of spline or unit curve
      f_path <- ggalluvial::positions_to_flow(
        row$xmax.0, row$xmin.1,
        row$ymin.0, row$ymax.0, row$ymin.1, row$ymax.1,
        row$knot.pos.0, row$knot.pos.1,
        knot.prop = knot.prop,
        curve_type = curve_type, curve_range = curve_range,
        segments = segments
      )
      # aesthetics
      aes <- as.data.frame(row[flow_aes], stringsAsFactors = FALSE)
      # join aesthetics to path
      f_data <- cbind(f_path, aes[rep(1, nrow(f_path)), ])

      # transform (after calculating spline paths)
      f_coords <- coord$transform(f_data, panel_params)

      # graphics object for single row
      is_full_outline <- identical(outline.type, "full") || identical(outline.type, "both") #####################

      # polygon interior
      grob_polygon <- grid::xsplineGrob(
        x = f_coords$x, y = f_coords$y, shape = f_coords$shape,
        open = FALSE,
        gp = grid::gpar(
          fill = f_coords$fill, alpha = f_coords$alpha,
          col = if (is_full_outline) f_coords$colour else NA,
          lty = if (is_full_outline) f_coords$linetype else 1,
          lwd = if (is_full_outline) (rlang::`%||%`(  f_coords$linewidth ,f_coords$size)  ) * .pt else 0
        )
      )

      # if (is_full_outline) {
      #              # browser()
      #              # grob_polygon <- ggiraph:::add_interactive_attrs(grob_polygon, data, data_attr = "key-id", ipar = ggiraph:::get_ipar(panel_params))
      #   return(grob_polygon)
      # }
      #
      # # lower and upper bounds
      # if (identical(outline.type, "lower"))
      #   f_coords <- f_coords[f_coords$bound == 0L, ]
      # if (identical(outline.type, "upper"))
      #   f_coords <- f_coords[f_coords$bound == 1L, ]
      # grob_lines <- grid::xsplineGrob(
      #   x = f_coords$x, y = f_coords$y, shape = f_coords$shape,
      #   open = TRUE,
      #   id = f_coords$bound,
      #   gp = grid::gpar(
      #     col = f_coords$colour,
      #     lty = f_coords$linetype,
      #     lwd = ( rlang::`%||%`(  f_coords$linewidth , f_coords$size  )  ) * .pt
      #   )
      # )

      grob<-grob_polygon#grob <- grid::grobTree(grob_polygon, grob_lines)
                           CVVCAQA    # combine spline grobs
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "geom_flow")

    grob <- ggiraph:::add_interactive_attrs(grob, data, data_attr = "key-id", ipar = .ipar)

    grob
  },

  draw_key = ggiraph:::interactive_geom_draw_key,

  non_missing_aes = "size",
  rename_size = TRUE
)
