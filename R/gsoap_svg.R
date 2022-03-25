#' @export gsoap_svg
#' @import ggplot2
#' @import ggiraph
#' @import ggrepel
#' @importFrom ggforce GeomCircle

#' Modifies gsoap_plot.R for compatible .svg exporting as well as additional
#' functionality


geom_circle_interactive <- function(...) {
  if (requireNamespace("ggforce")) {
    layerfunc <- getExportedValue("ggforce", "geom_circle")
    ggiraph:::layer_interactive(layerfunc, ..., interactive_geom = get_circle_geom("GeomCircle"))
  }
}

#' Create an extension for ggiraph package for supporting circles for rendering
#' our plot.
GeomInteractiveCircle <- ggproto(
  "GeomInteractiveCircle",
  ggforce::GeomCircle,
  default_aes = ggiraph:::add_default_interactive_aes(ggforce::GeomCircle),
  parameters = ggiraph:::interactive_geom_parameters,
  draw_key = ggiraph:::interactive_geom_draw_key,
  draw_panel = function(self, data, panel_params, coord, ..., .ipar = IPAR_NAMES) {
    zz <- GeomCircle$draw_panel(data, panel_params, coord, ...)
    coords <- coord$transform(data, panel_params)
    ggiraph:::add_interactive_attrs(zz, coords, ipar = .ipar)
  }
)
#' A function to convert all circles in a plot to a ggproto object for
#' interactive plotting.
#'
#' This helper function is necessary for .svg file exportation. "ggforce",
#' a dependency package will output circles that disregards the row order of
#' the data frame. This implies that the .svg file produced either through
#' ggiraph or the base export function from RStudio will have tags that are
#' unable to be reassigned with the correct order of data.
geom_circs <- function(onerow, geom.map) {
  geom_circle_interactive(mapping = geom.map,
                          data = onerow, show.legend=FALSE)
}

#' A function to retrieve and compose layer information for interactive
#' plotting.
#'
#' This function along with [geom_circle_interactive] is an addition to the
#' base functionalities of ggiraph package.
get_circle_geom <- function(name){
  circlegeom <- getExportedValue("ggforce", name)
  ggproto(
    "GeomInteractiveCircle",
    circlegeom,
    default_aes = ggiraph:::add_default_interactive_aes(circlegeom),
    parameters = ggiraph:::interactive_geom_parameters,
    draw_key = ggiraph:::interactive_geom_draw_key,
    draw_panel = function(self, data, panel_params, coord, ..., .ipar = IPAR_NAMES) {
      GeomInteractiveCircle$draw_panel(data, panel_params, coord, ..., .ipar = .ipar)
    }
  )
}



#' A function that generates an interactive html svg plot based on the original
#' plotting function [gsoap_plot]
gsoap_svg <-function(layout,
                     as.color = NULL,
                     as.alpha = NULL,
                     which.labels = NULL,
                     show.size.guide = TRUE,
                     show.color.guide = TRUE,
                     show.alpha.guide = TRUE,
                     size.guide.loc = c(1., 0),
                     size.guide.no = 4,
                     default.color = 'black',
                     default.alpha = 0.5,
                     range.alpha = c(0.1, 0.9),
                     viridis.option = 'viridis',
                     viridis.direction = 1,
                     viridis.range = c(0, 1),
                     label.alpha = 1.0,
                     segment.alpha = 0.5,
                     repel.xynudges = c(0.1, 0.1),
                     repel.hvjust = c(0, 0),
                     repel.direction = 'both',
                     base.fontsize = 8,
                     size.guide.fontsize = base.fontsize - 1,
                     label.fontsize = base.fontsize - 1,
                     title = NULL,
                     subtitle = NULL,
                     xlabel = 'proj. 1',
                     ylabel = 'proj. 2',
                     xlimits = c(-0.1, 1.1),
                     ylimits = c(-0.1, 1.1),
                     void = FALSE){
  # -------------
  # Check inputs
  #--------------
  if (missing(layout)){
    stop('Layout is missing')
  }
  if (!is.data.frame(layout)){
    stop('Layout is not data frame')
  }
  if (!is.character(rownames(layout))){
    stop('Input has missing or improper rownames')
  }
  if (any(is.na(layout))){
    warning("Layout contains missing values")
  }

  # ----------------------------
  # Set default color and alpha
  # ----------------------------
  # Set defaults
  update_geom_defaults(ggforce::GeomCircle,
                       list(colours = default.color,
                            fill = default.color,
                            alpha = default.alpha))

  # Create aes mapping for each individual interactive circles
  # TODO: allow user to change tooltip thru function arg
  geom_map <- aes_(x0=quote(x),y0=quote(y),r=quote(radius),
                   tooltip=quote(paste0("Pathway: ", pnames, "\n",
                                        "Significance: ", significance, "\n",
                                        "Cluster: ", cluster, "\n",
                                        "p-value: ", pval, "\n",
                                        "FDR value: ", fdr, "\n",
                                        "Source: ", src, "\n")),
                   data_id=quote(cluster))

  # Apply geom_circs to data with previously created aes mapping
  layout2 <- sapply(1:nrow(layout), function(r) geom_circs(layout[r, ],geom_map))

  # --------------------
  # Initialize the plot
  # --------------------
  p <- ggplot()

  # Add individual circles to plot and set fixed coords
  p <- p + layout2 + coord_fixed()

  # Set theme
  if (void){
    p <- p + theme_void(base_size = base.fontsize)
  } else {
    p <- p + theme_light(base_size = base.fontsize)
  }
  # Set x and y labels
  p <- p + labs(title = title,
                subtitle = subtitle,
                x = xlabel,
                y = ylabel)
  # --------------
  # Resolve sizer
  # --------------
  # Set sizer
  if(show.size.guide){
    size.labeller = function(r){
      s = max(layout[,'size']) / max(layout[,'radius'])^2
      l = as.character(round(r^2 * s))
      return(l)
    }
    p <- p + gsoap:::geom_radius_legend(layout$radius,
                                        size.guide.loc[1],
                                        size.guide.loc[2],
                                        no = size.guide.no,
                                        font.size = size.guide.fontsize,
                                        labeller = size.labeller)
  }
  # --------------
  # Resolve color
  # --------------
  # Set color if color is used
  if(!is.null(as.color)){
    if (is.numeric(layout[,as.color])){
      p <- p + aes_string(color = as.color, fill = as.color)
      p <- p + ggiraph::scale_color_viridis_c_interactive(begin = viridis.range[1],
                                                          end = viridis.range[2],
                                                          direction = viridis.direction,
                                                          option = viridis.option)
      p <- p + ggiraph::scale_fill_viridis_c_interactive(begin = viridis.range[1],
                                                         end = viridis.range[2],
                                                         direction = viridis.direction,
                                                         option = viridis.option,
                                                         data_id = function(breaks) {
                                                           breaks
                                                         },
                                                         tooltip = function(breaks) {
                                                           breaks
                                                         })
    }
    if (is.factor(layout[,as.color]) | is.character(layout[,as.color])){
      p <- p + aes_string(color = as.color, fill = as.color)
      p <- p + ggiraph::scale_color_viridis_d_interactive(begin = viridis.range[1],
                                                          end = viridis.range[2],
                                                          direction = viridis.direction,
                                                          option = viridis.option)
      p <- p + ggiraph::scale_fill_viridis_d_interactive(begin = viridis.range[1],
                                                         end = viridis.range[2],
                                                         direction = viridis.direction,
                                                         option = viridis.option,
                                                         tooltip = function(breaks) {breaks})
    }
  }
  # --------------
  # Resolve alpha
  # --------------
  # Set alpha if alpha is used
  if(!is.null(as.alpha)){
    p <-  p + aes_string(alpha = as.alpha) # !!!!
    p <-  p + scale_alpha(range = range.alpha,
                          guide = ggiraph::guide_legend_interactive(
                            override.aes = list(fill=default.color,
                                                tooltip="significance")))
  }
  # ---------------
  # Resolve guides
  # ---------------
  if (!show.color.guide){
    p <- p + guides(color = FALSE, fill = FALSE)
  }
  if (!show.alpha.guide){
    p <- p + guides(alpha = FALSE)
  }
  # ---------------
  # Add annotation
  # ---------------
  if(!is.null(which.labels)){
    if(!all.equal(which.labels, as.integer(which.labels))){
      layout.subset <- layout[which(layout$pnames %in% which.labels),]
    } else {
      layout.subset <- layout[which.labels,]
    }

    p <- p + ggiraph::geom_text_repel_interactive(data = layout.subset,
                                                  nudge_x = repel.xynudges[1],
                                                  nudge_y = repel.xynudges[2],
                                                  hjust = repel.hvjust[1],
                                                  vjust = repel.hvjust[2],
                                                  force = 10,
                                                  direction = repel.direction,
                                                  segment.alpha = segment.alpha,
                                                  alpha = label.alpha,
                                                  color = 'black',
                                                  size = label.fontsize * 0.35,
                                                  max.iter = 1e+4,
                                                  aes(x = x,
                                                      y = y,
                                                      label = pnames))
  }
  # -------------
  # Resolve rest
  # -------------

  # Unconventional hack to force show legend, normally it is shown by default,
  # however by fixing the draw ordering previously which specifies legend to be
  # disabled to avoid multiple drawing geometries of the legend to stack on top
  # of each other for n rows of data.

  p$layers[[1]]$show.legend <- TRUE
  # Return the plot
  return(p)
}
