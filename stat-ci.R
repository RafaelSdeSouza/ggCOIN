#' Compute credible intervals for data contours
#'
#' @param level The level at which to draw an posterior
#' @param segments The number of segments to be used in drawing the interval.
#' @inheritParams layer
#' @inheritParams geom_point
#' @export
#' @examples
#' ggplot(faithful, aes(waiting, eruptions)) +
#'   geom_point() +
#'   stat_ci()
#'
#' ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3)) +
#'   geom_point() +
#'   stat_ci()
#'
#' ggplot(faithful, aes(waiting, eruptions, fill = eruptions > 3)) +
#'   stat_ci(geom = "polygon")
stat_ci <- function(mapping = NULL, data = NULL,
                         geom = "path", position = "identity",
                         ...,
                         level = 0.95,
                         segments = 51,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatCi,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      level = level,
      segments = segments,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatCi <- ggproto("StatCi", Stat,
                       required_aes = c("x", "y"),
                       
                       compute_group = function(data, scales, level = 0.95,
                                                segments = 200, na.rm = FALSE) {
                         calculate_ci(data = data, vars = c("x", "y"), 
                                           type = type,
                                           level = level, segments = segments)
                       }
)

calculate_ci <- function(data, vars, type, level, segments){
  
  v <- gplots::ci2d(data[,vars],show="none",
                      ci.levels = level,nbins=segments) 
  ci <- v$contours[names(v$contours)==level][[1]]
  ci <- as.data.frame(ci) 
  colnames(ci) <- vars
 ci
}
