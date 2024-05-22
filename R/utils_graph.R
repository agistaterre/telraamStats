#' Create subtitles for graphics.
#'
#' @param segments Character vector. Selected road segment to text, no precision if NULL (default).
#' @param modes Character vector. Different modes of transportation selected (heavy, car, bike, pedestrian). Default: NULL
#' @param directions Character vector. Directions of the traffic (lft, rgt, both) choosen. Default to NULL.
#' @param weekdays Character vector. Weekdays choosen. Default to NULL.
#' @param hours Numeric vector. Hours choosen. Default to NULL.
#'
#' @return Character, with a description of all parameters filled, usable as subtitle for graphs.
#' @export
#'
#' @keywords internal
#'
#' @examples
#' get_graph_subtitles(segments = c("Route1"),
#'   mode = "car", direction = "lft", weekdays = "monday", hours = 12:14)
#'
get_graph_subtitles <- function(segments = NULL,
                                modes = NULL,
                                directions = NULL,
                                weekdays = NULL,
                                hours = NULL
){
  subtitle_list = c()
  if(!is.null(modes)){
    subtitle_list <- c(subtitle_list,
                       paste("Modes:",paste(modes, collapse = ", ")))
  }
  if(!is.null(modes)){
    subtitle_list <- c(subtitle_list,
                       paste("Directions:",paste(directions, collapse = ", ")))
  }
  if(!is.null(weekdays)){
    subtitle_list <- c(subtitle_list,
                       paste("Weekdays:",paste(weekdays, collapse = ", ")))
  }
  if(!is.null(segments)){
    subtitle_list <- c(subtitle_list,
                       paste("Segments:",paste(segments, collapse = ", ")))
  }
  if(!is.null(hours)){
    subtitle_list <- c(subtitle_list,
                       paste("Hours:",paste(as.character(hours), collapse = ", ")))
  }

  return(paste(subtitle_list, collapse = "\n"))
}

#' Check if options are available in the options list, replace by a default otherwise.
#'
#' @param options_selected List of characters. Selected options.
#' @param options_available List of characters. Valid options.
#' @param default List of characters. Default options.
#'
#' @return Options consistent with the possibilities
#' @export
#'
#' @keywords internal
#'
#' @examples
#' check_options_graph(c('car','pedestrian'),
#'   c('car','pedestrian','bike','heavy'),c('car','heavy'))
#' check_options_graph(c('coucou','salut'),
#'   c('car','pedestrian','bike','heavy'),c('car','heavy'))
#' check_options_graph(NULL,
#'   c('car','pedestrian','bike','heavy'),c('car','heavy'))
#'
check_options_graph <- function(options_selected, options_available, default){
  unknown = setdiff(options_selected, options_available)
  if(is.null(options_selected)){
    options = default
  } else {
    if(length(unknown) > 0){
      if(length(unknown) == length(options_selected)){
        options = default # replace by default values
      } else {
        options = intersect(options_selected, options_available) # keep only available options
      }
    } else {
      options = options_selected
    }
  }
  return(options)
}

#' Colors palettes for each option (mode, direction, segment_name, weekday, hour)
#'
#' @param segments Character vectors. Segments name of the dataframe.
#'
#' @return list of color palettes (named vector) for each option (mode, direction, segment_name, weekday, hour)
#' @export
#'
#' @import paletteer
#'
#' @keywords internal
#'
#' @examples
#' segments <- c('Route1', 'Route2')
#' get_custom_palette(segments)
#'
get_custom_palette <- function(segments){

  colors_mode = c(
    "car" = '#F95335',
    "pedestrian" = '#FCAF38',
    "bike" = '#50A3A4',
    "heavy" = '#674A40')

  colors_direction = c(
    "both" = '#6d676e',
    "lft" = '#ff595e',
    "rgt" = '#8ac926'
  )

  colors_segment = paletteer_d("khroma::roma",
                               length(segments), type = 'continuous')
  names(colors_segment) = names(segments)

  colors_weekday = paletteer_d("rcartocolor::Temps")
  names(colors_weekday) = c('monday','tuesday','wednesday','thursday','friday',
                            'saturday','sunday')

  colors_hours = c(paletteer_d("khroma::iridescent"),'grey')
  names(colors_hours) = seq(1,23)

  colors = list("mode" = colors_mode,
                "direction" = colors_direction,
                "segment_name" = colors_segment,
                "weekday" = colors_weekday,
                "hour" = colors_hours)
  return(colors)
}

