#' Create subtitles for graphics.
#'
#' @param segments Character vector. Selected road segment, all if NULL (default).
#' @param modes Character vector. Different modes of transportation aggregated (heavy, car, bike, pedestrian) . Default: heavy & car
#' @param directions Character vector. Direction of the traffic (lft, rgt, both). Default to both.
#' @param weekdays Character vector. Weekday choosen. Default to the all week.
#'
#' @return DataFrame with one row per hour/segment/transportation mode/direction
#' @export
#'
#' @import dplyr
#' @import reshape2
#' @importFrom tidyr unnest
#'
#' @keywords internal
#'
graph_subtitles <- function(segments = NULL,
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
#'@param options_selected List of characters. Selected options.
#'@param options_available List of characters. Valid options.
#'@param default List of characters. Default options.
#'
#'@return Options consistent with the possibilities
#'@export
#'
#'@keywords internal
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

#' Colors palettes for each option
#'
#' @param segments List of characters. Segments name of the dataframe.
#'
#' @return list of color palettes (named vector) for each option
#' @export
#'
#' @import paletteer
#'
#' @keywords internal
#'
custom_colors_palette <- function(segments){

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

