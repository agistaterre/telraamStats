#'  Create config file if needed.
#'
#'  If the user needs to specify the IDs of his own sensors,
#'  this function create a local configuration template file
#'  in the `\inst` directory, to edit with specific information.
#'
#' @param overwrite If the file exist, should it be overwriten?
#'
#' @return Boolean, TRUE if the file is created, FALSE overwise (config already exists for example).
#'
#' @export
#'
#' @importFrom yaml write_yaml
#'
#' @examples
#' \dontrun{
#' create_config()
#' }
create_config <- function(overwrite=FALSE){
  file_path = "inst/config.yml"
  template <- list("default"=list("url"="https://telraam-api.net/v1",
                                  "segments"=list("segment-01"="9000000000",
                                                  "segment-02"="9000000000")))
  if(!file.exists(file_path) | overwrite){
    if(!dir.exists("inst/")){
      dir.create("inst/")
    }
    file.create(file_path)
    yaml::write_yaml(template, file_path)
    result = TRUE
  }
  else {
    warning("A configuration file already exists in the 'inst' directory")
    result = FALSE
  }
  return(result)
}
