#'  Create config file if needed.
#'
#'  If you want to specify the IDs of your own sensors,
#'  this function create a local configuration template file
#'  in the `\inst` directory, to edit with specific information.
#'  By default, the function doesn't create the file in the project directory
#'  but in a temp directory. If you want to have a permanent configuration,
#'  please use `r create_directory = TRUE`.
#'
#'  If you use the temporary options, please fill directly the name and number
#'  of your sensors in the "segments" argument.
#'
#' @param segments Named List of segments ("name1" = "9000000000", ...). Default to the example version.
#' @param create_directory Boolean: Does the file need to be created in the project directory? Default to FALSE.
#' @param overwrite Boolean: if the file exist, should it be overwriten? Default to FALSE.
#'
#' @return Boolean: TRUE if the file is created, FALSE overwise (config already exists for example).
#'
#' @export
#'
#' @importFrom yaml write_yaml
#'
#' @examples
#' create_config(create_directory=FALSE)
#' list_of_segments = list("Burel"= "9000002156", "Vitre" = "9000001844")
#' create_config(segments = list_of_segments,
#'   create_directory = FALSE,
#'   overwrite = TRUE) # the file already exists
create_config <- function(segments = list("segment-01"="9000000000",
                                          "segment-02"="9000000000"),
                          create_directory=FALSE,
                          overwrite=FALSE){
  # parameters
  project_folder = "inst/"
  config_name = "config.yml"
  template <- list("default"=list("url"="https://telraam-api.net/v1",
                                  "segments"=segments))

  if(!create_directory){ # if you don't want to create a directory in the project
    temp_folder = tempdir() # temp directory
    file_path = paste(temp_folder, config_name, sep = "/")
  }
  else{
    if(!dir.exists(project_folder)){
      dir.create(project_folder)
    }
    file_path = paste(project_folder, config_name, sep = "")
  }
  if(!file.exists(file_path) | overwrite){
    file.create(file_path)
    yaml::write_yaml(template, file_path)
    result = TRUE
  }
  else {
    warning("A configuration file already exists in the directory")
    result = FALSE
  }
  return(result)
}
