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

#'  Get the path of configuration file.
#'
#'  The configuration file could be in the `inst` directory, or in a temporary
#'  directory, according to the `create_directory` option of the `create_config()`
#'  function. If the configuration file doesn't exist, this function create a file
#'  in a temporary directory and send a message to the user.
#'
#' @return Character: path of the configuration file, needed for a lot of functions.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' create_config(create_directory=FALSE)
#' get_config_path()
get_config_path <- function(){
  project_folder = "inst/"
  tmp_folder = paste(tempdir(), "/", sep = "")
  config_name = "config.yml"
  if(dir.exists(project_folder)){
    project_file = paste(project_folder, config_name, sep = "")
     if(file.exists(project_file)) {
       return(project_file)
     }
  }
  if(dir.exists(tmp_folder)){
    tmp_file = paste(tmp_folder, config_name, sep = "")
    if(!file.exists(tmp_file)) {
      create_config()
      message("Creation of a default config file in the temp directory, please use create_config() to overwrite.")
      tmp_file = paste(tmp_folder, config_name, sep = "")
    }
    return(tmp_file)
  }
}
