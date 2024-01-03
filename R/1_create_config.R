#'  Create config file if needed.
#'
#'  If the user needs to specify the IDs of his own sensors,
#'  this function create a local configuration template file
#'  in the 'inst' directory, to edit with specific informations.
#'
#' @export
#'
#' @importFrom yaml write_yaml
#'
#'
create_config <- function(){
  file_path = "inst/config.yml"
  template <- list("default"=list("url"="https://telraam-api.net/v1"),
                   "segments"=list("segment-01"="0000000000","segment-02"="0000000000"))
  if(!file.exists(file_path)){
    if(!dir.exists("inst/")){
      dir.create("inst/")
    }
    file.create(file_path)
    yaml::write_yaml(template, file_path)
  }
  else {
    warning("A configuration file already exists in the 'inst' directory")
  }
}
