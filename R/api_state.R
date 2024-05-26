#'  Check API state
#'
#'  Return the state of the 'Telraam' API. Determine if updates can be made.
#'
#' @param key the API key (set by the set_telraam_token function - default -, or directly filled).
#'
#' @return Boolean: TRUE if the API responds well, FALSE otherwise.
#' @export
#'
#' @importFrom httr VERB
#'
#' @examples
#' my_token <- 'ThisIsNotAValidToken'
#' get_api_state(my_token)
get_api_state <- function(key = get_telraam_token()){
  file_path = get_config_path()
  if(!file.exists(file_path)){
    create_config(create_directory = FALSE)
  }
  key <- c(
    'X-Api-Key' = key
  )
  VERB("GET", url = config::get(file = file_path)$url,
       add_headers(key))$status_code == 200  # the request suceeded if equal to 200
}


#' Saves an authentication token for the 'Telraam' API.
#'
#' If you want to get this token after this instruction, please use \code{get_telraam_token()}.
#'
#' @param token a \code{string} with the token
#'
#' @return Boolean: TRUE if the token is correctly set
#' @export
#'
#' @examples
#' my_token <- "MyTelraamToken"
#' set_telraam_token(my_token)
#' get_telraam_token()
#'
set_telraam_token = function(token) {
  if (is.null(token)) {
    stop("No token provided")
  }
  return(Sys.setenv(key = token))
}

#' Get the current authentication token for the 'Telraam' API
#'
#' @return Token currently used, set by \code{set_telraam_token()}
#' @export
#'
#' @examples
#' my_token <- "MyTelraamToken"
#' set_telraam_token(my_token)
#' get_telraam_token()
#'
get_telraam_token=function(){
  PAT=Sys.getenv('key')
  if(PAT==""){
    TOKEN = Sys.getenv('token')
    if(TOKEN==""){
      stop("Telraam token has not been set. Use set_Telraam_Token")
    }
    return(TOKEN)
  }
  return(PAT)
}

