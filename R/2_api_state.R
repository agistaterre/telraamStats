#'  Check API State
#'
#'  Return the state of he Telraam API. Determine if updates can be made.
#'
#' @param key the api key (set by the set_telraam_token function)
#'
#' @return TRUE if the API responds well, FALSE otherwise
#' @export
#'
#' @importFrom httr VERB
#'
#'
api_state <- function(key = get_telraam_token()){
  key <- c(
    'X-Api-Key' = key
  )
  VERB("GET", url = "https://telraam-api.net/v1", add_headers(key))$status_code == 200  # the request suceeded if equal to 200
}


#' Saves an Authentication Token for the telraam API
#'
#' @param token a \code{string} with the token
#'
#' @return TRUE if token correctly set
#' @export
#'
#' @examples
#'
#' mytoken = "ivRgw7ZAGFedfwIdASezecdnETZDsdETB4Bqv3pbs5X8JDNnt1pQtpxDmpR6as2k"
#' set_telraam_token(mytoken)
#'
set_telraam_token = function(token) {
  if (is.null(token)) {
    stop("No token provided")
  }
  return(Sys.setenv(key = token))
}

get_telraam_token=function(){
  PAT=Sys.getenv('key')
  if(PAT==""){
    stop("Telraam token has not been set. Use set_Telraam_Token")
  }
  return(PAT)
}

