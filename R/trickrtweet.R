#' @title trickrtweet
#'
#' @description trickrtweet is package used for growing your Twitter network.
#'
#' @docType package
#' @name trickrtweet
#' @import rtweet
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Ready to build your Twitter network!")
  home_user()
  validate_token()
}

.trickrtweet <- new.env()

.state <- new.env(parent = emptyenv())

home_user <- function() UseMethod("home_user")

home_user.default <- function() {
  if (!exists(".trickrtweet")) {
    .trickrtweet <<- new.env()
  }
  if (exists(".user", envir = .trickrtweet)) {
    return(get(".user", envir = .trickrtweet))
  }
  user <- Sys.getenv("TWITTER_SCREEN_NAME")
  if (!identical(user, "")) {
    assign(".user", user, envir = .trickrtweet)
    return(user)
  }
  ## ask user for screen name
  user <- readline("What is your screen name on Twitter?")
  ## remove at sign
  user <- gsub("@", "", user)
  ## save as environment variable
  message("Saving your Twitter screen name as environment variable")
  cat(
    paste0("TWITTER_SCREEN_NAME=", user),
    fill = TRUE,
    file = file.path(normalizePath("~"), ".Renviron"),
    append = TRUE
  )
  ## store in pkg environment
  assign(".user", user, envir = .trickrtweet)
  ## return screen name
  invisible(user)
}

#' @importFrom rtweet get_tokens
validate_token <- function() {
  token <- rtweet::get_tokens()
  if (identical(home_user(), token$credentials$screen_name)) {
    return(invisible(TRUE))
  }
  message("Invalid token. Please save path to token associated with home user's account")
  message("as the \"TWITTER_PAT\" environment variable.")
  invisible(FALSE)
}
