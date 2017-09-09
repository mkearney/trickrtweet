##----------------------------------------------------------------------------##
#' @title trickrtweet
#'
#' @description trickrtweet is package used for growing your Twitter network.
#'
#' @docType package
#' @name trickrtweet
#' @import rtweet
NULL

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("Ready to build your Twitter network!")
    message("To view your Twitter stats, use the \"twitter_stats()\" function.")
  }
}

#' twitter statistics
#' 
#' Displays Twitter statistics of home user and user network and returns 
#'   friends and followers data.
#' 
#' @return Invisibly returns a list of friends, friends data, followers, 
#'   and followers data
#' @export
twitter_stats <- function() {
  eval(call("twitter_stats.default"))
}

twitter_stats.default <- function() {
  message("##----------------------------")
  message("## LOADING DATA")
  message("##----------------------------\n")
  user <- home_user()
  message(paste0("Screen name of home user is @", user))
  x <- TRUE
  validate_token()
  message("\nTwitter token validated.\n")
  message("Compiling user data...\n")
  x <- tryCatch(home_user_data(), error = function(e) NULL)
  if (is.null(x)) {
    stop("Unable to compile user data.", call. = FALSE)
  } 
  ## friends data
  fds <- get(".fds", envir = .trickrtweet)
  message("Friends identified.")
  fds_data <- get(".fds_data", envir = .trickrtweet)
  message("Friends data gathered.")

  ## followers data
  flw <- get(".flw", envir = .trickrtweet)
  message("Followers identified.")
  flw_data <- get(".flw_data", envir = .trickrtweet)
  message("Followers data gathered.")

  ## calculate metrics
  fds_fds <- round(median(fds_data$friends_count, na.rm = TRUE), 0)
  fds_flw <- round(median(fds_data$followers_count, na.rm = TRUE), 0)
  fds_ratio <- round(median(fds_data$followers_count / 
      fds_data$friends_count, na.rm = TRUE), 2)

  message("\n##----------------------------")
  message(paste0("## @", user, "'s friends:"))
  message("##----------------------------")
  ## submit messages
  message(paste("Median number of friends' friends:", fds_fds))
  message(paste("Median number of friends' followers:", fds_flw))
  message(paste("Median follower-to-friend ratio:", fds_ratio))
  message("\n##----------------------------")
  message(paste0("## @", user, ":"))
  message("##----------------------------")

  message(paste0("@", user, " number of friends: ", length(fds)))
  message(paste0("@", user, " number of followers: ", length(flw)))
  message(paste0("@", user, " follower-to-friend ratio: ",
                round(length(flw) / length(fds), 2)))
  invisible(list(
    fds = fds, 
    fds_data = fds_data, 
    flw = flw, 
    flw_data = flw_data)
  )
}

.trickrtweet <- new.env(parent = emptyenv())

home_user <- function() {
  eval(call("home_user.default"))
}

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
  if (interactive()) {
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
  } else {
    stop("no screen_name for home user", call. = FALSE)
  }
}

#' @importFrom rtweet get_tokens
validate_token <- function() {
  token <- tryCatch(rtweet::get_tokens(), error = function(e) NULL)
  if (is.null(token) || !inherits(token, "Token")) {
    stop(paste0(
      "Could not find token. Please save path to token associated with @",
      home_user(), "'s account as the \"TWITTER_PAT\" environment variable."), 
      call. = FALSE)
  }
  token_user <- token[["credentials"]][["screen_name"]]
  if (!identical(home_user(), token_user)) {
    stop(paste0(
      "Invalid token. This token belongs to @", 
      token_user, " and not @", home_user(), ".\n",
      "Please save path to token associated with @", home_user(),
      "'s account as the \"TWITTER_PAT\" environment variable."), 
      call. = FALSE)
  }
  TRUE
}

get_user_data_obj <- function(data) {
  lgl <- exists(data, envir = .trickrtweet)
  if (lgl) {
    data <- get(data, envir = .trickrtweet)
    return(data)
  } else {
    NULL
  }
}

load_fds_data <- function(home = NULL) {
  if (is.null(home)) {
    home <- system.file(package = "trickrtweet")
  }
  path <- file.path(home, "data/friends.rds")
  if (file.exists(path)) {
    fds_data <- readRDS(path)
    assign(".fds_data", fds_data, envir = .trickrtweet)
    fds_data
  } else {
    NULL
  }
}

load_flw_data <- function(home = NULL) {
  if (is.null(home)) {
    home <- system.file(package = "trickrtweet")
  }
  path <- file.path(home, "data/followers.rds")
  if (file.exists(path)) {
    flw_data <- readRDS(path)
    assign(".flw_data", flw_data, envir = .trickrtweet)
    flw_data
  } else {
    NULL
  }
}

home_user_data <- function(home = NULL) {
  if (is.null(home)) {
    home <- system.file(package = "trickrtweet")
  }
  ##----------------------
  ## user account data
  ##----------------------
  user <- home_user()
  user_data <- rtweet::lookup_users(user)
  assign(".user_data", user_data, envir = .trickrtweet)

  ##----------------------
  ## friends
  ##----------------------
  fds <- as.character(get_user_data_obj(".fds"))
  ## if null get them
  if (is.null(fds) || length(fds) == 0L) {
    fds <- as.character(rtweet::get_friends(user)[["user_id"]])
    ## store in .trickrtweet
    assign(".fds", fds, envir = .trickrtweet)
  }
  fds_data <- get_user_data_obj(".fds_data")
  ## if null then load
  if (is.null(fds_data) || length(fds) == 0L) {
    fds_data <- load_fds_data()
    ## if still null then lookup
    if (is.null(fds_data) || length(fds) == 0L) {
      fds_data <- rtweet::lookup_users(fds)
    }
  }
  ## if it needs updating, then update
  if (any(!fds %in% fds_data$user_id, na.rm = TRUE)) {
    x <- rtweet::lookup_users(fds[!fds %in% fds_data$user_id])
    fds_data <- dplyr::bind_rows(fds_data, x)
  }
  ## store in .trickrtweet
  assign(".fds_data", fds_data, envir = .trickrtweet)
  ## save data
  path <- file.path(home, "data/friends.rds")
  saveRDS(fds_data, path)

  ##----------------------
  ## get followers
  ##----------------------
  flw <- as.character(get_user_data_obj(".flw"))
  ## if null lookup and store in .trickrtweet
  if (is.null(flw) || length(flw) == 0L) {
    flw <- as.character(rtweet::get_followers(user)[["user_id"]])
    assign(".flw", flw, envir = .trickrtweet)
  }
  ## get followers data
  flw_data <- get_user_data_obj(".flw_data")
  ## if null then load
  if (is.null(flw_data) || length(flw) == 0L) {
    flw_data <- load_flw_data()
    ## if still null then lookup
    if (is.null(flw_data) || length(flw) == 0L) {
      flw_data <- rtweet::lookup_users(flw)
    }
  }
  ## update if necessary
  if (any(!flw %in% flw_data$user_id, na.rm = TRUE)) {
    x <- rtweet::lookup_users(flw[!flw %in% flw_data$user_id])
    flw_data <- dplyr::bind_rows(flw_data, x)
  }
  ## store in environment and save to data file
  assign(".flw_data", flw_data, envir = .trickrtweet)
  path <- file.path(home, "data/followers.rds")
  saveRDS(fds_data, path)
  return(invisible(TRUE))
}
