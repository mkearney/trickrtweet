
#' unfollowback
#'
#' Removes friends who don't follow back (with exceptions)
#'
#' @param keep Character vector of screen names or IDs to not unfollow.
#'   These are probably popular accounts that you don't expect reciprocity
#'   from. Default is NULL. If "../data/keepers.rds" exists, it will be
#'   read in and used regardless of whether this value is NULL.
#' @param ... Passed to \code{\link{unfollowback.default}}
#' @return List where each element corresponds to an API response object.
#'   Elements should be of status 200 if unfollow request was successful.
#' @importFrom rtweet get_friends get_followers lookup_users
#' @export
unfollowback <- function(keep = NULL, ...) {
  UseMethod("unfollowback")
}


#' unfollowback
#'
#' Removes friends who don't follow back (with exceptions)
#'
#' @param keep Character vector of screen names or IDs to not unfollow.
#'   These are probably popular accounts that you don't expect reciprocity
#'   from. Default is NULL. If there is a TWITTER_KEEPERS path, it will be
#'   read in and used regardless of whether this value is NULL.
#' @return List where each element corresponds to an API response object.
#'   Elements should be of status 200 if unfollow request was successful.
#' @importFrom rtweet get_friends get_followers lookup_users
#' @export
unfollowback.default <- function(keep = NULL) {
  user <- home_user()
  ## friends
  fds <- rtweet::get_friends(user)
  fds <- fds$user_id
  ## followers
  flw <- rtweet::get_followers(user)
  flw <- flw$user_id
  ## id people who don't follow back
  tounf <- fds[!fds %in% flw]
  if (length(tounf) == 0L) {
    message("No haters. Nothing but follow backs!")
    return(invisible())
  }
  tu_users <- rtweet::lookup_users(tounf)
  ## keepers
  keepers <- Sys.getenv("TWITTER_KEEPERS")
  if (file.exists(keepers) && !identical("", keepers)) {
    keepers <- readRDS(keepers)
    keep <- c(keepers, keep)
  }
  if (!is.null(keep)) {
    kprs_data <- rtweet::lookup_users(keep)
    tounf <- tu_users$user_id[!tu_users$user_id %in% kprs_data$user_id]
  }
  ## final vector to unfollow
  tounf <- fds[fds %in% tounf]
  ## if none then say so
  if (length(tounf) == 0L) {
    message("No one left to unfollow!")
    return(invisible())
  }
  ## unfollow
  out <- Map(
    unfollow_user, user = tounf
  )
  ## return invisible
  return(out)
}

#' @importFrom rtweet post_unfollow_user
unfollow_user <- function(..., sleeper = TRUE) {
  if (sleeper) {
    ## sleep an average of 2 seconds per call
    Sys.sleep(runif(1, .5, 3))
  }
  post_unfollow_user(...)
}
