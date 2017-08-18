
#' follow_
#'
#' Follows users with random sleep timer between calls.
#'
#' @param users Screen names or user IDs of users to follow.
#' @return API response objects (invisibly)
#' @importFrom rtweet post_follow
#' @export
follow_ <- function(users) {
  UseMethod("follow_")
}

#' follow_.default
#'
#' Follows users with random sleep timer between calls.
#'
#' @param users Screen names or user IDs of users to follow.
#' @return API response objects (invisibly)
#' @importFrom rtweet post_follow
#' @export
follow_.default <- function(users) {
  f <- function(x) {
    Sys.sleep(runif(1, 1, 3))
    x <- rtweet::post_follow(x)
    invisible(x)
  }
  sh <- lapply(users, f)
  invisible(sh)
}


follow_friends_followers <- function(users, ...) {
  UseMethod("follow_friends_followers")
}

follow_friends_followers <- function(users) {
  users
}


