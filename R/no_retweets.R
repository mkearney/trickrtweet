

#' no_retweets
#'
#' Mutes retweets from most recent n users
#' 
#' @param n Number of most recent users to mute retweets.
#' @return API response object.
#' @export
#' @importFrom rtweet post_follow
#' @importFrom httr warn_for_status
no_retweets <- function(n = 1000L) {
  UseMethod("no_retweets")
}

#' no_retweets
#'
#' Mutes retweets from most recent n users
#' 
#' @param n Number of most recent users to mute retweets.
#' @return API response object.
#' @export
#' @importFrom rtweet post_follow
#' @importFrom httr warn_for_status
no_retweets.default <- function(n = 1000L) {
  f <- function(x) {
    for (i in seq_along(x)) {
      r <- rtweet::post_follow(x[i], retweets = FALSE)
      httr::warn_for_status(r)
    }
  }
  f(fds[seq_len(n)])
}
