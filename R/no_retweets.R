##----------------------------------------------------------------------------##

#' no_retweets
#'
#' Mutes retweets from most recent n users
#' 
#' @param n Number of most recent users to mute retweets.
#' @param refresh Logical indicating whether to refresh friends list.
#' @return API response object.
#' @export
#' @importFrom rtweet post_follow
#' @importFrom httr warn_for_status
no_retweets <- function(n = 1000L, refresh = TRUE) {
  do.call("no_retweets_", list(n, refresh))
}

#' no_retweets_
#'
#' Mutes retweets from most recent n users
#' 
#' @param n Number of most recent users to mute retweets.
#' @param refresh Logical indicating whether to refresh friends list.
#' @return API response object.
#' @importFrom rtweet post_follow
#' @importFrom httr warn_for_status
no_retweets_ <- function(n, refresh) {
  user <- home_user()
  if (refresh || !".fds" %in% ls(envir = .trickrtweet, all.names = TRUE)) {
    fds <- as.character(rtweet::get_friends(user)[["user_id"]])
    assign(".fds", fds, envir = .trickrtweet)
  } else {
    fds <- get(".fds", envir = .trickrtweet)
  }
  ## modify to follow without retweets
  f <- function(x) {
    for (i in seq_along(x)) {
      r <- rtweet::post_follow(x[i], retweets = FALSE)
      httr::warn_for_status(r)
    }
  }
  f(fds[seq_len(n)])
}
