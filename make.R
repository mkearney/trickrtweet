## trickrtweet make file

## load/install package
tfse::make_package()

## view twitter stats
twitter_stats()

## load rtweet and tidyverse
library(rtweet)
suppressPackageStartupMessages(library(tidyverse))

## get friends and followers
fds <- get(".fds", envir = trickrtweet:::.trickrtweet)
flw <- get(".flw", envir = trickrtweet:::.trickrtweet)

## friends and followers data
fds_data <- get(".fds_data", envir = trickrtweet:::.trickrtweet)
flw_data <- get(".flw_data", envir = trickrtweet:::.trickrtweet)

## get keepers
kprs <- readRDS(Sys.getenv("TWITTER_KEEPERS"))

## unfollow users function
unfollow_users <- function(x) {
  j <- 0L
  sh <- vector("list", length(x))
  for (i in seq_along(x)) {
    sh[[i]] <- trickrtweet:::unfollow_user(x[i])
  }
}

## tweets i've favorited
favs <- get_favorites("kearneymw", n = 3000)

## tweets i've posted
twts <- get_timeline("kearneymw", n = 3000)

## select users I've interacted with
interacted_with <- unique(c(
  unlist(favs$user_id),
  unlist(twts$mentions_user_id)
))

## filter accounts that aren't in keepers, interacted with, or
## among current followers
uf <- fds_data %>%
  dplyr::filter(
    user_id %in% fds,
    !user_id %in% kprs,
    !user_id %in% interacted_with,
    !user_id %in% flw)

## number of users to unfollow
length(uf$user_id)

## unfollow those users
sh <- unfollow_users(uf$user_id)
