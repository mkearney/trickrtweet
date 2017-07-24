

devtools::load_all()
devtools::document()

x <- unfollowback()

x


help(package = "devtools")

if (!"rtweet" %in% loadedNamespaces()) {
  library(rtweet)
}
if (!"dplyr" %in% loadedNamespaces()) {
  suppressPackageStartupMessages(library(dplyr))
}

## my post token
#token <- create_token(
#  "post_tw_app",
#  "vLafjz2yH77mY5r2AGpyfzqwx",
#  "v2uaqsSrdyefJu3rkCZ8BjuzHzXASyORsQCtKWEzcX5SzuraEq"
#)

token <- readRDS("~/rtw.rds")

##------------------------------------------------------------------##
##                       rm ability to retweet                      ##
##------------------------------------------------------------------##

no_retweets <- function(n = 1000L) {
  f <- function(x) {
    for (i in seq_along(x)) {
      r <- post_follow(x[i], retweets = FALSE, token = token)
      httr::warn_for_status(r)
    }
  }
  f(fds[seq_len(n)])
}

##------------------------------------------------------------------##
##                       rm inactive friends                        ##
##------------------------------------------------------------------##

## get user data for friends
if (!"fdsdf" %in% ls()) {
  fdsdf <- lookup_users(fds)
}
## merge most recent column with users data
names(attributes(fdsdf))
tweets_data(fdsdf)

fdsdf$most_recent <- tweets_data(fdsdf)[["created_at"]]

str(tweets_data(fdsdf), 1)

## inspect
inacts <- fdsdf %>%
    dplyr::filter(!screen_name %in% c("KUCommStudies", "wesrumbaugh",
                               "MaryBanwart", "Ryan_Resch",
                               "jcregnier", "AEJMC_PCIG",
                               "ashleymuddiman", "MIZterReed",
                               "astridvillamil", "brycejdietrich",
                               "emilybosch", "claymwebb",
                               "BKite22", "KellyWinfrey",
                               "ermo", "BetweenSt"),
                  most_recent < as.POSIXct("2017-03-15 00:00:00"))

inacts %>%
  dplyr::arrange(most_recent) %>%
  dplyr::select(screen_name, most_recent) %>%
  print(n = 100)

    unlist(use.names = FALSE)

## print screen names
inacts

## unfollow
out <- lapply(inacts,
              post_unfollow_user,
              token = token)
httpstatus(out)

## subset only reasonable accounts
usrs <- fdsdf %>%
    subset(friends_count > 100 &
           friends_count < 2000 &
           statuses_count > 300 &
           followers_count > 100 &
           followers_count < 1200 &
           friends_count / followers_count < 3 &
           friends_count / followers_count > .8)
## rm accounts I already follow
usrs <- usrs[!usrs %in% fds]


##------------------------------------------------------------------##
##                add new friends (shared networks)                 ##
##------------------------------------------------------------------##

## lookup followers data
flwdat <- lookup_users(flw)

flwdat %>%
  filter(grepl("phd|ph\\.d|prof", description)) %>%
  select(screen_name, followers_count, friends_count) %>%
  arrange(-followers_count) %>%
  ##.[100:150, ] %>%
  print(n = 50)

ffusrs <- c("EXAGolo", "ArkangelScrap", "simongerman600", "mloughlin", "techpearce2", "MSMMatBU")
ffusrsdat <- lapply(ffusrs, get_followers, n = 5000, token = token)
ffusrsdat <- do.call(rbind, ffusrsdat)
ffusrs2flw <- ffusrsdat$user_id
ffusrs2flw <- sample(ffusrs2flw, length(ffusrs2flw))

## follow sleep timer
follow_ <- function(x, token, n = 1) {
    x <- post_follow(x, token = token)
    Sys.sleep(n)
    invisible(x)
}

## look for follow bots
#toadd <- search_users("auto follow", n = 1000)
#toadd <- data.frame(toadd, stringsAsFactors = FALSE)
#toadd[, c('screen_name', 'followers_count', 'friends_count')]
#toadd$description
#toadd2 <- search_users("auto follow", n = 1000)
#toadd$screen_name[23]
toflw <- readRDS("~/r/.grow_twitter_ids2follow.rds")
toflw2 <- readRDS("~/r/.grow_twitter_ids3follow.rds")
## pick friends who have similar interests
#piks <- list("dataandme",
#             "SeattleDataGuy",
#             "jclewis",
#             "DaveRubal")
## get their followers
#potentials <- lapply(piks, get_followers)
#pots <- potentials %>%
#  lapply("[[", "user_id") %>%
#  lapply("[", 1:250) %>%
#  unlist() %>%
#  unique()

## save these for next time
#toflw <- potentials %>%
#  lapply("[[", "user_id") %>%
#  lapply("[", 251:2050) %>%
#  unlist() %>%
#  unique()
#toflw <- toflw[!toflw %in% fds]
#saveRDS(toflw, "~/r/.grow_twitter_ids2follow.rds")

## get user data for their followers
##potdf <- lookup_users(pots)
## subset only reasonable accounts
##usrs <- potdf %>%
#    subset(friends_count > 100 &
#           friends_count < 2000 &
#           statuses_count > 300 &
#           followers_count > 100 &
#           followers_count < 1200 &
#           friends_count / followers_count < 3 &
#           friends_count / followers_count > .8)
## rm accounts I already follow
#usrs <- usrs[!usrs %in% fds]
## follow users (with sleep timer)
out <- lapply(toflw[2001:3000], follow_, token = token, n = 2)
out <- lapply(toflw2[1:1000], follow_, token = token, n = 2)

##------------------------------------------------------------------##
##               add new friends (search interests)                 ##
##------------------------------------------------------------------##

## search and follow new tweeps
q <- list("data", "phd", "quantitative", "statistics")
usrs <- lapply(q, search_users, n = 500)
usrs <- do.call("rbind", usrs)
usrs <- unique(usrs)
## subset only reasonable accounts
usrs <- usrs %>%
    filter(friends_count > 100,
           friends_count < 1200,
           statuses_count > 300,
           followers_count > 100,
           followers_count < 1200,
           friends_count / followers_count < 1.2,
           friends_count / followers_count > .8,
           select = user_id) %>%
    select(user_id)

##------------------------------------------------------------------##
##                         rm favorites                             ##
##------------------------------------------------------------------##

## get favorites
favs <- get_favorites("kearneymw", n = 5000)
## oldest 1000 favs (status id)
favs <- favs[(nrow(favs) - 1000):nrow(favs),
             "status_id"]
## rm favorites
sh <- lapply(favs,
             post_favorite,
             destroy = TRUE,
             token = token)

##------------------------------------------------------------------##
##                   fav tweets of nonfollowers                     ##
##------------------------------------------------------------------##

## favorite tweets of non-followers
home <- get_timeline("kearneymw", home = TRUE,
                     n = 3000, token = token)
tofav <- which(!home$user_id %in% flw)
tofav <- home$status_id[tofav]
favd <- tofav %>%
    lapply(favorite_tweet, token = token)
