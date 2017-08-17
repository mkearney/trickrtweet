#' regen_token
#'
#' Regenerates oauth token.
#'
#' @param browse Logical indicating whether or not to open the web
#'   browser to apps.twitter.com/app/
#' @return Token (invisible).
#' @export
#' @importFrom rtweet get_tokens create_token
regen_token <- function(browse = TRUE) {
  message("1. Select your app in browser")
  message("2. Click \"Keys and Access Tokens\"")
  message("3. Click \"Regenerate Consumer key and Secret\"")
  if (browse) {
    browseURL("https://apps.twitter.com/app/")
  }
  message("\nLet me know when you're done...")
  r <- menu(
    c("Yes, I've regenerated my token.",
      "No, I don't want to do this right now.")
  )
  if (r == 2) return(invisible())
  key <- readline("What is your new key?")
  key <- gsub('"', "", key)
  secret <- readline("What is your new secret?")
  secret <- gsub('"', "", secret)
  token <- rtweet::get_tokens()
  twitter_tokens <- rtweet::create_token(
    token$app$appname,
    key,
    secret
  )
  rm("twitter_tokens", envir = rtweet:::.state)
  assign("twitter_tokens", twitter_tokens, envir = rtweet:::.state)
  saveRDS(twitter_tokens, file = Sys.getenv("TWITTER_PAT"))
  readRenviron("~/.Renviron")
  invisible(twitter_tokens)
}