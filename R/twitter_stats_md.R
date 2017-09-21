#' twitter_stats_md
#'
#' Generates via rmarkdown an HTML report of Twitter stats
#'
#' @export
twitter_stats_md <- function(dir = ".") {
  x <- capture.output(twitter_stats(), type = "message")
  x <- c(
    paste0("# Twitter stats"),
    "&nbsp;\n",
    "## Friends' stats",
    "```",
    x[19:21],
    "```\n",
    "&nbsp;\n",
    paste0("## @", trickrtweet:::home_user(), "'s data"),
    "```",
    x[26:28],
    "```\n"
  )
  file_name <- paste0("tw_stat_", format(Sys.time(), "%Y-%m-%d-%H%p"), ".html")
  file_name <- file.path(dir, file_name)
  writeLines(x, file_name)
  prettysimplemd::renderPSM(file_name)
  x
}
