# data/fetch_data.R
# ---------------------------------------------------------------------------
# Fetch daily-return time series from Yahoo Finance for the tickers used in the
# portfolio-optimisation analysis.
#
# IMPORTANT (2024+): the `quantmod::getSymbols` Yahoo path used by the original
# project no longer works. Yahoo removed the `/v7/finance/download` endpoint,
# so getSymbols returns "download failed". This script uses the still-working
# `v8/chart` API directly.
#
# Snapshots produced by this script are committed under data/cache/ so the
# analysis can run without network access. Re-run this script to refresh them.
#
# Usage:
#   Rscript data/fetch_data.R
#
# Notes on missing tickers in the current Yahoo API (refresh date noted in the
# generated README under data/cache/):
#   * JWN    — returns HTTP 404 on the v8 free API (data gap, not a delisting).
#   * DISCA  — delisted after the Warner Bros. Discovery merger (April 2022).
#   * SQ     — renamed to BLOCK (XYZ); use "XYZ" if needed.
# The analysis handles missing tickers gracefully.
# ---------------------------------------------------------------------------

suppressMessages({ library(httr); library(xts); library(jsonlite) })

CACHE_DIR <- "data/cache"
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# --- ticker groups used in the study ---------------------------------------

stocks_part1      <- c("QQQ", "VOO", "AAPL", "HRB", "AMZN", "ETHE", "GBTC", "JWN", "ZM", "NVDA")
stocks_part2_top10 <- c("ETHE", "GBTC", "ZM", "SNAP", "SQ", "SPCE", "TQQQ", "TSLA", "RIOT", "MRNA")
stocks_part2_universe <- c(
  stocks_part1,
  "AMD", "TSM", "PFE", "ARKK", "DIS", "DISCA", "SNAP", "PYPL", "GOOG", "SQ",
  "MELI", "SHOP", "SPCE", "IBM", "MSFT", "COKE", "SIG", "UPS", "VFF", "CRM",
  "AMC", "SQQQ", "TQQQ", "EA", "ISRG", "NKE", "MANU", "NFLX", "ROKU", "TSLA",
  "AXON", "MCD", "ORCL", "QCOM", "TEAM", "WMT", "DAL", "RIOT", "M", "MRNA"
)

# --- core fetcher ----------------------------------------------------------

fetch_one <- function(symbol, p1, p2, ua, attempt = 1) {
  url <- sprintf(
    "https://query1.finance.yahoo.com/v8/finance/chart/%s?period1=%d&period2=%d&interval=1d&events=div,split",
    symbol, p1, p2
  )
  r <- tryCatch(httr::GET(url, httr::user_agent(ua), httr::timeout(30)), error = function(e) NULL)
  if (is.null(r)) return(NULL)
  if (httr::status_code(r) != 200) {
    # transient rate-limiting: retry with backoff up to 3 attempts
    if (attempt < 4) {
      Sys.sleep(2 * attempt)
      return(fetch_one(symbol, p1, p2, ua, attempt + 1))
    }
    return(NULL)
  }
  txt <- httr::content(r, as = "text")
  payload <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  res <- payload$chart$result[[1]]
  if (is.null(res)) return(NULL)

  ts    <- as.POSIXct(as.numeric(res$timestamp), origin = "1970-01-01", tz = "UTC")
  close <- as.numeric(res$indicators$quote[[1]]$close)

  # apply stock splits retrospectively so the series is continuous
  splits <- res$events$`split`
  if (length(splits) > 0) {
    for (i in seq_along(splits)) {
      ev     <- splits[[i]]
      cutoff <- as.numeric(names(splits)[i])
      ratio  <- ev$numerator / ev$denominator
      close[res$timestamp < cutoff] <- close[res$timestamp < cutoff] * ratio
    }
  }

  x <- xts(close, order.by = ts)
  colnames(x) <- symbol
  x
}

fetch_yahoo_returns <- function(tickers, from = "2020-01-01", to = "2021-01-01") {
  p1 <- as.integer(as.POSIXct(from, tz = "UTC"))
  p2 <- as.integer(as.POSIXct(to, tz = "UTC"))
  ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
  cols <- list()
  for (s in tickers) {
    x <- fetch_one(s, p1, p2, ua)
    if (is.null(x)) {
      message(sprintf("  %s: FAIL", s))
    } else {
      cols[[s]] <- x
      message(sprintf("  %s: OK (%d rows)", s, nrow(x)))
    }
    Sys.sleep(0.4)
  }
  merged <- do.call(merge, cols)
  diff(merged, arithmetic = FALSE) - 1
}

# --- run all fetches -------------------------------------------------------

main <- function() {
  cat("Part 1 training set (2020-2021)...\n")
  r <- fetch_yahoo_returns(stocks_part1, "2020-01-01", "2021-01-01")
  saveRDS(r, file.path(CACHE_DIR, "returns_10_2020_2021.rds"))
  cat(sprintf("  -> %s (%d x %d)\n", "returns_10_2020_2021.rds", nrow(r), ncol(r)))

  cat("Part 1 validation set (2021-2022)...\n")
  r <- fetch_yahoo_returns(stocks_part1, "2021-01-01", "2022-01-01")
  saveRDS(r, file.path(CACHE_DIR, "returns_10_2021_2022.rds"))
  cat(sprintf("  -> %s (%d x %d)\n", "returns_10_2021_2022.rds", nrow(r), ncol(r)))

  cat("Part 2 universe (2020-2021)...\n")
  r <- fetch_yahoo_returns(unique(stocks_part2_universe), "2020-01-01", "2021-01-01")
  saveRDS(r, file.path(CACHE_DIR, "returns_50_2020_2021.rds"))
  cat(sprintf("  -> %s (%d x %d)\n", "returns_50_2020_2021.rds", nrow(r), ncol(r)))

  cat("\nDone. Snapshots in data/cache/. Commits refresh-date notes in README.\n")
}

if (!interactive()) main()
