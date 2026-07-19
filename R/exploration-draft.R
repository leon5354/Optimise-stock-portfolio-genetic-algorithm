# Exploration draft (superseded by report/portfolio-ga-analysis.Rmd)
#
# This file is the earlier, experimental version of the portfolio-optimisation
# study documented in report/portfolio-ga-analysis.Rmd. It is kept for
# transparency — it shows how the analysis was developed before being cleaned
# up into the final report. The .Rmd is the canonical, runnable artefact.
#
# The original draft had two known issues that are fixed here:
#   1. `library(GA)` was missing, so `ga()` would error out when sourced.
#   2. `obj_return_less_risk` was a copy/paste of `obj_return_max` — it called
#      `return_max(x)` instead of the risk term, so the "less risk" GA actually
#      maximised return. Fixed below.
#
# Data: see data/fetch_data.R. As of 2024, quantmod::getSymbols no longer works
# against Yahoo Finance (the /v7/download endpoint was removed). Use the
# bundled snapshots in data/cache/ or the v8/chart API in fetch_data.R.

library(quantmod)
library(tidyverse)
library(dplyr)
library(GA)

# Tickers chosen to span ETFs, tech, retail and crypto-equity exposure.
myStocks <- c("QQQ", "VOO", "AAPL", "HRB", "AMZN", "ETHE", "GBTC", "JWN", "ZM", "NVDA")

# NOTE: with the broken quantmod path, use the bundled snapshot instead:
# myRetData <- readRDS("data/cache/returns_10_2020_2021.rds")
# Otherwise (if a working Yahoo fetch is available):
getSymbols(myStocks, src = "yahoo", from = "2020-01-01", to = "2021-01-01")

# Transfer daily returns into a time series.
a <- lapply(myStocks, function(sym) {
  dailyReturn(na.omit(getSymbols(sym, from = "2020-01-01", to = "2021-01-01", auto.assign = FALSE)))
})
myRetData <- do.call(merge.xts, a)
colnames(myRetData) <- myStocks

# ----- helper functions -----------------------------------------------------

# Weighted portfolio daily-return time series.
returnofport <- function(x) {
  returns <- 0
  for (i in seq_along(x)) {
    returns <- returns + myRetData[, i] * x[i]
  }
  returns
}

# Sharpe ratio with risk-free rate = 0.
sharpe <- function(x) {
  returns <- returnofport(x)
  mean(as.numeric(returns)) / sqrt(cov(as.numeric(returns)))
}

# Quadratic penalty enforcing sum(x) == 1 and 0 <= x_i <= 1.
all_weight <- function(x) {
  w <- (sum(x) - 1)^2
  for (i in seq_along(x)) {
    w <- w + max(c(0, x[i] - 1))^2 + max(c(0, -x[i]))^2
  }
  w
}

# Fitness for the maximum-Sharpe portfolio.
obj <- function(x) -sharpe(x) + 100 * all_weight(x)

# ----- GA run 1: maximum Sharpe --------------------------------------------

ga_res <- ga(
  type = "real-valued",
  fitness = function(x) -obj(x),
  lower = rep(0, ncol(myRetData)),
  upper = rep(1, ncol(myRetData)),
  maxiter = 5000, run = 500, monitor = TRUE, seed = 1
)
sol <- as.vector(summary(ga_res)$solution)

# ----- GA run 2: maximum return --------------------------------------------

return_max <- function(x) mean(as.numeric(returnofport(x)))
obj_return_max <- function(x) -return_max(x) + 100 * all_weight(x)

ga_res_return <- ga(
  type = "real-valued",
  fitness = function(x) -obj_return_max(x),
  lower = rep(0, ncol(myRetData)),
  upper = rep(1, ncol(myRetData)),
  maxiter = 5000, run = 500, monitor = TRUE, seed = 1
)

# ----- GA run 3: minimum risk (FIXED) --------------------------------------
# The original draft mistakenly used `return_max(x)` inside `obj_return_less_risk`,
# which made this GA behave identically to the max-return GA. It now minimises
# the portfolio volatility as intended.

return_less_risk <- function(x) {
  returns <- returnofport(x)
  sqrt(cov(as.numeric(returns)))
}
obj_return_less_risk <- function(x) return_less_risk(x) + 100 * all_weight(x)

ga_res_less_risk <- ga(
  type = "real-valued",
  fitness = function(x) -obj_return_less_risk(x),
  lower = rep(0, ncol(myRetData)),
  upper = rep(1, ncol(myRetData)),
  maxiter = 5000, run = 500, monitor = TRUE, seed = 1
)

# ----- collect solutions and plot ------------------------------------------

sol1 <- as.vector(summary(ga_res_less_risk)$solution)
sol2 <- as.vector(summary(ga_res_return)$solution)

opt_port      <- data.frame(stock = names(myRetData), weight = sol)
return_port   <- data.frame(stock = names(myRetData), weight = sol1)
min_risk_port <- data.frame(stock = names(myRetData), weight = sol2)

library(ggplot2)

opt_bar <- ggplot(opt_port, aes(x = fct_reorder(stock, weight), y = weight, fill = stock)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = NULL, y = NULL, title = "Best Sharpe Solution") +
  scale_y_continuous(labels = scales::percent)

risk_bar <- ggplot(min_risk_port, aes(x = fct_reorder(stock, weight), y = weight, fill = stock)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = NULL, y = NULL, title = "Min Risk Solution") +  # was "Min Wisk Solution"
  scale_y_continuous(labels = scales::percent)

return_bar <- ggplot(return_port, aes(x = fct_reorder(stock, weight), y = weight, fill = stock)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = NULL, y = NULL, title = "Max Return Solution") +
  scale_y_continuous(labels = scales::percent)

return_bar
risk_bar
opt_bar
