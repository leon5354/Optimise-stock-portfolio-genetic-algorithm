# Data

The analysis uses daily-return time series pulled from Yahoo Finance.

## Source

* **Provider:** Yahoo Finance (`https://finance.yahoo.com/`)
* **API used:** `https://query1.finance.yahoo.com/v8/finance/chart/<SYMBOL>`
  (the `quantmod::getSymbols` path via `/v7/finance/download` was removed by
  Yahoo in 2024 and no longer works).
* **Tickers:** see the `stocks_*` vectors in `fetch_data.R`.
* **Date ranges:**
  * 2020-01-01 → 2021-01-01 (Part 1 + Part 2 training set)
  * 2021-01-01 → 2022-01-01 (out-of-sample validation)

## Cache directory

Daily returns are cached as `.rds` files in `cache/`. These are committed so
the report renders without network access. Each file is a wide `xts` matrix of
daily returns (one column per ticker, one row per trading day, first row `NA`
from `diff()`).

| File | Tickers | Period |
|------|---------|--------|
| `cache/returns_10_2020_2021.rds`  | Part-1 basket, 10 tickers  | 2020–2021 |
| `cache/returns_10_2021_2022.rds`  | Part-1 basket, 10 tickers  | 2021–2022 |
| `cache/returns_50_2020_2021.rds`  | Part-2 universe, 50 tickers| 2020–2021 |

## Refreshing

```bash
Rscript data/fetch_data.R
```

The script re-downloads everything and overwrites the cache.

## Known missing tickers

| Ticker | Reason |
|--------|--------|
| `JWN`   | HTTP 404 on the v8 free API (data gap, not delisted). |
| `DISCA` | Delisted after the Warner Bros. Discovery merger (April 2022). |
| `SQ`    | Renamed to BLOCK; current ticker is `XYZ`. |

The fetcher and the analysis both skip missing tickers silently.
