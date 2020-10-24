README
================

## Dispersion study

In the data folder, I give you the intra-day dispersion numbers for the
J200 (ALSI Top 40) and J400 (Swix top 40).

Also included is the weekly and 1 and 3 monthly calculated dispersion,
avg stock correlation and avg SD calcs for J200, J400, J300 and J430.
This is a treasure trove of data.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# Intraday dispersion last 4 years:
ID_Disp <- read_rds("data/ID_Disp.rds") %>% filter(date >= as.Date("2016-07-29") & date <= as.Date("2020-07-31"))

ID_Disp %>% 
  ggplot() + 
  geom_line(aes(date, ID_Dispersion_W_J400)) + 
  geom_line(aes(date, ID_Dispersion_W_J200), color = "red")
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
# Intraday weighted average realized volatility:
Ivol <- bind_rows(read_rds("data/J200_IVol.rds") %>% select(date, Avg_RV, W_Avg_RV = W_Avg_RV_J200) %>% mutate(Idx = "J200") %>% gather(Type, Value, -date, -Idx), 
                  read_rds("data/J400_IVol.rds") %>% select(date, Avg_RV, W_Avg_RV = W_Avg_RV_J400) %>% mutate(Idx = "J400") %>% gather(Type, Value, -date, -Idx))

Ivol %>% filter(Type == "W_Avg_RV") %>% 
  ggplot() + 
  geom_line(aes(date, Value, color = Idx)) + 
  labs(title = "Weighted Avg Realized Volatility") + theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
# Dispersion - lower frequency
W_Dispersion <- 
  read_rds("data/Dispersion/Weekly.rds")
M_Dispersion <- 
  read_rds("data/Dispersion/Monthly.rds")

# Correlations will be similar (as constituents are quite similar)
M_Dispersion %>% filter(Freq == "1_Month") %>% 
  ggplot() + geom_line(aes(date, Avg_Corr, color = Idx))
```

![](README_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
M_Dispersion %>% filter(Freq == "1_Month") %>% 
  ggplot() + geom_line(aes(date, Dispersion_W, color = Idx))
```

![](README_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

``` r
M_Dispersion %>% filter(Freq == "1_Month") %>% 
  ggplot() + geom_line(aes(date, Dispersion_U, color = Idx))
```

![](README_files/figure-gfm/unnamed-chunk-1-5.png)<!-- -->

``` r
M_Dispersion %>% filter(Freq == "3_Month") %>% 
  ggplot() + geom_line(aes(date, Dispersion_W, color = Idx))
```

![](README_files/figure-gfm/unnamed-chunk-1-6.png)<!-- -->

``` r
W_Dispersion %>% 
  ggplot() + geom_line(aes(date, W_SD, color = Idx))
```

![](README_files/figure-gfm/unnamed-chunk-1-7.png)<!-- -->

## Importing index returns data

``` r
library(tbl2xts)

# the JSE top 40 index used to create a simple return index

TP40 <- fmxdat::SA_Indexes %>% filter(Tickers == "TOP40TR Index") %>% 
    mutate(SimpleRet = Price / lag(Price)-1) %>% 
    ungroup() %>% select(date, SimpleRet) %>% tbl2xts::tbl_xts()

Plotdata <- cbind(TP40, TP40^2, abs(TP40))
colnames(Plotdata) <- c("Returns", "Returns_Sqd", "Returns_Abs")

Plotdata <- Plotdata %>% xts_tbl() %>% gather(ReturnType, Returns, 
    -date)

ggplot(Plotdata) + geom_line(aes(x = date, y = Returns, colour = ReturnType, 
    alpha = 0.5)) + ggtitle("Return Type Persistence: TOP40TR Index") + 
    facet_wrap(~ReturnType, nrow = 3, ncol = 1, scales = "free") + 
    guides(alpha = FALSE, colour = FALSE) + theme_bw()
```

    ## Warning: Removed 3 row(s) containing missing values (geom_path).

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# Estimating the GARCH

``` r
library(rugarch)
```

    ## Loading required package: parallel

    ## 
    ## Attaching package: 'rugarch'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     reduce

    ## The following object is masked from 'package:stats':
    ## 
    ##     sigma

``` r
# combining the two datasets to make sure that they line up 

comb <- ID_Disp %>% select(date, ID_Dispersion_W_J400) %>%  left_join(xts_tbl(TP40), by="date") %>%
  tbl_xts()

exreg <- comb[,1]
fit <- comb[,2]

garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", 
    "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[1], garchOrder = c(1, 
    1), external.regressors = exreg), mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
    distribution.model = c("norm", "snorm", "std", "sstd", "ged","sged", "nig", "ghyp", "jsu")[1])

# Now to fit the garch to the returns

#garchfit1 = ugarchfit(spec = garch11, data = fit)

#Error: $ operator is invalid for atomic vectors
#In addition: Warning message:
#In .makefitmodel(garchmodel = "sGARCH", f = .sgarchLLH, T = T, m = m,  : 
#rugarch-->warning: failed to invert hessian

#garchfit1
```

# Created:

``` r
# library(RA)
# load_core()
# Root <- setroot()
# EQSScreen <- "Tickers"
# UniverseSelect <- "JALSHAll"
# currency <- c("Local", "USD")[1]
# Price_Field = c("TOT_RETURN_INDEX_GROSS_DVDS", "PX_LAST")[1]
# 
# NonTradeDays <- RA::NonTradeDays(Root)
# IntraDays <-
#   datload(Root, "FullIDay", Intraday_Data) %>%
#   filter(!date %in% NonTradeDays) %>% mutate(Tickers = gsub(" SJ Equity", "", Tickers))
# 
# wts <-
#   datload(Root, SaveName = "EQS_Return_Daily", Db_Factors_Merge, UQ(EQSScreen), UQ(UniverseSelect), UQ(currency), UQ(Price_Field), EQS_Return_Daily)
# 
# ID_Disp <- IntraDays %>% select(date, contains("_Dispersion"), contains("_PXMove") ) %>% unique
# 
# IVol_J200 <-
#   left_join(IntraDays,
#             wts %>% mutate(Tickers = gsub(" SJ Equity", "", Tickers)) %>% select(date, Tickers, wt = J200_W_Adj) %>% filter(!is.na(wt)),
#             by = c("date", "Tickers")) %>%
#   select(date, Tickers, RV, J200 = wt) %>% filter(!is.na(J200)) %>%
#   group_by(date) %>% mutate(J200 = J200 / sum(J200)) %>% summarise(Avg_RV = mean(RV), W_Avg_RV_J200 = sum(J200 * RV) ) %>% ungroup() %>% unique
# 
# datstore(Root, SaveName = "J200_IVol", dataframe = IVol_J200,
#          Research, FMX_Projects_2020, Charles, data)
# 
# IVol_J400 <-
#   left_join(IntraDays,
#             wts %>% mutate(Tickers = gsub(" SJ Equity", "", Tickers)) %>% select(date, Tickers, wt = J400_W_Adj) %>% filter(!is.na(wt)),
#             by = c("date", "Tickers")) %>%
#   select(date, Tickers, RV, J400 = wt) %>% filter(!is.na(J400)) %>%
#     group_by(date) %>% mutate(J400 = J400 / sum(J400)) %>% summarise(Avg_RV = mean(RV), W_Avg_RV_J400 = sum(J400 * RV)) %>% ungroup() %>% unique
# 
# datstore(Root, SaveName = "J400_IVol", dataframe = IVol_J400,
#          Research, FMX_Projects_2020, Charles, data)
# 
# datstore(Root, SaveName = "ID_Disp", dataframe = ID_Disp,
#          Research, FMX_Projects_2020, Charles, data)
# 
# bind_rows(
#   "D:/Work/RAnalytics/Data/Dispersion/Monthly/1" %>% list.files(., full.names = T, recursive = T) %>%
#   as.list() %>% map_df(~read_rds(.)) %>% select(date, Dispersion_U, Dispersion_W, Avg_Corr, W_SD, Avg_SD, Idx) %>% unique %>% mutate(Freq = "1_Month"),
#   "D:/Work/RAnalytics/Data/Dispersion/Monthly/3" %>% list.files(., full.names = T, recursive = T) %>%
#   as.list() %>% map_df(~read_rds(.)) %>% select(date, Dispersion_U, Dispersion_W, Avg_Corr, W_SD, Idx) %>% unique %>% mutate(Freq = "3_Month")
# ) %>%
#   write_rds("data/Dispersion/Monthly.rds")
# 
#  "D:/Work/RAnalytics/Data/Dispersion/Weekly/1" %>% list.files(., full.names = T, recursive = T) %>%
#  map_df(~read_rds(.)) %>% select(date, Dispersion_U, Dispersion_W, Avg_Corr, W_SD, Idx) %>% unique %>% mutate(Freq = "Weekly") %>% 
#  write_rds("data/Dispersion/Weekly.rds")
```
