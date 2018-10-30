Writing functions
================

writing a z-score function

``` r
z_scores = function(x) {
  return (mean(x) / sd(x))
}
```

now check if it works

``` r
# ignore the error, and keep going

y = runif(100)

z_scores(y)
## [1] 1.784896

z_scores(3)
## [1] NA
z_scores("my name is jeff")
## Warning in mean.default(x): argument is not numeric or logical: returning
## NA
## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm
## = na.rm): 强制改变过程中产生了NA
## [1] NA
z_scores(iris)
## Warning in mean.default(x): argument is not numeric or logical: returning
## NA
## Error in is.data.frame(x): (串列)对象不能强制改变成'double'种类
z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE))
## [1] 0.8
```

now put in some checks on inputs

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  z = mean(x) / sd(x)
  
  z
}
```

multiple input

``` r
sim_regression = function(n, beta0 = 2, beta1 = 3) {
  
  sim_data = tibble(
    x = rnorm(n, mean = 1, sd = 1),
    y = beta0 + beta1 * x + rnorm(n, 0, 1)
  )
  
  ls_fit = lm(y ~ x, data = sim_data)
  
  tibble(
    beta0_hat = coef(ls_fit)[1],
    beta1_hat = coef(ls_fit)[2]
  )
}
```

check

``` r
sim_regression(1000) # it's better to put the parameter in "x = , y = , z = " format
## # A tibble: 1 x 2
##   beta0_hat beta1_hat
##       <dbl>     <dbl>
## 1      2.03      2.96
```

``` r
read_page_reviews <- function(url) {
  
  h = read_html(url)
  
  review_titles = h %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  review_stars = h %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  review_text = h %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  read_page_reviews(urls[1]),
  read_page_reviews(urls[2]),
  read_page_reviews(urls[3]),
  read_page_reviews(urls[4]),
  read_page_reviews(urls[5])
)
```

for loops
---------

``` r
l = list(vec_numeric = 5:8,
         mat         = matrix(1:8, 2, 4),
         vec_logical = c(TRUE, FALSE),
         summary     = summary(rnorm(1000))
         )

df = data_frame(
  a = rnorm(20, 3, 1),
  b = rnorm(20, 0, 5),
  c = rnorm(20, 10, .2),
  d = rnorm(20, -3, 1)
)

mean_and_sd = function(x) {
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

write a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  output[[i]] = mean_and_sd(df[[i]])
}
```

replace for loop with map
-------------------------

``` r
output = map(df, mean_and_sd)

# or use tidy to apply on some specific column
df %>% 
  select(a,b) %>% 
  map(mean_and_sd)
## $a
## # A tibble: 1 x 2
##    mean    sd
##   <dbl> <dbl>
## 1  2.97 0.919
## 
## $b
## # A tibble: 1 x 2
##    mean    sd
##   <dbl> <dbl>
## 1  1.20  4.66

# default of output format of map is list, now we use some other format output
output = map_df(df, mean_and_sd)
```

nest list columns
-----------------

``` r
library(rnoaa)

weather = 
  meteo_pull_monitors(c("USW00094728", "USC00519397", "USS0023B17S"),
                      var = c("PRCP", "TMIN", "TMAX"), 
                      date_min = "2016-01-01",
                      date_max = "2016-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY", 
                      USC00519397 = "Waikiki_HA",
                      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())

weather_nest = 
  weather %>% 
  nest(date:tmin) # you could use group_by first to specify the column you wanna nest

weather_nest %>% pull(name)
## [1] "CentralPark_NY" "Waikiki_HA"     "Waterhole_WA"
weather_nest %>% pull(data)
## [[1]]
## # A tibble: 366 x 4
##    date        prcp  tmax  tmin
##    <date>     <dbl> <dbl> <dbl>
##  1 2016-01-01     0   5.6   1.1
##  2 2016-01-02     0   4.4   0  
##  3 2016-01-03     0   7.2   1.7
##  4 2016-01-04     0   2.2  -9.9
##  5 2016-01-05     0  -1.6 -11.6
##  6 2016-01-06     0   5    -3.8
##  7 2016-01-07     0   7.8  -0.5
##  8 2016-01-08     0   7.8  -0.5
##  9 2016-01-09     0   8.3   4.4
## 10 2016-01-10   457  15     4.4
## # ... with 356 more rows
## 
## [[2]]
## # A tibble: 366 x 4
##    date        prcp  tmax  tmin
##    <date>     <dbl> <dbl> <dbl>
##  1 2016-01-01     0  29.4  16.7
##  2 2016-01-02     0  28.3  16.7
##  3 2016-01-03     0  28.3  16.7
##  4 2016-01-04     0  28.3  16.1
##  5 2016-01-05     0  27.2  16.7
##  6 2016-01-06     0  27.2  20  
##  7 2016-01-07    46  27.8  18.3
##  8 2016-01-08     3  28.3  17.8
##  9 2016-01-09     8  27.8  19.4
## 10 2016-01-10     3  28.3  18.3
## # ... with 356 more rows
## 
## [[3]]
## # A tibble: 366 x 4
##    date        prcp  tmax  tmin
##    <date>     <dbl> <dbl> <dbl>
##  1 2016-01-01     0   1.7  -5.9
##  2 2016-01-02    25  -0.1  -6  
##  3 2016-01-03     0  -5   -10  
##  4 2016-01-04    25   0.3  -9.8
##  5 2016-01-05    25   1.9  -1.8
##  6 2016-01-06    25   1.4  -2.6
##  7 2016-01-07     0   1.4  -3.9
##  8 2016-01-08     0   1.1  -4  
##  9 2016-01-09     0   1.4  -4.5
## 10 2016-01-10     0   2.3  -3.8
## # ... with 356 more rows
```

unnest

``` r
unnest(weather_nest)
## # A tibble: 1,098 x 6
##    name           id          date        prcp  tmax  tmin
##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
##  1 CentralPark_NY USW00094728 2016-01-01     0   5.6   1.1
##  2 CentralPark_NY USW00094728 2016-01-02     0   4.4   0  
##  3 CentralPark_NY USW00094728 2016-01-03     0   7.2   1.7
##  4 CentralPark_NY USW00094728 2016-01-04     0   2.2  -9.9
##  5 CentralPark_NY USW00094728 2016-01-05     0  -1.6 -11.6
##  6 CentralPark_NY USW00094728 2016-01-06     0   5    -3.8
##  7 CentralPark_NY USW00094728 2016-01-07     0   7.8  -0.5
##  8 CentralPark_NY USW00094728 2016-01-08     0   7.8  -0.5
##  9 CentralPark_NY USW00094728 2016-01-09     0   8.3   4.4
## 10 CentralPark_NY USW00094728 2016-01-10   457  15     4.4
## # ... with 1,088 more rows
```

linear model
------------

``` r
# linear model for tmax and tmin column
lm(tmax ~ tmin, data = weather_nest$data[[1]])
## 
## Call:
## lm(formula = tmax ~ tmin, data = weather_nest$data[[1]])
## 
## Coefficients:
## (Intercept)         tmin  
##       7.779        1.045
lm(tmax ~ tmin, data = weather_nest$data[[2]])
## 
## Call:
## lm(formula = tmax ~ tmin, data = weather_nest$data[[2]])
## 
## Coefficients:
## (Intercept)         tmin  
##      22.489        0.326
lm(tmax ~ tmin, data = weather_nest$data[[3]])
## 
## Call:
## lm(formula = tmax ~ tmin, data = weather_nest$data[[3]])
## 
## Coefficients:
## (Intercept)         tmin  
##       6.851        1.245

# or use map function
weather_lm = function(df) {
  lm(tmax ~ tmin, data = df)
}

map(weather_nest$data, weather_lm)
## [[1]]
## 
## Call:
## lm(formula = tmax ~ tmin, data = df)
## 
## Coefficients:
## (Intercept)         tmin  
##       7.779        1.045  
## 
## 
## [[2]]
## 
## Call:
## lm(formula = tmax ~ tmin, data = df)
## 
## Coefficients:
## (Intercept)         tmin  
##      22.489        0.326  
## 
## 
## [[3]]
## 
## Call:
## lm(formula = tmax ~ tmin, data = df)
## 
## Coefficients:
## (Intercept)         tmin  
##       6.851        1.245
```
