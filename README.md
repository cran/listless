[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)
[![Is the package on CRAN?](http://www.r-pkg.org/badges/version/listless)](http://www.r-pkg.org/pkg/listless)
[![Build Status](https://semaphoreci.com/api/v1/richierocks/listless/branches/master/badge.svg)](https://semaphoreci.com/richierocks/listless)
[![Build status](https://ci.appveyor.com/api/projects/status/gmtnjxrbrk3943q5?svg=true)](https://ci.appveyor.com/project/richierocks/listless)



# listless

A lightweight R package for converting lists to tidy data frames.


## Installation

To install the stable version, type:


```r
install.packages("listless")
```

To install the development version, you first need the *devtools* package.


```r
install.packages("devtools")
```

Then you can install the *listless* package using


```r
devtools::install_bitbucket("graumannlabtools/listless")
```

## Functions

### `list_to_data.frame` 

Converts a list to a tidy `data.frame`.

For example:



```r
library(listless)
library(magrittr)
l <- list(
  a = 1,
  2:3,                             # missing names are blank
  c = list(ca = 4:6, 7:10, list(cca = 11:15)),
  d = list()                       # empty elt's silently ignored
)
list_to_data.frame(l)
```

```
##    names names.1 names.2 values
## 1      a    <NA>    <NA>      1
## 2           <NA>    <NA>      2
## 3           <NA>    <NA>      3
## 4      c      ca    <NA>      4
## 5      c      ca    <NA>      5
## 6      c      ca    <NA>      6
## 7      c            <NA>      7
## 8      c            <NA>      8
## 9      c            <NA>      9
## 10     c            <NA>     10
## 11     c             cca     11
## 12     c             cca     12
## 13     c             cca     13
## 14     c             cca     14
## 15     c             cca     15
```

```r
# You can set custom column names
list_to_data.frame(l, c("group", "subgroup", "element"), "amount") %>% 
  head(3)
```

```
##   group subgroup element amount
## 1     a     <NA>    <NA>      1
## 2           <NA>    <NA>      2
## 3           <NA>    <NA>      3
```

### `list_str`

Summarises the contents of a list, in a tidy `data.frame`.

For example:


```r
l2 <- list(
  a = 1,
  b = matrix(1:6, 2),
  c = list(
    ca = y ~ x,
    list(cba = median, cbb = quote(1 + 1), cbc = expression(1 + 1)),
    cc = list(cca = as.name("xyz"))
  ),
  d = array(1:24, 2:4)
)
list_str(l2)
```

```
##   names names.1 names.2      class       mode length    dims
## 1     a    <NA>    <NA>    numeric    numeric      1        
## 2     b    <NA>    <NA>     matrix    numeric      6    2, 3
## 3     c      ca    <NA>    formula       call      3        
## 4     c     cba    <NA>   function   function      1        
## 5     c     cbb    <NA>       call       call      3        
## 6     c     cbc    <NA> expression expression      1        
## 7     c      cc     cca       name       name      1        
## 8     d    <NA>    <NA>      array    numeric     24 2, 3, 4
```

### `list_depth` 

Returns the depth (number of nested levels) of a list.

For example:


```r
list_depth(l)
```

```
## [1] 3
```

```r
# Atomic variables have depth 0
list_depth(1)
```

```
## [1] 0
```

```r
# Empty elements can be pruned before counting
list_depth(list())
```

```
## [1] 1
```

```r
list_depth(list(), prune_empty_elts = TRUE)
```

```
## [1] 0
```
