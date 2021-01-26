HW01 Exploring Data
================
Lendi N. Joy
1/25/2021

### Load and Examine

First we need to load the psych package and the data. Let’s also look at
the structure of the data at this time.

``` r
library(psych)
data(bfi)
str(bfi)
```

    ## 'data.frame':    2800 obs. of  28 variables:
    ##  $ A1       : int  2 2 5 4 2 6 2 4 4 2 ...
    ##  $ A2       : int  4 4 4 4 3 6 5 3 3 5 ...
    ##  $ A3       : int  3 5 5 6 3 5 5 1 6 6 ...
    ##  $ A4       : int  4 2 4 5 4 6 3 5 3 6 ...
    ##  $ A5       : int  4 5 4 5 5 5 5 1 3 5 ...
    ##  $ C1       : int  2 5 4 4 4 6 5 3 6 6 ...
    ##  $ C2       : int  3 4 5 4 4 6 4 2 6 5 ...
    ##  $ C3       : int  3 4 4 3 5 6 4 4 3 6 ...
    ##  $ C4       : int  4 3 2 5 3 1 2 2 4 2 ...
    ##  $ C5       : int  4 4 5 5 2 3 3 4 5 1 ...
    ##  $ E1       : int  3 1 2 5 2 2 4 3 5 2 ...
    ##  $ E2       : int  3 1 4 3 2 1 3 6 3 2 ...
    ##  $ E3       : int  3 6 4 4 5 6 4 4 NA 4 ...
    ##  $ E4       : int  4 4 4 4 4 5 5 2 4 5 ...
    ##  $ E5       : int  4 3 5 4 5 6 5 1 3 5 ...
    ##  $ N1       : int  3 3 4 2 2 3 1 6 5 5 ...
    ##  $ N2       : int  4 3 5 5 3 5 2 3 5 5 ...
    ##  $ N3       : int  2 3 4 2 4 2 2 2 2 5 ...
    ##  $ N4       : int  2 5 2 4 4 2 1 6 3 2 ...
    ##  $ N5       : int  3 5 3 1 3 3 1 4 3 4 ...
    ##  $ O1       : int  3 4 4 3 3 4 5 3 6 5 ...
    ##  $ O2       : int  6 2 2 3 3 3 2 2 6 1 ...
    ##  $ O3       : int  3 4 5 4 4 5 5 4 6 5 ...
    ##  $ O4       : int  4 3 5 3 3 6 6 5 6 5 ...
    ##  $ O5       : int  3 3 2 5 3 1 1 3 1 2 ...
    ##  $ gender   : int  1 2 2 2 1 2 1 1 1 2 ...
    ##  $ education: int  NA NA NA NA NA 3 NA 2 1 NA ...
    ##  $ age      : int  16 18 17 17 17 21 18 19 19 17 ...

Alright, 2,800 cases and 28 variables.

### Add Value Labels

We saw that gender and education are coded as integer variables, so
let’s add some value labels.  
**gender**: 1 = male, 2 = female  
**education**: 1 = HS, 2 = finished HS, 3 = some college, 4 = college
graduate 5 = graduate degree

``` r
bfi$gender<- factor(bfi$gender, levels = c(1,2), labels = c("male", "female"))
bfi$education<- factor(bfi$education, levels = c(1,2,3,4,5), labels = c("HS", "finished HS", "some college", "college grad", "grad degree"))
```

### Check Missing Data

We also saw that there were some missing values when we looked at the
structure. Let’s find out how many complete cases there are in the
dataset.

``` r
sum(complete.cases(bfi)) #Complete cases
```

    ## [1] 2236

``` r
sum(!complete.cases(bfi)) #Incomplete cases
```

    ## [1] 564

### Clean it up

It’s probably not good practice to just throw out data, but let’s do it
anyway and keep only complete cases.

``` r
x <- na.omit(bfi)
```

Interestingly, there are some participants under 18.  
Let’s find out how many now that we are only including complete cases.

``` r
sum(x$age < 18)
```

    ## [1] 51

OK, only 51. Let’s pretend that we have parental consent and keep them
in.

### Summarize

Let’s get a summary of our complete data.

``` r
summary(x)
```

    ##        A1              A2              A3              A4             A5       
    ##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.00   Min.   :1.000  
    ##  1st Qu.:1.000   1st Qu.:4.000   1st Qu.:4.000   1st Qu.:4.00   1st Qu.:4.000  
    ##  Median :2.000   Median :5.000   Median :5.000   Median :5.00   Median :5.000  
    ##  Mean   :2.365   Mean   :4.834   Mean   :4.629   Mean   :4.75   Mean   :4.585  
    ##  3rd Qu.:3.000   3rd Qu.:6.000   3rd Qu.:6.000   3rd Qu.:6.00   3rd Qu.:6.000  
    ##  Max.   :6.000   Max.   :6.000   Max.   :6.000   Max.   :6.00   Max.   :6.000  
    ##        C1             C2              C3              C4              C5       
    ##  Min.   :1.00   Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:4.00   1st Qu.:4.000   1st Qu.:4.000   1st Qu.:1.000   1st Qu.:2.000  
    ##  Median :5.00   Median :5.000   Median :5.000   Median :2.000   Median :3.000  
    ##  Mean   :4.57   Mean   :4.401   Mean   :4.323   Mean   :2.501   Mean   :3.255  
    ##  3rd Qu.:5.00   3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:4.000   3rd Qu.:5.000  
    ##  Max.   :6.00   Max.   :6.000   Max.   :6.000   Max.   :6.000   Max.   :6.000  
    ##        E1             E2              E3             E4              E5       
    ##  Min.   :1.00   Min.   :1.000   Min.   :1.00   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:2.00   1st Qu.:2.000   1st Qu.:3.00   1st Qu.:4.000   1st Qu.:4.000  
    ##  Median :3.00   Median :3.000   Median :4.00   Median :5.000   Median :5.000  
    ##  Mean   :2.97   Mean   :3.121   Mean   :4.01   Mean   :4.431   Mean   :4.419  
    ##  3rd Qu.:4.00   3rd Qu.:4.000   3rd Qu.:5.00   3rd Qu.:6.000   3rd Qu.:5.000  
    ##  Max.   :6.00   Max.   :6.000   Max.   :6.00   Max.   :6.000   Max.   :6.000  
    ##        N1              N2              N3              N4       
    ##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:2.000   1st Qu.:2.000   1st Qu.:2.000   1st Qu.:2.000  
    ##  Median :3.000   Median :4.000   Median :3.000   Median :3.000  
    ##  Mean   :2.908   Mean   :3.486   Mean   :3.199   Mean   :3.175  
    ##  3rd Qu.:4.000   3rd Qu.:5.000   3rd Qu.:4.000   3rd Qu.:4.000  
    ##  Max.   :6.000   Max.   :6.000   Max.   :6.000   Max.   :6.000  
    ##        N5              O1              O2              O3       
    ##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:2.000   1st Qu.:4.000   1st Qu.:1.000   1st Qu.:4.000  
    ##  Median :3.000   Median :5.000   Median :2.000   Median :5.000  
    ##  Mean   :2.952   Mean   :4.822   Mean   :2.689   Mean   :4.483  
    ##  3rd Qu.:4.000   3rd Qu.:6.000   3rd Qu.:4.000   3rd Qu.:5.000  
    ##  Max.   :6.000   Max.   :6.000   Max.   :6.000   Max.   :6.000  
    ##        O4              O5           gender            education   
    ##  Min.   :1.000   Min.   :1.000   male  : 735   HS          : 198  
    ##  1st Qu.:4.000   1st Qu.:1.000   female:1501   finished HS : 250  
    ##  Median :5.000   Median :2.000                 some college:1078  
    ##  Mean   :4.948   Mean   :2.455                 college grad: 346  
    ##  3rd Qu.:6.000   3rd Qu.:3.000                 grad degree : 364  
    ##  Max.   :6.000   Max.   :6.000                                    
    ##       age       
    ##  Min.   : 3.00  
    ##  1st Qu.:21.00  
    ##  Median :26.00  
    ##  Mean   :29.51  
    ##  3rd Qu.:36.00  
    ##  Max.   :86.00

Lastly, let’s take a look at education level by gender.

``` r
table(x$gender, x$education)
```

    ##         
    ##           HS finished HS some college college grad grad degree
    ##   male    82          92          306          120         135
    ##   female 116         158          772          226         229
