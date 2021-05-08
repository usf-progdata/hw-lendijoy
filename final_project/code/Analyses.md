Final Project
================
Lendi Nicole Joy
5/3/2021

### Load the needed libraries.

``` r
library(tidyverse)
library(psych)
library(GPArotation)
```

### Load the dataset.

``` r
Motivation <- read_csv(here::here("data", "Activities_and_Mood.csv"))
```

### Clean the data.

Next, clean up the data using variable Q166\_7, which indicates whether
the survey was completed.

``` r
Motivation <- 
  Motivation %>% 
  drop_na("Q166_7")
```

There were 149 cases removed.

Now let’s select only the variables needed for the EFA (remove V4 and
V5, which are attention check items).

``` r
Motivation_EFA <- 
  Motivation %>% 
  select(VFI.U1:Int_4, 
         -c(V4, V5))  
```

##### Reverse code items.

After selecting the appropriate variables, I will reverse code the items
so that higher scores indicate more positive attitudes. A few items are
reverse coded, so exclude those variables from recoding.

``` r
Motivation_EFA<-  
  8 - Motivation_EFA[, -c(31, 33:35, 52, 57)]
```

### Save the dataset.

Save the resulting dataset.

``` r
write.csv(Motivation_EFA,
          here::here("data", "Motivation_EFA.csv"))
```

### Run the EFA.

The original hypothesis is that there will be 7 belief factors, 3
factors indicating attitudes, subjective norms and perceived behavior
control, then 1 factor to indicate intention or motivation to volunteer.

I start with extracting these 11 factors since this is the original
hypothesis.

``` r
EFAresult1 = factanal(~ ., 
                      data = Motivation_EFA, 
                      factors = 11, 
                      fm = "pa", 
                      rotation = "oblimin")
EFAresult1
```

    ## 
    ## Call:
    ## factanal(x = ~., factors = 11, data = Motivation_EFA, rotation = "oblimin",     fm = "pa")
    ## 
    ## Uniquenesses:
    ##  VFI.U1  VFI.U2  VFI.U3  VFI.U4  VFI.U5  VFI.V1  VFI.V2  VFI.V3  VFI.V4  VFI.V5 
    ##   0.464   0.427   0.382   0.574   0.405   0.473   0.685   0.307   0.413   0.407 
    ##  VFI.E1  VFI.E2  VFI.E3  VFI.E4  VFI.E5  VFI.P1  VFI.P2  VFI.P3  VFI.P4  VFI.P5 
    ##   0.530   0.397   0.356   0.246   0.515   0.447   0.520   0.662   0.315   0.261 
    ##  VFI.C1  VFI.C2  VFI.C3  VFI.C4  VFI.C5 VFI.NB1 VFI.NB2 VFI.NB3 VFI.NB4 VFI.NB5 
    ##   0.535   0.311   0.502   0.284   0.404   0.545   0.288   0.242   0.413   0.330 
    ##  Ctrl_2   Att_1   Att_2   Att_3   Att_4   Att_5   Att_6   Att_7   Att_8   Att_9 
    ##   0.660   0.221   0.251   0.246   0.174   0.245   0.264   0.235   0.284   0.266 
    ##    SN_1    SN_2    SN_3    SN_4    SN_5   PBC_1   PBC_2   PBC_4   PBC_5   Int_1 
    ##   0.235   0.242   0.259   0.718   0.385   0.413   0.595   0.558   0.305   0.115 
    ##   Int_2   Int_4 
    ##   0.069   0.115 
    ## 
    ## Loadings:
    ##         Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7 Factor8 Factor9
    ## VFI.U1                           0.144                   0.301           0.457 
    ## VFI.U2                                           0.136   0.313           0.370 
    ## VFI.U3                  -0.113   0.180   0.195   0.122   0.159           0.474 
    ## VFI.U4                                           0.143   0.151           0.355 
    ## VFI.U5                           0.141           0.152                   0.531 
    ## VFI.V1                   0.104                           0.647                 
    ## VFI.V2           0.203          -0.127                   0.402           0.102 
    ## VFI.V3   0.107                                           0.713                 
    ## VFI.V4   0.124                                   0.110   0.631                 
    ## VFI.V5   0.112                           0.120           0.217           0.491 
    ## VFI.E1                                           0.576                         
    ## VFI.E2                           0.102           0.569                         
    ## VFI.E3                                           0.631                   0.113 
    ## VFI.E4                                           0.745                         
    ## VFI.E5                           0.231           0.150  -0.157           0.348 
    ## VFI.P1   0.179   0.110          -0.150           0.121   0.189           0.154 
    ## VFI.P2          -0.120                           0.336                         
    ## VFI.P3  -0.113                   0.198           0.394   0.172          -0.115 
    ## VFI.P4                                           0.183                         
    ## VFI.P5   0.115                                   0.232                         
    ## VFI.C1                   0.115   0.671                                         
    ## VFI.C2                   0.108   0.775                                         
    ## VFI.C3                           0.481                                   0.263 
    ## VFI.C4           0.154           0.643   0.143  -0.126                         
    ## VFI.C5                  -0.104   0.712           0.190                         
    ## VFI.NB1                  0.227           0.485  -0.138   0.142                 
    ## VFI.NB2                                  0.837                                 
    ## VFI.NB3                  0.214           0.705                                 
    ## VFI.NB4                  0.402           0.393                                 
    ## VFI.NB5                                  0.764                                 
    ## Ctrl_2  -0.105   0.184   0.149           0.162                   0.240   0.215 
    ## Att_1    0.612                                                           0.143 
    ## Att_2    0.772                                                                 
    ## Att_3    0.858                                                                 
    ## Att_4    0.842                                                                 
    ## Att_5    0.617   0.136                                                         
    ## Att_6    0.570   0.101                                   0.153                 
    ## Att_7    0.842                                                                 
    ## Att_8    0.592   0.178                                                         
    ## Att_9    0.667                                   0.104                   0.168 
    ## SN_1                     0.763                           0.100                 
    ## SN_2                     0.767                                                 
    ## SN_3                     0.765                   0.119                         
    ## SN_4                     0.259                                   0.206         
    ## SN_5     0.105           0.480           0.222                                 
    ## PBC_1                                                            0.767         
    ## PBC_2    0.147   0.230   0.179                          -0.242   0.330   0.135 
    ## PBC_4    0.132   0.118                                           0.401   0.217 
    ## PBC_5                                                            0.853         
    ## Int_1            0.938                                                         
    ## Int_2            0.977                                                         
    ## Int_4            0.902                                                         
    ##         Factor10 Factor11
    ## VFI.U1                   
    ## VFI.U2                   
    ## VFI.U3                   
    ## VFI.U4                   
    ## VFI.U5   0.142           
    ## VFI.V1                   
    ## VFI.V2                   
    ## VFI.V3                   
    ## VFI.V4                   
    ## VFI.V5            0.221  
    ## VFI.E1  -0.129           
    ## VFI.E2   0.167           
    ## VFI.E3   0.164           
    ## VFI.E4   0.136           
    ## VFI.E5   0.235   -0.169  
    ## VFI.P1   0.360   -0.112  
    ## VFI.P2   0.335           
    ## VFI.P3   0.132           
    ## VFI.P4   0.631           
    ## VFI.P5   0.648           
    ## VFI.C1                   
    ## VFI.C2           -0.119  
    ## VFI.C3           -0.209  
    ## VFI.C4   0.258    0.152  
    ## VFI.C5                   
    ## VFI.NB1                  
    ## VFI.NB2                  
    ## VFI.NB3                  
    ## VFI.NB4          -0.138  
    ## VFI.NB5           0.110  
    ## Ctrl_2                   
    ## Att_1   -0.101    0.247  
    ## Att_2                    
    ## Att_3    0.137           
    ## Att_4            -0.101  
    ## Att_5             0.202  
    ## Att_6   -0.122    0.246  
    ## Att_7            -0.142  
    ## Att_8             0.199  
    ## Att_9             0.167  
    ## SN_1                     
    ## SN_2     0.100           
    ## SN_3                     
    ## SN_4              0.178  
    ## SN_5              0.219  
    ## PBC_1                    
    ## PBC_2                    
    ## PBC_4             0.196  
    ## PBC_5                    
    ## Int_1                    
    ## Int_2                    
    ## Int_4                    
    ## 
    ##                Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7 Factor8
    ## SS loadings      4.866   2.987   2.515   2.510   2.408   2.262   2.050   1.753
    ## Proportion Var   0.094   0.057   0.048   0.048   0.046   0.043   0.039   0.034
    ## Cumulative Var   0.094   0.151   0.199   0.248   0.294   0.337   0.377   0.411
    ##                Factor9 Factor10 Factor11
    ## SS loadings      1.719    1.445    0.650
    ## Proportion Var   0.033    0.028    0.012
    ## Cumulative Var   0.444    0.471    0.484
    ## 
    ## Factor Correlations:
    ##          Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7 Factor8
    ## Factor1    1.000 -0.3378 -0.1763 -0.1678   0.535 -0.2764  0.5068  -0.236
    ## Factor2   -0.338  1.0000  0.1983  0.3481  -0.190  0.1636 -0.2436   0.450
    ## Factor3   -0.176  0.1983  1.0000  0.1884  -0.321  0.1137 -0.2241   0.153
    ## Factor4   -0.168  0.3481  0.1884  1.0000  -0.210  0.0127 -0.1256   0.204
    ## Factor5    0.535 -0.1904 -0.3205 -0.2097   1.000 -0.2510  0.3208  -0.141
    ## Factor6   -0.276  0.1636  0.1137  0.0127  -0.251  1.0000 -0.0915   0.023
    ## Factor7    0.507 -0.2436 -0.2241 -0.1256   0.321 -0.0915  1.0000  -0.200
    ## Factor8   -0.236  0.4501  0.1526  0.2043  -0.141  0.0230 -0.1997   1.000
    ## Factor9   -0.337  0.2103  0.6507  0.1260  -0.452  0.1695 -0.3042   0.152
    ## Factor10  -0.384  0.2947  0.1353  0.3842  -0.280  0.1978 -0.3794   0.267
    ## Factor11  -0.240  0.0312  0.0452  0.0789  -0.267  0.0581 -0.0791  -0.115
    ##          Factor9 Factor10 Factor11
    ## Factor1  -0.3368  -0.3844  -0.2398
    ## Factor2   0.2103   0.2947   0.0312
    ## Factor3   0.6507   0.1353   0.0452
    ## Factor4   0.1260   0.3842   0.0789
    ## Factor5  -0.4520  -0.2797  -0.2675
    ## Factor6   0.1695   0.1978   0.0581
    ## Factor7  -0.3042  -0.3794  -0.0791
    ## Factor8   0.1516   0.2673  -0.1154
    ## Factor9   1.0000   0.2249   0.0516
    ## Factor10  0.2249   1.0000   0.0564
    ## Factor11  0.0516   0.0564   1.0000
    ## 
    ## Test of the hypothesis that 11 factors are sufficient.
    ## The chi square statistic is 1271.76 on 809 degrees of freedom.
    ## The p-value is 3.27e-23

##### Determine number of factors.

We look at the sum of squared loadings (SS loadings), which are the
eigenvalues. The Kaiser Rule suggests that eigenvalues greater than 1
indicate meaningful factors. This rule, would suggest that there are *10
meaningful* factors.

We can look at the item loadings and see that even though ten factors
have eigenvalues greater than 1, the items do not load very highly on
some of the factors.

###### Use another method to determine number of factors.

Next, I will run a parallel analysis and examine the scree plot as
another way of determining the number of factors to extract.

``` r
set.seed(13115)
PA = fa.parallel(Motivation_EFA, 
                 fa = "fa",
                 quant = .95)
```

    ## Parallel analysis suggests that the number of factors =  8  and the number of components =  NA

Instead of displaying the scree plot that is created, I will make a
nicer plot of the parallel analysis.

First, create a data frame with the observed values.

``` r
obs = data.frame(PA$fa.values)
obs$type = c('Observed Data')
obs$num = c(row.names(obs))
obs$num = as.numeric(obs$num)
colnames(obs) = 
  c('eigenvalue', 
    'type', 
    'num')
```

Next, calculate and save the 95% quantiles for the eigenvalues for the
observed data.

``` r
percent = apply(PA$values,
                2,
                function(x) 
                  quantile(x,.95))
min = as.numeric(nrow(obs))
min = (4*min) - (min-1)
max = as.numeric(nrow(obs))
max = 4*max
percentile = percent[min:max]
```

Now create a data frame for the simulated eigenvalues.

``` r
sim = data.frame(percentile)
sim$type = c('Simulated Data (95th %ile)')
sim$num = c(row.names(obs))
sim$num = as.numeric(sim$num)
colnames(sim) = 
  c('eigenvalue', 
    'type', 
    'num')
```

After creating the appropriate data frames, I will put them together
into one data frame.

``` r
PAdata = rbind(obs,sim)
```

Create the plot.

``` r
ggplot(PAdata, 
                  aes(x = num, 
                      y = eigenvalue, 
                      shape = type)) +
geom_line()+
geom_point(size = 1.5)+
scale_y_continuous(name = 'Eigenvalue')+
scale_x_continuous(name = 'Factor Number', 
                   breaks = min(PAdata$num):max(PAdata$num))+
scale_shape_manual(values = 
                     c(16,5)) +
geom_vline(xintercept = PA$nfact, linetype = 'dashed') +
  ggtitle("Scree Plot")
```

<img src="C:/Users/lendi/Documents/hw-lendijoy/final_project/output/figuresPretty Scree Plot-1.png" style="display: block; margin: auto;" />

### Rerun EFA.

Now I will rerun the EFA with only 8 factors using the “fa” function
from the psych package, but I will not print the results just yet.

``` r
EFAresult2 = fa(Motivation_EFA, 
                      nfactors = 8, 
                      fm = "pa", 
                      rotate = "oblimin")
```
