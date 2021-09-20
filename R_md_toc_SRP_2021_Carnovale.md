Load Required Packages
======================

``` r
library(psych) # For descriptives
library(jtools) # For regression model summaries
library(inspectdf) # For EDA
library(summarytools) # For EDA
library(DataExplorer) # For EDA
library(TripleR) # For estimating Social Relations Model
library(stringi) # Data wrangling
library(tidymodels) # For various tidyverse functions
library(lavaan) # SEM
library(DescTools) # To implement Jonckheere's trend test
library(ggplot2) # Plots
```

``` r
installed.packages()[names(sessionInfo()$otherPkgs), "Version"]
```

    ##    DescTools       lavaan    yardstick workflowsets    workflows         tune        tidyr       tibble      rsample 
    ##    "0.99.43"      "0.6-9"      "0.0.8"      "0.0.2"      "0.2.2"      "0.1.5"      "1.1.3"      "3.1.2"      "0.1.0" 
    ##      recipes        purrr      parsnip    modeldata        infer        dplyr        dials       scales        broom 
    ##     "0.1.16"      "0.3.4"      "0.1.6"      "0.1.0"      "0.5.4"      "1.0.6"      "0.0.9"      "1.1.1"      "0.7.7" 
    ##   tidymodels      stringi      TripleR      ggplot2 DataExplorer summarytools    inspectdf       jtools        psych 
    ##      "0.1.3"      "1.4.6"      "1.5.3"      "3.3.4"      "0.8.2"      "0.9.6"     "0.0.11"      "2.1.3"  "1.9.12.31" 
    ##    rmarkdown     markdown 
    ##        "2.9"        "1.1"

``` r
#### Import data ####
# Baseline data
df_baseline = read.csv("https://raw.githubusercontent.com/michaelcarnovale/SRP-2021-Poster/main/Data/Baseline_Data_Carnovale.csv", 
header = T)
df_baseline = df_baseline[,-1] # Removing extra column

# Post-interaction round robin data
df_end = read.csv("https://raw.githubusercontent.com/michaelcarnovale/SRP-2021-Poster/main/Data/RoundRobin_Data_Carnovale.csv",
                  header = T)
df_end = df_end[,-1] # Removing extra column
```

Data Cleaning/Structuring/Management
====================================

``` r
#### Recoding variables of interest ####
df_end$letter = df_end$letter - 1
names(df_end) = c("Study_ID", "Letter", "Session_ID",
                  "ext1.a", "con1.a", "opn1.a", "agr1.a", "neu1.a", "agr2.a", "neu2.a", "ext2.a", "con2.a", "opn2.a",
                  "ext1.b", "con1.b", "opn1.b", "agr1.b", "neu1.b", "agr2.b", "neu2.b", "ext2.b", "con2.b", "opn2.b",
                  "ext1.c", "con1.c", "opn1.c", "agr1.c", "neu1.c", "agr2.c", "neu2.c", "ext2.c", "con2.c", "opn2.c",
                  "ext1.d", "con1.d", "opn1.d", "agr1.d", "neu1.d", "agr2.d", "neu2.d", "ext2.d", "con2.d", "opn2.d",
                  "ext1.e", "con1.e", "opn1.e", "agr1.e", "neu1.e", "agr2.e", "neu2.e", "ext2.e", "con2.e", "opn2.e",
                  "letter")

df_end$Session_ID = as.numeric(df_end$Session_ID) # Making a 'perceiver' variable
df_end$perceiver = df_end$Session_ID + (df_end$letter/10)
df_end = df_end %>%
  filter(Session_ID !=999, Session_ID !=2999) # Removing 'test' sessions run by RAs

#### Formatting the round robin data from wide to long ####
df_end = df_end %>%
  distinct(perceiver, .keep_all = T)
df_end = reshape(df_end,
                 varying = list(c("ext1.a", "ext1.b", "ext1.c", "ext1.d", "ext1.e"),
                                c("ext2.a", "ext2.b", "ext2.c", "ext2.d", "ext2.e"),
                                c("con1.a", "con1.b", "con1.c", "con1.d", "con1.e"),
                                c("con2.a", "con2.b", "con2.c", "con2.d", "con2.e"),
                                c("agr1.a", "agr1.b", "agr1.c", "agr1.d", "agr1.e"),
                                c("agr2.a", "agr2.b", "agr2.c", "agr2.d", "agr2.e"),
                                c("neu1.a", "neu1.b", "neu1.c", "neu1.d", "neu1.e"),
                                c("neu2.a", "neu2.b", "neu2.c", "neu2.d", "neu2.e"),
                                c("opn1.a", "opn1.b", "opn1.c", "opn1.d", "opn1.e"),
                                c("opn2.a", "opn2.b", "opn2.c", "opn2.d", "opn2.e")),
                 direction = "long",
                 v.names = c("ext1", "ext2", "con1", "con2", "agr1", "agr2", "neu1", "neu2", "opn1", "opn2"),
                 idvar = c("Session_ID", "perceiver"),
                 timevar = "letter")


#### Scoring variables ####

## Target variable
df_end$target = df_end$Session_ID + (df_end$letter/10)

## TIPI
df_end = df_end %>%
  mutate(ext1 = recode(ext1,
                       '1  Not at all' = '1',
                       '7  Extremely' = '7'),
         con1 = recode(con1,
                       '1  Not at all' = '1',
                       '7  Extremely' = '7'),
         agr1 = recode(agr1,
                       '1  Not at all' = '1',
                       '7  Extremely' = '7'),
         neu1 = recode(neu1,
                       '1  Not at all' = '1',
                       '7  Extremely' = '7'),
         opn1 = recode(opn1,
                       '1  Not at all' = '1',
                       '7  Extremely' = '7'),
         ext2 = recode(ext2,
                       '1  Not at all' = '1',
                       '7  Extremely' = '7'),
         con2 = recode(con2,
                       '1  Not at all' = '1',
                       '7  Extremely' = '7'),
         agr2 = recode(agr2,
                       '1  Not at all' = '1',
                       '7  Extremely' = '7'),
         neu2 = recode(neu2,
                       '1  Not at all' = '1',
                       '7  Extremely' = '7'),
         opn2 = recode(opn2,
                       '1  Not at all' = '1',
                       '7  Extremely' = '7')) %>%
  mutate(ext1 = as.numeric(ext1),
         con1 = as.numeric(con1),
         agr1 = as.numeric(agr1),
         neu1 = as.numeric(neu1),
         opn1 = as.numeric(opn1),
         ext2 = as.numeric(ext2),
         con2 = as.numeric(con2),
         agr2 = as.numeric(agr2),
         neu2 = as.numeric(neu2),
         opn2 = as.numeric(opn2)) 



## PID-5-BF (self-report)
df_baseline_names = names(df_baseline)
df_baseline[,df_baseline_names] = lapply(df_baseline[,df_baseline_names], as.numeric)
df_baseline = df_baseline %>%
  mutate(negativeaffect = rowMeans(select(., PID8_1, PID9_1, PID10_1, PID11_1, PID15_1), na.rm = T),
         detachment = rowMeans(select(., PID4_1, PID13_1, PID14_1, PID16_1, PID18_1), na.rm = T),
         antagonism = rowMeans(select(., PID17_1, PID19_1, PID20_1, PID22_1, PID25_1), na.rm = T),
         disinhibition = rowMeans(select(., PID1_1, PID2_1, PID3_1, PID5_1, PID6_1), na.rm = T),
         psychoticism = rowMeans(select(., PID7_1, PID12_1, PID21_1, PID23_1, PID24_1), na.rm = T))
```

Estimating Social Relations Models
==================================

The Social Relations Model was estimated for each TIPI item that was
rated in the round robin design.

``` r
#### Round robin models ####
neu1.rr = RR(neu1 ~ perceiver*target|Session_ID, data = df_end, na.rm = T)
neu2.rr = RR(neu2 ~ perceiver*target|Session_ID, data = df_end, na.rm = T)
ext1.rr = RR(ext1 ~ perceiver*target|Session_ID, data = df_end, na.rm = T)
ext2.rr = RR(ext2 ~ perceiver*target|Session_ID, data = df_end, na.rm = T)
opn1.rr = RR(opn1 ~ perceiver*target|Session_ID, data = df_end, na.rm = T)
opn2.rr = RR(opn2 ~ perceiver*target|Session_ID, data = df_end, na.rm = T)
agr1.rr = RR(agr1 ~ perceiver*target|Session_ID, data = df_end, na.rm = T)
agr2.rr = RR(agr2 ~ perceiver*target|Session_ID, data = df_end, na.rm = T)
con1.rr = RR(con1 ~ perceiver*target|Session_ID, data = df_end, na.rm = T)
con2.rr = RR(con2 ~ perceiver*target|Session_ID, data = df_end, na.rm = T)



## Putting perceiver effects in separate dataframe
df_p = neu1.rr$effects[,c(1, 3)]
df_p = merge(df_p, neu2.rr$effects[,c(1, 3)], by = "id")
df_p = merge(df_p, ext1.rr$effects[,c(1, 3)], by = "id")
df_p = merge(df_p, ext2.rr$effects[,c(1, 3)], by = "id")
df_p = merge(df_p, opn1.rr$effects[,c(1, 3)], by = "id")
df_p = merge(df_p, opn2.rr$effects[,c(1, 3)], by = "id")
df_p = merge(df_p, agr1.rr$effects[,c(1, 3)], by = "id") 
df_p = merge(df_p, agr2.rr$effects[,c(1, 3)], by = "id")
df_p = merge(df_p, con1.rr$effects[,c(1, 3)], by = "id")
df_p = merge(df_p, con2.rr$effects[,c(1, 3)], by = "id")

df_end_names = names(df_end)
df_end[,df_end_names] = lapply(df_end[,df_end_names], as.numeric)
test = merge(df_end, df_baseline, by = "Study_ID")

df_baseline2 = test %>%
  dplyr::rename(id = perceiver) %>%
  select(id, Study_ID, negativeaffect, detachment, antagonism, disinhibition, psychoticism,
         PID1_1:PID25_1, Session_ID, Age, Gender, Ethnicity)

df_full = merge(df_p, df_baseline2, by = "id")
df_full = df_full %>%
  distinct(id, .keep_all = T) %>%
  filter(Session_ID!=2067) # Coding errors in this session
```

``` r
##### Making total score ######
df_full = df_full %>%
  mutate(total = rowMeans(select(., negativeaffect, detachment, antagonism, disinhibition, psychoticism), 
                          na.rm = T))
```

Exploratory Data Analysis (Self-Report)
=======================================

``` r
## Descriptive stats (IVs only)
df_full %>% 
  select(negativeaffect, detachment, antagonism, disinhibition, psychoticism, total, Age) %>% 
  describe(quant = c(.25, .75))
```

    ##                vars   n  mean   sd median trimmed  mad min   max range  skew kurtosis   se Q0.25 Q0.75
    ## negativeaffect    1 145  2.46 0.67    2.4    2.47 0.59   1  3.80  2.80 -0.13    -0.53 0.06  2.00  3.00
    ## detachment        2 145  2.00 0.60    2.0    1.99 0.59   1  3.60  2.60  0.23    -0.64 0.05  1.60  2.40
    ## antagonism        3 145  1.58 0.53    1.4    1.51 0.59   1  3.40  2.40  1.24     1.50 0.04  1.20  1.80
    ## disinhibition     4 145  1.93 0.70    1.8    1.88 0.89   1  3.80  2.80  0.51    -0.65 0.06  1.40  2.40
    ## psychoticism      5 145  2.11 0.73    2.0    2.08 0.89   1  3.80  2.80  0.22    -0.92 0.06  1.50  2.60
    ## total             6 145  2.02 0.46    2.0    2.01 0.47   1  3.08  2.08  0.08    -0.58 0.04  1.68  2.36
    ## Age               7 117 18.45 0.93   18.0   18.23 0.00  18 23.00  5.00  2.72     8.04 0.09 18.00 19.00

``` r
df_full %>% 
  select(PID1_1:PID25_1, Gender) %>% 
  freq()
```

    ## Frequencies  
    ## df_full$PID1_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     76     52.41          52.41     52.41          52.41
    ##           2     36     24.83          77.24     24.83          77.24
    ##           3     27     18.62          95.86     18.62          95.86
    ##           4      6      4.14         100.00      4.14         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID2_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     46     31.72          31.72     31.72          31.72
    ##           2     52     35.86          67.59     35.86          67.59
    ##           3     39     26.90          94.48     26.90          94.48
    ##           4      8      5.52         100.00      5.52         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID3_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     51     35.66          35.66     35.17          35.17
    ##           2     38     26.57          62.24     26.21          61.38
    ##           3     42     29.37          91.61     28.97          90.34
    ##           4     12      8.39         100.00      8.28          98.62
    ##        <NA>      2                               1.38         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID4_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     39     27.46          27.46     26.90          26.90
    ##           2     45     31.69          59.15     31.03          57.93
    ##           3     41     28.87          88.03     28.28          86.21
    ##           4     17     11.97         100.00     11.72          97.93
    ##        <NA>      3                               2.07         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID5_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     82     56.55          56.55     56.55          56.55
    ##           2     39     26.90          83.45     26.90          83.45
    ##           3     19     13.10          96.55     13.10          96.55
    ##           4      5      3.45         100.00      3.45         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID6_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     51     35.17          35.17     35.17          35.17
    ##           2     43     29.66          64.83     29.66          64.83
    ##           3     33     22.76          87.59     22.76          87.59
    ##           4     18     12.41         100.00     12.41         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID7_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     38     26.21          26.21     26.21          26.21
    ##           2     40     27.59          53.79     27.59          53.79
    ##           3     47     32.41          86.21     32.41          86.21
    ##           4     20     13.79         100.00     13.79         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID8_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     16     11.11          11.11     11.03          11.03
    ##           2     39     27.08          38.19     26.90          37.93
    ##           3     41     28.47          66.67     28.28          66.21
    ##           4     48     33.33         100.00     33.10          99.31
    ##        <NA>      1                               0.69         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID9_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     35     24.48          24.48     24.14          24.14
    ##           2     37     25.87          50.35     25.52          49.66
    ##           3     39     27.27          77.62     26.90          76.55
    ##           4     32     22.38         100.00     22.07          98.62
    ##        <NA>      2                               1.38         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID10_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     34     23.45          23.45     23.45          23.45
    ##           2     34     23.45          46.90     23.45          46.90
    ##           3     47     32.41          79.31     32.41          79.31
    ##           4     30     20.69         100.00     20.69         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID11_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     30     20.69          20.69     20.69          20.69
    ##           2     59     40.69          61.38     40.69          61.38
    ##           3     41     28.28          89.66     28.28          89.66
    ##           4     15     10.34         100.00     10.34         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID12_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     93     64.58          64.58     64.14          64.14
    ##           2     27     18.75          83.33     18.62          82.76
    ##           3     21     14.58          97.92     14.48          97.24
    ##           4      3      2.08         100.00      2.07          99.31
    ##        <NA>      1                               0.69         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID13_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     46     31.72          31.72     31.72          31.72
    ##           2     41     28.28          60.00     28.28          60.00
    ##           3     45     31.03          91.03     31.03          91.03
    ##           4     13      8.97         100.00      8.97         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID14_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     79     54.86          54.86     54.48          54.48
    ##           2     49     34.03          88.89     33.79          88.28
    ##           3     12      8.33          97.22      8.28          96.55
    ##           4      4      2.78         100.00      2.76          99.31
    ##        <NA>      1                               0.69         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID15_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     36     24.83          24.83     24.83          24.83
    ##           2     56     38.62          63.45     38.62          63.45
    ##           3     40     27.59          91.03     27.59          91.03
    ##           4     13      8.97         100.00      8.97         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID16_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     43     29.86          29.86     29.66          29.66
    ##           2     49     34.03          63.89     33.79          63.45
    ##           3     37     25.69          89.58     25.52          88.97
    ##           4     15     10.42         100.00     10.34          99.31
    ##        <NA>      1                               0.69         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID17_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1    112     77.24          77.24     77.24          77.24
    ##           2     21     14.48          91.72     14.48          91.72
    ##           3     10      6.90          98.62      6.90          98.62
    ##           4      2      1.38         100.00      1.38         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID18_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     66     45.52          45.52     45.52          45.52
    ##           2     43     29.66          75.17     29.66          75.17
    ##           3     31     21.38          96.55     21.38          96.55
    ##           4      5      3.45         100.00      3.45         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID19_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     49     33.79          33.79     33.79          33.79
    ##           2     48     33.10          66.90     33.10          66.90
    ##           3     40     27.59          94.48     27.59          94.48
    ##           4      8      5.52         100.00      5.52         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID20_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     91     63.19          63.19     62.76          62.76
    ##           2     41     28.47          91.67     28.28          91.03
    ##           3     12      8.33         100.00      8.28          99.31
    ##        <NA>      1                               0.69         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID21_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     42     29.17          29.17     28.97          28.97
    ##           2     41     28.47          57.64     28.28          57.24
    ##           3     38     26.39          84.03     26.21          83.45
    ##           4     23     15.97         100.00     15.86          99.31
    ##        <NA>      1                               0.69         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID22_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     93     64.58          64.58     64.14          64.14
    ##           2     34     23.61          88.19     23.45          87.59
    ##           3     14      9.72          97.92      9.66          97.24
    ##           4      3      2.08         100.00      2.07          99.31
    ##        <NA>      1                               0.69         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID23_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     37     25.52          25.52     25.52          25.52
    ##           2     34     23.45          48.97     23.45          48.97
    ##           3     46     31.72          80.69     31.72          80.69
    ##           4     28     19.31         100.00     19.31         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID24_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     62     42.76          42.76     42.76          42.76
    ##           2     42     28.97          71.72     28.97          71.72
    ##           3     31     21.38          93.10     21.38          93.10
    ##           4     10      6.90         100.00      6.90         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$PID25_1  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     91     62.76          62.76     62.76          62.76
    ##           2     28     19.31          82.07     19.31          82.07
    ##           3     20     13.79          95.86     13.79          95.86
    ##           4      6      4.14         100.00      4.14         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00
    ## 
    ## df_full$Gender  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1     43     29.66          29.66     29.66          29.66
    ##           2    102     70.34         100.00     70.34         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    145    100.00         100.00    100.00         100.00

``` r
## Univariate visualizations
# Categorical variables
df_full %>% 
  select(PID1_1:PID25_1, Gender) %>% 
  mutate_if(is.numeric, as.factor) %>%
  inspect_cat() %>%
  show_plot()
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
df_full %>% 
  select(PID1_1:PID25_1, Gender) %>% 
  mutate_if(is.numeric, as.factor) %>%
  plot_bar()
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-7-2.png)![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-7-3.png)![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-7-4.png)

``` r
# Numeric variables
df_full %>%
  select(negativeaffect, detachment, antagonism, disinhibition, psychoticism, total, Age) %>%
  plot_histogram()
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-7-5.png)

``` r
df_full %>%
  select(negativeaffect, detachment, antagonism, disinhibition, psychoticism, total, Age) %>%
  plot_density()
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-7-6.png)

Regression Models (Self-Report Data)
====================================

``` r
## Creating PCA variable for pos/neg perceiver effects
test_pca = df_full %>%
  select(neu1.a:con2.a) %>%
  principal(nfactors = 1)

df_full$perceiver_effect = test_pca$scores
```

``` r
## Making function to estimate models and correct p-values
model.function = function(var1, var2, var3, var4, var5, var6, data){
  m1 = lm(perceiver_effect ~ var1, data = data) %>% summ(scale = T, robust = T, confint = T, digits = 3) # Robust SEs
  m2 = lm(perceiver_effect ~ var2, data = data) %>% summ(scale = T, robust = T, confint = T, digits = 3)
  m3 = lm(perceiver_effect ~ var3, data = data) %>% summ(scale = T, robust = T, confint = T, digits = 3)
  m4 = lm(perceiver_effect ~ var4, data = data) %>% summ(scale = T, robust = T, confint = T, digits = 3)
  m5 = lm(perceiver_effect ~ var5, data = data) %>% summ(scale = T, robust = T, confint = T, digits = 3)
  m6 = lm(perceiver_effect ~ var6, data = data) %>% summ(scale = T, robust = T, confint = T, digits = 3)
  
  pval = c(rep(NA, 6)) # Vector to hold p-values associated with each b1 coefficient
  pval[1] = m1$coeftable[2, 5]
  pval[2] = m2$coeftable[2, 5]
  pval[3] = m3$coeftable[2, 5]
  pval[4] = m4$coeftable[2, 5]
  pval[5] = m5$coeftable[2, 5]
  pval[6] = m6$coeftable[2, 5]
  
  print(m1) # Printing model output
  print(m2)
  print(m3)
  print(m4)
  print(m5)
  print(m6)
  
  p.adjust(pval, method = "BH") # Adjusting b1 p-values with BH method
  
}

## Bivariate relations
model.function(df_full$negativeaffect, df_full$detachment, df_full$antagonism, 
               df_full$disinhibition, df_full$psychoticism, df_full$total, df_full) # Note the order of variables
```

    ## MODEL INFO:
    ## Observations: 145
    ## Dependent Variable: perceiver_effect
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,143) = 1.106, p = 0.295
    ## R² = 0.008
    ## Adj. R² = 0.001 
    ## 
    ## Standard errors: Robust, type = HC3
    ## ------------------------------------------------------------
    ##                       Est.     2.5%   97.5%   t val.       p
    ## ----------------- -------- -------- ------- -------- -------
    ## (Intercept)         -0.000   -0.165   0.165   -0.000   1.000
    ## var1                -0.088   -0.258   0.083   -1.017   0.311
    ## ------------------------------------------------------------
    ## 
    ## Continuous predictors are mean-centered and scaled by 1 s.d.
    ## MODEL INFO:
    ## Observations: 145
    ## Dependent Variable: perceiver_effect
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,143) = 2.969, p = 0.087
    ## R² = 0.020
    ## Adj. R² = 0.013 
    ## 
    ## Standard errors: Robust, type = HC3
    ## ------------------------------------------------------------
    ##                       Est.     2.5%   97.5%   t val.       p
    ## ----------------- -------- -------- ------- -------- -------
    ## (Intercept)         -0.000   -0.164   0.164   -0.000   1.000
    ## var2                -0.143   -0.301   0.016   -1.782   0.077
    ## ------------------------------------------------------------
    ## 
    ## Continuous predictors are mean-centered and scaled by 1 s.d.
    ## MODEL INFO:
    ## Observations: 145
    ## Dependent Variable: perceiver_effect
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,143) = 11.216, p = 0.001
    ## R² = 0.073
    ## Adj. R² = 0.066 
    ## 
    ## Standard errors: Robust, type = HC3
    ## -------------------------------------------------------------
    ##                       Est.     2.5%    97.5%   t val.       p
    ## ----------------- -------- -------- -------- -------- -------
    ## (Intercept)         -0.000   -0.160    0.160   -0.000   1.000
    ## var3                -0.270   -0.424   -0.116   -3.464   0.001
    ## -------------------------------------------------------------
    ## 
    ## Continuous predictors are mean-centered and scaled by 1 s.d.
    ## MODEL INFO:
    ## Observations: 145
    ## Dependent Variable: perceiver_effect
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,143) = 9.283, p = 0.003
    ## R² = 0.061
    ## Adj. R² = 0.054 
    ## 
    ## Standard errors: Robust, type = HC3
    ## -------------------------------------------------------------
    ##                       Est.     2.5%    97.5%   t val.       p
    ## ----------------- -------- -------- -------- -------- -------
    ## (Intercept)         -0.000   -0.161    0.161   -0.000   1.000
    ## var4                -0.247   -0.408   -0.086   -3.034   0.003
    ## -------------------------------------------------------------
    ## 
    ## Continuous predictors are mean-centered and scaled by 1 s.d.
    ## MODEL INFO:
    ## Observations: 145
    ## Dependent Variable: perceiver_effect
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,143) = 3.508, p = 0.063
    ## R² = 0.024
    ## Adj. R² = 0.017 
    ## 
    ## Standard errors: Robust, type = HC3
    ## ------------------------------------------------------------
    ##                       Est.     2.5%   97.5%   t val.       p
    ## ----------------- -------- -------- ------- -------- -------
    ## (Intercept)         -0.000   -0.164   0.164   -0.000   1.000
    ## var5                -0.155   -0.336   0.027   -1.687   0.094
    ## ------------------------------------------------------------
    ## 
    ## Continuous predictors are mean-centered and scaled by 1 s.d.
    ## MODEL INFO:
    ## Observations: 145
    ## Dependent Variable: perceiver_effect
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,143) = 9.561, p = 0.002
    ## R² = 0.063
    ## Adj. R² = 0.056 
    ## 
    ## Standard errors: Robust, type = HC3
    ## -------------------------------------------------------------
    ##                       Est.     2.5%    97.5%   t val.       p
    ## ----------------- -------- -------- -------- -------- -------
    ## (Intercept)         -0.000   -0.161    0.161   -0.000   1.000
    ## var6                -0.250   -0.412   -0.089   -3.070   0.003
    ## -------------------------------------------------------------
    ## 
    ## Continuous predictors are mean-centered and scaled by 1 s.d.

    ## [1] 0.310811778 0.112602410 0.004220175 0.005742392 0.112602410 0.005742392

Cleaning and Scoring Informant Data
===================================

``` r
df_inf = read.csv("https://raw.githubusercontent.com/michaelcarnovale/SRP-2021-Poster/main/Data/Informant_Data_Carnovale.csv",
                  header = T)
df_inf = df_inf[,-1] # Removing extra column

df_inf$Informant_ID = stri_trim_both(df_inf$Informant_ID)
df_inf$Informant_ID = stri_sub(df_inf$Informant_ID, 3)
```

``` r
## Scoring
df_inf = df_inf %>%
  mutate(pid1 = recode(informant_pid_1_1,
                       'Very False or Often False' = '1',
                       'Sometimes or Somewhat False' = '2',
                       'Sometimes or Somewhat True' = '3',
                       'Very True or Often True' = '4'),
         pid2 = recode(informant_pid_2_1,
                       'Very False or Often False' = '1',
                       'Sometimes or Somewhat False' = '2',
                       'Sometimes or Somewhat True' = '3',
                       'Very True or Often True' = '4'),
         pid3 = recode(informant_pid_3_1,
                       'Very False or Often False' = '1',
                       'Sometimes or Somewhat False' = '2',
                       'Sometimes or Somewhat True' = '3',
                       'Very True or Often True' = '4'),
         pid4 = recode(informant_pid_4_1,
                       'Very False or Often False' = '1',
                       'Sometimes or Somewhat False' = '2',
                       'Sometimes or Somewhat True' = '3',
                       'Very True or Often True' = '4'),
         pid5 = recode(informant_pid_5_1,
                       'Very False or Often False' = '1',
                       'Sometimes or Somewhat False' = '2',
                       'Sometimes or Somewhat True' = '3',
                       'Very True or Often True' = '4'),
         pid6 = recode(informant_pid_6_1,
                       'Very False or Often False' = '1',
                       'Sometimes or Somewhat False' = '2',
                       'Sometimes or Somewhat True' = '3',
                       'Very True or Often True' = '4'),
         pid7 = recode(informant_pid_7_1,
                       'Very False or Often False' = '1',
                       'Sometimes or Somewhat False' = '2',
                       'Sometimes or Somewhat True' = '3',
                       'Very True or Often True' = '4'),
         pid8 = recode(informant_pid_8_1,
                       'Very False or Often False' = '1',
                       'Sometimes or Somewhat False' = '2',
                       'Sometimes or Somewhat True' = '3',
                       'Very True or Often True' = '4'),
         pid9 = recode(informant_pid_9_1,
                       'Very False or Often False' = '1',
                       'Sometimes or Somewhat False' = '2',
                       'Sometimes or Somewhat True' = '3',
                       'Very True or Often True' = '4'),
         pid10 = recode(informant_pid_10_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid11 = recode(informant_pid_11_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid12 = recode(informant_pid_12_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid13 = recode(informant_pid_13_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid14 = recode(informant_pid_14_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid15 = recode(informant_pid_15_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid16 = recode(informant_pid_16_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid17 = recode(informant_pid_17_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid18 = recode(informant_pid_18_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid19 = recode(informant_pid_19_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid20 = recode(informant_pid_20_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid21 = recode(informant_pid_21_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid22 = recode(informant_pid_22_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid23 = recode(informant_pid_23_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid24 = recode(informant_pid_24_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4'),
         pid25 = recode(informant_pid_25_1,
                        'Very False or Often False' = '1',
                        'Sometimes or Somewhat False' = '2',
                        'Sometimes or Somewhat True' = '3',
                        'Very True or Often True' = '4')) %>%
  mutate(pid1 = as.numeric(pid1),
         pid2 = as.numeric(pid2),
         pid3 = as.numeric(pid3),
         pid4 = as.numeric(pid4),
         pid5 = as.numeric(pid5),
         pid6 = as.numeric(pid6),
         pid7 = as.numeric(pid7),
         pid8 = as.numeric(pid8),
         pid9 = as.numeric(pid9),
         pid10 = as.numeric(pid10),
         pid11 = as.numeric(pid11),
         pid12 = as.numeric(pid12),
         pid13 = as.numeric(pid13),
         pid14 = as.numeric(pid14),
         pid15 = as.numeric(pid15),
         pid16 = as.numeric(pid16),
         pid17 = as.numeric(pid17),
         pid18 = as.numeric(pid18),
         pid19 = as.numeric(pid19),
         pid20 = as.numeric(pid20),
         pid21 = as.numeric(pid21),
         pid22 = as.numeric(pid22),
         pid23 = as.numeric(pid23),
         pid24 = as.numeric(pid24),
         pid25 = as.numeric(pid25),
         Informant_ID = as.factor(Informant_ID))


df_inf = df_inf %>%
  select(Informant_ID, pid1:pid25)


df_inf = df_inf %>%
  mutate(negativeaffect_inf = rowMeans(select(., pid8, pid9, pid10, pid11, pid15), na.rm = T),
         detachment_inf = rowMeans(select(., pid4, pid13, pid14, pid16, pid18), na.rm = T),
         antagonism_inf = rowMeans(select(., pid17, pid19, pid20, pid22, pid25), na.rm = T),
         disinhibition_inf = rowMeans(select(., pid1, pid2, pid3, pid5, pid6), na.rm = T),
         psychoticism_inf = rowMeans(select(., pid7, pid12, pid21, pid23, pid24), na.rm = T))

df_inf = df_inf %>%
  drop_na(negativeaffect_inf:psychoticism_inf)


# Randomly select one informant if target has more than one informant
set.seed(2021)
df_inf = df_inf %>%
  group_by(Informant_ID) %>%
  sample_n(1) %>% 
  ungroup()

df_inf = df_inf %>%
  mutate(total_inf = rowMeans(select(., negativeaffect_inf, detachment_inf, antagonism_inf, disinhibition_inf, 
                                     psychoticism_inf), 
                          na.rm = T))
```

Merging with Target Data
========================

``` r
###### Merging data with target data #######
df_inf = df_inf %>%
  rename(Study_ID = Informant_ID)
df_full2 = merge(df_full, df_inf, by = "Study_ID")
```

Exploratory Data Analysis (Informant-Report)
============================================

``` r
df_full2 %>% 
  select(negativeaffect_inf, detachment_inf, antagonism_inf, disinhibition_inf, psychoticism_inf, total_inf) %>% 
  describe(quant = c(.25, .75))
```

    ##                    vars   n mean   sd median trimmed  mad  min  max range skew kurtosis   se Q0.25 Q0.75
    ## negativeaffect_inf    1 102 1.93 0.61   1.80    1.91 0.59 1.00 3.40  2.40 0.31    -0.79 0.06  1.40   2.4
    ## detachment_inf        2 102 1.90 0.58   1.80    1.85 0.59 1.00 3.40  2.40 0.58    -0.28 0.06  1.40   2.2
    ## antagonism_inf        3 102 1.42 0.58   1.20    1.30 0.30 1.00 4.00  3.00 2.15     5.25 0.06  1.00   1.6
    ## disinhibition_inf     4 102 1.67 0.65   1.60    1.60 0.89 1.00 3.60  2.60 0.66    -0.61 0.06  1.00   2.2
    ## psychoticism_inf      5 102 1.74 0.62   1.80    1.68 0.59 1.00 3.80  2.80 0.75     0.22 0.06  1.20   2.0
    ## total_inf             6 102 1.73 0.45   1.68    1.70 0.47 1.04 2.96  1.92 0.53    -0.37 0.04  1.36   2.0

``` r
# Univariate visualizations
df_full2 %>%
  select(negativeaffect_inf, detachment_inf, antagonism_inf, disinhibition_inf, psychoticism_inf, total_inf) %>%
  plot_histogram()
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
df_full2 %>%
  select(negativeaffect_inf, detachment_inf, antagonism_inf, disinhibition_inf, psychoticism_inf, total_inf) %>%
  plot_density()
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-13-2.png)

Regression Models (Informant-Report Data)
=========================================

``` r
## Univariate relations (note order of variables)
model.function(df_full2$negativeaffect_inf, df_full2$detachment_inf, df_full2$antagonism_inf,
               df_full2$disinhibition_inf, df_full2$psychoticism_inf, df_full2$total_inf, df_full2) 
```

    ## MODEL INFO:
    ## Observations: 102
    ## Dependent Variable: perceiver_effect
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,100) = 1.570, p = 0.213
    ## R² = 0.015
    ## Adj. R² = 0.006 
    ## 
    ## Standard errors: Robust, type = HC3
    ## ------------------------------------------------------------
    ##                       Est.     2.5%   97.5%   t val.       p
    ## ----------------- -------- -------- ------- -------- -------
    ## (Intercept)         -0.047   -0.228   0.133   -0.522   0.603
    ## var1                -0.113   -0.296   0.069   -1.232   0.221
    ## ------------------------------------------------------------
    ## 
    ## Continuous predictors are mean-centered and scaled by 1 s.d.
    ## MODEL INFO:
    ## Observations: 102
    ## Dependent Variable: perceiver_effect
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,100) = 0.018, p = 0.894
    ## R² = 0.000
    ## Adj. R² = -0.010 
    ## 
    ## Standard errors: Robust, type = HC3
    ## ------------------------------------------------------------
    ##                       Est.     2.5%   97.5%   t val.       p
    ## ----------------- -------- -------- ------- -------- -------
    ## (Intercept)         -0.047   -0.229   0.134   -0.519   0.605
    ## var2                -0.012   -0.182   0.158   -0.142   0.887
    ## ------------------------------------------------------------
    ## 
    ## Continuous predictors are mean-centered and scaled by 1 s.d.
    ## MODEL INFO:
    ## Observations: 102
    ## Dependent Variable: perceiver_effect
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,100) = 0.796, p = 0.374
    ## R² = 0.008
    ## Adj. R² = -0.002 
    ## 
    ## Standard errors: Robust, type = HC3
    ## ------------------------------------------------------------
    ##                       Est.     2.5%   97.5%   t val.       p
    ## ----------------- -------- -------- ------- -------- -------
    ## (Intercept)         -0.047   -0.229   0.134   -0.518   0.605
    ## var3                -0.081   -0.299   0.136   -0.740   0.461
    ## ------------------------------------------------------------
    ## 
    ## Continuous predictors are mean-centered and scaled by 1 s.d.
    ## MODEL INFO:
    ## Observations: 102
    ## Dependent Variable: perceiver_effect
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,100) = 1.563, p = 0.214
    ## R² = 0.015
    ## Adj. R² = 0.006 
    ## 
    ## Standard errors: Robust, type = HC3
    ## ------------------------------------------------------------
    ##                       Est.     2.5%   97.5%   t val.       p
    ## ----------------- -------- -------- ------- -------- -------
    ## (Intercept)         -0.047   -0.228   0.133   -0.522   0.603
    ## var4                -0.113   -0.293   0.066   -1.251   0.214
    ## ------------------------------------------------------------
    ## 
    ## Continuous predictors are mean-centered and scaled by 1 s.d.
    ## MODEL INFO:
    ## Observations: 102
    ## Dependent Variable: perceiver_effect
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,100) = 3.718, p = 0.057
    ## R² = 0.036
    ## Adj. R² = 0.026 
    ## 
    ## Standard errors: Robust, type = HC3
    ## -------------------------------------------------------------
    ##                       Est.     2.5%    97.5%   t val.       p
    ## ----------------- -------- -------- -------- -------- -------
    ## (Intercept)         -0.047   -0.226    0.131   -0.528   0.598
    ## var5                -0.173   -0.339   -0.007   -2.066   0.041
    ## -------------------------------------------------------------
    ## 
    ## Continuous predictors are mean-centered and scaled by 1 s.d.
    ## MODEL INFO:
    ## Observations: 102
    ## Dependent Variable: perceiver_effect
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,100) = 2.304, p = 0.132
    ## R² = 0.023
    ## Adj. R² = 0.013 
    ## 
    ## Standard errors: Robust, type = HC3
    ## ------------------------------------------------------------
    ##                       Est.     2.5%   97.5%   t val.       p
    ## ----------------- -------- -------- ------- -------- -------
    ## (Intercept)         -0.047   -0.227   0.132   -0.524   0.601
    ## var6                -0.137   -0.319   0.045   -1.492   0.139
    ## ------------------------------------------------------------
    ## 
    ## Continuous predictors are mean-centered and scaled by 1 s.d.

    ## [1] 0.3312239 0.8873776 0.5532971 0.3312239 0.2485159 0.3312239

Structural-After-Measurement SEMs
=================================

``` r
## Negative Affect
# Self-report
n_model_self = '
naffect =~ PID8_1 + PID9_1 +  PID10_1 + PID11_1 + PID15_1 
pos =~ neu1.a + neu2.a + ext1.a + ext2.a + opn1.a + opn2.a + agr1.a + agr2.a + con1.a + con2.a

pos ~ naffect'
n_out_self = sam(n_model_self, data = df_full, std.lv = T)
summary(n_out_self, ci = T) 
```

    ## This is lavaan 0.6-9 -- using the SAM approach to SEM
    ## 
    ##   SAM method                                     LOCAL
    ##   Mapping matrix M method                           ML
    ##   Number of measurement blocks                       2
    ##   Estimator measurement part                        ML
    ##   Estimator  structural part                        ML
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           142         145
    ##                                                                   
    ## Summary Information Measurement + Structural:
    ## 
    ##   Block  Latent Nind   Chisq Df
    ##       1 naffect    5   7.285  5
    ##       2     pos   10 100.692 35
    ## 
    ##   Model-based reliability latent variables:
    ## 
    ##   naffect   pos
    ##     0.762 0.825
    ## 
    ##   Summary Information Structural part:
    ## 
    ##   chisq df pvalue cfi rmsea  srmr
    ##   0.006  2  0.997   1     0 0.005
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                              Twostep
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##   pos ~                                                                 
    ##     naffect          -0.095    0.106   -0.890    0.373   -0.303    0.114
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##     naffect           1.000                               1.000    1.000
    ##    .pos               1.000                               1.000    1.000

``` r
# Informant
n_model_inf = '
naffect =~ pid8 + pid9 +  pid10 + pid11 + pid15 
pos =~ neu1.a + neu2.a + ext1.a + ext2.a + opn1.a + opn2.a + agr1.a + agr2.a + con1.a + con2.a

pos ~ naffect'
n_out_inf = sam(n_model_inf, data = df_full2, std.lv = T)
summary(n_out_inf, fit.measures = T, ci = T) 
```

    ## This is lavaan 0.6-9 -- using the SAM approach to SEM
    ## 
    ##   SAM method                                     LOCAL
    ##   Mapping matrix M method                           ML
    ##   Number of measurement blocks                       2
    ##   Estimator measurement part                        ML
    ##   Estimator  structural part                        ML
    ##                                                       
    ##   Number of observations                           102
    ##                                                       
    ## Summary Information Measurement + Structural:
    ## 
    ##   Block  Latent Nind  Chisq Df
    ##       1 naffect    5  7.963  5
    ##       2     pos   10 81.199 35
    ## 
    ##   Model-based reliability latent variables:
    ## 
    ##   naffect   pos
    ##     0.723 0.813
    ## 
    ##   Summary Information Structural part:
    ## 
    ##   chisq df pvalue cfi rmsea  srmr
    ##   0.018  2  0.991   1     0 0.011
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                              Twostep
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##   pos ~                                                                 
    ##     naffect          -0.137    0.131   -1.048    0.295   -0.394    0.119
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##     naffect           1.000                               1.000    1.000
    ##    .pos               1.000                               1.000    1.000

``` r
## Detachment
# Self-report
det_model_self = '
det =~ PID4_1 + PID13_1 + PID14_1 + PID16_1 + PID18_1 
pos =~ neu1.a + neu2.a + ext1.a + ext2.a + opn1.a + opn2.a + agr1.a + agr2.a + con1.a + con2.a

pos ~ det'
det_out_self = sam(det_model_self, data = df_full, std.lv = T)
summary(det_out_self, fit.measures = T, ci = T) 
```

    ## This is lavaan 0.6-9 -- using the SAM approach to SEM
    ## 
    ##   SAM method                                     LOCAL
    ##   Mapping matrix M method                           ML
    ##   Number of measurement blocks                       2
    ##   Estimator measurement part                        ML
    ##   Estimator  structural part                        ML
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           140         145
    ##                                                                   
    ## Summary Information Measurement + Structural:
    ## 
    ##   Block Latent Nind  Chisq Df
    ##       1    det    5 10.887  5
    ##       2    pos   10 95.913 35
    ## 
    ##   Model-based reliability latent variables:
    ## 
    ##    det   pos
    ##   0.79 0.825
    ## 
    ##   Summary Information Structural part:
    ## 
    ##   chisq df pvalue cfi rmsea  srmr
    ##   0.115  2  0.944   1     0 0.023
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                              Twostep
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##   pos ~                                                                 
    ##     det              -0.200    0.108   -1.860    0.063   -0.411    0.011
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##     det               1.000                               1.000    1.000
    ##    .pos               1.000                               1.000    1.000

``` r
# Informant
det_model_inf = '
det =~ pid4 + pid13 + pid14 + pid16 + pid18
pos =~ neu1.a + neu2.a + ext1.a + ext2.a + opn1.a + opn2.a + agr1.a + agr2.a + con1.a + con2.a

pos ~ det'
det_out_inf = sam(det_model_inf, data = df_full2, std.lv = T)
summary(det_out_inf, fit.measures = T, ci = T) 
```

    ## This is lavaan 0.6-9 -- using the SAM approach to SEM
    ## 
    ##   SAM method                                     LOCAL
    ##   Mapping matrix M method                           ML
    ##   Number of measurement blocks                       2
    ##   Estimator measurement part                        ML
    ##   Estimator  structural part                        ML
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           100         102
    ##                                                                   
    ## Summary Information Measurement + Structural:
    ## 
    ##   Block Latent Nind  Chisq Df
    ##       1    det    5 11.008  5
    ##       2    pos   10 82.061 35
    ## 
    ##   Model-based reliability latent variables:
    ## 
    ##     det   pos
    ##   0.699 0.816
    ## 
    ##   Summary Information Structural part:
    ## 
    ##   chisq df pvalue cfi rmsea  srmr
    ##       0  2      1   1     0 0.001
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                              Twostep
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##   pos ~                                                                 
    ##     det               0.034    0.133    0.257    0.797   -0.226    0.294
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##     det               1.000                               1.000    1.000
    ##    .pos               1.000                               1.000    1.000

``` r
## Antagonism
# Self-report
ant_model_self = '
ant =~ PID17_1 + PID19_1 + PID20_1 + PID22_1 + PID25_1 
pos =~ neu1.a + neu2.a + ext1.a + ext2.a + opn1.a + opn2.a + agr1.a + agr2.a + con1.a + con2.a

pos ~ ant'
ant_out_self = sam(ant_model_self, data = df_full, std.lv = T)
summary(ant_out_self, fit.measures = T, ci = T) 
```

    ## This is lavaan 0.6-9 -- using the SAM approach to SEM
    ## 
    ##   SAM method                                     LOCAL
    ##   Mapping matrix M method                           ML
    ##   Number of measurement blocks                       2
    ##   Estimator measurement part                        ML
    ##   Estimator  structural part                        ML
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           143         145
    ##                                                                   
    ## Summary Information Measurement + Structural:
    ## 
    ##   Block Latent Nind   Chisq Df
    ##       1    ant    5  13.677  5
    ##       2    pos   10 102.962 35
    ## 
    ##   Model-based reliability latent variables:
    ## 
    ##    ant   pos
    ##   0.85 0.821
    ## 
    ##   Summary Information Structural part:
    ## 
    ##   chisq df pvalue cfi rmsea srmr
    ##   1.532  2  0.465   1     0 0.08
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                              Twostep
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##   pos ~                                                                 
    ##     ant              -0.373    0.108   -3.451    0.001   -0.585   -0.161
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##     ant               1.000                               1.000    1.000
    ##    .pos               1.000                               1.000    1.000

``` r
# Informant
ant_model_inf = '
ant =~ pid17 + pid19 + pid20 + pid22 + pid25 
pos =~ neu1.a + neu2.a + ext1.a + ext2.a + opn1.a + opn2.a + agr1.a + agr2.a + con1.a + con2.a

pos ~ ant'
ant_out_inf = sam(ant_model_inf, data = df_full2, std.lv = T)
summary(ant_out_inf, fit.measures = T, ci = T) 
```

    ## This is lavaan 0.6-9 -- using the SAM approach to SEM
    ## 
    ##   SAM method                                     LOCAL
    ##   Mapping matrix M method                           ML
    ##   Number of measurement blocks                       2
    ##   Estimator measurement part                        ML
    ##   Estimator  structural part                        ML
    ##                                                       
    ##   Number of observations                           102
    ##                                                       
    ## Summary Information Measurement + Structural:
    ## 
    ##   Block Latent Nind  Chisq Df
    ##       1    ant    5  9.588  5
    ##       2    pos   10 81.199 35
    ## 
    ##   Model-based reliability latent variables:
    ## 
    ##     ant   pos
    ##   0.867 0.813
    ## 
    ##   Summary Information Structural part:
    ## 
    ##   chisq df pvalue cfi rmsea  srmr
    ##   0.023  2  0.989   1     0 0.012
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                              Twostep
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##   pos ~                                                                 
    ##     ant              -0.145    0.119   -1.213    0.225   -0.379    0.089
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##     ant               1.000                               1.000    1.000
    ##    .pos               1.000                               1.000    1.000

``` r
## Disinhibition
# Self-report
dis_model_self = '
dis =~ PID1_1 + PID2_1 + PID3_1 + PID5_1 + PID6_1 
pos =~ neu1.a + neu2.a + ext1.a + ext2.a + opn1.a + opn2.a + agr1.a + agr2.a + con1.a + con2.a

pos ~ dis'
dis_out_self = sam(dis_model_self, data = df_full, std.lv = T)
summary(dis_out_self, fit.measures = T, ci = T) 
```

    ## This is lavaan 0.6-9 -- using the SAM approach to SEM
    ## 
    ##   SAM method                                     LOCAL
    ##   Mapping matrix M method                           ML
    ##   Number of measurement blocks                       2
    ##   Estimator measurement part                        ML
    ##   Estimator  structural part                        ML
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           143         145
    ##                                                                   
    ## Summary Information Measurement + Structural:
    ## 
    ##   Block Latent Nind   Chisq Df
    ##       1    dis    5  25.825  5
    ##       2    pos   10 104.406 35
    ## 
    ##   Model-based reliability latent variables:
    ## 
    ##     dis   pos
    ##   0.834 0.818
    ## 
    ##   Summary Information Structural part:
    ## 
    ##   chisq df pvalue cfi rmsea  srmr
    ##   0.596  2  0.742   1     0 0.051
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                              Twostep
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##   pos ~                                                                 
    ##     dis              -0.298    0.106   -2.795    0.005   -0.506   -0.089
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##     dis               1.000                               1.000    1.000
    ##    .pos               1.000                               1.000    1.000

``` r
# Informant
dis_model_inf = '
dis =~ pid1 + pid2 + pid3 + pid5 + pid6
pos =~ neu1.a + neu2.a + ext1.a + ext2.a + opn1.a + opn2.a + agr1.a + agr2.a + con1.a + con2.a

pos ~ dis'
dis_out_inf = sam(dis_model_inf, data = df_full2, std.lv = T)
summary(dis_out_inf, fit.measures = T, ci = T)
```

    ## This is lavaan 0.6-9 -- using the SAM approach to SEM
    ## 
    ##   SAM method                                     LOCAL
    ##   Mapping matrix M method                           ML
    ##   Number of measurement blocks                       2
    ##   Estimator measurement part                        ML
    ##   Estimator  structural part                        ML
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           100         102
    ##                                                                   
    ## Summary Information Measurement + Structural:
    ## 
    ##   Block Latent Nind  Chisq Df
    ##       1    dis    5  6.659  5
    ##       2    pos   10 80.109 35
    ## 
    ##   Model-based reliability latent variables:
    ## 
    ##     dis  pos
    ##   0.845 0.81
    ## 
    ##   Summary Information Structural part:
    ## 
    ##   chisq df pvalue cfi rmsea  srmr
    ##    0.04  2   0.98   1     0 0.016
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                              Twostep
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##   pos ~                                                                 
    ##     dis              -0.168    0.123   -1.364    0.173   -0.408    0.073
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##     dis               1.000                               1.000    1.000
    ##    .pos               1.000                               1.000    1.000

``` r
## Psychoticism
# Self-report
psych_model_self = '
psych =~ PID7_1 + PID12_1 + PID21_1 + PID23_1 + PID24_1 
pos =~ neu1.a + neu2.a + ext1.a + ext2.a + opn1.a + opn2.a + agr1.a + agr2.a + con1.a + con2.a

pos ~ psych'
psych_out_self = sam(psych_model_self, data = df_full, std.lv = T)
summary(psych_out_self, fit.measures = T, ci = T)
```

    ## This is lavaan 0.6-9 -- using the SAM approach to SEM
    ## 
    ##   SAM method                                     LOCAL
    ##   Mapping matrix M method                           ML
    ##   Number of measurement blocks                       2
    ##   Estimator measurement part                        ML
    ##   Estimator  structural part                        ML
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           143         145
    ##                                                                   
    ## Summary Information Measurement + Structural:
    ## 
    ##   Block Latent Nind   Chisq Df
    ##       1  psych    5  17.873  5
    ##       2    pos   10 103.299 35
    ## 
    ##   Model-based reliability latent variables:
    ## 
    ##   psych   pos
    ##   0.824 0.823
    ## 
    ##   Summary Information Structural part:
    ## 
    ##   chisq df pvalue cfi rmsea  srmr
    ##   0.048  2  0.976   1     0 0.015
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                              Twostep
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##   pos ~                                                                 
    ##     psych            -0.161    0.103   -1.558    0.119   -0.363    0.041
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##     psych             1.000                               1.000    1.000
    ##    .pos               1.000                               1.000    1.000

``` r
# Informant
psych_model_inf = '
psych =~ pid7 + pid12 + pid21 + pid23 + pid24 
pos =~ neu1.a + neu2.a + ext1.a + ext2.a + opn1.a + opn2.a + agr1.a + agr2.a + con1.a + con2.a

pos ~ psych'
psych_out_inf = sam(psych_model_inf, data = df_full2, std.lv = T)
summary(psych_out_inf, fit.measures = T, ci = T)
```

    ## This is lavaan 0.6-9 -- using the SAM approach to SEM
    ## 
    ##   SAM method                                     LOCAL
    ##   Mapping matrix M method                           ML
    ##   Number of measurement blocks                       2
    ##   Estimator measurement part                        ML
    ##   Estimator  structural part                        ML
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           101         102
    ##                                                                   
    ## Summary Information Measurement + Structural:
    ## 
    ##   Block Latent Nind  Chisq Df
    ##       1  psych    5  8.726  5
    ##       2    pos   10 80.246 35
    ## 
    ##   Model-based reliability latent variables:
    ## 
    ##   psych   pos
    ##    0.77 0.812
    ## 
    ##   Summary Information Structural part:
    ## 
    ##   chisq df pvalue cfi rmsea  srmr
    ##   0.067  2  0.967   1     0 0.021
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                              Twostep
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##   pos ~                                                                 
    ##     psych            -0.190    0.129   -1.476    0.140   -0.442    0.062
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##     psych             1.000                               1.000    1.000
    ##    .pos               1.000                               1.000    1.000

``` r
## Total scores
# Self-report
total_model_self = '
t =~ negativeaffect + detachment + antagonism + disinhibition + psychoticism
pos =~ neu1.a + neu2.a + ext1.a + ext2.a + opn1.a + opn2.a + agr1.a + agr2.a + con1.a + con2.a

pos ~ t'
total_out_self = sam(total_model_self, data = df_full, std.lv = T)
summary(total_out_self, fit.measures = T, ci = T)
```

    ## This is lavaan 0.6-9 -- using the SAM approach to SEM
    ## 
    ##   SAM method                                     LOCAL
    ##   Mapping matrix M method                           ML
    ##   Number of measurement blocks                       2
    ##   Estimator measurement part                        ML
    ##   Estimator  structural part                        ML
    ##                                                       
    ##   Number of observations                           145
    ##                                                       
    ## Summary Information Measurement + Structural:
    ## 
    ##   Block Latent Nind   Chisq Df
    ##       1      t    5   3.192  5
    ##       2    pos   10 103.472 35
    ## 
    ##   Model-based reliability latent variables:
    ## 
    ##       t   pos
    ##   0.792 0.822
    ## 
    ##   Summary Information Structural part:
    ## 
    ##   chisq df pvalue cfi rmsea  srmr
    ##   0.468  2  0.792   1     0 0.045
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                              Twostep
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##   pos ~                                                                 
    ##     t                -0.280    0.108   -2.592    0.010   -0.491   -0.068
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##     t                 1.000                               1.000    1.000
    ##    .pos               1.000                               1.000    1.000

``` r
# Informant
total_model_inf = '
t =~ negativeaffect_inf + detachment_inf + antagonism_inf + disinhibition_inf + psychoticism_inf
pos =~ neu1.a + neu2.a + ext1.a + ext2.a + opn1.a + opn2.a + agr1.a + agr2.a + con1.a + con2.a

pos ~ t'
total_out_inf = sam(total_model_inf, data = df_full2, std.lv = T)
summary(total_out_inf, fit.measures = T, ci = T)
```

    ## This is lavaan 0.6-9 -- using the SAM approach to SEM
    ## 
    ##   SAM method                                     LOCAL
    ##   Mapping matrix M method                           ML
    ##   Number of measurement blocks                       2
    ##   Estimator measurement part                        ML
    ##   Estimator  structural part                        ML
    ##                                                       
    ##   Number of observations                           102
    ##                                                       
    ## Summary Information Measurement + Structural:
    ## 
    ##   Block Latent Nind  Chisq Df
    ##       1      t    5 17.782  5
    ##       2    pos   10 81.199 35
    ## 
    ##   Model-based reliability latent variables:
    ## 
    ##       t   pos
    ##   0.856 0.813
    ## 
    ##   Summary Information Structural part:
    ## 
    ##   chisq df pvalue cfi rmsea  srmr
    ##   0.045  2  0.978   1     0 0.017
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                              Twostep
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##   pos ~                                                                 
    ##     t                -0.171    0.121   -1.420    0.156   -0.408    0.065
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    ##     t                 1.000                               1.000    1.000
    ##    .pos               1.000                               1.000    1.000

``` r
## Adjusting p-values of regression coefficients for multiple testing

pvals = c(.372, .235, .063, .267, .001, .392, .005, .098, .119, .195, .010, .079)
p.adjust(pvals, method = "BH") # self-antagonism, self-disinhibition, and self-total still statistically significant
```

    ##  [1] 0.3920000 0.3133333 0.1890000 0.3204000 0.0120000 0.3920000 0.0300000 0.1960000 0.2040000 0.2925000 0.0400000
    ## [12] 0.1896000

Jonckheere’s Trend Test
=======================

Tests null hypothesis that k1 = k2 = k3 = kn, where kn is ordinal level
‘n’

``` r
# Antagonism items
JonckheereTerpstraTest(df_full$perceiver_effect, ordered(df_full$PID17_1))
```

    ## 
    ##  Jonckheere-Terpstra test
    ## 
    ## data:  df_full$perceiver_effect and ordered(df_full$PID17_1)
    ## JT = 1595, p-value = 0.06882
    ## alternative hypothesis: two.sided

``` r
JonckheereTerpstraTest(df_full$perceiver_effect, ordered(df_full$PID19_1))
```

    ## 
    ##  Jonckheere-Terpstra test
    ## 
    ## data:  df_full$perceiver_effect and ordered(df_full$PID19_1)
    ## JT = 3716, p-value = 0.8515
    ## alternative hypothesis: two.sided

``` r
JonckheereTerpstraTest(df_full$perceiver_effect, ordered(df_full$PID20_1))
```

    ## 
    ##  Jonckheere-Terpstra test
    ## 
    ## data:  df_full$perceiver_effect and ordered(df_full$PID20_1)
    ## JT = 2303, p-value = 0.1495
    ## alternative hypothesis: two.sided

``` r
JonckheereTerpstraTest(df_full$perceiver_effect, ordered(df_full$PID22_1))
```

    ## 
    ##  Jonckheere-Terpstra test
    ## 
    ## data:  df_full$perceiver_effect and ordered(df_full$PID22_1)
    ## JT = 1732, p-value = 0.0001044
    ## alternative hypothesis: two.sided

``` r
JonckheereTerpstraTest(df_full$perceiver_effect, ordered(df_full$PID25_1))
```

    ## 
    ##  Jonckheere-Terpstra test
    ## 
    ## data:  df_full$perceiver_effect and ordered(df_full$PID25_1)
    ## JT = 2237, p-value = 0.01053
    ## alternative hypothesis: two.sided

``` r
# Disinhibition items
JonckheereTerpstraTest(df_full$perceiver_effect, ordered(df_full$PID1_1))
```

    ## 
    ##  Jonckheere-Terpstra test
    ## 
    ## data:  df_full$perceiver_effect and ordered(df_full$PID1_1)
    ## JT = 2492, p-value = 0.002552
    ## alternative hypothesis: two.sided

``` r
JonckheereTerpstraTest(df_full$perceiver_effect, ordered(df_full$PID2_1))
```

    ## 
    ##  Jonckheere-Terpstra test
    ## 
    ## data:  df_full$perceiver_effect and ordered(df_full$PID2_1)
    ## JT = 2870, p-value = 0.004676
    ## alternative hypothesis: two.sided

``` r
JonckheereTerpstraTest(df_full$perceiver_effect, ordered(df_full$PID3_1))
```

    ## 
    ##  Jonckheere-Terpstra test
    ## 
    ## data:  df_full$perceiver_effect and ordered(df_full$PID3_1)
    ## JT = 2938, p-value = 0.01197
    ## alternative hypothesis: two.sided

``` r
JonckheereTerpstraTest(df_full$perceiver_effect, ordered(df_full$PID5_1))
```

    ## 
    ##  Jonckheere-Terpstra test
    ## 
    ## data:  df_full$perceiver_effect and ordered(df_full$PID5_1)
    ## JT = 2502, p-value = 0.02221
    ## alternative hypothesis: two.sided

``` r
JonckheereTerpstraTest(df_full$perceiver_effect, ordered(df_full$PID6_1))
```

    ## 
    ##  Jonckheere-Terpstra test
    ## 
    ## data:  df_full$perceiver_effect and ordered(df_full$PID6_1)
    ## JT = 3338, p-value = 0.1058
    ## alternative hypothesis: two.sided

``` r
# Adjusting p-values for multiple testing
pvals.jt = c(.06882, .8515, .1495, .0001044, .01053, .002552, .004676, .01197, .02221, .1058)
p.adjust(pvals.jt, method = "BH") # Items 22, 25 from antagonism; 1, 2, 3, 5 from disinhibition still statistically significant
```

    ##  [1] 0.09831429 0.85150000 0.16611111 0.00104400 0.02394000 0.01276000 0.01558667 0.02394000 0.03701667 0.13225000

Plots
=====

``` r
ggplot(df_full, aes(negativeaffect, perceiver_effect))+
  geom_smooth(method = "lm", color = "cyan")+
  geom_point(color = "cornflowerblue")+
  labs(x = "Negative Affect (Self)", y = "Perceiver Effect (Neg-Pos)")+
  theme(axis.title = element_text(size = 19))
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
ggplot(df_full, aes(detachment, perceiver_effect))+
  geom_smooth(method = "lm", color = "antiquewhite1")+
  geom_point(color = "antiquewhite4")+
  labs(x = "Detachment (Self)", y = "Perceiver Effect (Neg-Pos)")+
  theme(axis.title = element_text(size = 19))
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-17-2.png)

``` r
ggplot(df_full, aes(antagonism, perceiver_effect))+
  geom_smooth(method = "lm", color = "deeppink1")+
  geom_point(color = "brown2")+
  labs(x = "Antagonism (Self)", y = "Perceiver Effect (Neg-Pos)")+
  theme(axis.title = element_text(size = 19))
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-17-3.png)

``` r
ggplot(df_full, aes(disinhibition, perceiver_effect))+
  geom_smooth(method = "lm", color = "darkorchid1")+
  geom_point(color = "darkmagenta")+
  labs(x = "Disinhibition (Self)", y = "Perceiver Effect (Neg-Pos)")+
  theme(axis.title = element_text(size = 19))
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-17-4.png)

``` r
ggplot(df_full, aes(psychoticism, perceiver_effect))+
  geom_smooth(method = "lm", color = "darkseagreen2")+
  geom_point(color = "darkolivegreen")+
  labs(x = "Psychoticism (Self)", y = "Perceiver Effect (Neg-Pos)")+
  theme(axis.title = element_text(size = 19))
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-17-5.png)

``` r
ggplot(df_full, aes(total, perceiver_effect))+
  geom_smooth(method = "lm", color = "beige")+
  geom_point(color = "darkgreen")+
  labs(x = "Total (Self)", y = "Perceiver Effect (Neg-Pos)")+
  theme(axis.title = element_text(size = 19))
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-17-6.png)

``` r
# Informant
ggplot(df_full2, aes(negativeaffect_inf, perceiver_effect))+
  geom_smooth(method = "lm", color = "cyan")+
  geom_point(color = "cornflowerblue")+
  labs(x = "Negative Affect (Inf)", y = "Perceiver Effect (Neg-Pos)")+
  theme(axis.title = element_text(size = 19))
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-17-7.png)

``` r
ggplot(df_full2, aes(detachment_inf, perceiver_effect))+
  geom_smooth(method = "lm", color = "antiquewhite1")+
  geom_point(color = "antiquewhite4")+
  labs(x = "Detachment (Inf)", y = "Perceiver Effect (Neg-Pos)")+
  theme(axis.title = element_text(size = 19))
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-17-8.png)

``` r
ggplot(df_full2, aes(antagonism_inf, perceiver_effect))+
  geom_smooth(method = "lm", color = "deeppink1")+
  geom_point(color = "brown2")+
  labs(x = "Antagonism (Inf)", y = "Perceiver Effect (Neg-Pos)")+
  theme(axis.title = element_text(size = 19))
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-17-9.png)

``` r
ggplot(df_full2, aes(disinhibition_inf, perceiver_effect))+
  geom_smooth(method = "lm", color = "darkorchid1")+
  geom_point(color = "darkmagenta")+
  labs(x = "Disinhibition (Inf)", y = "Perceiver Effect (Neg-Pos)")+
  theme(axis.title = element_text(size = 19))
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-17-10.png)

``` r
ggplot(df_full2, aes(psychoticism_inf, perceiver_effect))+
  geom_smooth(method = "lm", color = "darkseagreen2")+
  geom_point(color = "darkolivegreen")+
  labs(x = "Psychoticism (Inf)", y = "Perceiver Effect (Neg-Pos)")+
  theme(axis.title = element_text(size = 19))
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-17-11.png)

``` r
ggplot(df_full2, aes(total_inf, perceiver_effect))+
  geom_smooth(method = "lm", color = "beige")+
  geom_point(color = "darkgreen")+
  labs(x = "Total (Inf)", y = "Perceiver Effect (Neg-Pos)")+
  theme(axis.title = element_text(size = 19))
```

![](R_md_toc_SRP_2021_Carnovale_files/figure-markdown_github/unnamed-chunk-17-12.png)
