#-------------------------------------------------------------------------------------------------------------------------

# import libraries 
library("tidyverse")
library('lmerTest')
library('r2glmm')
library('patchwork')
library("ggtext")
library("scales")
library('sessioninfo')

#-------------------------------------------------------------------------------------------------------------------------

######################
# Measure Level Data #
######################

# Import data
AnalysisData_MeasureLevel <- tibble(read.csv("AnalysisData_MeasureLevel.csv"))

# Remove apostrophe from binned data (added for protecting against excel changing values to dates)
AnalysisData_MeasureLevel$SocialJetlag_ABS_Binned <- sub("^'", "", AnalysisData_MeasureLevel$SocialJetlag_ABS_Binned)
AnalysisData_MeasureLevel$SleepDurationAvg_Binned <- sub("^'", "", AnalysisData_MeasureLevel$SleepDurationAvg_Binned)

# Preview data
head(AnalysisData_MeasureLevel)

# data dictionary:
## "Participant" = Unique participant identifier
## "NewsMeasure" = Used to identify which of four sub-components for news headline task (e.g., Fake-Covid)
## "Sample" = Conservative or Liberal sampling pool
## "Ratings_M" = Average belief ratings for specific news measure
## "Ratings_Scored" = Accuracy of belief ratings for specific news measure (1=100% correct, 0=0% correct)
## "SocialJetlag_ABS" = the absolute value of social jetlag (which measures total shifting from work to non-work days)
## "SocialJetlag_ABS_Binned" = social jetlag binned into whole hour ranges
## "Chronotype" = Chronotype calculated from sleep and wake times for work and non-work days. 
## "SleepDurationAvg" = The average sleep duration that a participant sleeps (calculated from wake and sleep times for work and non-work days)
## "SleepDurationAvg_Binned" = average sleep durations binned between whole hours (e.g., 7.5 would be 7-8)
## "SampleCoded" = Sample effects coding used for mixed-effects models (conservative=.5, liberal=-.5)
## "CRT" = Count of correct items in Cognitive Reflection Test (0-4)
## "HL" = Count of correct items in Health Literacy test (0-6)
## "Age" = Participant Age
## "Edu" = Participant Education (2=I did not attend high school, 3=I did not complete high school, 4=I completed high school or got a G.E.D., 5=I completed some college classes but did not receive a degree, 6=I received an associate's degree, 7=I received a bachelor’s degree, 8=I attended graduate school but did not receive a degree, 9=I received a graduate degree (master's, MBA, Ph.D., etc).)
## "Gender" = Gender (choices: Male, Female, Non-binary, Other/prefer not to say)
## "NewsType" = Either Fake or Real for news headline task
## "NewsDomain" = Either Covid or The Big Lie for domain of news headline task
## "NewsTypeCoded" = NewsType effects coding used for mixed-effects models (fake news=-.5, real news=.5)
## "NewsDomainCoded" = NewsDomain effects coding used for mixed-effects models (covid=-.5, the big lie = .5)
## "GenderCoded" = Gender effects coding used for mixed-effects models (male=.5, female=-.5, other values = NA)
##  Note: For four variables below: "belief confidence ratings >50 were labeled as “confident” (>50) and “not confident” (≤50) allowing categorization of “confidently-correct,” and “confidently-incorrect” responses."
## "Pct_Confidently_Correct" = An average of confidently-correct responses (which were scored as a 1; other responses given a 0 (see note above)
## "Pct_Unconfidently_Correct" = An average of Unconfidently-correct responses (which were scored as a 1; other responses given a 0 (see note above)
## "Pct_Confidently_Incorrect" = An average of confidently-incorrect responses (which were scored as a 1; other responses given a 0 (see note above)
## "Pct_Unconfidently_Incorrect" = An average of unconfidently-incorrect responses (which were scored as a 1; other responses given a 0 (see note above)

#-------------------------------------------------------------------------------------------------------------------------

###################
# Item Level Data #
###################

# Import data
AnalysisData_ItemLevel <- tibble(read.csv("AnalysisData_ItemLevel.csv"))

# Remove apostrophe from binned data (added for protecting against excel changing values to dates)
AnalysisData_ItemLevel$SocialJetlag_ABS_Binned <- sub("^'", "", AnalysisData_ItemLevel$SocialJetlag_ABS_Binned)
AnalysisData_ItemLevel$SleepDurationAvg_Binned <- sub("^'", "", AnalysisData_ItemLevel$SleepDurationAvg_Binned)

# Omit Confidence Ratings
AnalysisData_ItemLevel <- AnalysisData_ItemLevel %>% 
  filter(RatingsCategory=="belief") # only include belief ratings (confidence ratings included in uploaded dataset for those interested)


# Preview data
head(AnalysisData_ItemLevel)

# data dictionary: 
## "Participant" = Unique participant identifier
## "Sample" = Conservative or Liberal sampling pool
## "ItemNum" = Item number for news headline task (1-15) each measure has it's own number (i.e., fake-covid, real-covid, fake-the big lie, real-the big lie)
## "NewsType" = Either Fake or Real for news headline task
## "NewsDomain" = Either Covid or The Big Lie for domain of news headline task
## "RatingsCategory" = Either Belief or Confidence for type or rating provided by participant
## "Ratings" = 0-100 rating for eith belief or confidence of news headline task item
## "Ratings_Scored" = To assess overall accuracy, each news item was scored as “correct” (1) if rated between 51-100 (incorrect if placed between 0-50) for true news items and between 0-49 (incorrect: 50-100) for false items (0). An "NA" value was given for confidence ratings. 
## "Conservatism" = 1-7 ratings for how conservative a participant identifies themself (1="Extremely liberal", 2="Liberal", 3="Slightly liberal", 4="Moderate; middle of the road", 5="Slightly conservative", 6="Conservative", 7="Extremely conservative")
## "Age" = Participant Age
## "Edu" = Participant Education (2=I did not attend high school, 3=I did not complete high school, 4=I completed high school or got a G.E.D., 5=I completed some college classes but did not receive a degree, 6=I received an associate's degree, 7=I received a bachelor’s degree, 8=I attended graduate school but did not receive a degree, 9=I received a graduate degree (master's, MBA, Ph.D., etc).)
## "Gender" = Gender (choices: Male, Female, Non-binary, Other/prefer not to say)
## "Ethnic" = Ethnicity answer (choices: American Indian / Alaska Native, Asian, Black / African American, Hispanic / Latino, Other, White / Caucasian)
## "CRT" = Count of correct items in Cognitive Reflection Test (0-4)
## "HL" = Count of correct items in Health Literacy test (0-6)
## "SleepDurationAvg" = The average sleep duration that a participant sleeps (calculated from wake and sleep times for work and non-work days)
## "SleepDurationAvg_Binned" = average sleep durations binned between whole hours (e.g., 7.5 would be 7-8)
## "Chronotype" = Chronotype calculated from sleep and wake times for work and non-work days. 
## "SocialJetlag" = The amount of social jetlag people experience (could be positive or negative value depending on if they go to bed earlier or later on non-workdays)
## "SocialJetlag_ABS" = the absolute value of social jetlag (which measures total shifting from work to non-work days)
## "SocialJetlag_ABS_Binned" = social jetlag binned into whole hour ranges
## "CRT_Duration" = The amount of time (seconds) it took to complete Cognitive Reflection Test
## "HL_Duration" = The amount of time (seconds) it took to complete Health Literacy Test
## "FN_C_Duration" = The amount of time (seconds) it took to complete news headline task for Fake News-Covid items 
## "RN_C_Duration" = The amount of time (seconds) it took to complete news headline task for Real News-Covid items 
## "FN_TBL_Duration" = The amount of time (seconds) it took to complete news headline task for Fake News-The Big Lie items 
## "RN_TBL_Duration" = The amount of time (seconds) it took to complete news headline task for Real News-The Big Lie items 
## "MT_C_Duration" = The amount of time (seconds) it took to complete news headline task for Covid items (combining fake/real) 
## "MT_TBL_Duration" = The amount of time (seconds) it took to complete news headline task for The Big Lie items (combining fake/real) 
## "MT_FN_Duration" = The amount of time (seconds) it took to complete news headline task for fake news items (combining covid/the big lie)
## "MT_RN_Duration" = The amount of time (seconds) it took to complete news headline task for real news items (combining covid/the big lie)
## "MT_Duration" = The amount of time (seconds) it took to complete the entire news headline task
## "NewsTypeCoded" = NewsType effects coding used for mixed-effects models (fake news=-.5, real news=.5)
## "NewsDomainCoded" = NewsDomain effects coding used for mixed-effects models (covid=-.5, the big lie = .5)
## "SampleCoded" = Sample effects coding used for mixed-effects models (conservative=.5, liberal=-.5)
## "GenderCoded" = Gender effects coding used for mixed-effects models (male=.5, female=-.5, other values = NA)
## "NewsMeasure" = Used to identify which of four sub-components for news headline task (e.g., Fake-Covid)


#-------------------------------------------------------------------------------------------------------------------------

#########################
# R Environment Details #
#########################

# session_info() # function to capture session details

# ─ Session info ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.2.2 (2022-10-31 ucrt)
# os       Windows 10 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-12-11
# rstudio  2022.12.0+353 Elsbeth Geranium (desktop)
# pandoc   2.19.2 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# package     * version    date (UTC) lib source
# backports     1.4.1      2021-12-13 [1] CRAN (R 4.2.0)
# base64enc     0.1-3      2015-07-28 [1] CRAN (R 4.2.0)
# boot          1.3-28     2021-05-03 [2] CRAN (R 4.2.2)
# checkmate     2.3.1      2023-12-04 [1] CRAN (R 4.2.3)
# cli           3.6.0      2023-01-09 [1] CRAN (R 4.2.2)
# cluster       2.1.4      2022-08-22 [2] CRAN (R 4.2.2)
# colorspace    2.1-0      2023-01-23 [1] CRAN (R 4.2.2)
# commonmark    1.9.0      2023-03-17 [1] CRAN (R 4.2.3)
# crayon        1.5.2      2022-09-29 [1] CRAN (R 4.2.2)
# data.table    1.14.8     2023-02-17 [1] CRAN (R 4.2.2)
# digest        0.6.31     2022-12-11 [1] CRAN (R 4.2.2)
# dplyr       * 1.1.0      2023-01-29 [1] CRAN (R 4.2.2)
# ellipsis      0.3.2      2021-04-29 [1] CRAN (R 4.2.2)
# evaluate      0.20       2023-01-17 [1] CRAN (R 4.2.2)
# fansi         1.0.4      2023-01-22 [1] CRAN (R 4.2.2)
# farver        2.1.1      2022-07-06 [1] CRAN (R 4.2.2)
# fastmap       1.1.1      2023-02-24 [1] CRAN (R 4.2.2)
# forcats     * 1.0.0      2023-01-29 [1] CRAN (R 4.2.2)
# foreign       0.8-83     2022-09-28 [2] CRAN (R 4.2.2)
# Formula       1.2-5      2023-02-24 [1] CRAN (R 4.2.2)
# generics      0.1.3      2022-07-05 [1] CRAN (R 4.2.2)
# ggplot2     * 3.4.3      2023-08-14 [1] CRAN (R 4.2.3)
# ggtext      * 0.1.2      2022-09-16 [1] CRAN (R 4.2.3)
# glue          1.6.2      2022-02-24 [1] CRAN (R 4.2.2)
# gridExtra     2.3        2017-09-09 [1] CRAN (R 4.2.2)
# gridtext      0.1.5      2022-09-16 [1] CRAN (R 4.2.3)
# gtable        0.3.1      2022-09-01 [1] CRAN (R 4.2.2)
# Hmisc         5.1-1      2023-09-12 [1] CRAN (R 4.2.3)
# hms           1.1.2      2022-08-19 [1] CRAN (R 4.2.2)
# htmlTable     2.4.2      2023-10-29 [1] CRAN (R 4.2.3)
# htmltools     0.5.4      2022-12-07 [1] CRAN (R 4.2.2)
# htmlwidgets   1.6.2      2023-03-17 [1] CRAN (R 4.2.3)
# knitr         1.42       2023-01-25 [1] CRAN (R 4.2.2)
# labeling      0.4.2      2020-10-20 [1] CRAN (R 4.2.0)
# lattice       0.20-45    2021-09-22 [2] CRAN (R 4.2.2)
# lifecycle     1.0.3      2022-10-07 [1] CRAN (R 4.2.2)
# lme4        * 1.1-31     2022-11-01 [1] CRAN (R 4.2.2)
# lmerTest    * 3.1-3      2020-10-23 [1] CRAN (R 4.2.2)
# lubridate   * 1.9.2      2023-02-10 [1] CRAN (R 4.2.2)
# magrittr      2.0.3      2022-03-30 [1] CRAN (R 4.2.2)
# markdown      1.7        2023-05-16 [1] CRAN (R 4.2.3)
# MASS          7.3-58.1   2022-08-03 [2] CRAN (R 4.2.2)
# Matrix      * 1.5-1      2022-09-13 [2] CRAN (R 4.2.2)
# minqa         1.2.5      2022-10-19 [1] CRAN (R 4.2.2)
# munsell       0.5.0      2018-06-12 [1] CRAN (R 4.2.2)
# nlme          3.1-160    2022-10-10 [2] CRAN (R 4.2.2)
# nloptr        2.0.3      2022-05-26 [1] CRAN (R 4.2.2)
# nnet          7.3-18     2022-09-28 [2] CRAN (R 4.2.2)
# numDeriv      2016.8-1.1 2019-06-06 [1] CRAN (R 4.2.0)
# patchwork   * 1.2.0      2024-01-08 [1] CRAN (R 4.2.3)
# pillar        1.8.1      2022-08-19 [1] CRAN (R 4.2.2)
# pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 4.2.2)
# purrr       * 1.0.1      2023-01-10 [1] CRAN (R 4.2.2)
# r2glmm      * 0.1.2      2017-08-05 [1] CRAN (R 4.2.2)
# R6            2.5.1      2021-08-19 [1] CRAN (R 4.2.2)
# ragg          1.2.5      2023-01-12 [1] CRAN (R 4.2.2)
# Rcpp          1.0.10     2023-01-22 [1] CRAN (R 4.2.2)
# readr       * 2.1.4      2023-02-10 [1] CRAN (R 4.2.2)
# rlang         1.1.1      2023-04-28 [1] CRAN (R 4.2.3)
# rmarkdown     2.20       2023-01-19 [1] CRAN (R 4.2.2)
# rpart         4.1.19     2022-10-21 [2] CRAN (R 4.2.2)
# rstudioapi    0.14       2022-08-22 [1] CRAN (R 4.2.2)
# scales      * 1.2.1      2022-08-20 [1] CRAN (R 4.2.2)
# sessioninfo * 1.2.2      2021-12-06 [1] CRAN (R 4.2.3)
# stringi       1.7.12     2023-01-11 [1] CRAN (R 4.2.2)
# stringr     * 1.5.0      2022-12-02 [1] CRAN (R 4.2.2)
# systemfonts   1.0.4      2022-02-11 [1] CRAN (R 4.2.2)
# textshaping   0.3.6      2021-10-13 [1] CRAN (R 4.2.2)
# tibble      * 3.1.8      2022-07-22 [1] CRAN (R 4.2.2)
# tidyr       * 1.3.0      2023-01-24 [1] CRAN (R 4.2.2)
# tidyselect    1.2.0      2022-10-10 [1] CRAN (R 4.2.2)
# tidyverse   * 2.0.0      2023-02-22 [1] CRAN (R 4.2.3)
# timechange    0.2.0      2023-01-11 [1] CRAN (R 4.2.2)
# tzdb          0.3.0      2022-03-28 [1] CRAN (R 4.2.2)
# utf8          1.2.3      2023-01-31 [1] CRAN (R 4.2.2)
# vctrs         0.5.2      2023-01-23 [1] CRAN (R 4.2.2)
# withr         2.5.0      2022-03-03 [1] CRAN (R 4.2.2)
# xfun          0.39       2023-04-20 [1] CRAN (R 4.2.3)
# xml2          1.3.3      2021-11-30 [1] CRAN (R 4.2.2)

#-------------------------------------------------------------------------------------------------------------------------
