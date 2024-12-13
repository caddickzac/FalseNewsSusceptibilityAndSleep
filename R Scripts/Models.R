#-------------------------------------------------------------------------------------------------------------------------

# Preview analysis dataset
head(AnalysisData_MeasureLevel)
# Output: 
# A tibble: 6 × 25
# Participant NewsMeas…¹ Sample Ratin…² Ratin…³ Socia…⁴ Socia…⁵ Chron…⁶ Sleep…⁷ Sleep…⁸ Sampl…⁹   CRT    HL   Age   Edu Gender NewsT…˟ NewsD…˟ NewsT…˟ NewsD…˟ Gende…˟ Pct_C…˟ Pct_U…˟ Pct_C…˟
# <int> <chr>      <chr>    <dbl>   <dbl>   <dbl> <chr>     <dbl>   <dbl> <chr>     <dbl> <int> <int> <int> <int> <chr>  <chr>   <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
# 1         333 Fake-Covid Conse…    0      1         0   0-1        25.5    7    7-8         0.5     4     6    42     6 Male   Fake    Covid      -0.5    -0.5     0.5   100       0      0   
# 2         333 Fake-The … Conse…    3.33   0.933     0   0-1        25.5    7    7-8         0.5     4     6    42     6 Male   Fake    TBL        -0.5     0.5     0.5    93.3     0      0   
# 3         333 Real-Covid Conse…   60      0.6       0   0-1        25.5    7    7-8         0.5     4     6    42     6 Male   Real    Covid       0.5    -0.5     0.5    60       0     40   
# 4         333 Real-The … Conse…   86.7    0.867     0   0-1        25.5    7    7-8         0.5     4     6    42     6 Male   Real    TBL         0.5     0.5     0.5    86.7     0     13.3 
# 5         334 Fake-Covid Conse…   26.9    0.867     0.5 0-1        27.9    4.71 4-6         0.5     4     6    57     7 Male   Fake    Covid      -0.5    -0.5     0.5    53.3    33.3   13.3 
# 6         334 Fake-The … Conse…   29.1    0.733     0.5 0-1        27.9    4.71 4-6         0.5     4     6    57     7 Male   Fake    TBL        -0.5     0.5     0.5    46.7    26.7    6.67
# # … with 1 more variable: Pct_Unconfidently_Incorrect <dbl>, and abbreviated variable names ¹​NewsMeasure, ²​Ratings_M, ³​Ratings_Scored, ⁴​SocialJetlag_ABS, ⁵​SocialJetlag_ABS_Binned,
# #   ⁶​Chronotype, ⁷​SleepDurationAvg, ⁸​SleepDurationAvg_Binned, ⁹​SampleCoded, ˟​NewsType, ˟​NewsDomain, ˟​NewsTypeCoded, ˟​NewsDomainCoded, ˟​GenderCoded, ˟​Pct_Confidently_Correct,
# #   ˟​Pct_Unconfidently_Correct, ˟​Pct_Confidently_Incorrect

#-------------------------------------------------------------------------------------------------------------------------

##########################
# Rating Accuracy Models #
##########################

# sleep duration
ml_ratingsScored_model_sleepduration <- lmer(Ratings_Scored ~ NewsDomainCoded+ SampleCoded+CRT+HL+
                                               SleepDurationAvg*NewsTypeCoded +
                                               GenderCoded + Age + Edu + 
                                               (1|Participant),
                                             data=AnalysisData_MeasureLevel)
summary(ml_ratingsScored_model_sleepduration)
# Output:
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Ratings_Scored ~ NewsDomainCoded + SampleCoded + CRT + HL + SleepDurationAvg *  
#     NewsTypeCoded + GenderCoded + Age + Edu + (1 | Participant)
#    Data: AnalysisData_MeasureLevel
# 
# REML criterion at convergence: -42.6
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -2.64215 -0.67904  0.07664  0.70215  2.17999 
# 
# Random effects:
#  Groups      Name        Variance Std.Dev.
#  Participant (Intercept) 0.006323 0.07952 
#  Residual                0.046021 0.21453 
# Number of obs: 884, groups:  Participant, 221
# 
# Fixed effects:
#                                  Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                     5.610e-01  9.536e-02  2.130e+02   5.883 1.55e-08 ***
# NewsDomainCoded                -2.142e-02  1.443e-02  6.600e+02  -1.484  0.13823    
# SampleCoded                    -1.834e-01  1.989e-02  2.130e+02  -9.220  < 2e-16 ***
# CRT                             1.156e-02  9.655e-03  2.130e+02   1.197  0.23268    
# HL                              1.804e-02  7.317e-03  2.130e+02   2.466  0.01446 *  
# SleepDurationAvg               -5.855e-03  8.333e-03  2.130e+02  -0.703  0.48299    
# NewsTypeCoded                   1.496e-01  1.013e-01  6.600e+02   1.478  0.13999    
# GenderCoded                     8.474e-03  1.916e-02  2.130e+02   0.442  0.65868    
# Age                            -1.087e-03  6.941e-04  2.130e+02  -1.566  0.11881    
# Edu                             7.523e-03  5.935e-03  2.130e+02   1.267  0.20637    
# SleepDurationAvg:NewsTypeCoded -3.619e-02  1.300e-02  6.600e+02  -2.784  0.00552 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) NwsDmC SmplCd CRT    HL     SlpDrA NwsTyC GndrCd Age    Edu   
# NewsDomnCdd  0.000                                                               
# SampleCoded -0.117  0.000                                                        
# CRT         -0.244  0.000  0.056                                                 
# HL          -0.430  0.000  0.071 -0.115                                          
# SlepDrtnAvg -0.738  0.000  0.103  0.134 -0.013                                   
# NewsTypeCdd  0.000  0.000  0.000  0.000  0.000  0.000                            
# GenderCoded -0.040  0.000 -0.300 -0.068  0.000  0.006  0.000                     
# Age         -0.414  0.000 -0.217 -0.026  0.140  0.136  0.000  0.103              
# Edu         -0.342  0.000  0.159 -0.159  0.093 -0.023  0.000  0.087 -0.081       
# SlpDrtA:NTC  0.000  0.000  0.000  0.000  0.000  0.000 -0.990  0.000  0.000  0.000

ml_ratingsScored_model_sleepduration_r2 = r2beta(model=ml_ratingsScored_model_sleepduration, partial=TRUE, method='nsj')
ml_ratingsScored_model_sleepduration_r2

# Output:
#                            Effect   Rsq upper.CL lower.CL
# 1                           Model 0.227    0.279    0.188
# 3                     SampleCoded 0.116    0.157    0.080
# 5                              HL 0.009    0.026    0.001
# 11 SleepDurationAvg:NewsTypeCoded 0.008    0.023    0.000
# 9                             Age 0.004    0.016    0.000
# 10                            Edu 0.002    0.013    0.000
# 4                             CRT 0.002    0.013    0.000
# 2                 NewsDomainCoded 0.002    0.013    0.000
# 7                   NewsTypeCoded 0.002    0.013    0.000
# 6                SleepDurationAvg 0.001    0.009    0.000
# 8                     GenderCoded 0.000    0.007    0.000

# Social jetlag
ml_ratingsScored_model_SocialJetlag_ABS <- lmer(Ratings_Scored ~ NewsDomainCoded+ SampleCoded+CRT+HL+
                                                  SocialJetlag_ABS*NewsTypeCoded +
                                                  GenderCoded + Age + Edu + 
                                                  (1|Participant),
                                                data=AnalysisData_MeasureLevel)
summary(ml_ratingsScored_model_SocialJetlag_ABS)
# Output:
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Ratings_Scored ~ NewsDomainCoded + SampleCoded + CRT + HL + SocialJetlag_ABS *  
#     NewsTypeCoded + GenderCoded + Age + Edu + (1 | Participant)
#    Data: AnalysisData_MeasureLevel
# 
# REML criterion at convergence: -43.6
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.7456 -0.6665  0.1087  0.6855  2.2044 
# 
# Random effects:
#  Groups      Name        Variance Std.Dev.
#  Participant (Intercept) 0.00624  0.07899 
#  Residual                0.04604  0.21457 
# Number of obs: 884, groups:  Participant, 221
# 
# Fixed effects:
#                                  Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                     5.378e-01  6.784e-02  2.130e+02   7.927 1.24e-13 ***
# NewsDomainCoded                -2.142e-02  1.443e-02  6.600e+02  -1.484  0.13832    
# SampleCoded                    -1.798e-01  1.983e-02  2.130e+02  -9.066  < 2e-16 ***
# CRT                             1.300e-02  9.557e-03  2.130e+02   1.360  0.17525    
# HL                              1.750e-02  7.311e-03  2.130e+02   2.394  0.01754 *  
# SocialJetlag_ABS               -1.077e-02  8.968e-03  2.130e+02  -1.201  0.23093    
# NewsTypeCoded                  -1.807e-01  2.368e-02  6.600e+02  -7.632 8.13e-14 ***
# GenderCoded                     4.874e-03  1.936e-02  2.130e+02   0.252  0.80145    
# Age                            -1.247e-03  7.115e-04  2.130e+02  -1.753  0.08112 .  
# Edu                             7.222e-03  5.923e-03  2.130e+02   1.219  0.22404    
# SocialJetlag_ABS:NewsTypeCoded  3.773e-02  1.380e-02  6.600e+02   2.734  0.00643 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) NwsDmC SmplCd CRT    HL     ScJ_ABS NwsTyC GndrCd Age    Edu   
# NewsDomnCdd  0.000                                                                
# SampleCoded -0.027  0.000                                                         
# CRT         -0.190  0.000  0.047                                                  
# HL          -0.634  0.000  0.067 -0.116                                           
# SclJtlg_ABS -0.322  0.000 -0.093 -0.046  0.054                                    
# NewsTypeCdd  0.000  0.000  0.000  0.000  0.000  0.000                             
# GenderCoded -0.100  0.000 -0.312 -0.076  0.009  0.158   0.000                     
# Age         -0.513  0.000 -0.250 -0.055  0.152  0.265   0.000  0.140              
# Edu         -0.514  0.000  0.159 -0.158  0.094  0.029   0.000  0.090 -0.068       
# ScJ_ABS:NTC  0.000  0.000  0.000  0.000  0.000  0.000  -0.793  0.000  0.000  0.000

ml_ratingsScored_model_socialjetlag_r2 = r2beta(model=ml_ratingsScored_model_SocialJetlag_ABS, partial=TRUE, method='nsj')
ml_ratingsScored_model_socialjetlag_r2
# Output: 
#                            Effect   Rsq upper.CL lower.CL
# 1                           Model 0.228    0.280    0.189
# 3                     SampleCoded 0.112    0.153    0.077
# 7                   NewsTypeCoded 0.055    0.087    0.030
# 5                              HL 0.009    0.025    0.001
# 11 SocialJetlag_ABS:NewsTypeCoded 0.007    0.023    0.000
# 9                             Age 0.005    0.018    0.000
# 4                             CRT 0.003    0.014    0.000
# 10                            Edu 0.002    0.013    0.000
# 6                SocialJetlag_ABS 0.002    0.013    0.000
# 2                 NewsDomainCoded 0.002    0.013    0.000
# 8                     GenderCoded 0.000    0.006    0.000

#-------------------------------------------------------------------------------------------------------------------------

#############################################
# Confidence Ratings Models: Sleep Duration #
#############################################

# Pct_Confidently_Correct: Sleep Duration
Conf_Correct_SleepDuration_Model <- lmer(Pct_Confidently_Correct ~ NewsDomainCoded+ SampleCoded+CRT+HL+
                                           SleepDurationAvg*NewsTypeCoded + 
                                           GenderCoded + Age + Edu + 
                                           (1|Participant),
                                         data=AnalysisData_MeasureLevel)
summary(Conf_Correct_SleepDuration_Model)
# Output:
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Pct_Confidently_Correct ~ NewsDomainCoded + SampleCoded + CRT +      HL + SleepDurationAvg * NewsTypeCoded + GenderCoded + Age +  
#   Edu + (1 | Participant)
# Data: AnalysisData_MeasureLevel
# 
# REML criterion at convergence: 8145.7
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -2.36106 -0.66129  0.04479  0.66144  2.51766 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev.
# Participant (Intercept) 254.9    15.97   
# Residual                455.3    21.34   
# Number of obs: 884, groups:  Participant, 221
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                     42.42855   13.71327 212.99997   3.094  0.00224 ** 
#   NewsDomainCoded                 -3.33333    1.43530 660.00001  -2.322  0.02051 *  
#   SampleCoded                    -17.91692    2.86106 212.99999  -6.262 2.06e-09 ***
#   CRT                              1.76126    1.38841 212.99999   1.269  0.20599    
# HL                               2.59654    1.05229 212.99998   2.468  0.01439 *  
#   SleepDurationAvg                -1.47793    1.19830 212.99999  -1.233  0.21880    
# NewsTypeCoded                   12.85631   10.07148 660.00001   1.277  0.20223    
# GenderCoded                      1.62603    2.75493 212.99999   0.590  0.55566    
# Age                             -0.03044    0.09982 212.99999  -0.305  0.76069    
# Edu                              0.35845    0.85355 212.99998   0.420  0.67494    
# SleepDurationAvg:NewsTypeCoded  -2.27216    1.29306 660.00001  -1.757  0.07935 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) NwsDmC SmplCd CRT    HL     SlpDrA NwsTyC GndrCd Age    Edu   
# NewsDomnCdd  0.000                                                               
# SampleCoded -0.117  0.000                                                        
# CRT         -0.244  0.000  0.056                                                 
# HL          -0.430  0.000  0.071 -0.115                                          
# SlepDrtnAvg -0.738  0.000  0.103  0.134 -0.013                                   
# NewsTypeCdd  0.000  0.000  0.000  0.000  0.000  0.000                            
# GenderCoded -0.040  0.000 -0.300 -0.068  0.000  0.006  0.000                     
# Age         -0.414  0.000 -0.217 -0.026  0.140  0.136  0.000  0.103              
# Edu         -0.342  0.000  0.159 -0.159  0.093 -0.023  0.000  0.087 -0.081       
# SlpDrtA:NTC  0.000  0.000  0.000  0.000  0.000  0.000 -0.990  0.000  0.000  0.000

Conf_Correct_SleepDuration_Model_r2 = r2beta(model=Conf_Correct_SleepDuration_Model, partial=TRUE, method='nsj')
Conf_Correct_SleepDuration_Model_r2
# Output:
#                            Effect   Rsq upper.CL lower.CL
# 1                           Model 0.135    0.185    0.105
# 3                     SampleCoded 0.084    0.122    0.053
# 5                              HL 0.014    0.034    0.003
# 2                 NewsDomainCoded 0.004    0.016    0.000
# 4                             CRT 0.004    0.016    0.000
# 6                SleepDurationAvg 0.004    0.016    0.000
# 11 SleepDurationAvg:NewsTypeCoded 0.002    0.013    0.000
# 7                   NewsTypeCoded 0.001    0.010    0.000
# 8                     GenderCoded 0.001    0.009    0.000
# 10                            Edu 0.000    0.007    0.000
# 9                             Age 0.000    0.007    0.000

# Pct_Confidently_Incorrect: Sleep Duration
Conf_Incorrect_SleepDuration_Model <- lmer(Pct_Confidently_Incorrect ~ NewsDomainCoded+ SampleCoded+CRT+HL+
                                             SleepDurationAvg*NewsTypeCoded + 
                                             GenderCoded + Age + Edu + 
                                             (1|Participant),
                                           data=AnalysisData_MeasureLevel)
summary(Conf_Incorrect_SleepDuration_Model)
# Output:
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Pct_Confidently_Incorrect ~ NewsDomainCoded + SampleCoded + CRT +      HL + SleepDurationAvg * NewsTypeCoded + GenderCoded + Age +  
#   Edu + (1 | Participant)
# Data: AnalysisData_MeasureLevel
# 
# REML criterion at convergence: 7724.9
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.4191 -0.5930 -0.1858  0.5066  3.2123 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev.
# Participant (Intercept) 106.5    10.32   
# Residual                302.2    17.38   
# Number of obs: 884, groups:  Participant, 221
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                     26.32150    9.63576 212.99998   2.732  0.00683 ** 
#   NewsDomainCoded                  1.28205    1.16940 660.00000   1.096  0.27334    
# SampleCoded                     14.79554    2.01035 213.00000   7.360 3.95e-12 ***
#   CRT                             -2.27404    0.97558 213.00000  -2.331  0.02069 *  
#   HL                              -1.02844    0.73940 212.99999  -1.391  0.16570    
# SleepDurationAvg                 0.52188    0.84200 212.99999   0.620  0.53604    
# NewsTypeCoded                  -32.71234    8.20572 660.00000  -3.987 7.45e-05 ***
#   GenderCoded                      0.02816    1.93578 213.00000   0.015  0.98841    
# Age                              0.09599    0.07014 213.00000   1.369  0.17257    
# Edu                             -0.41641    0.59975 212.99999  -0.694  0.48825    
# SleepDurationAvg:NewsTypeCoded   4.24906    1.05352 660.00000   4.033 6.15e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) NwsDmC SmplCd CRT    HL     SlpDrA NwsTyC GndrCd Age    Edu   
# NewsDomnCdd  0.000                                                               
# SampleCoded -0.117  0.000                                                        
# CRT         -0.244  0.000  0.056                                                 
# HL          -0.430  0.000  0.071 -0.115                                          
# SlepDrtnAvg -0.738  0.000  0.103  0.134 -0.013                                   
# NewsTypeCdd  0.000  0.000  0.000  0.000  0.000  0.000                            
# GenderCoded -0.040  0.000 -0.300 -0.068  0.000  0.006  0.000                     
# Age         -0.414  0.000 -0.217 -0.026  0.140  0.136  0.000  0.103              
# Edu         -0.342  0.000  0.159 -0.159  0.093 -0.023  0.000  0.087 -0.081       
# SlpDrtA:NTC  0.000  0.000  0.000  0.000  0.000  0.000 -0.990  0.000  0.000  0.000

Conf_Incorrect_SleepDuration_Model_r2 = r2beta(model=Conf_Incorrect_SleepDuration_Model, partial=TRUE, method='nsj')
Conf_Incorrect_SleepDuration_Model_r2
# Output:
#                            Effect   Rsq upper.CL lower.CL
# 1                           Model 0.162    0.213    0.128
# 3                     SampleCoded 0.099    0.137    0.065
# 11 SleepDurationAvg:NewsTypeCoded 0.013    0.032    0.003
# 7                   NewsTypeCoded 0.013    0.032    0.002
# 4                             CRT 0.011    0.028    0.001
# 5                              HL 0.004    0.016    0.000
# 9                             Age 0.004    0.016    0.000
# 2                 NewsDomainCoded 0.001    0.009    0.000
# 10                            Edu 0.001    0.009    0.000
# 6                SleepDurationAvg 0.001    0.009    0.000
# 8                     GenderCoded 0.000    0.006    0.000

#-------------------------------------------------------------------------------------------------------------------------

############################################
# Confidence Ratings Models: Social Jetlag #
############################################

# Pct_Confidently_Correct: Social Jetlag 
Conf_Correct_SocialJetlag_Model <- lmer(Pct_Confidently_Correct ~ NewsDomainCoded+ SampleCoded+CRT+HL+
                                          SocialJetlag_ABS*NewsTypeCoded + 
                                          GenderCoded + Age + Edu + 
                                          (1|Participant),
                                        data=AnalysisData_MeasureLevel)
summary(Conf_Correct_SocialJetlag_Model)
# Output:
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Pct_Confidently_Correct ~ NewsDomainCoded + SampleCoded + CRT +      HL + SocialJetlag_ABS * NewsTypeCoded + GenderCoded + Age +  
#   Edu + (1 | Participant)
# Data: AnalysisData_MeasureLevel
# 
# REML criterion at convergence: 8140.6
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -2.39614 -0.67236  0.04041  0.66212  2.53542 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev.
# Participant (Intercept) 257.5    16.05   
# Residual                451.4    21.25   
# Number of obs: 884, groups:  Participant, 221
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                     32.40766    9.79845 212.99999   3.307  0.00111 ** 
#   NewsDomainCoded                 -3.33333    1.42911 660.00000  -2.332  0.01998 *  
#   SampleCoded                    -17.34432    2.86423 213.00000  -6.055 6.25e-09 ***
#   CRT                              2.04061    1.38032 213.00000   1.478  0.14079    
# HL                               2.53510    1.05599 213.00000   2.401  0.01722 *  
#   SocialJetlag_ABS                -1.00840    1.29525 213.00000  -0.779  0.43712    
# NewsTypeCoded                  -10.18947    2.34487 660.00000  -4.345 1.61e-05 ***
#   GenderCoded                      1.30174    2.79600 213.00000   0.466  0.64200    
# Age                             -0.03486    0.10277 213.00000  -0.339  0.73481    
# Edu                              0.31483    0.85549 212.99999   0.368  0.71323    
# SocialJetlag_ABS:NewsTypeCoded   4.06431    1.36660 660.00000   2.974  0.00305 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) NwsDmC SmplCd CRT    HL     ScJ_ABS NwsTyC GndrCd Age    Edu   
# NewsDomnCdd  0.000                                                                
# SampleCoded -0.027  0.000                                                         
# CRT         -0.190  0.000  0.047                                                  
# HL          -0.634  0.000  0.067 -0.116                                           
# SclJtlg_ABS -0.322  0.000 -0.093 -0.046  0.054                                    
# NewsTypeCdd  0.000  0.000  0.000  0.000  0.000  0.000                             
# GenderCoded -0.100  0.000 -0.312 -0.076  0.009  0.158   0.000                     
# Age         -0.513  0.000 -0.250 -0.055  0.152  0.265   0.000  0.140              
# Edu         -0.514  0.000  0.159 -0.158  0.094  0.029   0.000  0.090 -0.068       
# ScJ_ABS:NTC  0.000  0.000  0.000  0.000  0.000  0.000  -0.793  0.000  0.000  0.000

Conf_Correct_model_socialjetlag_r2 = r2beta(model=Conf_Correct_SocialJetlag_Model, partial=TRUE, method='nsj')
Conf_Correct_model_socialjetlag_r2
# Output:
#                            Effect   Rsq upper.CL lower.CL
# 1                           Model 0.137    0.187    0.106
# 3                     SampleCoded 0.080    0.116    0.049
# 5                              HL 0.013    0.033    0.003
# 7                   NewsTypeCoded 0.013    0.032    0.003
# 11 SocialJetlag_ABS:NewsTypeCoded 0.006    0.021    0.000
# 4                             CRT 0.005    0.019    0.000
# 2                 NewsDomainCoded 0.004    0.016    0.000
# 6                SocialJetlag_ABS 0.001    0.011    0.000
# 8                     GenderCoded 0.001    0.008    0.000
# 10                            Edu 0.000    0.007    0.000
# 9                             Age 0.000    0.007    0.000

# Pct_Confidently_Incorrect: Social Jetlag 
Conf_Incorrect_SocialJetlag_Model <- lmer(Pct_Confidently_Incorrect ~ NewsDomainCoded+ SampleCoded+CRT+HL+
                                            SocialJetlag_ABS*NewsTypeCoded + 
                                            GenderCoded + Age + Edu + 
                                            (1|Participant),
                                          data=AnalysisData_MeasureLevel)
summary(Conf_Incorrect_SocialJetlag_Model)
# Output:
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Pct_Confidently_Incorrect ~ NewsDomainCoded + SampleCoded + CRT +      HL + SocialJetlag_ABS * NewsTypeCoded + GenderCoded + Age +  
#   Edu + (1 | Participant)
# Data: AnalysisData_MeasureLevel
# 
# REML criterion at convergence: 7734.9
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.3326 -0.5977 -0.1897  0.5008  3.7018 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev.
# Participant (Intercept) 104.4    10.22   
# Residual                307.4    17.53   
# Number of obs: 884, groups:  Participant, 221
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                     28.23798    6.85606 212.99999   4.119 5.45e-05 ***
#   NewsDomainCoded                  1.28205    1.17934 660.00001   1.087   0.2774    
# SampleCoded                     14.45645    2.00413 212.99999   7.213 9.42e-12 ***
#   CRT                             -2.40556    0.96582 212.99999  -2.491   0.0135 *  
#   HL                              -0.97743    0.73889 212.99999  -1.323   0.1873    
# SocialJetlag_ABS                 1.02127    0.90630 212.99999   1.127   0.2611    
# NewsTypeCoded                    3.44907    1.93505 660.00001   1.782   0.0751 .  
# GenderCoded                      0.36989    1.95639 212.99999   0.189   0.8502    
# Age                              0.11153    0.07191 212.99999   1.551   0.1224    
# Edu                             -0.38848    0.59859 212.99999  -0.649   0.5170    
# SocialJetlag_ABS:NewsTypeCoded  -2.50218    1.12776 660.00001  -2.219   0.0268 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) NwsDmC SmplCd CRT    HL     ScJ_ABS NwsTyC GndrCd Age    Edu   
# NewsDomnCdd  0.000                                                                
# SampleCoded -0.027  0.000                                                         
# CRT         -0.190  0.000  0.047                                                  
# HL          -0.634  0.000  0.067 -0.116                                           
# SclJtlg_ABS -0.322  0.000 -0.093 -0.046  0.054                                    
# NewsTypeCdd  0.000  0.000  0.000  0.000  0.000  0.000                             
# GenderCoded -0.100  0.000 -0.312 -0.076  0.009  0.158   0.000                     
# Age         -0.513  0.000 -0.250 -0.055  0.152  0.265   0.000  0.140              
# Edu         -0.514  0.000  0.159 -0.158  0.094  0.029   0.000  0.090 -0.068       
# ScJ_ABS:NTC  0.000  0.000  0.000  0.000  0.000  0.000  -0.793  0.000  0.000  0.000

Conf_Incorrect_model_socialjetlag_r2 = r2beta(model=Conf_Incorrect_SocialJetlag_Model, partial=TRUE, method='nsj')
Conf_Incorrect_model_socialjetlag_r2
# Output:
#                            Effect   Rsq upper.CL lower.CL
# 1                           Model 0.156    0.207    0.123
# 3                     SampleCoded 0.094    0.132    0.061
# 4                             CRT 0.012    0.031    0.002
# 9                             Age 0.005    0.018    0.000
# 11 SocialJetlag_ABS:NewsTypeCoded 0.004    0.017    0.000
# 5                              HL 0.003    0.015    0.000
# 7                   NewsTypeCoded 0.003    0.014    0.000
# 6                SocialJetlag_ABS 0.003    0.013    0.000
# 2                 NewsDomainCoded 0.001    0.009    0.000
# 10                            Edu 0.001    0.009    0.000
# 8                     GenderCoded 0.000    0.006    0.000

#-------------------------------------------------------------------------------------------------------------------------
