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

ml_ratingsScored_model_sleepduration_r2 = r2beta(model=ml_ratingsScored_model_sleepduration, partial=TRUE, method='nsj')
ml_ratingsScored_model_sleepduration_r2

# Social jetlag
ml_ratingsScored_model_SocialJetlag_ABS <- lmer(Ratings_Scored ~ NewsDomainCoded+ SampleCoded+CRT+HL+
                                                  SocialJetlag_ABS*NewsTypeCoded +
                                                  GenderCoded + Age + Edu + 
                                                  (1|Participant),
                                                data=AnalysisData_MeasureLevel)
summary(ml_ratingsScored_model_SocialJetlag_ABS)

ml_ratingsScored_model_socialjetlag_r2 = r2beta(model=ml_ratingsScored_model_SocialJetlag_ABS, partial=TRUE, method='nsj')
ml_ratingsScored_model_socialjetlag_r2

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
Conf_Correct_SleepDuration_Model_r2 = r2beta(model=Conf_Correct_SleepDuration_Model, partial=TRUE, method='nsj')
Conf_Correct_SleepDuration_Model_r2

# Pct_Confidently_Incorrect: Sleep Duration
Conf_Incorrect_SleepDuration_Model <- lmer(Pct_Confidently_Incorrect ~ NewsDomainCoded+ SampleCoded+CRT+HL+
                                             SleepDurationAvg*NewsTypeCoded + 
                                             GenderCoded + Age + Edu + 
                                             (1|Participant),
                                           data=AnalysisData_MeasureLevel)
summary(Conf_Incorrect_SleepDuration_Model)
Conf_Incorrect_SleepDuration_Model_r2 = r2beta(model=Conf_Incorrect_SleepDuration_Model, partial=TRUE, method='nsj')
Conf_Incorrect_SleepDuration_Model_r2

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
Conf_Correct_model_socialjetlag_r2 = r2beta(model=Conf_Correct_SocialJetlag_Model, partial=TRUE, method='nsj')
Conf_Correct_model_socialjetlag_r2

# Pct_Confidently_Incorrect: Social Jetlag 
Conf_Incorrect_SocialJetlag_Model <- lmer(Pct_Confidently_Incorrect ~ NewsDomainCoded+ SampleCoded+CRT+HL+
                                            SocialJetlag_ABS*NewsTypeCoded + 
                                            GenderCoded + Age + Edu + 
                                            (1|Participant),
                                          data=AnalysisData_MeasureLevel)
summary(Conf_Incorrect_SocialJetlag_Model)
Conf_Incorrect_model_socialjetlag_r2 = r2beta(model=Conf_Incorrect_SocialJetlag_Model, partial=TRUE, method='nsj')
Conf_Incorrect_model_socialjetlag_r2

#-------------------------------------------------------------------------------------------------------------------------
