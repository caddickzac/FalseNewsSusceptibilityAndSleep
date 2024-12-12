#-------------------------------------------------------------------------------------------------------------------------

#####################################################
# Belief Ratings Simple Scoring: For Sleep Duration #
#####################################################

# Calculate the number of participants per group
AnalysisData_ItemLevel %>%
  group_by(NewsType, SleepDurationAvg_Binned) %>%
  summarise(
    Ratings_Scored_n = n_distinct(Participant),
    .groups = 'drop'
  )

# Sleep Duration: single point (sleep duration hrs binned)
## Note: This figure is the top half of Figure 4 in the publication
SD_Plot <- AnalysisData_ItemLevel %>%
  mutate(NewsType = factor(NewsType, levels = c("Real", "Fake"))) %>% # flip order
  group_by(Participant, NewsType, SleepDurationAvg_Binned) %>%
  summarise(
    Ratings_Scored_M = mean(Ratings_Scored, na.rm = TRUE),
    Ratings_Scored_n = n(),
    Ratings_Scored_SD = sd(Ratings_Scored, na.rm = TRUE),
    Ratings_Scored_SE = Ratings_Scored_SD / sqrt(Ratings_Scored_n),
    Ratings_Scored_CI_High = Ratings_Scored_M + Ratings_Scored_SE * 1.96,
    Ratings_Scored_CI_Low = Ratings_Scored_M - Ratings_Scored_SE * 1.96,
    SleepDurationAvg = mean(SleepDurationAvg, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = SleepDurationAvg_Binned, y = Ratings_Scored_M)) +
  geom_boxplot(aes(y = Ratings_Scored_M), outlier.size = 0.5, color = "lightgray") +
  stat_summary(fun = median, geom = "crossbar", width = 0.8, color = "red", size = 0.5) + # median
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 0), 
               geom = "errorbar", width = 0.8, linetype = "dotted", color = "black", size = 0.5) + # mean
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) + # Change y-axis labels to percentage
  scale_x_discrete(expand = expansion(add = 1)) + # Add space on the x-axis
  coord_cartesian(ylim = c(0, 1)) + # Set the y-axis limits without removing data points
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5), # Center-align x-axis text
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = NA), # Change facet background to white
    strip.text = element_text(face = "bold"), # Make facet labels bold
    plot.margin = margin(10, 10, 10, 10) # Add margins to fit annotations
  ) +
  facet_grid(. ~ NewsType,
             labeller = labeller(NewsType = c(Fake = "False Items", Real = "True Items"))) +
  labs(x = "Sleep Duration (h)", 
       y = 'Correctly Identified (%)'
       # y = 'Scored Ratings (avg.)'
  ) +
  geom_text(aes(x = 5.7, y = .68, label = "Better"), size = 2.5, angle = 90, vjust = 1, color='darkgray') +
  geom_text(aes(x = 5.7, y = .33, label = "Worse"), size = 2.5, angle = 90, vjust = 1, color='darkgray') +
  geom_segment(aes(x = 5.8, xend = 5.8, y = .85, yend = .99), arrow = arrow(length = unit(0.2, "cm")), color='darkgray') +
  geom_segment(aes(x = 5.8, xend = 5.8, y = .15, yend = .01), arrow = arrow(length = unit(0.2, "cm")), color='darkgray')

SD_Plot
# ggsave(SD_Plot, file='SD_Plot.png', dpi=300, height=2, width=4)

#------------------------------------------------------------------------------------------------------------------------------------

####################################################
# Belief Ratings Simple Scoring: For Social Jetlag #
####################################################

# Calculate the number of participants per group
AnalysisData_ItemLevel %>%
  group_by(NewsType, SocialJetlag_ABS_Binned) %>%
  summarise(
    Ratings_Scored_n = n_distinct(Participant),
    .groups = 'drop'
  )

# Sleep Duration: single point (sleep duration hrs binned)
## Note: This figure is the top half of Figure 5 in the publication
SJL_Plot <- AnalysisData_ItemLevel %>%
  mutate(NewsType = factor(NewsType, levels = c("Real", "Fake"))) %>% # flip order
  group_by(Participant, NewsType, SocialJetlag_ABS_Binned) %>%
  summarise(
    Ratings_Scored_M = mean(Ratings_Scored, na.rm = TRUE),
    Ratings_Scored_n = n(),
    Ratings_Scored_SD = sd(Ratings_Scored, na.rm = TRUE),
    Ratings_Scored_SE = Ratings_Scored_SD / sqrt(Ratings_Scored_n),
    Ratings_Scored_CI_High = Ratings_Scored_M + Ratings_Scored_SE * 1.96,
    Ratings_Scored_CI_Low = Ratings_Scored_M - Ratings_Scored_SE * 1.96,
    SocialJetlag_ABS = mean(SocialJetlag_ABS, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = SocialJetlag_ABS_Binned, y = Ratings_Scored_M)) +
  geom_boxplot(aes(y = Ratings_Scored_M), outlier.size = 0.5, color = "lightgray") +
  stat_summary(fun = median, geom = "crossbar", width = 0.8, color = "red", size = 0.5) + # median
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 0), 
               geom = "errorbar", width = 0.8, linetype = "dotted", color = "black", size = 0.5) + # mean
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) + # Change y-axis labels to percentage
  scale_x_discrete(expand = expansion(add = 1)) + # Add space on the x-axis
  coord_cartesian(ylim = c(0, 1)) + # Set the y-axis limits without removing data points
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5), # Center-align x-axis text
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = NA), # Change facet background to white
    strip.text = element_text(face = "bold"), # Make facet labels bold
    plot.margin = margin(10, 10, 10, 10) # Add margins to fit annotations
  ) +
  facet_grid(. ~ NewsType,
             labeller = labeller(NewsType = c(Fake = "False Items", Real = "True Items"))) +
  labs(x = "Social Jetlag (h)",
       y = 'Correctly Identified (%)'
  ) +
  geom_text(aes(x = 5.7, y = .68, label = "Better"), size = 2.5, angle = 90, vjust = 1, color='darkgray') +
  geom_text(aes(x = 5.7, y = .33, label = "Worse"), size = 2.5, angle = 90, vjust = 1, color='darkgray') +
  geom_segment(aes(x = 5.8, xend = 5.8, y = .85, yend = .99), arrow = arrow(length = unit(0.2, "cm")), color='darkgray') +
  geom_segment(aes(x = 5.8, xend = 5.8, y = .15, yend = .01), arrow = arrow(length = unit(0.2, "cm")), color='darkgray')

SJL_Plot
# ggsave(SJL_Plot, file='SJL_Plot.png', dpi=300, height=2, width=4)

#----------------------------------------------------------------------------------------------------------------------

######################
# Confidence Ratings # 
######################

# Filter the data for Misinformation only and pivot longer
combined_data <- AnalysisData_MeasureLevel %>%
  filter(NewsType == "Fake") %>%
  select(Participant, SleepDurationAvg_Binned, SocialJetlag_ABS_Binned, Pct_Confidently_Correct, Pct_Confidently_Incorrect) %>%
  pivot_longer(cols = c(Pct_Confidently_Correct, Pct_Confidently_Incorrect), 
               names_to = "Belief_Confidence_Category", 
               values_to = "Pct_Confident") %>%
  pivot_longer(cols = c(SleepDurationAvg_Binned, SocialJetlag_ABS_Binned), 
               names_to = "Sleep_Measure", 
               values_to = "Sleep_Bin")

# Filter the data for the specific bins
sleep_duration_data <- combined_data %>%
  filter(Sleep_Measure == "SleepDurationAvg_Binned" & Sleep_Bin %in% c("4-6", "6-7", "7-8", "8-9", "9-10"))

social_jetlag_data <- combined_data %>%
  filter(Sleep_Measure == "SocialJetlag_ABS_Binned" & Sleep_Bin %in% c("0-1", "1-2", "2-3", "3-4", "4-5"))

bel_conf_cat_GM <- sleep_duration_data %>%
  group_by(Belief_Confidence_Category) %>% 
  summarise(GM = mean(Pct_Confident))

# Create the plot for Sleep Duration (confidence ratings)
## Note: This figure is the bottom half of Figure 4 in the publication
sleep_duration_plot <- sleep_duration_data %>%
  group_by(Participant, Belief_Confidence_Category, Sleep_Bin) %>%
  summarise(
    Pct_Confident_M = mean(Pct_Confident, na.rm = TRUE),
    Pct_Confident_n = n(),
    Pct_Confident_SD = sd(Pct_Confident, na.rm = TRUE),
    Pct_Confident_SE = Pct_Confident_SD / sqrt(Pct_Confident_n),
    Pct_Confident_CI_High = Pct_Confident_M + Pct_Confident_SE * 1.96,
    Pct_Confident_CI_Low = Pct_Confident_M - Pct_Confident_SE * 1.96,
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = Sleep_Bin, y = Pct_Confident_M)) +
  geom_boxplot(aes(y = Pct_Confident_M), outlier.size = 0.5, color = "lightgray") +
  stat_summary(fun = median, geom = "crossbar", width = .8, color = "red", size = 0.5) + # median
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 0), 
               geom = "errorbar", width = 0.8, linetype = "dotted", color = "black", size = 0.5) + # mean
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) + # Change y-axis labels to percentage
  scale_x_discrete(expand = expansion(add = 1)) + # Add space on the x-axis
  coord_cartesian(ylim = c(0, 100)) + # Set the y-axis limits without removing data points
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = .5), # Rotate x-axis text for better fit
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = NA), # Change facet background to white
    strip.text = element_text(face = "bold"), # Make facet labels bold
    plot.margin = margin(10, 10, 10, 10) # Add margins to fit annotations
  ) +
  facet_wrap(~ Belief_Confidence_Category, ncol = 2, labeller = labeller(Belief_Confidence_Category = c(Pct_Confidently_Correct = "Confidently Correct", 
                                                                                                        Pct_Confidently_Incorrect = "Confidently Incorrect"))) +
  labs(x = "Sleep Duration (h)", y = 'Confidence (%)') +
  geom_text(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Correct"), aes(x = 5.7, y = 68, label = "Better"), size = 2.5, angle = 90, vjust = 1, color='darkgray', fontface = "bold") +
  geom_text(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Correct"), aes(x = 5.7, y = 33, label = "Worse"), size = 2.5, angle = 90, vjust = 1, color='darkgray', fontface = "bold") +
  geom_segment(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Correct"), aes(x = 5.8, xend = 5.8, y = 85, yend = 99), arrow = arrow(length = unit(0.2, "cm")), color='darkgray') +
  geom_segment(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Correct"), aes(x = 5.8, xend = 5.8, y = 15, yend = 01), arrow = arrow(length = unit(0.2, "cm")), color='darkgray') +
  geom_text(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Incorrect"), aes(x = 5.7, y = 68, label = "Worse"), size = 2.5, angle = 90, vjust = 1, color='darkgray', fontface = "bold") +
  geom_text(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Incorrect"), aes(x = 5.7, y = 33, label = "Better"), size = 2.5, angle = 90, vjust = 1, color='darkgray', fontface = "bold") +
  geom_segment(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Incorrect"), aes(x = 5.8, xend = 5.8, y = 85, yend = 99), arrow = arrow(length = unit(0.2, "cm")), color='darkgray') +
  geom_segment(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Incorrect"), aes(x = 5.8, xend = 5.8, y = 15, yend = 01), arrow = arrow(length = unit(0.2, "cm")), color='darkgray')
sleep_duration_plot

# Create the plot for Social Jetlag (confidence ratings)
## Note: This figure is the bottom half of Figure 5 in the publication
social_jetlag_plot <- social_jetlag_data %>%
  group_by(Participant, Belief_Confidence_Category, Sleep_Bin) %>%
  summarise(
    Pct_Confident_M = mean(Pct_Confident, na.rm = TRUE),
    Pct_Confident_n = n(),
    Pct_Confident_SD = sd(Pct_Confident, na.rm = TRUE),
    Pct_Confident_SE = Pct_Confident_SD / sqrt(Pct_Confident_n),
    Pct_Confident_CI_High = Pct_Confident_M + Pct_Confident_SE * 1.96,
    Pct_Confident_CI_Low = Pct_Confident_M - Pct_Confident_SE * 1.96,
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = Sleep_Bin, y = Pct_Confident_M)) +
  geom_boxplot(aes(y = Pct_Confident_M), outlier.size = 0.5, color = "lightgray") +
  stat_summary(fun = median, geom = "crossbar", width = 0.8, color = "red", size = 0.5) + # median
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 0), 
               geom = "errorbar", width = 0.8, linetype = "dotted", color = "black", size = 0.5) + # mean
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) + # Change y-axis labels to percentage
  scale_x_discrete(expand = expansion(add = 1)) + # Add space on the x-axis
  coord_cartesian(ylim = c(0, 100)) + # Set the y-axis limits without removing data points
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = .5), # Rotate x-axis text for better fit
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = NA), # Change facet background to white
    strip.text = element_text(face = "bold"), # Make facet labels bold
    plot.margin = margin(10, 10, 10, 10) # Add margins to fit annotations
  ) +
  facet_wrap(~ Belief_Confidence_Category, ncol = 2, labeller = labeller(Belief_Confidence_Category = c(Pct_Confidently_Correct = "Confidently Correct", 
                                                                                                        Pct_Confidently_Incorrect = "Confidently Incorrect"))) +
  labs(x = "Social Jetlag (h)", y = 'Confidence (%)') +
  geom_text(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Correct"), aes(x = 5.7, y = 68, label = "Better"), size = 2.5, angle = 90, vjust = 1, color='darkgray', fontface = "bold") +
  geom_text(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Correct"), aes(x = 5.7, y = 33, label = "Worse"), size = 2.5, angle = 90, vjust = 1, color='darkgray', fontface = "bold") +
  geom_segment(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Correct"), aes(x = 5.8, xend = 5.8, y = 85, yend = 99), arrow = arrow(length = unit(0.2, "cm")), color='darkgray') +
  geom_segment(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Correct"), aes(x = 5.8, xend = 5.8, y = 15, yend = 01), arrow = arrow(length = unit(0.2, "cm")), color='darkgray') +
  geom_text(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Incorrect"), aes(x = 5.7, y = 68, label = "Worse"), size = 2.5, angle = 90, vjust = 1, color='darkgray', fontface = "bold") +
  geom_text(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Incorrect"), aes(x = 5.7, y = 33, label = "Better"), size = 2.5, angle = 90, vjust = 1, color='darkgray', fontface = "bold") +
  geom_segment(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Incorrect"), aes(x = 5.8, xend = 5.8, y = 85, yend = 99), arrow = arrow(length = unit(0.2, "cm")), color='darkgray') +
  geom_segment(data = data.frame(Belief_Confidence_Category = "Pct_Confidently_Incorrect"), aes(x = 5.8, xend = 5.8, y = 15, yend = 01), arrow = arrow(length = unit(0.2, "cm")), color='darkgray')

social_jetlag_plot

#----------------------------------------------------------------------------------------------------------------------

#################
# Combine Plots #
#################

# Figure 4
sleep_duration_combined_plot <- SD_Plot / sleep_duration_plot + plot_layout(ncol=1, heights=c(1,1))
ggsave(sleep_duration_combined_plot, file='sleep_duration_combined_plot.png', dpi=300, height=4, width=5)

# Figure 5
social_jetlag_combined_plot <- SJL_Plot / social_jetlag_plot + plot_layout(ncol=1, heights=c(1,1))
ggsave(social_jetlag_combined_plot, file='social_jetlag_combined_plot.png', dpi=300, height=4, width=5)

#----------------------------------------------------------------------------------------------------------------------
