#-----------------------------------------------------------------------------------------------

# News Item Identification Accuracy: Sleep Duration Model

# Define labels with markdown formatting for specific predictors
new_labels <- c(
  SampleCoded = "Political Identity* ",
  HL = "Health Literacy* ",
  'SleepDurationAvg:NewsTypeCoded' = "<span style='color:red'>Sleep Duration * News Type</span>* ",
  Age = "Age",
  Edu = "Education",
  CRT = "Cognitive Reflection Test",
  NewsDomainCoded = "News Domain",
  NewsTypeCoded = "News Type",
  SleepDurationAvg = "Sleep Duration",
  GenderCoded = "Gender"
)

ID_Accuracy_model_sleepduration_r2_plot <- 
  ml_ratingsScored_model_sleepduration_r2 %>%
  filter(Effect != 'Model') %>%
  mutate(Effect = reorder(Effect, Rsq)) %>%
  ggplot(aes(x = Effect, y = Rsq)) +
  geom_point(aes(color = ifelse(Effect == "SleepDurationAvg:NewsTypeCoded", "red", "black"))) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, color = ifelse(Effect == "SleepDurationAvg:NewsTypeCoded", "red", "black")), width = 0.2) +
  scale_color_identity() +
  scale_y_continuous(limits=c(0, .16), labels=c("0·00", "0·05", "0·10", "0·15")) + 
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "") +
  labs(x = "", y = bquote("Effect Size" ~ (italic(R)^2) ~ "")) +
  theme_bw() +
  theme(
    plot.margin = unit(c(2, 2, 2, 2), "mm"),
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold"),
    plot.title.position = "plot", # Title outside plot area
    plot.title = element_text(size = 14, hjust = 0, vjust = -0.5),
    axis.text.y = element_markdown()  # Enable markdown for y-axis labels
  ) +
  scale_x_discrete(labels = new_labels) +
  # ggtitle('Identification Accuracy True/False')
  ggtitle('Identification Accuracy (Sleep Duration)')
ID_Accuracy_model_sleepduration_r2_plot
# ggsave(ID_Accuracy_model_sleepduration_r2_plot, 
#        file = "ID_Accuracy_model_sleepduration_r2_plot.png", 
#        dpi = 300, 
#        width = 5, height = 2)

#-----------------------------------------------------------------------------------------------

# News Item Identification Accuracy: Social Jetlag Model

# Define labels with markdown formatting for specific predictors
new_labels <- c(
  SampleCoded = "Political Identity* ",
  HL = "Health Literacy* ",
  'SocialJetlag_ABS:NewsTypeCoded' = "<span style='color:red'>Social Jetlag * News Type</span>* ",
  Age = "Age",
  Edu = "Education",
  CRT = "Cognitive Reflection Test",
  NewsDomainCoded = "News Domain",
  NewsTypeCoded = "News Type* ",
  SocialJetlag_ABS = "Social Jetlag",
  GenderCoded = "Gender"
)

ID_Accuracy_model_socialjetlag_r2_plot <- 
  ml_ratingsScored_model_socialjetlag_r2 %>%
  filter(Effect != 'Model') %>%
  mutate(Effect = reorder(Effect, Rsq)) %>%
  ggplot(aes(x = Effect, y = Rsq)) +
  geom_point(aes(color = ifelse(Effect == "SocialJetlag_ABS:NewsTypeCoded", "red", "black"))) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, color = ifelse(Effect == "SocialJetlag_ABS:NewsTypeCoded", "red", "black")), width = 0.2) +
  scale_y_continuous(labels=c("0·00", "0·05", "0·10", "0·15")) +
  scale_color_identity() +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = bquote("Effect Size" ~ (italic(R)^2) ~ "")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold"),
    plot.title.position = "plot", # Title outside plot area
    # plot.title = element_text(hjust = 0, vjust = -0.5),
    plot.title = element_text(size = 14, hjust = 0, vjust = -0.5), 
    axis.text.y = element_markdown()  # Enable markdown for y-axis labels
  ) +
  scale_x_discrete(labels = new_labels) +
  ggtitle('Identification Accuracy (Social Jetlag)')

ID_Accuracy_model_socialjetlag_r2_plot
# ggsave(ID_Accuracy_model_socialjetlag_r2_plot, 
#        file = "ID_Accuracy_model_socialjetlag_r2_plot.png", 
#        dpi = 300, 
#        width = 5, height = 2)

#-----------------------------------------------------------------------------------------------

# stack plots
## Note: This figure is Figure 2 in the publication
ID_Accuracy_model_combined_plot <- ID_Accuracy_model_sleepduration_r2_plot / ID_Accuracy_model_socialjetlag_r2_plot + plot_layout(ncol=1, heights=c(1,1))
ggsave(ID_Accuracy_model_combined_plot, file='ID_Accuracy_model_combined_plot.png', dpi=300, height=4, width=5)

#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

# Confidently Correct: Sleep Duration Model

# Define labels with markdown formatting for specific predictors
new_labels <- c(
  SampleCoded = "Political Identity* ",
  HL = "Health Literacy* ",
  'SleepDurationAvg:NewsTypeCoded' = "<span style='color:red'>Sleep Duration * News Type</span>",
  Age = "Age",
  Edu = "Education",
  CRT = "Cognitive Reflection Test",
  NewsDomainCoded = "News Domain* ",
  NewsTypeCoded = "News Type",
  SleepDurationAvg = "Sleep Duration",
  GenderCoded = "Gender"
)

Conf_Correct_model_sleepduration_r2_plot <- 
  Conf_Correct_SleepDuration_Model_r2 %>%
  filter(Effect != 'Model') %>%
  mutate(Effect = reorder(Effect, Rsq)) %>%
  ggplot(aes(x = Effect, y = Rsq)) +
  geom_point(aes(color = ifelse(Effect == "SleepDurationAvg:NewsTypeCoded", "red", "black"))) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, color = ifelse(Effect == "SleepDurationAvg:NewsTypeCoded", "red", "black")), width = 0.2) +
  scale_y_continuous(breaks = pretty_breaks(n = 5),
                     labels = number_format(accuracy = 0.01)) +
  scale_y_continuous(breaks=c(.00, .03, .06, .09, .12),
                     labels=c("0·00", "0·03", "0·06", "0·09", "0·12")) + 
  scale_color_identity() +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "") +
  labs(x = "", y = bquote("Effect Size" ~ (italic(R)^2) ~ "")) +
  theme_bw() +
  theme(plot.margin = unit(c(2, 2, 2, 2), "mm"),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(face = "bold"),
        plot.title.position = "plot", # Title outside plot area
        # plot.title = element_text(hjust = 0, vjust = -0.5),
        plot.title = element_text(size = 14, hjust = 0, vjust = -0.5), 
        axis.text.y = element_markdown()  # Enable markdown for y-axis labels
  ) +
  scale_x_discrete(labels = new_labels) +
  ggtitle('Confidently Correct (Sleep Duration)')
# ggtitle('Confidently Correct (Sleep Duration)')
Conf_Correct_model_sleepduration_r2_plot

#-----------------------------------------------------------------------------------------------

# Confidently Correct: Social Jetlag Model

# Define labels with markdown formatting for specific predictors
new_labels <- c(
  SampleCoded = "Political Identity* ",
  HL = "Health Literacy*",
  'SocialJetlag_ABS:NewsTypeCoded' = "<span style='color:red'>Social Jetlag * News Type*</span>",
  Age = "Age",
  Edu = "Education",
  CRT = "Cognitive Reflection Test",
  NewsDomainCoded = "News Domain*",
  NewsTypeCoded = "News Type*",
  SocialJetlag_ABS = "Social Jetlag",
  GenderCoded = "Gender"
)

Conf_Correct_model_socialjetlag_r2_plot <- 
  Conf_Correct_model_socialjetlag_r2 %>%
  filter(Effect != 'Model') %>%
  mutate(Effect = reorder(Effect, Rsq)) %>%
  ggplot(aes(x = Effect, y = Rsq)) +
  geom_point(aes(color = ifelse(Effect == "SocialJetlag_ABS:NewsTypeCoded", "red", "black"))) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, color = ifelse(Effect == "SocialJetlag_ABS:NewsTypeCoded", "red", "black")), width = 0.2) +
  scale_y_continuous(labels=c("0·00", "0·03", "0·06", "0·09", "0·12")) + 
  scale_color_identity() +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = bquote("Effect Size" ~ (italic(R)^2) ~ "")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold"),
    plot.title.position = "plot", # Title outside plot area
    # plot.title = element_text(hjust = 0, vjust = -0.5),
    plot.title = element_text(size = 14, hjust = 0, vjust = -0.5), 
    axis.text.y = element_markdown()  # Enable markdown for y-axis labels
  ) +
  scale_x_discrete(labels = new_labels) +
  ggtitle('Confidently Correct (Social Jetlag)')
Conf_Correct_model_socialjetlag_r2_plot

#-----------------------------------------------------------------------------------------------

# Confidently Incorrect: Sleep Duration Model

# Define labels with markdown formatting for specific predictors
new_labels <- c(
  SampleCoded = "Political Identity* ",
  HL = "Health Literacy",
  'SleepDurationAvg:NewsTypeCoded' = "<span style='color:red'>Sleep Duration * News Type</span>* ",
  Age = "Age",
  Edu = "Education",
  CRT = "Cognitive Reflection Test* ",
  NewsDomainCoded = "News Domain",
  NewsTypeCoded = "News Type* ",
  SleepDurationAvg = "Sleep Duration",
  GenderCoded = "Gender"
)

Conf_Incorrect_model_sleepduration_r2_plot <- 
  Conf_Incorrect_SleepDuration_Model_r2 %>%
  filter(Effect != 'Model') %>%
  mutate(Effect = reorder(Effect, Rsq)) %>%
  ggplot(aes(x = Effect, y = Rsq)) +
  geom_point(aes(color = ifelse(Effect == "SleepDurationAvg:NewsTypeCoded", "red", "black"))) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, color = ifelse(Effect == "SleepDurationAvg:NewsTypeCoded", "red", "black")), width = 0.2) +
  scale_color_identity() +
  scale_y_continuous(limits=c(0, .16),
                     breaks=c(0, .05, .10), labels=c("0·00", "0·05", "0·10")) + 
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = bquote("Effect Size" ~ (italic(R)^2) ~ "")) +
  theme_bw() +
  theme(
    plot.margin = unit(c(2, 2, 2, 2), "mm"),
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold"),
    plot.title.position = "plot", # Title outside plot area
    # plot.title = element_text(hjust = 0, vjust = -0.5),
    plot.title = element_text(size = 14, hjust = 0, vjust = -0.5), 
    axis.text.y = element_markdown()  # Enable markdown for y-axis labels
  ) +
  scale_x_discrete(labels = new_labels) +
  # ggtitle('Confidently Incorrect')
ggtitle('Confidently Incorrect (Sleep Duration)')
Conf_Incorrect_model_sleepduration_r2_plot

#-----------------------------------------------------------------------------------------------

# Confidently Incorrect: Social Jetlag Model

# Define labels with markdown formatting for specific predictors
new_labels <- c(
  SampleCoded = "Political Identity* ",
  HL = "Health Literacy",
  'SocialJetlag_ABS:NewsTypeCoded' = "<span style='color:red'>Social Jetlag * News Type</span>* ",
  Age = "Age",
  Edu = "Education",
  CRT = "Cognitive Reflection Test* ",
  NewsDomainCoded = "News Domain",
  NewsTypeCoded = "News Type",
  SocialJetlag_ABS = "Social Jetlag",
  GenderCoded = "Gender"
)

Conf_Incorrect_model_socialjetlag_r2_plot <- 
  Conf_Incorrect_model_socialjetlag_r2 %>%
  filter(Effect != 'Model') %>%
  mutate(Effect = reorder(Effect, Rsq)) %>%
  ggplot(aes(x = Effect, y = Rsq)) +
  geom_point(aes(color = ifelse(Effect == "SocialJetlag_ABS:NewsTypeCoded", "red", "black"))) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, color = ifelse(Effect == "SocialJetlag_ABS:NewsTypeCoded", "red", "black")), width = 0.2) +
  scale_y_continuous(breaks=c(0, .05, .10), labels=c("0·00", "0·05", "0·10")) +
  scale_color_identity() +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = bquote("Effect Size" ~ (italic(R)^2) ~ "")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold"),
    plot.title.position = "plot", # Title outside plot area
    # plot.title = element_text(hjust = 0, vjust = -0.5),
    plot.title = element_text(size = 14, hjust = 0, vjust = -0.5), 
    axis.text.y = element_markdown()  # Enable markdown for y-axis labels
  ) +
  scale_x_discrete(labels = new_labels) +
  ggtitle('Confidently Incorrect (Social Jetlag)')
Conf_Incorrect_model_socialjetlag_r2_plot

#-----------------------------------------------------------------------------------------------

# stack plots
## Note: This figure is Figure 3 in the publication
Belief_cat_Pct_combined_plot <- Conf_Correct_model_sleepduration_r2_plot / Conf_Incorrect_model_sleepduration_r2_plot / Conf_Correct_model_socialjetlag_r2_plot/ Conf_Incorrect_model_socialjetlag_r2_plot + plot_layout(ncol=2, heights=c(2,2))
ggsave(Belief_cat_Pct_combined_plot, file='Belief_cat_Pct_combined_plot.png', dpi=300, height=4, width=10)

#-----------------------------------------------------------------------------------------------
