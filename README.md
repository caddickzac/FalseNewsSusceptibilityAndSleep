# Sleep and Susceptibility to False Information in the Digital Age

Authors: [Caddick, Z. A.](https://orcid.org/0000-0002-3369-7727), [Dieckmann, N. F.](https://orcid.org/0000-0001-5061-9889), [Shafer, B. M.](https://orcid.org/0009-0004-4059-7849), [McHill, A. W.](https://orcid.org/0000-0002-9428-6884)

Laboratory: [Sleep, Chronobiology, & Health Laboratory](https://sites.google.com/view/ohsusleepchronobiologylab/home)

Citation: Caddick, Z. A., Dieckmann, N.F., Shafer, B.M., McHill, A. W. (In Prep). Sleep and Susceptibility to False Information in the Digital Age.

For help or more information contact [caddickzac@gmail.com](mailto:caddickzac@gmail.com).

## Measures
- [News Task](https://github.com/caddickzac/FalseNewsSusceptibilityAndSleep/tree/main/Measures/NewsTaskStimuli): 60 news headlines with an accompanied image were curated using a format similar to social media websites. True news items were extracted from news websites (e.g., Reuters) and false news items were extracted from a mix of websites debunking false claims. The 60 items were equally divided by type (true vs false; 30 items) and domain (Covid-19, “The Big Lie” about the outcome of the 2020 election; 30 items). Participants rated their belief in the veracity of each question (“to what extent you believe the above headline represents accurate/true news?”) and a belief confidence question (“how confident are you in your assessment of the headlines’ truthfulness?”), both on a 0-100 visual analog scale. 
- [Sleep Measure](https://github.com/caddickzac/FalseNewsSusceptibilityAndSleep/blob/main/Measures/SleepMeasure/Sleep%20Measure.png): Sleep and wake times for work and non-work days.
- [Cognitive Reflection Test (v2)](https://github.com/caddickzac/FalseNewsSusceptibilityAndSleep/tree/main/Measures/CognitiveReflectionTest): Cognitive reflection measures the ability to override intuitive responses. 
- [Health Literacy](https://github.com/caddickzac/FalseNewsSusceptibilityAndSleep/tree/main/Measures/HealthLiteracy): Behavioral measure of health literacy. 
- [News Task Debrief](https://github.com/caddickzac/FalseNewsSusceptibilityAndSleep/tree/main/Measures/NewsTaskDebrief): Participants were debriefed at the conclusion of the survey about the accuracy of each headline presented.
- [Measure Scoring](https://github.com/caddickzac/FalseNewsSusceptibilityAndSleep/blob/main/Measures/All%20Measures.docx): See "All Measures.docx" for scoring information alongside each measure.
- [Qualtrics Survey](https://github.com/caddickzac/FalseNewsSusceptibilityAndSleep/tree/main/Qualtrics): A copy of the Qualtrics survey has been made available for import. 

## Data
- Note: Item Level Data is at the level of a headline task item, whereas the Measure Level Data is the at level of each sub-headline measure (e.g., "real-news: Covid, fake-news: Covid, real-news: The Big Lie, fake-news: The Big Lie). For simplicity, derivation data is omitted but is available upon request (email: [caddickzac@gmail.com](mailto:caddickzac@gmail.com)). 
- [Item Level Data](https://github.com/caddickzac/FalseNewsSusceptibilityAndSleep/blob/main/Data/AnalysisData_ItemLevel.csv)
- [Measure Level Data](https://github.com/caddickzac/FalseNewsSusceptibilityAndSleep/blob/main/Data/AnalysisData_MeasureLevel.csv)

## R Scripts
- Note: The setup script imports data, loads relevant packages and presents a data dictionary for each of the uploaded datasets. The "Models" script must run before the "Figure 2 & 3" script is run.   
- [Analysis & Figure Setup Script](https://github.com/caddickzac/FalseNewsSusceptibilityAndSleep/blob/main/R%20Scripts/SetupScript.R)
- [Mixed-Effects Models](https://github.com/caddickzac/FalseNewsSusceptibilityAndSleep/blob/main/R%20Scripts/Models.R)
- [Figure 2 & 3](https://github.com/caddickzac/FalseNewsSusceptibilityAndSleep/blob/main/R%20Scripts/Figures_2_3.R)
- [Figures 4 & 5](https://github.com/caddickzac/FalseNewsSusceptibilityAndSleep/blob/main/R%20Scripts/Figures_4_5.R)

## Publication Figures 

Figure 1. STROBE diagram flow chart. 

<img src="https://raw.githubusercontent.com/caddickzac/FalseNewsSusceptibilityAndSleep/main/Figures/strobe_diagram.jpg" width="50%">
<br><br>


Figure 2. Predictors for accurately identifying news items as true or false using mixed-effects modeling (95% CI). Political identity denotes participants that self-identified as liberal (_n_=109) or conservative (_n_=118), news type denotes false vs real news items, and news domain denotes Covid-19 vs the 2020 US election news items. Predictors with an asterisk denote statistical significance (_p_<0.05) within the model. Sleep duration and social jetlag by news type (false vs real) interactions are highlighted in red as they were the primary hypotheses in the current investigation. Total variance explained using all predictors: sleep duration model _R_<sup>2</sup> = 22.67% & social jet lag model _R_<sup>2</sup> = 22.76%.

<img src="https://raw.githubusercontent.com/caddickzac/FalseNewsSusceptibilityAndSleep/main/Figures/Identification_accuracy_models.png" width="50%">
<br><br>


Figure 3. Predictors for confidence in accuracy of identifying news items as true or false using mixed-effects modeling (95% CI). Participants could be confidently correct (accurately assessing something that is false as false or real as real) or confidently incorrect (inaccurately assessing something that is false as real or real as false). Political identity denotes participants that self-identified as liberal (_n_ = 109) or conservative (_n_ = 118), news type denotes false vs real news items, and news domain denotes Covid-19 vs the 2020 US election news items. Predictors with an asterisk denote statistical significance (_p_ < 0.05) within the model. Sleep duration and social jetlag by news type (false vs real) interactions are highlighted in red as they were the primary hypotheses in the current investigation. Total variance explained using all predictors, confidently correct: sleep duration model _R_<sup>2</sup> = 13.54% & social jet lag model _R_<sup>2</sup> = 13.71%; confidently incorrect: sleep duration model _R_<sup>2</sup> = 16.20% & social jet lag model _R_<sup>2</sup> = 15.56%.

<img src="https://raw.githubusercontent.com/caddickzac/FalseNewsSusceptibilityAndSleep/main/Figures/Confidence_accuracy_models.png" width="50%">
<br><br>


Figure 4. Accuracy of correctly identifying news items as either true or false & confidence percentages across sleeping duration. Note: The red bar represents group medians and the dotted line means. Sample size for each group: 4-6<sub><i>n</i></sub> = 13, 6-7<sub><i>n</i></sub> = 28, 7-8<sub><i>n</i></sub> = 85, 8-9<sub><i>n</i></sub> = 69, 9-10<sub><i>n</i></sub> = 32.

<img src="https://raw.githubusercontent.com/caddickzac/FalseNewsSusceptibilityAndSleep/main/Figures/sleep_duration_combined_plot.png" width="50%">
<br><br>


Figure 5. Accuracy of correctly identifying news items as either true or false & confidence percentages across social jetlag. Note: The red bar represents group medians and the dotted line means. Sample size for each group: 0-1<sub><i>n</i></sub> = 87, 1-2<sub><i>n</i></sub> = 81, 2-3<sub><i>n</i></sub> = 36, 3-4<sub><i>n</i></sub> = 17, 4-5<sub><i>n</i></sub> = 6.

<img src="https://raw.githubusercontent.com/caddickzac/FalseNewsSusceptibilityAndSleep/main/Figures/social_jetlag_combined_plot.png" width="50%">
