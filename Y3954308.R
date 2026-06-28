# *********************************
# BIO00052M Data Analysis Project
# **********************************


#Brief: 
#Smoking is a known risk factor for poor lung function, however the magnitude of 
# its risks is said to differ between male and females. As there are biological and 
#physiological differences such lung volume, airflow rates, Bronchioles and hormones like
#estrogen affecting inflammation etc it leads to differences in lung disease #susceptibility, 
#prevention and even treatment amonst femal and males. Hence variables such as 'FEV1' (lung function measured by 
#Forced Expiratory volume in the first second in mL), 'Smoke100'(smoking history;participants have smoked 
#at least 100 cigarettes in their life(Yes/No)) and finally biological sex are worth investigating for this analysis. 

# This project aims to analyse whether FEV1 is influenced by Smoke100 and Sex of the participant. 
#Specifically, we test whether individuals with smoking history indicate lower lung functioning
#in comparison to those with no history, whether male and females differ in lung function and whether there is an 
#interaction between smoking history and sex on lung function. Evaluating this potential interaction will 
#shed light on groups who are particularly more vulnerable to respiratory effects of smoking. 


# ****************************************
#Question: Does smoking history interact with sex to influence lung function (FEV1)?

# ****************************************

# I: Loading necessary packages


library(tidyverse)
library(ggplot2)
library(emmeans)
library(viridis) #color palette choice for this project

# ****************************************

#II: Reading and inspect the data file

#a. 
smoke <- read.csv("raw_data/NHANES - NHANES.csv")

#b. Inspecting variables used in analysis
str(smoke)

#Statistical choice: 2-way Anova is ideal measure because:
# there is 2 categorical IVs Smoking history(Yes/No), Sex(Female/Male),
# 1 continuous DV: Lung Function measured by FEV1 in mL
# and aim is to test if two smoking history and sex interact to influence the FEV1


#c. Converting categorical variable into factors to 
#ensure accurate reproducibility and clarity of interpretation
smoke$Smoke100 <- factor(smoke$Smoke100, levels = c("No", "Yes"))
smoke$Sex <- factor(smoke$Sex, levels = c("female", "male"),
                               labels = c("Female", "Male")) #Making all variables Uppercase

# *********************************************************************

#III: Exploratory visualization of FEV1 by smoking status and sex

#a. Plotting
exploring_violin <- ggplot(data = smoke, 
                            aes(x = Smoke100, y = FEV1, fill = Sex)) +
  geom_violin() +
  
  labs(
    x = "Participant smoked 100 cigarettes in lifetime ",  # New x-axis label
    y = "Forced Expiratory Volume (FEV1) in mL") +    # New y-axis label
  scale_fill_viridis_d(option = "mako", #palette choice
                        begin = 0.4, end=0.8 ) + #changes where the color palette starts and ends from 
  theme_light()
 

#calling the plot to diplay
exploring_violin

#Saving the boxplot to 'Figures' folder
ggsave("figures/1.exploring_plot.png",
       plot = exploring_violin,
       device = "png",
       width = 10,
       height = 9,
       units = "in",
       dpi = 300)


#Figure 1 shows that men have higher lung function than women regardless of smoking condition. 
#Also indicates that smokers, have lower levels of FEV1 than non smokers, across sex. 
#However there is a greater difference between FEV1 levels observed in male 
#smokers vs non-smokers than both smoking groups in females. 



# b. Summarising the data for FEV1 for each sex-smoke100 combination.These statistics are used to 
#calculate and plot mean ± SE error bars, so that it can be used in our later plots.  

#By calculating the means, standard deviations, sample sizes and standard errors. 
smokes_summary <- smoke |>  
  group_by(Smoke100, Sex) |>  
  summarise(mean = mean(FEV1),
            sd = sd(FEV1),
            n = length(FEV1),
            se = sd / sqrt(n), 
            )

#calling the summary table for output
smokes_summary

#This summary validates that participants with a history of smoking exhibited 
#lower lung function compared to non-smokers in both sexes. Males exhibited 
#higher FEV1 than females across both smoking groups. Non-smoker males had 
#a mean FEV1 of 3852, non-smoker females had a mean FEV1 of 2760, smoker-males
#had a mean FEV1 of 3436 and smoker females with a mean FEV1 of 2625. 


# *******************************************************************************************************
# Statistical Analysis

# IV: creating a two-way ANOVA model to test whether smoke100 and sex interact to predict FEV1:
mods <- lm(data = smoke, FEV1 ~ Smoke100 * Sex)

#examining this model
summary(mods)

#The overall  model validates that there is  a significant amount of the variation in lung 
#function(FEV1) (p-value: 2.2e-16),  indicating that smoking history, sex and their 
#interaction jointly predicts lung function. However Anova model needs to be conducted
#to check which of the 3 effects are significant .


# b. Obtaining the ANOVA table to assess the significance of main effects
# and the smoking history × sex interaction

anova(mods)

#There is a significant effect of smoking history (F=7.65; d.f.=1, 1115 p = 0.0057),
# sex (F=490.14; d.f.=1, 1115 p= 2.2e-16) and its interaction as well on lung 
#function (F=10.44; d.f.=1, 1115 p= 0.0012). However  a post-hoc test needs to be carried out
# to investigate which within group comparisons(smokers vs non-smokers in females/males) are significant. 



#c. Carrying out Post-hoc test to see which comparisions are significant

emmeans(mods, ~ Smoke100  *Sex) |> pairs()


# d. Plotting Post-Hoc test

## Saving as  object 
em <- emmeans(mods, ~ Smoke100  *Sex) 

## Converting to datafame 
em2 <- as.data.frame(em)

## Plotting Post-hoc test with ggplot 
posthoc_plot2<- ggplot(em2, aes(x = Smoke100, y = emmean, colour = Sex)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2,
                position = position_dodge(width = 0.4)) +
  theme_light()+
  labs(
    x = "
    Participant smoked 100 cigarettes in lifetime ",  
    y = "Estimated Marginal Mean of FEV1" ) +
      
      scale_color_viridis_d(option = "mako", begin = 0.4, end=0.8 ) 
      
#calling the plot to display
posthoc_plot2 

#Saving figure 2 to 'figure folder'
ggsave("figures/2.Posthoc_plot.png",
       plot = posthoc_plot2,
       device = "png",
       width = 8,
       height = 9,
       units = "in",
       dpi = 300)


#Tukey-adjusted post-hoc comparisons indicated females had significantly lower lung function
#than males regardless of smoking history: among non-smokers(t = -1092; d.f. = 1115;
#p =< .0001) and smokers (t = -811; d.f. = 1115; p= < .0001). Additionally, male non-smokers had 
#significantly higher lung function than male smokers (t = 416; d.f. = 1115; p= < .0001). 
#However no significant difference was found between smoking history and lung function 
#among females (t = 135; d.f. = 1115; p=0.1194). 




# *******************************************************************************************************
# V: Check model assumptions: normality of residuals

#a. Residual vs Fitted values Plot

plot(mods, which = 1)


#Saving figure 3 to 'figures' folder' 
#using dev(off) as ggsave is not applicable for this plot type

png("figures/3.Residual_plot.png",
    width = 8,
    height = 7,
    units = "in",
    res = 300)

plot(mods, which = 1)

dev.off()

#The residuals versus fitted values plot indicated 
#no substantial deviations from linearity or homoscedasticity. 
#In line with normality assumption


#b. Histogram plot

Histogram_plot<- ggplot(mapping = aes(x = mods$residuals)) + 
  geom_histogram(bins = 10)

#Calling the plot to display
Histogram_plot

#Saving figure 4 to 'figure folder'
ggsave("figures/4.Histogram_plot.png",
       plot = Histogram_plot,
       device = "png",
       width = 4,
       height = 3,
       units = "in",
       dpi = 300)

# Histogram is relatively symmetrical, hence the residuals were approximately normally distributed. 
#Given the large sample size, minor deviations from normality is not considered problematic.
#In line with normality assumption

#c. Shapiro test for Normality 

shapiro.test(mods$residuals)

#The p-value is greater than 0.05 indicating that the normality assumption is not significant.
#In line with normality assumption


#** Considering all the three above tests,  validates that assumptions of normality is met
#and homogeneity of variance is probably not violated **

# *******************************************************************************************************

#VI. Final Report 


#The overall  model showed that there is  a significant amount of  variation in lung 
#function (p-value: 2.2e-16),  indicating that  smoking history, sex and their
#interaction jointly predicts lung function. A two-way analysis of variance indicated 
#there was a significant main effect of smoking history (F=7.65; d.f.=1, 1115 p = .0057),
#sex (F=490.14; d.f.=1, 1115 p= 2.2e-16) and its interaction as well on lung function (F=10.44;
#d.f.=1, 1115 p= 0.0012). Post-hoc Tukey-adjusted comparisons revealed males with smoking history
#had significantly lower lung function than males with no smoking history (t = 416; 
# d.f. = 1115; p= < .0001). Additionally females had significantly lower lung function
#than males regardless of smoking history: among non-smokers(t = -1092; d.f. = 1115;
#p =< .0001) and smokers (t = -811; d.f. = 1115; p= < .0001). Importantly, although female non-smokers had
#higher lung function by 135mL than female smokers, the difference wasn't significant in the 
#Tukey post-hoc test (p= 0.1194).This demonstrates that the effect of smoking on lung function differed 
#between males and females, consistent with the interaction of sex and smoking history 
#on lung function (F=10.44; d.f.=1, 1115 p= 0.0012). Below plot represents the results for this report:

Report_plot <- ggplot() +
  # Raw data points
  geom_point(
    data = smoke,
    aes(x = Smoke100, y = FEV1, fill = Sex, colour = Sex),
    shape = 21,
    size = 1.5,
    stroke = 0.1,
    position = position_jitterdodge(
      dodge.width = 1,
      jitter.width = 0.15,
      jitter.height = 0
    )
  ) +
  
  # Mean ± SE error bars
  geom_errorbar(
    data = smokes_summary,
    aes(x = Smoke100, ymin = mean - se, ymax = mean + se, group = Sex),
    width = 0.5,
    linewidth = 0.5,
    position = position_dodge(width = 1)
  ) +
  
  # Mean bars
  geom_errorbar(
    data = smokes_summary,
    aes(x = Smoke100, ymin = mean, ymax = mean, group = Sex),
    width = 0.5,
    linewidth = 0.6,
    position = position_dodge(width = 1)
  ) +
  
  
  # Axis labels and limits
  scale_x_discrete(name = "Participant smoked 100 cigarettes in lifetime") +
  scale_y_continuous(
    name = "Lung Function (FEV1)",
    limits = c(0, 7000),
    expand = c(0, 0)
  ) +
  labs(caption="
      Figure 5. Lung function (FEV1) across smoking status (smoked ≥100 cigarettes vs not) and sex. 
       Points represent individual participants, horizontal bars indicate group means,
       and error bars show ± standard error.") +
  theme_light() +
  
  theme(
    plot.caption = element_text( #adjusting the position of the caption
      hjust = 0.5,
      margin = margin(t = 8)
    ),
    plot.caption.position = "plot"
  ) +
  
  
  # Colour scheme 
  scale_fill_viridis_d(option = "mako",
                       begin = 0.4, end=0.8) +
  scale_color_viridis_d(option = "mako",
                        begin = 0.4, end=0.8) +
  
  # Male smokers vs non-smokers
  annotate(
    "segment",
    x = 1.55, xend = 2.10,
    y = 5755, yend = 5755
  ) +
  annotate(
    "text",
    x = 1.95, y = 5890,
    label = "p < .0001"
  ) +
  
  # Male vs female smokers
  annotate(
    "segment",
    x = 1.65, xend = 2.20,
    y = 6000, yend = 6000
  ) +
  annotate(
    "text",
    x = 2.05, y = 6145,
    label = "p < .0001"
  ) +
  
   #Non smokers Female vs Non-smokers Male 
  annotate(
    "segment",
    x = 1.75, xend = 2.30,
    y = 6255, yend = 6255
  ) +
  annotate(
    "text",
    x = 2.15, y = 6390,
    label = "p < .0001"
  ) +
  
  
  # Theme
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.1),
    legend.background = element_rect(colour = "black"),
    legend.title = element_blank()
  )

#calling the plot to dsiplay 
Report_plot

#Saving figure 5 to 'figure folder'
ggsave("figures/5.Report_plot.png",
       plot = Report_plot,
       device = "png",
       width = 10,
       height = 7,
       units = "in",
       dpi = 300)


# *******************************************************************************************************


