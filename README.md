# Lung Function Analysis: Does Smoking History Interact with Sex to Influence FEV1?

## Brief

Smoking is a known risk factor for poor lung function, however the magnitude of its risks is said to differ between males and females. As there are biological and physiological differences such as lung volume, airflow rates, bronchioles and hormones like estrogen affecting inflammation, it leads to differences in lung disease susceptibility, prevention and even treatment amongst females and males. Hence variables such as **FEV1** (lung function measured by Forced Expiratory Volume in the first second, in mL), **Smoke100** (smoking history; whether participants have smoked at least 100 cigarettes in their life, Yes/No) and biological **Sex** are worth investigating for this analysis.

This project aims to analyse whether FEV1 is influenced by Smoke100 and Sex of the participant. Specifically, it tests whether individuals with a smoking history show lower lung functioning compared to those with no history, whether males and females differ in lung function, and whether there is an interaction between smoking history and sex on lung function. Evaluating this potential interaction sheds light on groups who are particularly more vulnerable to the respiratory effects of smoking.

> **Research question:** Does smoking history interact with sex to influence lung function (FEV1)?

---

## I. Loading Necessary Packages

```r
library(tidyverse)
library(ggplot2)
library(emmeans)
library(viridis)   # colour palette choice for this project
```

---

## II. Reading and Inspecting the Data

```r
# a. Read the data
smoke <- read.csv("raw_data/NHANES - NHANES.csv")

# b. Inspect variables used in analysis
str(smoke)
```

```
# paste str(smoke) console output here
```

**Statistical choice:** a two-way ANOVA is the ideal measure because there are two categorical IVs — Smoking history (Yes/No) and Sex (Female/Male), one continuous DV — lung function measured by FEV1 in mL, and the aim is to test whether smoking history and sex interact to influence FEV1.

```r
# c. Convert categorical variables into factors for reproducibility
#    and clarity of interpretation
smoke$Smoke100 <- factor(smoke$Smoke100, levels = c("No", "Yes"))
smoke$Sex <- factor(smoke$Sex, levels = c("female", "male"),
                    labels = c("Female", "Male"))   # making labels uppercase
```

---

## III. Exploratory Visualisation of FEV1 by Smoking Status and Sex

```r
# a. Plotting
exploring_violin <- ggplot(data = smoke,
                           aes(x = Smoke100, y = FEV1, fill = Sex)) +
  geom_violin() +
  labs(
    x = "Participant smoked 100 cigarettes in lifetime",
    y = "Forced Expiratory Volume (FEV1) in mL") +
  scale_fill_viridis_d(option = "mako",
                       begin = 0.4, end = 0.8) +
  theme_light()

# calling the plot to display
exploring_violin

# saving the plot to the 'figures' folder
ggsave("figures/1.exploring_plot.png",
       plot = exploring_violin,
       device = "png",
       width = 10, height = 9, units = "in", dpi = 300)
```

![img1](figures/1.exploring_plot.png)

Figure 1 shows that men have higher lung function than women regardless of smoking condition. It also indicates that smokers have lower levels of FEV1 than non-smokers, across sex. However, there is a greater difference between FEV1 levels observed in male smokers vs non-smokers than between the two smoking groups in females.

```r
# b. Summarise FEV1 for each Sex-Smoke100 combination.
#    These statistics (mean, sd, n, se) are used to plot mean +/- SE error bars later.
smokes_summary <- smoke |>
  group_by(Smoke100, Sex) |>
  summarise(mean = mean(FEV1),
            sd = sd(FEV1),
            n = length(FEV1),
            se = sd / sqrt(n))

# calling the summary table for output
smokes_summary
```

```
# paste smokes_summary console output here
# (known group means from analysis:
#   No  / Female = 2760,  No  / Male = 3852,
#   Yes / Female = 2625,  Yes / Male = 3436)
```

This summary validates that participants with a history of smoking exhibited lower lung function compared to non-smokers in both sexes. Males exhibited higher FEV1 than females across both smoking groups. Non-smoker males had a mean FEV1 of 3852, non-smoker females 2760, smoker males 3436, and smoker females 2625.

---

## IV. Statistical Analysis — Two-Way ANOVA

```r
# Creating a two-way ANOVA model to test whether Smoke100 and Sex
# interact to predict FEV1
mods <- lm(data = smoke, FEV1 ~ Smoke100 * Sex)

# examining this model
summary(mods)
```

```
# paste summary(mods) console output here
```

The overall model shows that a significant amount of the variation in lung function (FEV1) is explained (p < 2.2e-16), indicating that smoking history, sex and their interaction jointly predict lung function. An ANOVA table is needed to check which of the three effects are significant.

```r
# b. ANOVA table to assess significance of main effects and the
#    smoking history x sex interaction
anova(mods)
```

```
# paste anova(mods) console output here
# (reported results:
#   Smoke100      F = 7.65    df = 1, 1115   p = 0.0057
#   Sex           F = 490.14  df = 1, 1115   p < 2.2e-16
#   Smoke100:Sex  F = 10.44   df = 1, 1115   p = 0.0012)
```

There is a significant effect of smoking history (F = 7.65; d.f. = 1, 1115; p = 0.0057), sex (F = 490.14; d.f. = 1, 1115; p < 2.2e-16), and their interaction on lung function (F = 10.44; d.f. = 1, 1115; p = 0.0012). A post-hoc test is needed to investigate which within-group comparisons (smokers vs non-smokers in females/males) are significant.

```r
# c. Post-hoc test to see which comparisons are significant
emmeans(mods, ~ Smoke100 * Sex) |> pairs()
```

```
# paste emmeans pairs() console output here
```

```r
# d. Plotting the post-hoc test

# save as object
em <- emmeans(mods, ~ Smoke100 * Sex)

# convert to data frame
em2 <- as.data.frame(em)

# plot with ggplot
posthoc_plot2 <- ggplot(em2, aes(x = Smoke100, y = emmean, colour = Sex)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2,
                position = position_dodge(width = 0.4)) +
  theme_light() +
  labs(
    x = "Participant smoked 100 cigarettes in lifetime",
    y = "Estimated Marginal Mean of FEV1") +
  scale_color_viridis_d(option = "mako", begin = 0.4, end = 0.8)

# calling the plot to display
posthoc_plot2

# saving figure 2 to the 'figures' folder
ggsave("figures/2.Posthoc_plot.png",
       plot = posthoc_plot2,
       device = "png",
       width = 8, height = 9, units = "in", dpi = 300)
```

![img2](figures/2.Posthoc_plot.png)

Tukey-adjusted post-hoc comparisons indicated females had significantly lower lung function than males regardless of smoking history: among non-smokers (p < .0001) and smokers (p < .0001). Additionally, male non-smokers had significantly higher lung function than male smokers (p < .0001). However, no significant difference was found between smoking history and lung function among females (p = 0.1194).

---

## V. Checking Model Assumptions

### a. Residuals vs Fitted Values

```r
plot(mods, which = 1)

# saving figure 3 to the 'figures' folder
# using png()/dev.off() as ggsave is not applicable for this base plot type
png("figures/3.Residual_plot.png",
    width = 8, height = 7, units = "in", res = 300)
plot(mods, which = 1)
dev.off()
```

![img3](figures/3.Residual_plot.png)

The residuals versus fitted values plot indicated no substantial deviations from linearity or homoscedasticity, in line with the normality assumption.

### b. Histogram of Residuals

```r
Histogram_plot <- ggplot(mapping = aes(x = mods$residuals)) +
  geom_histogram(bins = 10)

# calling the plot to display
Histogram_plot

# saving figure 4 to the 'figures' folder
ggsave("figures/4.Histogram_plot.png",
       plot = Histogram_plot,
       device = "png",
       width = 4, height = 3, units = "in", dpi = 300)
```

![img4](figures/4.Histogram_plot.png)

The histogram is relatively symmetrical, so the residuals were approximately normally distributed. Given the large sample size, minor deviations from normality are not considered problematic.

### c. Shapiro-Wilk Test for Normality

```r
shapiro.test(mods$residuals)
```

```
# paste shapiro.test(mods$residuals) console output here
# (reported: p > 0.05, i.e. normality assumption not violated)
```

The p-value is greater than 0.05, indicating the normality assumption is not violated.

> **Considering all three checks above, the assumption of normality is met and homogeneity of variance is probably not violated.**

---

## VI. Final Report

The overall model showed a significant amount of variation in lung function (p < 2.2e-16), indicating that smoking history, sex and their interaction jointly predict lung function. A two-way analysis of variance indicated a significant main effect of smoking history (F = 7.65; d.f. = 1, 1115; p = .0057), sex (F = 490.14; d.f. = 1, 1115; p < 2.2e-16), and their interaction on lung function (F = 10.44; d.f. = 1, 1115; p = 0.0012).

Post-hoc Tukey-adjusted comparisons revealed males with a smoking history had significantly lower lung function than males with no smoking history (p < .0001). Additionally, females had significantly lower lung function than males regardless of smoking history: among non-smokers (p < .0001) and smokers (p < .0001). Importantly, although female non-smokers had higher lung function (by ~135 mL) than female smokers, the difference was not significant in the Tukey post-hoc test (p = 0.1194).

This demonstrates that the effect of smoking on lung function differed between males and females, consistent with the significant interaction of sex and smoking history on lung function (F = 10.44; d.f. = 1, 1115; p = 0.0012). The plot below summarises the results for this report.

```r
Report_plot <- ggplot() +
  # Raw data points
  geom_point(
    data = smoke,
    aes(x = Smoke100, y = FEV1, fill = Sex, colour = Sex),
    shape = 21, size = 1.5, stroke = 0.1,
    position = position_jitterdodge(
      dodge.width = 1, jitter.width = 0.15, jitter.height = 0)
  ) +

  # Mean +/- SE error bars
  geom_errorbar(
    data = smokes_summary,
    aes(x = Smoke100, ymin = mean - se, ymax = mean + se, group = Sex),
    width = 0.5, linewidth = 0.5,
    position = position_dodge(width = 1)
  ) +

  # Mean bars
  geom_errorbar(
    data = smokes_summary,
    aes(x = Smoke100, ymin = mean, ymax = mean, group = Sex),
    width = 0.5, linewidth = 0.6,
    position = position_dodge(width = 1)
  ) +

  # Axis labels and limits
  scale_x_discrete(name = "Participant smoked 100 cigarettes in lifetime") +
  scale_y_continuous(
    name = "Lung Function (FEV1)",
    limits = c(0, 7000), expand = c(0, 0)
  ) +
  labs(caption =
    "Figure 5. Lung function (FEV1) across smoking status (smoked >=100 cigarettes vs not) and sex.
     Points represent individual participants, horizontal bars indicate group means,
     and error bars show +/- standard error.") +
  theme_light() +
  theme(
    plot.caption = element_text(hjust = 0.5, margin = margin(t = 8)),
    plot.caption.position = "plot"
  ) +

  # Colour scheme
  scale_fill_viridis_d(option = "mako", begin = 0.4, end = 0.8) +
  scale_color_viridis_d(option = "mako", begin = 0.4, end = 0.8) +

  # Male smokers vs non-smokers
  annotate("segment", x = 1.55, xend = 2.10, y = 5755, yend = 5755) +
  annotate("text", x = 1.95, y = 5890, label = "p < .0001") +

  # Male vs female smokers
  annotate("segment", x = 1.65, xend = 2.20, y = 6000, yend = 6000) +
  annotate("text", x = 2.05, y = 6145, label = "p < .0001") +

  # Non-smoker female vs non-smoker male
  annotate("segment", x = 1.75, xend = 2.30, y = 6255, yend = 6255) +
  annotate("text", x = 2.15, y = 6390, label = "p < .0001") +

  # Legend theme
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.1),
    legend.background = element_rect(colour = "black"),
    legend.title = element_blank()
  )

# calling the plot to display
Report_plot

# saving figure 5 to the 'figures' folder
ggsave("figures/5.Report_plot.png",
       plot = Report_plot,
       device = "png",
       width = 10, height = 7, units = "in", dpi = 300)
```

![img5](figures/5.Report_plot.png)

---

## Repository Structure

```
.
├── raw_data/
│   └── NHANES - NHANES.csv
├── figures/
│   ├── 1.exploring_plot.png
│   ├── 2.Posthoc_plot.png
│   ├── 3.Residual_plot.png
│   ├── 4.Histogram_plot.png
│   └── 5.Report_plot.png
└── analysis.R
```
