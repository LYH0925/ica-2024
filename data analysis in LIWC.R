setwd("~/Desktop")

install.packages("tidyverse")
install.packages("anytime")
install.packages("lme4")
install.packages("lmerTest")
install.packages("interactions")
install.packages("stringr")
install.packages("sentimentr")

library(tidyverse)
library(anytime)
library(lme4)
library(lmerTest)
library(interactions)
library(stringr)
library(sentimentr)
install.packages("readxl")
library(readxl)
library(ggplot2)
library(readxl)
library(dplyr)
install.packages("dplyr")

LIWC_result <- read_excel("LIWC_result.xlsx")
View(LIWC_result)


colnames(LIWC_result)
str(LIWC_result)

# Convert the 'time_category' column to datetime
LIWC_result$time_category <- anytime(LIWC_result$time_category)

# Format as 'Year' format
LIWC_result <- LIWC_result %>%
  mutate(year = format(as.Date(time_category), "%Y"))

head(LIWC_result)

library(dplyr)

group_mean_comments_1 <- LIWC_result %>%
  mutate(time_category = format(time_category, "%Y-%m")) %>%  # 提取年月
  group_by(comment_stance, time_category) %>%
  summarise(analytic_mean = mean(Analytic, na.rm = TRUE), .groups = 'drop')

head(group_mean_comments_1)


group_mean_comments_1$time_category <- anydate(group_mean_comments_1$time_category)

group_mean_comments_1$comment_stance <- as.factor(group_mean_comments_1$comment_stance)

ggplot(group_mean_comments_1, aes(x = time_category, y = analytic_mean, group = comment_stance)) +
  geom_line(aes(color = comment_stance)) +
  scale_color_manual(values = c("gray", "orange", "blue"),
                     labels = c("Neutral","anti-TCM", "pro-TCM")) +
  labs(x = "Year", 
       y = "Analytical thinking",
       colour = "comment stance")


# Calculate the mean and standard deviation of analytical thinking scores for each stance
comments_analytic <- group_by(LIWC_result, `comment_stance`) %>%
  summarise(mean = mean(Analytic, na.rm = TRUE), sd = sd(Analytic, na.rm = TRUE))

# ANOVA analysis
anova_comments <- aov(Analytic ~ `comment_stance`, data = LIWC_result)
summary(anova_comments)

lm_model <- lm(Analytic ~ comment_stance, data = LIWC_result)
summary(lm_model)

group_mean_comments_2 <- LIWC_result %>%
  mutate(time_category = format(time_category, "%Y-%m")) %>%  # 提取年月
  group_by(theme, time_category) %>%
  summarise(analytic_mean = mean(Analytic, na.rm = TRUE), .groups = 'drop')

head(group_mean_comments_2)

group_mean_comments_2$time_category <- anydate(group_mean_comments_2$time_category)

group_mean_comments_2$theme <- as.factor(group_mean_comments_2$theme)


group_mean_comments_3 <- LIWC_result %>%
  mutate(time_category = format(time_category, "%Y-%m")) %>%  # 提取年月
  group_by(comment_level, time_category) %>%
  summarise(analytic_mean = mean(Analytic, na.rm = TRUE), .groups = 'drop')

head(group_mean_comments_3)

group_mean_comments_3$time_category <- anydate(group_mean_comments_3$time_category)


group_mean_comments_3$comment_level <- factor(group_mean_comments_3$comment_level, levels = c("1", "2"))

ggplot(group_mean_comments_3, aes(x = time_category, y = analytic_mean, group = comment_level)) +
  geom_line(aes(color = comment_level)) +
  scale_color_manual(values = c("blue", "yellow"),
                     labels = c("first-order","second-order")) +
  labs(x = "Year", 
       y = "Analytical thinking",
       colour = "comment order")


comments_analytic <- group_by(LIWC_result, theme) %>%
  summarise(mean=mean(Analytic, na.rm = TRUE), sd=sd(Analytic, na.rm = TRUE))

anova_comments <- aov(Analytic ~ comment_level, data = LIWC_result)
summary(anova_comments)

lm_model <- lm(Analytic ~ comment_level, data = LIWC_result)
summary(lm_model)

summary_df <- df_filtered %>%
  group_by(Narrative_framework, comment_stance) %>%
  summarise(
    mean_response = mean(Analytic, na.rm = TRUE),
    se_response = sd(Analytic, na.rm = TRUE) / sqrt(n())
  )

summary_df$comment_stance <- factor(summary_df$comment_stance,
                                    levels = c(1, 2),
                                    labels = c("pro-TCM", "anti-TCM"))

summary_df$Narrative_framework <- factor(summary_df$Narrative_framework)
levels(summary_df$Narrative_framework) <- c("Conspiracy theory", "Political and Culture inclination", "Scientific reasoning")


# Plot a paired dot plot and error bar plot
summary_df$Narrative_framework<-factor(summary_df$Narrative_framework)
ggplot(summary_df, aes(x = comment_stance, y = mean_response, color = Narrative_framework, group = Narrative_framework)) +
  geom_point(size = 4) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_response - se_response, ymax = mean_response + se_response), width = 0.2) +
  labs(x = "comment_stance",
       y = "Analytic thinking") +
  theme_bw()


