# This script analyses eye-tracking data from a visual world experiment  
# on pronouns and anaphors in German
# Created by: Nevena Klobučar.
# Updated: 03.02.2025.

# ..............................................................................
# Load packages.
library(tidyverse)
library(lmtest) 
options(scipen=999)

# Set wd
setwd("C:/Users/klobu/Dropbox/DPBE_German3_JB2_deploy/myfiles")

# Import eye tracking data
eyedata <- read.table("analysis.txt", 
                      sep = ",", 
                      header = TRUE, 
                      stringsAsFactors = TRUE, 
                      strip.white = TRUE, 
                      fill = TRUE, 
                      quote = "")

# Change TRUE/FALSE values in Aerea of Interest columns to 1/0
eyedata <- eyedata %>%
  mutate(
    Target = as.numeric(Target),     
    Competitor = as.numeric(Competitor) 
  )

# Convert data set from wider to longer
eyedata <- eyedata %>%
  pivot_longer(cols = c(Target, Competitor),  
               names_to = "IA",             
               values_to = "Fixation")   %>%
  arrange(IA, Time)

# Interst Area as factor and check levels
eyedata$IA <- factor(eyedata$IA, levels = c("Target", "Competitor"))
levels(eyedata$IA)

# Create subset of adult data
adults_eyedata <- eyedata %>%
  filter(group == "adults")

#create subset of child data
children_eyedata <- eyedata %>%
  filter(group == "children")

# ..............................................................................
# Plot adult data  ----
# ..............................................................................

# Sanity check: IA levels
levels(adults_eyedata$IA)
levels(children_eyedata$IA)

lines_IA  <- c("solid", "solid")
colour_IA <- c("#08519C","#D32F2F")
labels_IA <- c("Target", "Competitor")
labels_condition  <- c( anaphor_false = "Anaphor_False", anaphor_true = "Anaphor_True", 
                    pronoun_false = "Pronoun_False", pronoun_true = "Pronoun_True")

adults_eyedata %>%
  
  # Group fixations 
  group_by(Participant, Time, condition, IA) %>% 
  summarise(MeanFixation = mean(Fixation, na.rm = TRUE), .groups = "keep") %>%
  
  # Create plot with time on the x-axis and mean fixations on the y-axis
  ggplot(aes(x = Time, y = MeanFixation, color = IA)) +
  
  # Plot the mean fixation proportions by IA
  stat_summary(fun = mean, aes(linetype = IA),  geom = "path", linewidth = .7) +
  
  # Add a confidence interval representing variability between participants
  stat_summary(fun.data = mean_cl_boot, aes(group = IA, fill = IA), 
               geom = "ribbon", alpha = .2, color = NA) +
  
  # Create separate panels for each condition 
  facet_grid(condition ~., scales = "free_x", labeller = labeller(condition = labels_condition)) + 
  
  # Mark 50% point on the y-axis.
  geom_hline(yintercept = 0.50, linetype = "dotted", colour = "darkgrey") +

  # Mark critical window (pronoun/anaphor onset + 200 ms).
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = 315, linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = 715, linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = 1115, linetype = "dashed", color = "darkgrey") +
    
  # Format axes and scales
  labs(x = "Time from critical word onset (ms)", y = "Fixations") +
    coord_cartesian(xlim = c(-500, 2000), ylim = c(0,1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = colour_IA, 
                       labels = labels_IA, 
                       name = "Fixations to") +
    scale_fill_manual(values = colour_IA, 
                      labels = labels_IA, 
                      name = "Fixations to") +
    scale_linetype_manual(values = lines_IA, 
                          labels = labels_IA, 
                          name = "Fixations to")  +
    
    # Format plot theme.
    theme_light() +
    theme(panel.grid = element_blank()) +
    theme(legend.position = "top")

levels(adults_eyedata$IA)

# ..............................................................................
# Plot child data  ----
# ..............................................................................

children_eyedata %>%
  
  # Group fixations 
  group_by(Participant, Time, condition, IA) %>% 
  summarise(MeanFixation = mean(Fixation, na.rm = TRUE), .groups = "keep") %>%
  
  # Create plot with time on the x-axis and mean fixations on the y-axis
  ggplot(aes(x = Time, y = MeanFixation, color = IA)) +
  
  # Plot the mean fixation proportions by IA
  stat_summary(fun = mean, aes(linetype = IA),  geom = "path", linewidth = .7) +
  
  # Add a confidence interval representing variability between participants
  stat_summary(fun.data = mean_cl_boot, aes(group = IA, fill = IA), 
               geom = "ribbon", alpha = .2, color = NA) +
  
  # Create separate panels for each condition 
  facet_grid(condition ~., scales = "free_x", labeller = labeller(condition = labels_condition)) + 
  
  # Mark 50% point on the y-axis.
  geom_hline(yintercept = 0.50, linetype = "dotted", colour = "darkgrey") +
  
  # Mark critical window (pronoun/anaphor onset + 200 ms).
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = 315, linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = 715, linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = 1115, linetype = "dashed", color = "darkgrey") +
  
  # Format axes and scales
  labs(x = "Time from critical word onset (ms)", y = "Fixations") +
  coord_cartesian(xlim = c(-500, 2000), ylim = c(0,1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = colour_IA, 
                     labels = labels_IA, 
                     name = "Fixations to") +
  scale_fill_manual(values = colour_IA, 
                    labels = labels_IA, 
                    name = "Fixations to") +
  scale_linetype_manual(values = lines_IA, 
                        labels = labels_IA, 
                        name = "Fixations to")  +
  
  # Format plot theme.
  theme_light() +
  theme(panel.grid = element_blank()) +
  theme(legend.position = "top")

# ..............................................................................
# Check for autocorrelation  ----
# ..............................................................................

acf(na.omit(adults_eyedata$Fixation))
acf(na.omit(children_eyedata$Fixation))

dwtest(Fixation ~ Time + condition, data = adults_eyedata)
dwtest(Fixation ~ Time + condition, data = children_eyedata)  

# ..............................................................................
# Downsample  ----
# ..............................................................................

# Downsample data to every 100 ms.
eyedata_100 <- eyedata |> filter(Time %% 100 == 0)
head(eyedata_100)

# Create subset of adult data
adults_eyedata_100 <- eyedata_100 %>%
  filter(group == "adults")

# Create subset of child data
children_eyedata_100 <- eyedata_100 %>%
  filter(group == "children")

# Check for autocorrelation
acf(na.omit(adults_eyedata_100$Fixation))
acf(na.omit(children_eyedata_100$Fixation))
dwtest(Fixation ~ Time + condition, data = adults_eyedata_100)
dwtest(Fixation ~ Time + condition, data = children_eyedata_100)  

# ..............................................................................
#  Define dependent variable: Target vs. Competitor ----
# ..............................................................................

# Create a data set where only fixations to target are counted. 
# 1 for target means 0 for competitor.
targetdata <- eyedata_100 %>% 
  
  # Keep only rows where either the target or competitor were fixated.
  filter(Fixation == 1) %>%
  
  # Create a new variable indicating whether the target was fixated.
  mutate(Target = ifelse(IA == "Target", 1, 0)) %>%
  
  # Drop unnecessary information.
  dplyr::select(-c(IA, Fixation)) %>%
  distinct(.keep_all = TRUE) %>%
  droplevels()

# ..............................................................................
#  Code time windows ----
# ..............................................................................

# The critical window starts at the pronoun or anaphor onset (0 ms)
# and ends at 115ms, to which we add 200 ms to account for the time to 
# program and execute eye movement (Hallett, 1986; 
# Salverda, Kleinschmidt & Tanenhaus, 2014).
# We split the data set in the following windows:
# pre-critical: -500 to -1 ms
# critical: 0 to 315 ms
# post-critical: 315+ ms

# Create time windows.
targetdata$Window <- cut(targetdata$Time, right = FALSE,
                         breaks = c(-Inf, 0, 315, +Inf),
                         labels = c("pre-critical", "critical", "post-critical"))

# See table of counts (number of fixations) in each time window.
table(targetdata$Window, targetdata$Time)


# ..............................................................................
#  Create time window data ----
# ..............................................................................

# Compute the proportion of target fixations per participant  
# in the critical time window across conditions.

windowdata <- targetdata %>%

# Select only the critical window (from pronoun or anaphor onset to + 315 ms).
filter(Window == "critical") %>% 
  
  # Group fixations (collapsed across trials and conditions).
  group_by(across(-c("Time", "Trial", "Target", "Items"))) %>%  
  summarise(
    NFix      = length(Target), 
    TargetFix = sum(Target), 
    CompFix   = NFix - TargetFix, 
    PropFix   = mean(Target),
    .groups = "keep") 


# ..............................................................................
# Model with no predictors ----
# ..............................................................................

# Ran a model without any predictors to determine  
# the baseline probability of looking at the target

m <- glm(cbind(TargetFix, CompFix) ~ 1, 
         family = "binomial", 
         data = windowdata)

# Print model output.
summary(m)

# Participants looked at the target an average of 53%
# P value (0.01) suggests that this is above chance 

# ..............................................................................
# Model with a fixed effect of group ----
# ..............................................................................

# Run model.
m_group <- glm(cbind(TargetFix, CompFix) ~ group, 
               family = "binomial", 
               data = windowdata)

summary(m_group)

# Adults fixate on the target 56% of the time,
# which is significantly above chance (p < .001).
# children are significantly different from adults 
# and fixations to the target are not different from chance

# ..............................................................................
# Model adult data with a fixed effect of conditions ----
# ..............................................................................

# Filter data to include only adults
windowdata_adults <- windowdata %>%
  filter(group == "adults")

# Set "anaphor_true" as reference level for "condition"
windowdata_adults$condition <- relevel(windowdata_adults$condition, ref = "anaphor_true") 

# Run model.
m_adultsConditionAnaphor <- glm(cbind(TargetFix, CompFix) ~ condition, 
                           family = "binomial", 
                           data = windowdata_adults
)

summary(m_adultsConditionAnaphor)

# Adults fixate on the target app. 90% of the time in anaphor_true,
# this is significantly above chance.
# Adults are less likely to fixate on the Target in all other 
# conditions. 
# anaphor_false = 31%
# pronoun_false (24%) 
# pronoun_true (71%) 

# Set "pronoun_false" as reference level for "condition"
windowdata_adults$condition <- relevel(windowdata_adults$condition, ref = "pronoun_false") 

# Run model.
m_adultsConditionPronoun <- glm(cbind(TargetFix, CompFix) ~ condition, 
                                family = "binomial", 
                                data = windowdata_adults
)

summary(m_adultsConditionPronoun)

# They are significantly less likely to fixate on the Target
# in the pronoun_false condition (24% vs 71%) compared to
# pronoun_true condition.
# Marginal difference between anaphor false (31%)
# and pronoun false (24%).


# ..............................................................................
# Model children data with a fixed effect of conditions ----
# ..............................................................................

# Filter data to include only children
windowdata_children <- windowdata %>%
  filter(group == "children")

# Set "anaphor_true" as reference level for "condition"
windowdata_children$condition <- relevel(windowdata_children$condition, ref = "anaphor_true") 

# Run model.
m_childrenCondition <- glm(cbind(TargetFix, CompFix) ~ condition, 
                           family = "binomial", 
                           data = windowdata_children
                           )

summary(m_childrenCondition)

# Children fixate on the Target 76% of the time in anaphor_true
# this is significantly above chance.
# They are significantly less likely to fixate on the Target
# in other conditions. 
# anaphor_false (48%) 
# pronoun_false (15%)
# pronoun_true (44%) -> significant difference to anaphor_true

# Set "pronoun_false" as reference level for "condition"
windowdata_children$condition <- relevel(windowdata_children$condition, ref = "pronoun_false") 

# Run model.
m_childrenConditionPronoun <- glm(cbind(TargetFix, CompFix) ~ condition, 
                                family = "binomial", 
                                data = windowdata_children
)

summary(m_childrenConditionPronoun)

# They are significantly less likely to fixate on the Target
# in pronoun_false (15%) compared to pronoun_true (44%).
# But both are below chance.
# Significant difference between pronoun false and anaphor false
# but chance fixations in anaphor false.

# ..............................................................................
# Model with fixed effects of group and conditions ----
# ..............................................................................

# Set "anaphor_true" as reference level for "condition"
windowdata$condition <- relevel(windowdata$condition, ref = "anaphor_true") 

# Run model.
m_groupCondition <- glm(cbind(TargetFix, CompFix) ~ group * condition, 
                        family = "binomial", 
                        data = windowdata)

summary(m_groupCondition)


# Children are less likely to fixate on the target in both 
# true anaphor condition, but they are more likely to do
# so in the true anaphor condition.
# There is no significant difference between children and adults
# in true and false pronoun conditions.


# ..............................................................................
#  Analyse likelihood of switching gaze in the early post-critical time window -
# ..............................................................................

# Create early post-critical time window.
targetdata$Postcritical <- cut(targetdata$Time, right = FALSE,
                         breaks = c(315, 715),
                         labels = c("post-critical"))

# See table of counts (number of fixations) in each time window.
table(targetdata$Postcritical, targetdata$Time)


# Select only the early post-critical window (from 315 to 715 ms).
PostcriticalData <- targetdata %>%
  filter(Postcritical == "post-critical") 

# Compute probability of switching to the Target when the Competitor was previously fixated
compute_switch_to_target <- function(data) {
  data <- data %>%
    arrange(Participant, Trial, Time) %>%
    group_by(Participant, Trial) %>%
    mutate(
      PreviousFixation = lag(Target, default = NA),  # Get previous fixation
      SwitchToTarget = ifelse(!is.na(PreviousFixation) & 
                                PreviousFixation == 0 & 
                                Target == 1, 1, 0)  # 1 if switch was from competitor (0) to target (1)
    ) %>%
    ungroup()
  
  return(data)  # Return full dataset with SwitchToTarget column
}

# Apply the function to the dataset
PostcriticalData <- compute_switch_to_target(PostcriticalData)

# Compute probability of switching to the Target when the Competitor was previously fixated
switch_to_target_probs <- PostcriticalData %>%
  group_by(group, condition) %>%
  summarise(
    SwitchToTargetRate = mean(SwitchToTarget, na.rm = TRUE),  # Mean gives probability
    Count = n(),  # Number of observations per group/condition
    .groups = "drop"
  )

# Print the results
print(switch_to_target_probs)

# Set "anaphor_true" as reference level for "condition"
PostcriticalData$condition <- relevel(PostcriticalData$condition, ref = "anaphor_false") 

# Run logistic regression to analyze switching behavior
switch_glm <- glm(SwitchToTarget ~ group * condition, 
                  family = binomial, 
                  data = PostcriticalData)

# Print the results
summary(switch_glm)


# No group effect
# nor interaction between group and condition
# adults are significantly more likely to switch to target in anaphor false 
# compared to true and in pronoun false compared to anaphor true
# when the reference level is pronoun false
# it reveals no significant difference between true and false pronouns, 
# nor between false pronoun and false anaphor

PostcriticalData_children <- PostcriticalData %>%
  filter(group == "children")

switch_children <- glm(SwitchToTarget ~ condition, 
                  family = binomial, 
                  data = PostcriticalData_children)

# Print the results
summary(switch_children)

# No significant effects


# ..............................................................................
#  Analyse likelihood of switching gaze in the mid post-critical time window --
# ..............................................................................


# Create mid post-critical time window.
targetdata$MidPostcritical <- cut(targetdata$Time, right = FALSE,
                               breaks = c(715, 1115),
                               labels = c("Mid-post-critical"))

# See table of counts (number of fixations) in each time window.
table(targetdata$MidPostcritical, targetdata$Time)


# Select only the mid post-critical window (from 315 to 715 ms).
MidPostcritical <- targetdata %>%
  filter(MidPostcritical == "Mid-post-critical") 

# Compute probability of switching to the Target when the Competitor was previously fixated
compute_switch_to_target <- function(data) {
  data <- data %>%
    arrange(Participant, Trial, Time) %>%
    group_by(Participant, Trial) %>%
    mutate(
      PreviousFixation = lag(Target, default = NA),  # Get previous fixation
      SwitchToTarget = ifelse(!is.na(PreviousFixation) & PreviousFixation == 0 & Target == 1, 1, 0)  # 1 if switch was from competitor (0) to target (1)
    ) %>%
    ungroup()
  
  return(data)  # Return full dataset with SwitchToTarget column
}

# Apply the function to the dataset
MidPostcritical <- compute_switch_to_target(MidPostcritical)

# Compute probability of switching to the Target when the Competitor was previously fixated
switch_to_target_probs <- MidPostcritical %>%
  group_by(group, condition) %>%
  summarise(
    SwitchToTargetRate = mean(SwitchToTarget, na.rm = TRUE),  # Mean gives probability
    Count = n(),  # Number of observations per group/condition
    .groups = "drop"
  )

# Print the results
print(switch_to_target_probs)

# Set "anaphor_true" or "pronoun_false" as reference level for "condition"
MidPostcritical$condition <- relevel(MidPostcritical$condition, ref = "anaphor_false") 

# Run logistic regression to analyze switching behavior
switch2_glm <- glm(SwitchToTarget ~ group * condition, 
                  family = binomial, 
                  data = MidPostcritical)

# Print the results
summary(switch2_glm)

# No significant effects


# No group effect
# nor interaction between group and condition
# adults are significantly more likely to switch to target in anaphor false 
# compared to true and in pronoun false compared to anaphor true
# when the reference level is pronoun false
# it reveals no significant difference between true and false pronouns, 
# nor between false pronoun and false anaphor

MIdPostcriticalData_children <- MidPostcritical %>%
  filter(group == "children")

switch2_children <- glm(SwitchToTarget ~ condition, 
                       family = binomial, 
                       data = MIdPostcriticalData_children)

# Print the results
summary(switch2_children)

# Marginal effect of anaphor false: children were more likely to switch gaze 
# in anaphor false than anaphor true (p = .08) 

