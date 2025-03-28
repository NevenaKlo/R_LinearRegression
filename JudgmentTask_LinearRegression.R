library(emmeans)
library(lme4)
library(plyr)
library(ggplot2)
options(scipen=999)
library(dplyr)
library(tidyr)


#do pronoun type and/or stimuli type affect accuracy? Is there an interaction?
m1 = glmer(accuracy ~ pronoun_type * stimuli_type +
           (1|subjects) + (1|item),
           family = binomial(),
           data = Offline_children
           )

summary(m1)

#check for age effect
m2 = glmer(accuracy ~ age +
             (1|subjects) + (1|item),
           family = binomial(),
           data = Offline_children
)

summary(m2)

#Is accuracy significantly above chance with pronouns and/or anaphors?
#Create subsets 

pronoun_data = subset(Offline_children, pronoun_type=="pronoun") #subset of only pronouns

anaphor_data = subset(Offline_children, pronoun_type=="anaphor") #subset of only anaphors

#t-test
t.test(pronoun_data$accuracy, mu=0.5, alt="g") 


t.test(anaphor_data$accuracy, mu=0.5, alt="g") 


#create graphs

data = Offline_children

n = length(unique(data$subjects))

averages_by_subject = ddply(data,
                            .(subjects, pronoun_type, stimuli_type), summarize,
                            average = mean(accuracy, na.rm = TRUE))

averages = ddply(averages_by_subject,
                 .(pronoun_type, stimuli_type), summarize,
                 mean = mean(average, na.rm = TRUE),
                 se = sd(average, na.rm = TRUE)/sqrt(n))

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#graph with pronoun type and stimuli type

plot_truefalse = ggplot(averages, aes(x=pronoun_type, y=mean, fill=stimuli_type)) + 
  geom_bar(position=position_dodge(), stat="identity",
           color="black", # Use black outlines,
           linewidth=.3) +    # Thinner lines     
  geom_dotplot(data=averages_by_subject, aes(y=average),
               binaxis='y', stackdir='center', dotsize=.5,position=position_dodge())+
  geom_errorbar(aes(y=mean, ymin=mean-se, ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  ylab("accuracy") +
  scale_x_discrete(limits=c("pronoun", "anaphor"),
                   labels=c("pronoun", "anaphor"))+  
  scale_fill_manual(breaks=c("true", "false"),
                    values=c("#F0E442", "#009E73")) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw(base_size=20) 

plot(plot_truefalse) 


#graph with only pronoun type

#create averages by pronoun type

SubjectAverages = ddply(data,
                            .(subjects, pronoun_type), summarize,
                            average = mean(accuracy, na.rm = TRUE))

AveragesPrType = ddply(SubjectAverages,
                 .(pronoun_type), summarize,
                 mean = mean(average, na.rm = TRUE),
                 se = sd(average, na.rm = TRUE)/sqrt(n))

plot_onlypronouns = ggplot(AveragesPrType, aes(x=pronoun_type, y=mean, fill=pronoun_type)) + 
  geom_bar(position=position_dodge(), stat="identity",
           color="black", # Use black outlines,
           linewidth=.3) +    # Thinner lines     
  geom_dotplot(data=averages_by_subject, aes(y=average),
               binaxis='y', stackdir='center', dotsize=.5,position=position_dodge())+
  geom_errorbar(aes(y=mean, ymin=mean-se, ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  ylab("accuracy") +
  scale_x_discrete(limits=c("pronoun", "anaphor"),
                   labels=c("pronoun", "anaphor"))+  
  scale_fill_manual(breaks=c("pronoun", "anaphor"),
                    values=c("grey", "white")) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw(base_size=20) 

plot(plot_onlypronouns) 

# ..............................................................................
# Plot data with age and accuracy ----
# ..............................................................................

# Create subset of true pronouns
PronounsTrue <- Offline_children %>%
  filter(pronoun_type == "pronoun") %>%
  filter(stimuli_type == "true")

# Aggregate accuracy by age
Summary_pronounsTrue <- PronounsTrue %>%
  group_by(age) %>%
  summarise(mean_accuracy = mean(accuracy), 
            n = n())

# Plot using GAM
ggplot(Summary_pronounsTrue, aes(x = age, y = mean_accuracy)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps"), se = TRUE, color = "blue") +
  ylim(0, 1.05) +
  labs(title = "Effect of Age on Accuracy with True Pronouns",
       x = "Age (Days)",
       y = "Mean Accuracy",
       caption = "Smoothed GAM curve with 95% CI") +
  theme_minimal()          

# Create subset of false pronouns
PronounsFalse <- Offline_children %>%
  filter(pronoun_type == "pronoun") %>%
  filter(stimuli_type == "false")

# Aggregate accuracy by age
Summary_pronounsFalse <- PronounsFalse %>%
  group_by(age) %>%
  summarise(mean_accuracy = mean(accuracy), 
            n = n())

# Plot using GAM
ggplot(Summary_pronounsFalse, aes(x = age, y = mean_accuracy)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps"), se = TRUE, color = "blue") +
  ylim(0, 1.05) +
  labs(title = "Effect of Age on Accuracy with False Pronouns",
       x = "Age (Days)",
       y = "Mean Accuracy",
       caption = "Smoothed GAM curve with 95% CI") +
  theme_minimal()  

# Create susbset of true anaphors
AnaphorTrue <- Offline_children %>%
  filter(pronoun_type == "anaphor") %>%
  filter(stimuli_type == "true")

# Aggregate accuracy by age
Summary_anaphorTrue <- AnaphorTrue %>%
  group_by(age) %>%
  summarise(mean_accuracy = mean(accuracy), 
            n = n())

# Plot using GAM
ggplot(Summary_anaphorTrue, aes(x = age, y = mean_accuracy)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps"), se = TRUE, color = "blue") +
  ylim(0, 1.05) +
  labs(title = "Effect of Age on Accuracy with True Anaphors",
       x = "Age (Days)",
       y = "Mean Accuracy",
       caption = "Smoothed GAM curve with 95% CI") +
  theme_minimal()  

# Create susbset of false anaphors
AnaphorFalse <- Offline_children %>%
  filter(pronoun_type == "anaphor") %>%
  filter(stimuli_type == "false")

# Aggregate accuracy by age
Summary_anaphorFalse <- AnaphorFalse %>%
  group_by(age) %>%
  summarise(mean_accuracy = mean(accuracy), 
            n = n())

# Plot using GAM
ggplot(Summary_anaphorFalse, aes(x = age, y = mean_accuracy)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps"), se = TRUE, color = "blue") +
  ylim(0, 1.05) +
  labs(title = "Effect of Age on Accuracy with False Anaphors",
       x = "Age (Days)",
       y = "Mean Accuracy",
       caption = "Smoothed GAM curve with 95% CI") +
  theme_minimal()  


# ..............................................................................
# Plot individual data per condition ----
# ..............................................................................


#add 'condition' column
averages_by_subject$condition <- with(averages_by_subject, paste(pronoun_type, stimuli_type, sep = " "))

individual_data = ggplot(averages_by_subject, aes(x = subjects, y = average, group = condition, color = condition)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 1.5) + 
  theme_minimal() +
  labs(x = "Subject", y = "Accuracy", title = "Individual Accuracy Across Conditions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   # Rotate x-axis labels for readability
  

plot(individual_data)

#scatterplot with shapes instead of coloured points

shapes_plot = ggplot(averages_by_subject, aes(x = subjects, y = average, shape = condition, color = subjects)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 2) +  # Adjust point size
  theme_minimal() +
  labs(x = "Participant", y = "Accuracy", title = "Accuracy Across Conditions for Each Participant") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(shapes_plot)

#line plot for individual results

line_plot= ggplot(averages_by_subject, aes(x = subjects, y = average, group = condition, color = condition)) +
  geom_line(position = position_jitter(width = 0.2, height = 0), size = 0.5) +  # Line geometry
  #geom_point(size = 1.5) +  # Optional: Add points at each condition for clarity
  theme_minimal() +
  labs(x = "Condition", y = "Accuracy", title = "Accuracy Across Conditions for Each Participant") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(line_plot)

#calculate variance of accuracy across conditions per participant

participant_summary <- averages_by_subject |>
  group_by(subjects) |>
  summarise(average_variance = var(average))

# Order participants by accuracy range (larger differences come first)
participant_summary <- participant_summary |>
  arrange(desc(average_variance))

# Merge the original data with the new ordering
data_ordered <- averages_by_subject |>
  left_join(participant_summary, by = "subjects") |>
  mutate(subjects = factor(subjects, levels = participant_summary$subjects))

plot_variance = ggplot(data_ordered, aes(x = subjects, y = average, shape = condition, color = subjects)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 1.8) +  # Adjust point size
  theme_minimal() +
  labs(x = "Participant", y = "Accuracy", title = "Accuracy Across Conditions for Each Participant") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(plot_variance)

#same plot but with colours instead of shapes

plot_variance2 = ggplot(data_ordered, aes(x = subjects, y = average, group = condition, color = condition)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 1.5) +  # Adjust point size
  theme_minimal() +
  labs(x = "Participant", y = "Accuracy", title = "Accuracy Across Conditions for Each Participant") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(plot_variance2)

#line plot with variance
line_plot2= ggplot(data_ordered, aes(x = subjects, y = average, group = condition, color = condition)) +
  geom_line(position = position_jitter(width = 0.2, height = 0), size = 0.5) +  # Line geometry
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 1.5) +  # Optional: Add points at each condition for clarity
  theme_minimal() +
  labs(x = "Condition", y = "Accuracy", title = "Accuracy Across Conditions for Each Participant") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(line_plot2)


#compare subsets of data (pronoun: F vs T; anaphor: F vs T; pronoun F vs anaphor F and pronoun T vs anaphor T)

#create first subset (only pronouns)
pronouns_ordereddata = subset(data_ordered, pronoun_type=="pronoun")

#plot
individual_pronouns = ggplot(pronouns_ordereddata, aes(x = subjects, y = average, group = condition, color = condition)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 1.5) +  # Adjust point size
  theme_minimal() +
  labs(x = "Participant", y = "Accuracy", title = "Accuracy Across Conditions for Each Participant") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(individual_pronouns)

#create second subset (only anaphors)
anaphors_ordereddata = subset(data_ordered, pronoun_type=="anaphor")

#plot
individual_anaphors = ggplot(anaphors_ordereddata, aes(x = subjects, y = average, group = condition, color = condition)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 1.5) +  # Adjust point size
  theme_minimal() +
  labs(x = "Participant", y = "Accuracy", title = "Accuracy Across Conditions for Each Participant") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(individual_anaphors)

#create third subset (true pronouns and anaphors)
true_stimuli = subset(data_ordered, stimuli_type=="true")

#create plot
individual_true = ggplot(true_stimuli, aes(x = subjects, y = average, group = condition, color = condition)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 1.5) +  # Adjust point size
  theme_minimal() +
  labs(x = "Participant", y = "Accuracy", title = "Accuracy Across Conditions for Each Participant") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot
plot(individual_true)

#create fourth subset (false pronouns and anaphors)
false_stimuli = subset(data_ordered, stimuli_type=="false")

#create plot 
individual_false = ggplot(false_stimuli, aes(x = subjects, y = average, group = condition, color = condition)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 1.5) +  # Adjust point size
  theme_minimal() +
  labs(x = "Participant", y = "Accuracy", title = "Accuracy Across Conditions for Each Participant") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot
plot(individual_false)

#create scatterplot with conditions on x and y axes
# Reshape the pronouns data to wide format
pronouns_wide <- pronouns_ordereddata %>%
  select(subjects, stimuli_type, average) %>%
  pivot_wider(names_from = stimuli_type, values_from = average, names_prefix = "condition_")

# View the reshaped data
print(pronouns_wide)

scatterplot_pronouns = ggplot(pronouns_wide, aes(x = condition_true, y = condition_false)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 1.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add regression line
  labs(
    title = "Scatterplot of Pronoun True vs Pronoun False",
    x = "Accuracy in Pronoun True Condition",
    y = "Accuracy in Pronoun False Condition"
  ) +
  theme_minimal()

plot(scatterplot_pronouns)

#reshape the anaphors data to wide format
anaphors_wide <- anaphors_ordereddata |>
  select(subjects, stimuli_type, average) |>
  pivot_wider(names_from = stimuli_type, values_from = average, names_prefix = "condition_")

# View the reshaped data
print(anaphors_wide)

#scatterplot anaphors
scatterplot_anaphors = ggplot(anaphors_wide, aes(x = condition_true, y = condition_false)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 1.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add regression line
  labs(
    title = "Scatterplot of Anaphor True vs Anaphor False",
    x = "Accuracy in Anaphor True Condition",
    y = "Accuracy in Anaphor False Condition"
  ) +
  theme_minimal()

plot(scatterplot_anaphors)

#reshape true data to wide format
true_wide <- true_stimuli |>
  select(subjects, pronoun_type, average) |>
  pivot_wider(names_from = pronoun_type, values_from = average, names_prefix = "condition_")

print(true_wide)

#scatterplot true stimuli
scatterplot_true = ggplot(true_wide, aes(x = condition_anaphor, y = condition_pronoun)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 1.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add regression line
  labs(
    title = "Scatterplot of True Anaphor vs Pronoun stimuli",
    x = "Accuracy in Anaphor Condition",
    y = "Accuracy in Pronoun Condition"
  ) +
  theme_minimal()

plot(scatterplot_true)

#reshape false data to wide format
false_wide <- false_stimuli |>
  select(subjects, pronoun_type, average) |>
  pivot_wider(names_from = pronoun_type, values_from = average, names_prefix = "condition_")

print(false_wide)

#scatterplot false stimuli
scatterplot_false = ggplot(false_wide, aes(x = condition_anaphor, y = condition_pronoun)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), size = 1.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add regression line
  labs(
    title = "Scatterplot of False Anaphor vs Pronoun stimuli",
    x = "Accuracy in Anaphor Condition",
    y = "Accuracy in Pronoun Condition"
  ) +
  theme_minimal()

plot(scatterplot_false)