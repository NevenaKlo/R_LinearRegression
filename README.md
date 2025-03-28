# R_LinearRegression
This repository contains linear regression analyses of a Yes/No judgment task, and a visual world experiment with eyetracking.

**Judgment Task**
The judgment task file reports a linear regression analysis of a Picture Verification Task using yes/no responses. Responses were coded as 1=accurate, 0=inaccurate. The package lme4 was used to fit two generalized mixed effects model to the data. 

- Model m1 investigates the effect of two predictor variables on response accuracy;
- Model m2 checks for age effects.

Furthermore, a t-test is used to check whether group accuracy is significantly above chance or not.
Lastly, the package tidyverse is used to plot the data at both group and individual levels.

-----------------

**Eyetracking**
The eyetracking file reports a linear regression analysis of participants fixation patterns in a predifined window of time of the same task. Data were previously cleaned and preprocessed.

The package tidyverse is used to visualize the data. 

First, a null model is used to determine the baseline probability of looking at the target (dependent variable). Then, a glm model with fixed effects of group (children, adults) is used to evalute differences betwen the two groups. Given significant differences between the two groups, two dataframes are created for each group.

To each data set, a mixed effects model is fit to estimate the effect of the predictor variable "condition" on fixations to Target. 

----------------

Bonus: A probability of switching gaze from Competitor to Target is computed in the end for different time windows in the two groups.
