#Predictive Modeling of Subscription Behavior Using R
This project explores consumer subscription patterns using logistic regression analysis and data visualization in R Studio. The aim is to uncover insights into what drives individuals to subscribe based on various demographic and behavioral attributes.

Objective
The study investigates several key questions:

How does age influence subscription decisions?

Are there gender-based patterns in subscription behavior?

What is the impact of occupation, marital status, education level, salary, and communication method on subscription likelihood?

How do past campaign outcomes, contact duration, and economic indices affect consumer choices?

Methodology
The dataset was imported and pre-processed using readxl, dplyr, and tidyr. Missing values (e.g., age marked as 999) were handled, and categorical variables were converted into factors. Descriptive statistics and visualizations such as pie charts, bar plots, heatmaps, and mosaic plots were created using ggplot2 and base R plotting functions.

Inferential statistical tests including Chi-square, t-tests, and ANOVA were applied to identify significant relationships between predictors and subscription outcomes.

A series of logistic regression models—ranging from simple models (e.g., age only) to complex ones with multiple variables—were developed using the glm() function. Each model was evaluated based on pseudo R² statistics (Hosmer-Lemeshow, Cox & Snell, Nagelkerke), prediction accuracy, and classification performance using caret::postResample() and confusion matrices.

Conclusion
The final model highlights the strongest predictors of subscription behavior, offering actionable insights for targeted marketing strategies. This analysis demonstrates how data science can support decision-making in customer engagement and campaign optimization.

Tools Used: R Studio, readxl, dplyr, ggplot2, caret, car, psych, tidyr.

