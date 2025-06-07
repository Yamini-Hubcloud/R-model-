install.packages("readxl") 
install.packages("dplyr") 
install.packages("ggplot2") 
install.packages("caret") 
install.packages("car") 
install.packages("psych") 
install.packages("tidyr") 
library(readxl) 
# IMPORT DATA ## 
TD <- read_excel("C:/Users/Admin/Desktop/MSc BA/7100 SB/termsv.xlsx") 
View(TD) 
summary(TD) 
boxplot(TD$age)$OUT 
# OUTLIERS ### 
table(TD$occupation) 
table(TD$gender) 
table(TD$marital_status) 
table(TD$education_level) 
table(TD$contact_method) 

table(TD$salary) 
table(TD$subscribed) 
library(dplyr) 
# DATA FORMATTING #### 
TD <- TD %>% mutate_if(is.character, as.factor) 
TD<- TD %>% 
  mutate(age= ifelse(age == 999, NA, age)) 
table(TD$gender) 
TD<- TD %>% 
  mutate(gender= ifelse(gender == "m", "Male", gender)) 
data <- na.omit(TD)  
# VISUALIZATION ##### 
library(ggplot2) 
#  pie chart subscribed 
subscription_summary <- table(TD$subscribed) 
pie( 
  subscription_summary, 
  labels = paste(names(subscription_summary), "(", subscription_summary, ")", sep = ""), 
  main = "Subscription (Yes/No)", 
  col = c("green", "violet"))  
#  pie chart previous campaign outcome 
cmpn_summary <- table(TD$previous_campaign_outcome) 
pie( 
  cmpn_summary, 
  labels = paste(names(cmpn_summary), "(", cmpn_summary, ")", sep = ""), 
  main = "prvs cmpgn", 
  col = c("yellow","cadetblue","lightpink"))  
# 1 Plot a bar chart age vs subsription 
subscription_by_age <- table( TD$subscribed, TD$age) 
barplot( 
  subscription_by_age, 

  beside = TRUE, 
  col = c("hotpink","cadetblue"),   
  legend = rownames(subscription_by_age), 
  main = " Subscription by age ", 
  xlab = "edu level", 
  ylab = "Count" 
) 
aov(age ~ subscribed, data = TD) 
summary(anova_age) 
# 2 Plot a heat map chart gender vs subsription 
gender_subscription <- table(TD$gender,TD$subscribed) 
ggplot(as.data.frame(gender_subscription), aes(Var1, Var2, fill = Freq)) + 
  geom_tile() + 
  scale_fill_gradient(low = "mistyrose", high = "steelblue") + 
  labs(title = "Heatmap:   vs Subscription", x = "gender", y = "Subscription Status") + 
  theme_minimal() 
t.test(gender_subscription) 
# 3 Plot a staked bar chart occupation vs subsription 
occu_subscription <- table(TD$subscribed,TD$occupation) 
barplot( 
  occu_subscription, 
  beside = TRUE, 
  col = c("skyblue", "gold"), 
  legend = rownames(occu_subscription), 
  main = "Job Type vs Subscription", 
  xlab = "Subscription Status", 
  ylab = "Count", 
) 
chisq.test(  occu_subscription) 
# 4 Plot a mosacic marital status vs subsription 

marital_subscription <- table(TD$marital_status, TD$subscribed) 
# Mosaic plot 
mosaicplot( 
  marital_subscription, 
  main = "Mosaic Plot: Marital Status vs. Subscription", 
  xlab = "Marital Status", 
  ylab = "Subscription Status", 
  color = c("hotpink", "salmon"),   
  las = 1   
) 
t.test(marital_subscription) 
# 5 Plot a  bar chart educationlevel vs subsription 
subscription_edu <- table( TD$subscribed,TD$education_level) 
barplot( 
  subscription_edu, 
  beside = TRUE, 
  col = c("darkslategrey","cadetblue"),   
  legend = rownames(subscription_edu), 
  main = " Subscription by education level", 
  xlab = "edu level", 
  ylab = "Count" 
) 
t.test(subscription_edu) 
# 6 Plot a mosaicplot contact_method vs subsription 
cnct_subscription <- table(TD$contact_method, TD$subscribed) 
# Mosaic plot 
mosaicplot( 
  cnct_subscription, 
  main = "Mosaic Plot: contactmethod vs. Subscription", 
  xlab = "conact mehod ", 
  ylab = "Subscription Status", 
  19 
  color = c("hotpink", "greenyellow"),  
  las = 1   
) 
chisq.test( cnct_subscription) 
# 7 Plot a heatmap salary vs subsription 
salary_subscription <- table(TD$salary, TD$subscribed) 
ggplot(as.data.frame(salary_subscription), aes(Var1, Var2, fill = Freq)) + 
  geom_tile() + 
  scale_fill_gradient(low = "black", high = "lightyellow") + 
  labs(title = "Heatmap: sal vs Subscription", x = "salary outcome", y = "Subscription Status") + 
  theme_minimal() 
chisq.test( salary_subscription)test <- TD[-index, ] 

# Descriptive Statistics # 
BEFORE FACTOR  



AFTER FACTOR 








#Regression Model ####### 
library(car) 
library(caret) 
set.seed(40459080)  
index <- createDataPartition(TD$subscribed, p = 0.8, list = FALSE) 
train <- TD[index, ] 
formula <- subscribed ~ age 
#SIMPLE LINEAR REGRESSION # 
model <- glm(formula, data = train, family = "binomial") 
summary(model) 
# probality and illustrations # 
nd <- data.frame(age= c(17,  95)) 
probabilities <- model %>% predict(nd, type = "response") 
predicted.classes <- ifelse(probabilities > 0.5, "yes", "no") 
predicted.classes 
library(ggplot2) 
train %>% 
  mutate(prob = ifelse(subscribed == "yes", 1, 0)) %>% 
  ggplot(aes(age, prob)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial")) + 
  labs( 
    title = "Logistic Regression Model",  
    x = "concentration on subscription age", 
    y = "Probability " 
  ) 
#pseudo R Squared###### 
logisticPseudoR2s <- function(LogModel) { 
  dev <- LogModel$deviance  
  nullDev <- LogModel$null.deviance  
  modelN <- length(LogModel$fitted.values) 
  22 
  R.l <-  1 -  dev / nullDev 
  R.cs <- 1- exp ( -(nullDev - dev) / modelN) 
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN)))) 
  cat("Pseudo R^2 for logistic regression\n") 
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n") 
  cat("Cox and Snell R^2    
    ", round(R.cs, 3), "\n") 
  cat("Nagelkerke R^2         
} 
logisticPseudoR2s(model) 
# PREDICTION ### 
  ", round(R.n, 3),    "\n") 
  prediction <- predict(model, test, type  = "response") 
  class_pred <- as.factor(ifelse(prediction > 0.5, "yes", "no")) 
  postResample(class_pred, test$subscribed) 
  # MULTIPLE LOGISTICS REGRESSION # 
  # MODEL 1 # 
  formula1 <- subscribed ~ age+salary+occupation+emp_var_rate 
  model1 <- glm(formula1, data = train, family = "binomial") 
  summary(model1) 
  #pseudo R Squared###### 
  logisticPseudoR2s(model1) 
  # PREDICTION ### 
  prediction1 <- predict(model1, test, type  = "response") 
  class_pred1 <- as.factor(ifelse(prediction1 > 0.5, "yes", "no")) 
  postResample(class_pred1, test$subscribed) 
  # MODEL 2 # 
  formula2 <- subscribed ~ age+salary+occupation+emp_var_rate+education_level 
  model2 <- glm(formula2, data = train, family = "binomial") 
  summary(model2) 
  #pseudo R Squared###### 
  
  logisticPseudoR2s(model2) 
  # PREDICTION ### 
  prediction2<- predict(model2, test, type  = "response") 
  class_pred2 <- as.factor(ifelse(prediction2 > 0.5, "yes", "no")) 
  postResample(class_pred2, test$subscribed) 
  # MODEL 3 # 
  formula3 <- subscribed ~age+salary+occupation+contact_duration+previous_contacts 
  model3 <- glm(formula3, data = train, family = "binomial") 
  summary(model3) 
  #pseudo R Squared# 
  logisticPseudoR2s(model3) 
  # PREDICTION ### 
  prediction3<- predict(model3, test, type  = "response") 
  class_pred3 <- as.factor(ifelse(prediction3 > 0.5, "yes", "no")) 
  postResample(class_pred3, test$subscribed) 
  # MODEL 4 # 
  formula4<- subscribed ~age+salary+occupation+contact_duration+previous_contacts+marital_status 
  model4 <- glm(formula4, data = train, family = "binomial") 
  summary(model4) 
  #pseudo R Squared# 
  logisticPseudoR2s(model4) 
  # PREDICTION ### 
  prediction4<- predict(model4, test, type  = "response") 
 
  class_pred4<- as.factor(ifelse(prediction4 > 0.5, "yes", "no")) 
  postResample(class_pred4, test$subscribed) 
  # MODEL 5 # 
  formula5<- subscribed 
  ~age+gender+occupation+marital_status+education_level+cons_price_idx+contact_method+salary 
  model5<- glm(formula5, data = train, family = "binomial") 
  summary(model5) 
  #pseudo R Squared# 
  logisticPseudoR2s(model5) 
  # PREDICTION ### 
  prediction5<- predict(model5, test, type  = "response") 
  class_pred5<- as.factor(ifelse(prediction5 > 0.5, "yes", "no")) 
  postResample(class_pred5, test$subscribed) 
  # MODEL 6 # 
  formula6<- subscribed ~age+salary+occupation+contact_duration+previous_contacts+ 
    marital_status+previous_campaign_outcome+cons_price_idx+gender+mortgage+cons_conf_idx 
  model6 <- glm(formula6, data = train, family = "binomial") 
  summary(model6) 
  #pseudo R Squared# 
  logisticPseudoR2s(model6) 
  # PREDICTION ### 
  prediction6<- predict(model6, test, type  = "response") 
  class_pred6<- as.factor(ifelse(prediction6 > 0.5, "yes", "no")) 
  postResample(class_pred6, test$subscribed) 
  confusionMatrix(class_pred6, test$subscribed) 
  
  exp(model6$coefficients) 
  # Leverage 
  hatvalues(model4)  
  # Cook's distance 
  max(cooks.distance(model6))   
  #Multicolenarity # 
  vif(model6) 
  exp(model6$coefficients) 
  #LOG INT REGRESSION 
  train$agelogint <- log(train$age)*train$age 
  # MODEL 7 # 
  formula7<- subscribed 
  ~age+salary+occupation+contact_duration+previous_contacts+marital_status+train$agelogint 
  model7 <- glm(formula7, data = train, family = "binomial") 
  summary(model7) 
  #pseudo R Squared# 
  logisticPseudoR2s(model7) 
  # PREDICTION ### 
  prediction7<- predict(model7, test, type  = "response") 
  class_pred7<- as.factor(ifelse(prediction7 > 0.5, "yes", "no")) 
  postResample(class_pred7, test$subscribed)