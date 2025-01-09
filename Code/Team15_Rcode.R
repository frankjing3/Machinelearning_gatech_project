#setwd("C:/AA_OldComp/A_OMSA/MGT6203/project/Team-15/Code")
##############Step One - Data cleaning&Manipulation###############
rm(list=ls())
###import data###
#install.packages("readxl")
library("readxl")
#import raw data file without header
df <- read_excel("default_data.xls", col_names=FALSE)
#remove first row
df<-df[-1,]
#set first row as header, and remove the first row
names(df)<-df[1,]
df<-df[-1,]

#rename the last column to "default"
colnames(df)
names(df)[names(df)=="default payment next month"]<- "default"
View(df)


###remove rows with unexplained value 
df=df[df$EDUCATION != 0 & df$EDUCATION != 5 & df$EDUCATION != 6 & df$MARRIAGE !=0,]
View(df)
#originally 30000 rows of data, after removal 29601 rows left

###add variables###

#add variable number of months had delays in payment
df$num_months_delay<-rowSums(df[,7:12]>0)
#add variable maximum number of delays in month
df$max_delay_months<-pmax(df$PAY_0,df$PAY_2,df$PAY_3,df$PAY_4,df$PAY_5,df$PAY_6)
#add dummy variables to indicate if there was delay in past 1,2,3,4,5,6 month
library(dplyr)
df<-df%>%
  mutate(delay_month1=ifelse(PAY_0>0,1,0)) %>%
  mutate(delay_month2=ifelse(PAY_2>0,1,0)) %>%
  mutate(delay_month3=ifelse(PAY_3>0,1,0)) %>%
  mutate(delay_month4=ifelse(PAY_4>0,1,0)) %>%
  mutate(delay_month5=ifelse(PAY_5>0,1,0)) %>%
  mutate(delay_month6=ifelse(PAY_6>0,1,0))
#transform AGE variable to young(1),middle(2),senior(3) 
age_breaks<-c(0,40,60, Inf)
age_labels<-c(1,2,3)
df$AGE<-cut(as.numeric(df$AGE),breaks=age_breaks,labels=age_labels,include.lowest=TRUE)

View(df)


###remove columns we don't need###
col_keep<-c("default","LIMIT_BAL","SEX","AGE","EDUCATION","MARRIAGE","num_months_delay", "max_delay_months",
            "delay_month1","delay_month2","delay_month3","delay_month4","delay_month5","delay_month6")
df=df[col_keep]
View(df)


###split data into train and test set, resampling train data to resolve imbalanced data issue###

#plot number of 0 vs 1 in the default column
library(ggplot2)
ggplot(data=df, aes(x=default)) +
  geom_bar() +
  geom_text(stat="count", aes(label=..count..), vjust=-1)
#we have 22996 nondefault (78%) and 6605 default (22%) data


#Split data into train and test sets
set.seed(123)
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
train <- df[ind==1,]
test <- df[ind==2,]

#show number of nondefault vs default in the train dataset
table(train$default)
#16167 nondefault, 4596 default

#oversampling the train dataset
n_nondefault<-16167
new_frac_nondefault<- 0.5
new_n_total<-n_nondefault/new_frac_nondefault
library(ROSE)
oversampling <- ovun.sample(default~., data=train, method = "over", N=new_n_total)$data
new_train<-oversampling
table(oversampling$default)

#show number of data in the resampled train set
table(new_train$default)
#show number of data in test set
table(test$default)

#After resampling, we have 32334 data in the train set, 8838 in test set
#Total 41172 data


#convert a variable to a factor or numeric data type
str(new_train)
new_train$default = as.factor(new_train$default)
new_train$LIMIT_BAL = as.numeric(new_train$LIMIT_BAL)
new_train$SEX = as.factor(new_train$SEX)
new_train$AGE = as.factor(new_train$AGE)
new_train$EDUCATION = as.factor(new_train$EDUCATION)
new_train$MARRIAGE = as.factor(new_train$MARRIAGE)
new_train$max_delay_months= as.numeric(new_train$max_delay_months)
new_train$delay_month1 = as.factor(new_train$delay_month1)
new_train$delay_month2 = as.factor(new_train$delay_month2)
new_train$delay_month3 = as.factor(new_train$delay_month3)
new_train$delay_month4 = as.factor(new_train$delay_month4)
new_train$delay_month5 = as.factor(new_train$delay_month5)
new_train$delay_month6 = as.factor(new_train$delay_month6)
str(new_train)

str(test)
test$default = as.factor(test$default)
test$LIMIT_BAL = as.numeric(test$LIMIT_BAL)
test$SEX = as.factor(test$SEX)
test$AGE = as.factor(test$AGE)
test$EDUCATION = as.factor(test$EDUCATION)
test$MARRIAGE = as.factor(test$MARRIAGE)
test$max_delay_months= as.numeric(test$max_delay_months)
test$delay_month1 = as.factor(test$delay_month1)
test$delay_month2 = as.factor(test$delay_month2)
test$delay_month3 = as.factor(test$delay_month3)
test$delay_month4 = as.factor(test$delay_month4)
test$delay_month5 = as.factor(test$delay_month5)
test$delay_month6 = as.factor(test$delay_month6)
str(test)


##############Step Two - Data/Variable exploration###############


#install.packages("Ecdat")
library(Ecdat)
#install.packages("ISLR")
library(ISLR)
#install.packages("GGally")
library(GGally)
#install.packages("car")
library(car)
#install.packages("scatterplot3d")
library("scatterplot3d")
#install.packages("ggExtra")
library("ggExtra")
##install.packages("ROCR")
library("ROCR")
#install.packages("ggplot2")
library(ggplot2)
#install.packages("reshape2")
library(reshape2)


#explore correlation
#install.packages("vcd")
library(vcd)
#install.packages("gplots")
library(gplots)

# Set the categorical columns
cat_cols <- c("SEX", "EDUCATION", "MARRIAGE", "AGE", "default")

cramers_v <- function(x, y) {
  confusion_matrix <- table(x, y)
  chi2_result <- chisq.test(confusion_matrix)
  phi2 <- chi2_result$statistic / sum(confusion_matrix)
  r <- nrow(confusion_matrix)
  k <- ncol(confusion_matrix)
  phi2corr <- max(0, phi2 - ((k - 1) * (r - 1)) / (sum(confusion_matrix) - 1))
  rcorr <- r - ((r - 1)^2) / (sum(confusion_matrix) - 1)
  kcorr <- k - ((k - 1)^2) / (sum(confusion_matrix) - 1)
  sqrt(phi2corr / min((kcorr - 1), (rcorr - 1)))
}

# Create a matrix to store association coefficients
association_matrix <- matrix(NA, nrow = length(cat_cols), ncol = length(cat_cols))

# Calculate Cramér’s V for each pair of categorical variables
for (i in seq_along(cat_cols)) {
  for (j in seq_along(cat_cols)) {
    association_matrix[i, j] <- cramers_v(new_train[[cat_cols[i]]], new_train[[cat_cols[j]]])
  }
}

# Create a heatmap using the 'heatmap.2' function with blue to red gradient
heatmap.2(association_matrix, 
          trace = "none",  # Remove trace lines
          col = colorRampPalette(c("blue", "red"))(10),  # blue to red gradient
          key = TRUE, key.title = NA,
          xlab = "Categorical Variables", ylab = "Categorical Variables",
          labRow = cat_cols, labCol = cat_cols,
          cexRow = 0.6, cexCol = 0.6,  # Adjust font size
          main = "Cramér’s V Heatmap",
          cex.main = 1.2, cex.axis = 1.2,
          cellnote = round(association_matrix, 2),  # Display Cramér's V values
          notecol = "black",  # Text color
          density.info = "none",  # Remove density plot
          margins = c(5, 5)  # Add some margin space for labels
)


# # Calculate the correlation matrix
#new_train1 <- new_train%>% mutate_all(as.numeric)
# cor_matrix <- cor(new_train1)
# 
# # Create a ggplot with a correlation matrix heatmap
# print(ggplot(data = melt(cor_matrix), aes(Var1, Var2, fill = value)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                        midpoint = 0, limit = c(-1, 1), space = "Lab",
#                        name="Cramer's V") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, 
#                                    size = 10, hjust = 1),
#         axis.text.y = element_text(size = 10),
#         legend.position = "bottom",
#         legend.direction = "horizontal"))
# 


#plot default vs given credit
ggplot(data=new_train, aes(x=factor(default), y = LIMIT_BAL/1000, fill=default)) + 
  geom_boxplot() +
  ggtitle("Given Credit VS Default") + theme(plot.title = element_text(size = 20, face = "bold")) +
  labs(x="Default", y="Given Credit(in thousands) ") + 
  theme(axis.text.x = element_text(size=10), axis.text.y = element_text(size=10), 
        axis.title=element_text(size=6,face="bold"))

#plot gender vs given credit
ggplot(data = new_train, aes(x = factor(SEX), y = LIMIT_BAL/1000, fill = SEX)) +
  geom_boxplot() +
  ggtitle("Given Credit vs Gender") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  labs(x = "Gender", y = "Given Credit (in thousands)") +
  scale_fill_manual(values = c("1" = "blue", "2" = "pink"), 
                    labels = c("1" = "Male", "2" = "Female")) +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title = element_text(size = 6, face = "bold")) +
  guides(fill = guide_legend(title = "Gender")) 

#plot education vs given credit
ggplot(data = new_train, aes(x = factor(EDUCATION), y = LIMIT_BAL/1000, fill = EDUCATION)) +
  geom_boxplot() +
  ggtitle("Given Credit vs Education") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  labs(x = "Education", y = "Given Credit (in thousands)") +
  scale_fill_manual(values = c("1" = "blue", "2" = "pink","3" = "darkgreen", "4"= "red"), 
                    labels = c("1" = "Graduate School", "2" = "University","3" = "High School", "4"= "others"),
                    breaks = c("1", "2", "3", "4")) +
  scale_x_discrete(labels = c("1" = "Graduate School", "2" = "University", "3" = "High School", "4" = "Others")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title = element_text(size = 6, face = "bold")) +
  guides(fill = guide_legend(title = "Education")) 

#plot marital status vs given credit
ggplot(data = new_train, aes(x = factor(MARRIAGE), y = LIMIT_BAL/1000, fill = MARRIAGE)) +
  geom_boxplot() +
  ggtitle("Given Credit vs Marital Status") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  labs(x = "Marital Status", y = "Given Credit (in thousands)") +
  scale_fill_manual(values = c("1" = "blue", "2" = "pink","3" = "darkgreen"), 
                    labels = c("1" = "Married", "2" = "Single","3" = "Others"),
                    breaks = c("1", "2", "3")) +
  scale_x_discrete(labels = c("1" = "Married", "2" = "Single", "3" = "others")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title = element_text(size = 6, face = "bold")) +
  guides(fill = guide_legend(title = "Marital Status")) 

#plot age vs given credit
ggplot(data = new_train, aes(x = factor(AGE), y = LIMIT_BAL/1000, fill = AGE)) +
  geom_boxplot() +
  ggtitle("Given Credit vs AGE") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  labs(x = "AGE", y = "Given Credit (in thousands)") +
  scale_fill_manual(values = c("1" = "blue", "2" = "pink","3" = "darkgreen"), 
                    labels = c("1" = "YOUNG", "2" = "MIDDLE_AGE","3" = "SENIOR"),
                    breaks = c("1", "2", "3")) +
  scale_x_discrete(labels = c("1" = "YOUNG", "2" = "MIDDLE_AGE", "3" = "SENIOR")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title = element_text(size = 6, face = "bold")) +
  guides(fill = guide_legend(title = "AGE"))

#install.packages("corrplot")
library(corrplot)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("DataExplorer")
library(DataExplorer)
#bar plot of each variable
plot_bar(new_train)
#install.packages("SmartEDA")
library(SmartEDA)

#display proportions of different groups vs default
ExpCatViz(
  new_train %>%
    select(EDUCATION,default),
  target="EDUCATION")

ExpCatViz(
  new_train %>%
    select(SEX,default),
  target="SEX")

ExpCatViz(
  new_train %>%
    select(AGE,default),
  target="AGE")

ExpCatViz(
  new_train %>%
    select(MARRIAGE,default),
  target="MARRIAGE")




#install.packages("ggstatsplot")
library(ggstatsplot)

#visualization with statistical details
ggbarstats(data = new_train,x= SEX, y = default, label = "both")
ggbarstats(data = new_train,x= AGE, y = default, label = "both")
ggbarstats(data = new_train,x= EDUCATION, y = default, label = "both")
ggbarstats(data = new_train,x= MARRIAGE, y = default, label = "both")
ggbarstats(data = new_train,x= num_months_delay, y = default, label = "both")
ggbarstats(data = new_train,x= max_delay_months, y = default, label = "both")

#explore distribution with skewness & kurtosis tests
plot_histogram(new_train)
plot_density(new_train)
plot_qq(new_train)

#install.packages("dlookr")
library(dlookr)

#boxplot of categorical variables
plot_normality(new_train)
plot_boxplot(new_train,by="SEX")
plot_boxplot(new_train,by="AGE")
plot_boxplot(new_train,by="EDUCATION")
plot_boxplot(new_train,by="MARRIAGE")
plot_boxplot(new_train,by="default")

#conduct multiple pairwise comparisons and test significant differences between groups for limit_bal
ggbetweenstats(
  data = new_train,
  x = SEX,
  y = LIMIT_BAL,
  type = "np")

ggbetweenstats(
  data = new_train,
  x = AGE,
  y = LIMIT_BAL,
  type = "np")

ggbetweenstats(
  data = new_train,
  x = EDUCATION,
  y = LIMIT_BAL,
  type = "np")

ggbetweenstats(
  data = new_train,
  x = MARRIAGE,
  y = LIMIT_BAL,
  type = "np")
 
#explore correlation
new_train1 <- new_train%>% mutate_all(as.numeric)



ggcorrmat(
  data = new_train1,
  type = "np", # nonparametric Spearman correlation
  output = "dataframe")




##############Step Three - Models #########################


x_train <- subset(new_train, select=-default)
y_train <- new_train$default
x_test <- subset(test, select=-default) 
y_test <- test$default


########Model- Decision Tree ###############
# Load necessary libraries

# Train decision tree model
#dt_model <- rpart(default ~ ., data = new_train, method = "class")


library(rpart)
library(ROCR)
library(rpart.plot)
y_test_binary=ifelse(y_test== "1", 1, 0)
trctrl <- trainControl(method = "cv", number = 10)
dt_model <- train(default ~ ., 
                  data = new_train, 
                  method = "rpart",
                  trControl = trctrl,
                  control = rpart.control(minbucket = 25, cp = 0, maxdepth = 10))

prp(dt_model$finalModel, box.palette = "Reds", tweak = 1.2, varlen = 20)
print(plot(varImp(dt_model)))

# Display cross-validation results
printcp(dt_model$finalModel)

# Find the best CP value for pruning
best_cp <- 0.00088363

# Prune the tree based on the best CP value
tree_reduced_pruned <- prune(dt_model$finalModel, cp = best_cp)

# Plot the pruned tree
rpart.plot(tree_reduced_pruned)
printcp(tree_reduced_pruned)
tree_reduced_pruned

# Predict on the test set
dt_pred_prob <- predict(dt_model, newdata = x_test, type = "prob")
# Create prediction object for ROC curve
dt_pred_obj <- prediction(dt_pred_prob[2], y_test_binary)

dt_evalu <- performance(dt_pred_obj,"acc")
#identify best values
max=which.max(slot(dt_evalu,"y.values")[[1]])
dt_accuracy_mod <- slot(dt_evalu,"y.values")[[1]][max]
print(dt_accuracy_mod)


# Create ROC curve with more points for a smoother curve
roc_curve_dt <- performance(dt_pred_obj, "tpr", "fpr")
# Plot ROC curve
plot(roc_curve_dt, main = "Smooth ROC Curve", col = "blue", lwd = 2, legacy.axes = TRUE)
abline(a = 0, b = 1)

# Calculate AUC
dt_AUC <- performance(dt_pred_obj, measure = "auc")
dt_AUC <- dt_AUC@y.values[[1]]
print(dt_AUC)

legend(0.6, 0.3, round(dt_AUC, 4), title = "AUC", cex = 1)


########Model- KNN###############
library(caret)
mod_knn = train(x = x_train,
                y = y_train,
                method = "knn",
                preProcess = c("center", "scale"),
                tuneGrid = data.frame(k = c(1:5)),
                trControl = trainControl(method = "cv",
                                         number =10)
)

# View the fitted model.
mod_knn

# Use model to predict for the test set.
pred_knn = predict(mod_knn, x_test, type="prob")


library(ROCR)
kNNPred <- prediction(pred_knn[,2],y_test)
kNNPerf <- performance(kNNPred, "tpr","fpr")
plot(kNNPerf,main = "ROC Curve",col = 2,lwd = 2, ylab="Sensitivity",
     xlab="1-Specificity")
abline(a = 0,b = 1)
knn_AUC <- performance(kNNPred, measure = "auc")
knn_AUC <- knn_AUC@y.values[[1]]
knn_AUC
legend(.6,.3,round(knn_AUC,3),title="AUC",cex=1)

########Model- GLM ###############

#fit the GLM model

mod_glm <- glm(default~.,data = new_train, family= "binomial")
summary(mod_glm)
library(car)
alias(glm(default ~ LIMIT_BAL + SEX + AGE + EDUCATION + MARRIAGE + num_months_delay +
            max_delay_months + delay_month1 + delay_month2 + delay_month3 + 
            delay_month4 + delay_month5 + delay_month6,  family= "binomial", data = new_train))

mod_glm2 <- glm(default ~ LIMIT_BAL + SEX + AGE + EDUCATION + MARRIAGE + num_months_delay +
                  max_delay_months + delay_month1 + delay_month2 + delay_month3 + 
                  delay_month4 + delay_month5,  family= "binomial", data = new_train)
summary(mod_glm2)
vif_values <- vif(mod_glm2)
vif_values

#cross validation
#install.packages("boot")
library(boot)
library(ROCR)
pred <- predict(mod_glm2,newdata = test, type="response")
cv_err <- cv.glm(new_train,mod_glm2,K=10)
cv_err$delta

#model performance evaluation
pred <- prediction(pred,test$default)
evalu <- performance(pred,"acc")
plot(evalu)


#identify best values
max=which.max(slot(evalu,"y.values")[[1]])
accuracy_mod <- slot(evalu,"y.values")[[1]][max]
cut <- slot(evalu,"x.values")[[1]][max]
print(c(Accuracy=accuracy_mod,Cutoff = cut))

#reciever operating charateristic(ROC) Curve
roc <- performance(pred,"tpr","fpr")
plot(roc,
     colorsize=T,
     main="ROC Curve",
     ylab="Sensitivity",
     xlab="1-Specificity")
abline(a=0,b=1)

# area under curve(AUC)
AUC <- performance(pred,"auc")
AUC <- unlist(slot(AUC,"y.values"))
AUC <- round(AUC,4)
legend(.6,.2,AUC,title="AUC",cex=1)


########Model- XGBoost ###############
library(xgboost)

cv <- trainControl(method = "cv", number = 10)
xgb_grid = expand.grid(nrounds = c(100,200,300),
                       max_depth = c(3,4,5),
                       eta = 0.4,
                       min_child_weight = 1,
                       subsample = 0.8,
                       gamma = 0,
                       colsample_bytree = 0.8)

xgb_model <- train(default ~ .,
                   data = new_train,
                   method = "xgbTree",
                   trControl = cv,
                   tuneGrid =xgb_grid)
print(plot(varImp(xgb_model)))

# Best hyperparameters
best_params <- xgb_model$bestTune
print(best_params)

# Use the optimized model to make predictions on the test set
xgb_pred <- predict(xgb_model, newdata = x_test, type = "prob")
library(pROC)

xgb_pred_obj <- prediction(xgb_pred[2], y_test)
xgb_evalu <- performance(xgb_pred_obj,"acc")
#identify best values
max=which.max(slot(xgb_evalu,"y.values")[[1]])
xgb_accuracy_mod <- slot(xgb_evalu,"y.values")[[1]][max]
print(xgb_accuracy_mod)

# Create ROC curve
y_test=ifelse(y_test== "1", 1, 0)
roc_curve <- roc(y_test, xgb_pred[[2]])
xgb_AUC<-round(roc_curve$auc,4)
print(xgb_AUC)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2, legacy.axes = TRUE)
legend(.6,.3,xgb_AUC,title="AUC",cex=1)





