getwd()
setwd("/Users/n0342839/Desktop/Project/grad_school/Stats/Project")
install.packages("ROSE")
install.packages("corrplot")
library(corrplot)
library(ROSE)
install.packages("arm")
library(arm)

data <- read.csv(file = "healthcare-dataset-stroke-data.csv", header = TRUE)

## Omit all rows with NA data ##
omitted_data = na.omit(data)

################ Examining categorical parameters #################

## contingency table for stroke vs hypertension
hypertension <- omitted_data$hypertension
stroke <- omitted_data$stroke
(cont_table_hyp <- table(hypertension,stroke))

prop.table(cont_table_hyp,1)
chisq.test(cont_table_hyp,correct = FALSE)

## contingency table for stroke vs heart disease
heart_disease <- omitted_data$heart_disease
(cont_table_hd <- table(heart_disease,stroke))

prop.table(cont_table_hd,1)
chisq.test(cont_table_hd,correct = FALSE)

## contingency table for stroke vs smoking status
smoking <- omitted_data$smoking_status
(cont_table_smoke <- table(smoking,stroke))

prop.table(cont_table_smoke,1)
chisq.test(cont_table_smoke,correct = FALSE)

##################### Examining numerical parameters ##########################

## Show distributions of each variable to determine test to use ##
age <- omitted_data$age
glucose <- omitted_data$avg_glucose_level
bmi <- omitted_data$bmi

hist(age)
hist(glucose)
hist(bmi)

## Since they are not normally distributed, we will use the wilcoxon signed-rank test ##
stroke = omitted_data[omitted_data$stroke == 1, ]
nonstroke = omitted_data[omitted_data$stroke == 0, ]

wilcox.test(x=stroke$age, y=nonstroke$age, paired=FALSE, alternative = "two.sided")
wilcox.test(x=stroke$avg_glucose_level, y=nonstroke$avg_glucose_level, paired=FALSE, alternative = "two.sided")
wilcox.test(x=stroke$bmi, y=nonstroke$bmi, paired=FALSE, alternative = "two.sided")


########################### Normalizing numerical data #########################

data_norm <- data.frame(omitted_data)
data_norm$avg_glucose_level = (data_norm$avg_glucose_level-mean(data_norm$avg_glucose_level))/sd(data_norm$avg_glucose_level)
data_norm$bmi = (data_norm$bmi-mean(data_norm$bmi))/sd(data_norm$bmi)
data_norm$age = (data_norm$age-mean(data_norm$age))/sd(data_norm$age)

head(data_norm)

########################### Reformat data for analysis #########################

# Gender
male = ifelse(data_norm$gender == 'Male', 1, 0)
female = ifelse(data_norm$gender == 'Female', 1, 0)

# Smoking Status
smokes = ifelse(data_norm$smoking_status == 'smokes', 1, 0)
never_smoked = ifelse(data_norm$smoking_status == 'never smoked', 1, 0)
formerly_smoked = ifelse(data_norm$smoking_status == 'formerly_smoked', 1, 0)
unknown_smoker = ifelse(data_norm$smoking_status == 'Unknown', 1, 0)

# Married
married <- ifelse(data_norm$ever_married == "Yes", 1, 0)
unmarried = ifelse(data_norm$ever_married == "No", 1, 0)

# Work Type
private = ifelse(data_norm$work_type == "Private", 1, 0)
govt = ifelse(data_norm$work_type == "Govt_job", 1, 0)
self = ifelse(data_norm$work_type == "Self-employed", 1, 0)
children = ifelse(data_norm$work_type == "children", 1, 0)
never_worked = ifelse(data_norm$work_type == "Never_worked", 1, 0)


# For the ever_married column 'Urban' = 1, 'Rural' = 0
urban = ifelse(data_norm$Residence_type == "Urban", 1, 0)
rural = ifelse(data_norm$Residence_type == "Rural", 1, 0)

reg_df = data.frame(male=male,
                    female=female,
                    age=data_norm$age, 
                    hypertension=data_norm$hypertension,
                    heart_disease = data_norm$heart_disease, 
                    married=married,
                    unmarried=unmarried,
                    private=private,
                    govt = govt,
                    self=self,
                    children=children,
                    never_worked=never_worked,
                    urban=urban,
                    rural=rural,
                    glucose=data_norm$avg_glucose_level,
                    bmi=data_norm$bmi,
                    smokes=smokes,
                    never_smoked=never_smoked,
                    formerly_smoked=formerly_smoked,
                    unknown_smoker=unknown_smoker,
                    stroke=data_norm$stroke)

head(reg_df)

reg_df$hypertension <- factor(reg_df$hypertension)
reg_df$heart_disease <- factor(reg_df$heart_disease)
reg_df$male <- factor(reg_df$male)
reg_df$female <- factor(reg_df$female)
reg_df$married <- factor(reg_df$married)
reg_df$unmarried <- factor(reg_df$unmarried)
reg_df$private <- factor(reg_df$private)
reg_df$govt <- factor(reg_df$govt)
reg_df$self <- factor(reg_df$self)
reg_df$children <- factor(reg_df$children)
reg_df$never_worked <- factor(reg_df$never_worked)
reg_df$urban <- factor(reg_df$urban)
reg_df$rural <- factor(reg_df$rural)
reg_df$smokes <- factor(reg_df$smokes)
reg_df$never_smoked <- factor(reg_df$never_smoked)
reg_df$formerly_smoked <- factor(reg_df$formerly_smoked)
reg_df$unknown_smoker <- factor(reg_df$unknown_smoker)
reg_df$stroke <- factor(reg_df$stroke)


######################### Initial regression test #############################

## Create training and test data subsets ##
set.seed(123)
smp_size <- floor(0.75 * nrow(reg_df))
train_ind <- sample(seq_len(nrow(reg_df)), size = smp_size)

train <- reg_df[train_ind, ]
x_train = subset(train, select = -c(stroke))
y_train = train$stroke

test <- reg_df[-train_ind, ]


## logistic regression with all variables ##
lr = glm(stroke ~ male+female+married+unmarried+
           private+govt+self+children+never_worked+urban+
            rural+never_smoked+unknown_smoker+age+hypertension+
              heart_disease+glucose+bmi+smokes, data = train, family="binomial")

# removed formerly smoked, constant 0

result <- predict(lr, newdata=test)

# table for predictions
stroke.pred = rep("0", dim(test)[1])
stroke.pred[result > .5] = 1
table(stroke.pred, test$stroke)

## Attempting oversampling to re-balance the data
train_balanced = ROSE(stroke~male+female+married+unmarried+
                        private+govt+self+children+never_worked+urban+
                        rural+never_smoked+unknown_smoker+age+hypertension+
                        heart_disease+glucose+bmi+smokes, data = train, seed=1)$data

lr_balanced = glm(stroke~., data=train_balanced, family = "binomial")
summary(lr_balanced)

balanced_result = predict(lr_balanced, test)

# table for predictions
stroke.pred = rep("0", dim(test)[1])
stroke.pred[balanced_result > .5] = 1
table(stroke.pred, test$stroke)


accuracy.meas(test$stroke, balanced_result)

######################### Refine regression test ##############################

### Remove all variables that have not shown any significance ###
train_refined = ROSE(stroke~age+hypertension+
                        heart_disease+glucose+bmi+smokes, data = train, seed=1)$data

lr_refined = glm(stroke~., data=train_refined, family = "binomial")
summary(lr_refined)

balanced_result_refined = predict(lr_refined, test)

# table for predictions
stroke.pred = rep("0", dim(test)[1])
stroke.pred[balanced_result_refined > .5] = 1
table(stroke.pred, test$stroke)

accuracy.meas(test$stroke, balanced_result_refined)

### plot refined residuals ###
plot(lr_refined, which = 2)
binnedplot(fitted(lr_refined), residuals(lr_refined, type = "response"))

## Outliers seem to be 1341 and 2830 among others ##


######################### Correlation Table ##############################
datacorr <- subset(data_norm, select=c("age", "avg_glucose_level", "bmi"))

mydata.cor = cor(datacorr)

title = "Correlation of Variables"

corrplot(mydata.cor, method = "color", diag = FALSE, type = "upper", 
         order = "hclust", title = title, addCoef.col = "black", sig.level = 0.05,
         insig = "blank", mar = c(0,0,1,0))

################ Examining Correlation between categorical predictors #################

## contingency table for hypertension vs heart disease
hypertension <- omitted_data$hypertension
stroke <- omitted_data$stroke
(cont_table_hyp <- table(hypertension,heart_disease))

prop.table(cont_table_hyp,1)
chisq.test(cont_table_hyp,correct = FALSE)

## contingency table for heart_disease vs smokes
heart_disease <- omitted_data$heart_disease
(cont_table_hd <- table(heart_disease,smoking))

prop.table(cont_table_hd,1)
chisq.test(cont_table_hd,correct = FALSE)

## contingency table for hypertension vs smokes
smoking <- omitted_data$smoking_status
(cont_table_smoke <- table(smoking,hypertension))

prop.table(cont_table_smoke,1)
chisq.test(cont_table_smoke,correct = FALSE)


######################### Regression with Correlated variables removed ##############################

### Remove categorical variables that were correlated ###
train_correlated = ROSE(stroke~age+hypertension++glucose+bmi, data = train, seed=1)$data

lr_correlated = glm(stroke~., data=train_correlated , family = "binomial")
summary(lr_correlated)

balanced_result_correlated = predict(lr_correlated, test)

# table for predictions
stroke.pred = rep("0", dim(test)[1])
stroke.pred[balanced_result_correlated > .5] = 1
table(stroke.pred, test$stroke)

accuracy.meas(test$stroke, balanced_result_correlated)

### plot refined residuals ###
binnedplot(fitted(lr_correlated), residuals(lr_correlated, type = "response"))
plot(lr_correlated, which = 2)

## Outliers seem to be 1341 and 2830 among others ##