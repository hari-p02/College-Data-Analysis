collegedf$`College Location (States)` <- trimws(collegedf$`College Location (States)`)

CT <- collegedf[which(collegedf$`College Location (States)` == "CT"),]
ME <- collegedf[which(collegedf$`College Location (States)` == "ME"),]
MA <- collegedf[which(collegedf$`College Location (States)` == "MA"),]
NH <- collegedf[which(collegedf$`College Location (States)` == "NH"),]
RI <- collegedf[which(collegedf$`College Location (States)` == "RI"),]
VT <- collegedf[which(collegedf$`College Location (States)` == "VT"),]

t.test(CT$est_full_Price_2020_2021, ME$est_full_Price_2020_2021)
t.test(CT$acceptance_rate, ME$acceptance_rate)
pairwise.t.test(CT, ME)
var.test(CT$est_full_Price_2020_2021, ME$est_full_Price_2020_2021)

temp <- collegedf[which(collegedf$`College Location (States)` %in% c("CT", 
                                                                     "MA",
                                                                     "ME",
                                                                     "NH",
                                                                     "RI",
                                                                     "VT")),]
barplot(table(temp$SAT_ACT_required_for_Fall_2021, temp$`College Location (States)`))
pairs.panels(temp[, sapply(temp,is.numeric) | sapply(temp,is.integer)][,c(1,2,3,4,5)])

names(temp[c("est_full_Price_2020_2021", )])

library(Hmisc)
barplot(mean(CT$est_full_Price_2020_2021), mean(CT$est_price_for_students_who_receive_aid))



ggplot(temp,                                      # Grouped barplot using ggplot2
       aes(x = temp$`College Location (States)`,
           y = temp$`Median SAT Score`,
           fill = SAT_ACT_required_for_Fall_2021)) +
  geom_bar(stat = "identity",
           position = "dodge")
where(NH$SAT_ACT_required_for_Fall_2021 == "yes")
NH$SAT_ACT_required_for_Fall_2021
library(vioplot)
install.packages("vioplot")
vioplot(CT$enrollment, ME$enrollment, MA$enrollment, NH$enrollment, RI$enrollment, VT$enrollment)
#library(caret) dummyVars

t.test(CT$acceptance_rate, MA$acceptance_rate) #No significant diff in acceptance rate 
t.test(CT$acceptance_rate, MA$acceptance_rate) #No significant diff in acceptance rate 
names(temp)
aov.fit <- aov(enrollment ~ College_Location_States, data = temp)
summary(aov.fit)
(Tfit <- TukeyHSD(aov.fit))
?mcp
summary(Tfit)
?TukeyHSD
names(temp$`College Location (States)`)
?aov
class(temp$`College Location (States)`)
temp$`College Location (States)` <- factor(temp$`College Location (States)`)
names(temp)[25] <- "College_Location_States"
names(temp)
library(vioplot)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,3))
vioplot(CT$enrollment, ME$enrollment, MA$enrollment, 
        NH$enrollment, RI$enrollment, VT$enrollment)
vioplot(CT$`Median SAT Score`, ME$`Median SAT Score`, MA$`Median SAT Score`, 
        NH$`Median SAT Score`, RI$`Median SAT Score`, VT$`Median SAT Score`)
vioplot(CT$average_student_debt, ME$average_student_debt, MA$average_student_debt, 
        NH$average_student_debt, RI$average_student_debt, VT$average_student_debt)
vioplot(CT$average_time_to_a_degree, ME$average_time_to_a_degree, MA$average_time_to_a_degree, 
        NH$average_time_to_a_degree, RI$average_time_to_a_degree, VT$average_time_to_a_degree)
vioplot(CT$average_price_for_low_income_students, ME$average_price_for_low_income_students, 
        MA$average_price_for_low_income_students, NH$average_price_for_low_income_students, 
        RI$average_price_for_low_income_students, VT$average_price_for_low_income_students)
vioplot(CT$`Median ACT Score`, ME$`Median ACT Score`, MA$`Median ACT Score`, 
        NH$`Median ACT Score`, RI$`Median ACT Score`, VT$`Median ACT Score`)
par(opar)

pairs(~ acceptance_rate + est_price_for_students_who_receive_aid,data = temp)
library(psych)
pairs.panels(temp[c("acceptance_rate", 
                    "est_price_for_students_who_receive_aid", 
                    "est_full_Price_2020_2021", 
                    "percent_of_need_met",
                    "average_merit_grant",
                    "average_student_debt",
                    "average_price_for_low_income_students",
                    "percent_of_students_who_get_merit_grants",
                    "Est_price_with_average_grant")],
             lm = TRUE)
names(temp)
?pairs.panels

mytable <- xtabs(~ College_Location_States + enrollment, data = temp)
(cstest <- chisq.test(mytable))

names(temp)
str(temp)

library(mltools)
library(data.table)

newdata <- one_hot(as.data.table(data))
collegedf$`College Location (Town)` <- factor(collegedf$`College Location (Town)`)
collegedf$college_names <- factor(collegedf$college_names)
collegedf$regular_application <- factor(collegedf$regular_application)
temp$regular_application <- factor(temp$regular_application)
collegedf$`College Location (States)` <- factor(collegedf$`College Location (States)`)

library(mltools)
library(data.table)

data <- data.frame(
  Outcome = seq(1,100,by=1),
  Variable = sample(c("Red","Green","Blue"), 100, replace = TRUE)
)
data$Variable <- as.factor(data$Variable)
newdata <- one_hot(as.data.table(data))

library(caret)
install.packages("caret")
tempt <- temp
dummy <- dummyVars(" ~ .", data=tempt)
final_df <- data.frame(predict(dummy, newdata=tempt))

str(temp)
rm(model)
?subset
tempt <- subset(temp, select = c("graduation_rate", 
                                 "acceptance_rate", 
                                 "Median SAT Score", 
                                 "College_Location_States",
                                 "SAT_ACT_required_for_Fall_2021"))

collegedf$SAT_ACT_required_for_Fall_2021[collegedf$SAT_ACT_required_for_Fall_2021 == "1"] <- "yes"
collegedf$SAT_ACT_required_for_Fall_2021[collegedf$SAT_ACT_required_for_Fall_2021 == "0"] <- "no"
collegedf$SAT_ACT_required_for_Fall_2021 <- ifelse(collegedf$SAT_ACT_required_for_Fall_2021 == "yes", 1, 0)
class(temp$SAT_ACT_required_for_Fall_2021)

var <- names(college_df) %in% c("regular_application", "college_names", "College_Location_Town", "College_Location_States")
train <- sample(nrow(college_df), 0.8*nrow(college_df))
College.train <- college_df[train,][!var]
College.validate <- college_df[-train,][!var]
logistic_model <- glm(SAT_ACT_required_for_Fall_2021 ~ ., family = binomial(), College.train)
summary(logistic_model)
logRegPrivate.step <- step(logistic_model, direction="backward")
summary(logRegPrivate.step)
prob <- predict(logRegPrivate.step, College.validate, type="response")
logit.pred <- factor(prob > .5, labels=c("No", "Yes"))
table(logit.pred)
logit.perf <- table(College.validate$SAT_ACT_required_for_Fall_2021, logit.pred,
                    dnn=c("Actual", "Predicted"))
logit.perf

?vis_miss
?html_nodes

logistic_model <- glm(SAT_ACT_required_for_Fall_2021 ~ ., family = binomial(), tempt)
logRegPrivate.step <- step(logistic_model, direction="backward")
summary(logRegPrivate.step)
str(tempt)
summary(logistic_model)
levels(temp$College_Location_States)
str(temp)
fit <- aov(SAT_ACT_required_for_Fall_2021 ~ ., data = temp)
summary(fit)
collegedf$SAT_ACT_required_for_Fall_2021
str(collegedf)
tempt <- subset(collegedf, select = -c("college_names", "regular_application", "College Location (Town)", "Est_price_2020_21_without_aid", "Est_price_with_average_grant", "early_career_earnings"))
myvars <- names(collegedf) %in% c("college_names", "regular_application", "College Location (Town)", "College Location (States)")#, "Est_price_2020_21_without_aid", "Est_price_with_average_grant", "early_career_earnings")
myvars2 <- names(collegedf) %in% c("Est_price_2020_21_without_aid", "Est_price_with_average_grant", "early_career_earnings")
tempt <- collegedf[!myvars]
pairs.panels(detdf)
detdf <- collegedf[myvars2]
cdf <- college_df
cdf <- cdf[!duplicated(cdf), ]
cdf <- cdf[!duplicated(as.list(cdf))]
college_df <- college_df[!duplicated(college_df), ]
college_df <- college_df[!duplicated(as.list(college_df))]
which(college_df$College_Location_States == "CT")
CT <- college_df[which(college_df$College_Location_States == "CT"),]
summary(CT)
ME <- college_df[which(college_df$College_Location_States == "ME"),]
summary(ME)
MA <- college_df[which(college_df$College_Location_States == "MA"),]
summary(MA)
NH <- college_df[which(college_df$College_Location_States == "NH"),]
summary(NH)
RI <- college_df[which(college_df$College_Location_States == "RI"),]
summary(RI)
VT <- college_df[which(college_df$College_Location_States == "VT"),]
summary(VT)
new_england <- college_df[which(college_df$College_Location_States %in% c("CT", 
                                                                          "MA",
                                                                          "ME",
                                                                          "NH",
                                                                          "RI",
                                                                          "VT")),]
summary(new_england)
college_df$College_Location_States <- trimws(college_df$College_Location_States)
str(college_df)
college_df <- read.csv("college_data.csv")
?boxplot
?interaction.plot
install.packages("glmnet")
library(glmnet)
