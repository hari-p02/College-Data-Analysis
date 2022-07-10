install.packages("rvest")
library(rvest)
library(dplyr)

link <- paste("https://money.com/best-colleges/")
pg <-  read_html(link)
college_names <- pg %>% html_nodes("._1RI9D22X") %>% html_text()
Est_price_2020_21_without_aid <- pg %>% html_nodes("td:nth-child(2)") %>% html_text()
Est_price_with_average_grant <- pg %>% html_nodes("td:nth-child(3)") %>% html_text()
percent_of_students_who_get_any_grants <- pg %>% html_nodes("td:nth-child(4)") %>% html_text()
graduation_rate <- pg %>% html_nodes("td:nth-child(5)") %>% html_text()
average_student_debt <- pg %>% html_nodes("td:nth-child(6)") %>% html_text()
early_career_earnings <- pg %>% html_nodes("td:nth-child(7)") %>% html_text()
college_location <- pg %>% html_nodes("._1OtZ7j1R") %>% html_text()
#head(college_location)
?subset
df <- data.frame(matrix(ncol = 19, nrow = 0))
#link <- paste("https://money.com/best-colleges/")
#pg <- read_html(link)
baselink <- "https://money.com"
for (i in 1:748){
  sel <- paste("tr:nth-child(", i, ") ._1RI9D22X", sep="")
  restlink <- pg %>% html_nodes(sel) %>% html_attr("href")
  gotolink <- paste(baselink, restlink, sep="")
  datapg <- read_html(gotolink)
  
  vals <- datapg %>% html_nodes(".small-3") %>% html_text()
  vals2 <- datapg %>% html_nodes(".small-2") %>% html_text()
  
  temp <- append(vals, vals2)
  df <- rbind(df, temp)
}

coln <- c("est_full_Price_2020_2021",
          "est_price_for_students_who_receive_aid",
          "average_price_for_low_income_students",
          "acceptance_rate",
          "median_SAT_ACT_score",
          "SAT_ACT_required_for_Fall_2021",
          "enrollment",
          "percent_of_need_met",
          "percent_of_students_who_get_merit_grants",
          "average_merit_grant",
          "graduation_rate",
          "average_time_to_a_degree",
          "average_student_debt",
          "average_salary_within_5_years",
          "percent_earning_more_than_28000",
          "early_decision_application",
          "regular_application",
          "percent_of_students_who_get_any_grants",
          "percent_of_students_with_need_who_get_grants"
)

names(df) <- coln

#write.csv(df,"/content/college.csv", row.names = FALSE)
college_df <- read.csv("college.csv")
str(r)
df2 <- read.csv("college.csv")
df2["Est_price_2020_21_without_aid"] <- Est_price_2020_21_without_aid
df2["college_names"] <- college_names
df2["Est_price_with_average_grant"] <- Est_price_with_average_grant

df2["percent_of_students_who_get_any_grants"] <- percent_of_students_who_get_any_grants
df2["graduation_rate"] <- graduation_rate
df2["average_student_debt"] <- average_student_debt

df2["early_career_earnings"] <- early_career_earnings
df2["college_location"] <- college_location

college_df["Est_price_2020_21_without_aid"] <- Est_price_2020_21_without_aid
college_df["college_names"] <- college_names
college_df["Est_price_with_average_grant"] <- Est_price_with_average_grant
college_df["percent_of_students_who_get_any_grants"] <- percent_of_students_who_get_any_grants
college_df["graduation_rate"] <- graduation_rate
college_df["average_student_debt"] <- average_student_debt
college_df["early_career_earnings"] <- early_career_earnings
college_df["college_location"] <- college_location

replace_with_null <- function(data, ind){
  temp <- trimws(data[,ind])
  return(replace(temp, which(temp == "NA" | temp == "N/A"), NA))
}


cleandfcol <- function(data, ind){
  vec <- replace_with_null(data, ind)
  nonavec <- vec[!is.na(vec)]
  if (sum(substr(nonavec, start = 3, stop = 3) == "%") > 0){
    vec <- gsub("%", "", vec)
    return(as.numeric(vec))
  }
  else if (sum(substr(nonavec, start = 1, stop = 1) == "$") > 0){
    vec <- sub('.', '', gsub(",", "", vec))
    return(as.numeric(vec))
  }
  else if (sum(grepl(",", df[,7], fixed = TRUE)) > 0){
    vec <- gsub(",", "", vec)
    return(as.numeric(vec))
  }
  else {
    vec <- gsub("years", "", vec)
    return(as.numeric(vec))
  }
}

cleandfcol <- function(data, ind){
  vec <- replace_with_null(data, ind)
  nonavec <- vec[!is.na(vec)]
  if (sum(substr(nonavec, start = 3, stop = 3) == "%") > 0){
    vec <- gsub("%", "", vec)
    return(as.numeric(vec))
  }
  else if (sum(substr(nonavec, start = 1, stop = 1) == "$") > 0){
    vec <- sub('.', '', gsub(",", "", vec))
    return(as.numeric(vec))
  }
  else if (sum(grepl(",", data[,7], fixed = TRUE)) > 0){
    vec <- gsub(",", "", vec)
    return(as.numeric(vec))
  }
  else {
    vec <- gsub("years", "", vec)
    return(as.numeric(vec))
  }
}
str(df2)
for (i in c(1, 2, 3, 4, 7, 8, 9, 10, 11, 12, 13, 14, 15, 18, 19, 20, 22, 23)){
  df2[,i] <- cleandfcol(df2, i)
}

library(tidyr)
college_df <- separate(college_df, 5, c("Median SAT Score", "Median ACT Score"), "/", remove=TRUE, convert = TRUE)
str(df)

for (i in c(7, 17, 18, 22, 25)){
  college_df[,i] <- replace_with_null(college_df, i)
  
}



str(collegedf2)

sum(complete.cases(df))
#interpolation to fill in missing values

install.packages("visdat")
library(visdat)
library(naniar)
vis_miss(college_df)
temp = subset(college_df, -c(SAT_ACT_required_for_Fall_2021, regular_application, college_names, ))
mcar_test(college_df)
?mcar_test     
library(ggplot)
gg_miss_var(college_df)
rm(mtest)

collegedf2 <- college_df
install.packages("missForest")
library(missForest)
?missForest
collegedf2 <- missForest(collegedf2)
collegedf2["SAT_ACT_required_for_Fall_2021"] <- factor(collegedf2["SAT_ACT_required_for_Fall_2021"], levels = c(0,1), labels = c("no", "yes"))
?factor
collegedf2["regular_application"] <- factor(collegedf2["regular_application"], labels = 1:48, labels = l)
nchar(unique(collegedf2$regular_application))
class(unique(collegedf2$regular_application))
l <- c("Jan 1", "Jan 3", "Feb 1", "Nov 30", NA, "Dec 1", "March 1", "Jan 2", "Jan 15", "Dec 15", "Jan 5", "rolling", "Jan 8", "Jan 4", "May 1", "Jan 10", "Feb 7", "April 1", "March 16", "Jan 7", "Jan 6", "Aug 1", "Jan 31", "Jan 20", "June 30", "July 1", "June 7", "Feb 15", "Aug 15", "June 1", "March 31", "June 15", "Aug 10", "Aug 25", "March 15", "Aug 13", "March 0", "Nov 1", "July 29", "May 15", "April 15", "Nov 15", "April 30", "Aug 18", "July 15", "July 27", "Sept 7", "July 20")
length(l)
numintdata <- college_df[,sapply(collegedf2,is.numeric) | sapply(collegedf2,is.integer)]
categoricaldata <- college_df[,sapply(collegedf2,is.character)]
numintdata.imp <- missForest(numintdata)
numintdata.imp$ximp
numintdata <- numintdata.imp$ximp
rm(numdata)
numintdata.imp$OOBerror
sum(is.na(numintdata))
y <- collegedf2[,sapply(collegedf2,is.character)]
vis_miss(y)
mode(y$regular_application)
y <- missForest(y)
mode_func <- function(vec){
  
  
  
}
?t.test
unique(y$regular_application)
sounds <- c(
  "cat"="meow", 
  "dog"="woof", 
  "horse"="neigh"
)

tabulate(y)
as.vector(rep(table(y), table(y)))

cat_sound <- sounds["cat"]
sounds["cat"] <- "pee"
print(cat_sound)
?match
calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}
if (!require("tidyverse")) install.packages("tidyverse")

my_df <- data.frame(var_1 = c(3, 8, 0, 1, 3, 6, NA, 2, 4))
my_df

y %>% mutate(regular_application = if_else(is.na(regular_application), 
                         calc_mode(regular_application), 
                         regular_application))
y$regular_application = if_else(is.na(y$regular_application), 
                                "rolling", 
                                y$regular_application)

y$regular_application

temp <- cbind(numintdata.imp$ximp, categoricaldata)
str(temp)

library(psych)

pairs.panels(df1,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = TRUE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# Library
library(ggplot2)

# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)

# Heatmap 
ggplot(df1, aes(X, Y, fill= Z)) + 
  geom_tile()
heatmap(cor(df1[,c(1,2,3,6,7,8)]))
cor(df1[,c(1,2,3,6,7,8)])
names(df1[,c(1,2,3,6,7,8)])
library(ggplot2)
library(ggcorrplot)
install.packages("ggcorrplot")
# Correlation matrix
data(mtcars)
corr <- round(cor(mtcars), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)

g1 <- collegedf[which(collegedf$acceptance_rate > 50),]
g2 <- collegedf[which(collegedf$acceptance_rate <= 50),]
nrow(g1)
nrow(g2)
t.test(g1$percent_of_need_met, g2$percent_of_need_met)
length(g1$est_price_for_students_who_receive_aid)
length(g2$est_price_for_students_who_receive_aid)
class(g1$est_price_for_students_who_receive_aid)
size(g1$est_price_for_students_who_receive_aid)

UScrime
g1$percent_of_need_met
g2$percent_of_need_met
median(collegedf$acceptance_rate)
?median
college_df$acceptance_rate
class(college_df$acceptance_rate)
sum(is.na(collegedf))
sum(is.na(college_df$average_price_for_low_income_students))
