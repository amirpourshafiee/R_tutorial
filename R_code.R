########################################
# Functions
########################################

mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x,ux)))]
}

range <- function(x) {
    ma <- max(x)
    mi <- min(x)
    ma - mi
}

########################################
# Data Load
########################################

# Load data in cvs format
mydata = read.csv("practice.csv")

########################################
# Data cleanning
########################################

# Remove any row that has NA values
mydata = na.omit(mydata)

# Convert text entries to numeric format
gender = as.numeric(mydata$Gender)
visionloss = as.numeric(mydata$Visionloss)
outsidetravel = as.numeric(mydata$OutsideTravel)
headaccident = as.numeric(mydata$HeadAccident)
falls = as.numeric(mydata$Falls)

# Update mydata table
pid = mydata$PID
age = mydata$Age
aidduration = mydata$AidDuration

updated_data = data.frame(pid, gender, age, visionloss, aidduration,
                          outsidetravel, headaccident, falls)


########################################
# Descriptive analysis
########################################

# Central Tendency measurement
sapply(updated_data, mean)

sapply(updated_data, median)

mode(updated_data$age)
mode(updated_data$gender)
mode(updated_data$aidduration)

# Measure of spread
range(updated_data$age)

var(updated_data$age)
var(updated_data$gender)
var(updated_data$aidduration)

sd(updated_data$age)
sd(updated_data$gender)
sd(updated_data$aidduration)

########################################
# Analysis of Variance - Within group
########################################

# Hypothesis 1 - T test
# Older people tend to fall more frequently

t.test(updated_data$age, updated_data$falls, paired=TRUE)

# Hypothesis 2 - ANOVA
# Does the degree with vision loss depends upon gender?

aov(updated_data$visionloss ~ updated_data$gender, data=updated_data)

# Hypothesis 3 - Regression
# What is the relationship between age and falls?

lm(updated_data$falls ~ updated_data$age, data=updated_data)

# Hypothesis 4 - Chi-square
# Whether the participants visionloss is independent of falls

chisq.test(visionloss, falls)