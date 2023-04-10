fulltank <- c(24317, 24539, 24781, 25008, 25239, 25442, 25801, 26089)
miles_between_fulltank <- diff(fulltank)
miles_between_fulltank


max_miles_between_fulltank <- max(miles_between_fulltank)
max_miles_between_fulltank

average_miles_between_fulltank <- mean(miles_between_fulltank)
average_miles_between_fulltank

minutes_to_work <- c(32, 46, 41, 52, 36, 28, 37, 41, 38, 37)
minutes_to_work

minutes_to_work_over_40min <- sum(minutes_to_work>=40)
minutes_to_work_over_40min

pct_minutes_to_work_less_than_35min <- length(minutes_to_work[minutes_to_work<35]) / length(minutes_to_work * 100)
pct_minutes_to_work_less_than_35min

celcius_temp <- c(38, 16, 23)
celcius_temp

celcius_to_fahrenheit <- (celcius_temp*(9/5)+32)
celcius_to_fahrenheit

R <- c(1.54, 2.18, 3.82, 2.44, 2.73, 1.64)
H <- c(5.37, 8.25, 9.36, 7.70, 6.72, 5.59)
pi <- 3.14
V <- (1/3 * pi * R^2 * H)
mean_V <- mean(V)
median_V <- median(V)
sd_V <- sd(V)

R_snapshot <- subset(R, H < 7.5)
H_snapshot <- subset(R, H < 7.5)
V_snapshot <- (1/3 * pi * R_snapshot^2 * H_snapshot)
mean_V_snapshot <- mean(V_snapshot)

president_hist <- hist(mydata$Age)
pres_sort_by_age <- mydata[order(mydata$Age),]
top_10_youngest_pres <- head(pres_sort_by_age, 10)
top_10_youngest_pres

df1 <- read.csv("Demographics.csv", header=TRUE)
df2 <- read.csv("QuestionAnswers.csv", header=TRUE)
View(df1)
View(df2)

joined_data <- merge(df1, df2, by.x = "WorkerId", by.y = "WorkerId", all.x = TRUE, all.y = TRUE)
write.csv(joined_data, "C:/Users/devon/OneDrive/Documents/Texas_A&M_MIS/MIS-Data Mining/R/demoandquestionanwser.csv")