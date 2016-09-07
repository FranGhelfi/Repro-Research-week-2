
## Loading and processing data

setwd("C:/Users/Francisco/Documents/GitHub/ReproResearch_week2")
data <- read.csv("activity.csv")

## Mean of total number of steps per day

data_no_na <- data[is.na(data[,"steps"]) == FALSE,]
per_day <- data.frame(table(data_no_na[,"date"]), stringsAsFactors = FALSE)
colnames(per_day) <- c("date", "freq")

for (x in 1:nrow(per_day)) {
    per_day[x,"total_steps"] <- sum(data_no_na[data_no_na[,"date"] == per_day[x,"date"], "steps"]) 
}

hist(per_day[,"total_steps"])

steps_mean <- mean(per_day[,"total_steps"])
steps_median <- median(per_day[,"total_steps"])

## Average daily activity pattern

per_interval <- data.frame(table(data_no_na[,"interval"]), stringsAsFactors = FALSE)
colnames(per_interval) <- c("interval", "freq")

for (x in 1:nrow(per_interval)) {
    per_interval[x,"mean_steps"] <- mean(data_no_na[data_no_na[,"interval"] == per_interval[x,"interval"], "steps"]) 
}

plot(per_interval[,"interval"], per_interval[,"mean_steps"])

max_steps <- max(per_interval[,"mean_steps"])
top_interval <- per_interval[per_interval[,"mean_steps"] == max_steps, "interval"]

## Inputing missing values

toal_nas <- nrow(data[is.na(data[,"steps"]) == TRUE,])

data_mo_mv <- data

for (x in 1:nrow(data_mo_mv)) {
    
    if (is.na(data_mo_mv[x,"steps"]) == TRUE) {
        data_mo_mv[x,"steps"] <- per_interval[per_interval[,"interval"] == data_mo_mv[x,"interval"], "mean_steps"]
    }
}

per_day_2 <- data.frame(table(data_mo_mv[,"date"]), stringsAsFactors = FALSE)
colnames(per_day_2) <- c("date", "freq")

for (x in 1:nrow(per_day_2)) {
    per_day_2[x,"total_steps"] <- sum(data_mo_mv[data_mo_mv[,"date"] == per_day_2[x,"date"], "steps"]) 
}

hist(per_day_2[,"total_steps"])

steps_mean_2 <- mean(per_day_2[,"total_steps"])
steps_median_2 <- median(per_day_2[,"total_steps"])

mean_dif <- steps_mean_2 - steps_mean
median_dif <- steps_median_2 - steps_median

mean_impact <- (100*mean_dif) / steps_mean
median_impact <- (100*median_dif) / steps_median

## Weekdays vs weekends

data_mo_mv[,"date"] <- as.Date(data_mo_mv[,"date"])
data_mo_mv[,"weekday_type"] <- weekdays(data_mo_mv[,"date"])

for (x in 1:nrow(data_mo_mv)) {
    
    if (data_mo_mv[x,"weekday_type"] == "Saturday" | data_mo_mv[x,"weekday_type"] == "Sunday" ) {
        data_mo_mv[x,"week_moment"] <- "Weekend"
    } else {
        data_mo_mv[x,"week_moment"] <- "Weekday"
    }
}

per_week_moment <- data.frame(table(data_mo_mv[,"week_moment"], data_mo_mv[,"interval"]), stringsAsFactors = false)
colnames(per_week_moment) <- c("week_moment", "interval", "freq")

for (x in 1:nrow(per_week_moment)) {
    per_week_moment[x,"mean_steps"] <- mean(data_mo_mv[data_mo_mv[,"week_moment"] == per_week_moment[x,"week_moment"] 
                                                       & data_mo_mv[,"interval"] == per_week_moment[x,"interval"], "steps"])
}


week_days <- per_week_moment[per_week_moment[,"week_moment"]== "Weekday",]
week_days[,"interval"] <- as.numeric(week_days[,"interval"])
week_end <- per_week_moment[per_week_moment[,"week_moment"]== "Weekend",]
week_end[,"interval"] <- as.numeric(week_end[,"interval"])

plot(week_days[,"interval"], week_days[,"mean_steps"], type="l", col="blue",
     xlab="Interval", ylab="Avg. steps taken")
lines(week_end[,"interval"], week_end[,"mean_steps"], type="l", col="red")
legend ("topright", c("Weekdays", "Weekends"), col = c("blue", "red"), lty = 1)
