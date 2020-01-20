Loading and preprocessing the data
----------------------------------

I used the below packages for my code in this report

    library(plyr)
    library(dplyr)
    library(ggplot2)
    library(lubridate)
    library(reshape2)

I used the below code to load the data and remove any NA values

    #Load the data, remove NAs
    data_1 <- read.csv("C:\\Users\\Daniel\\Documents\\R\\data\\activity.csv")
    data_NA <- is.na(data_1$step)
    data_2 <- data_1[!data_NA,]

What is the mean total number of steps taken per day?
-----------------------------------------------------

First, I consolidated the data using tapply, (I had to remove the NA's
once again):

    tsum <- tapply(data_2[,1],data_2$date,FUN = sum)
    tsum_NA <- is.na(tsum)
    tsum <- tsum[!tsum_NA]
    tsum

    ## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 2012-10-09 
    ##        126      11352      12116      13294      15420      11015      12811 
    ## 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 2012-10-15 2012-10-16 
    ##       9900      10304      17382      12426      15098      10139      15084 
    ## 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 
    ##      13452      10056      11829      10395       8821      13460       8918 
    ## 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
    ##       8355       2492       6778      10119      11458       5018       9819 
    ## 2012-10-31 2012-11-02 2012-11-03 2012-11-05 2012-11-06 2012-11-07 2012-11-08 
    ##      15414      10600      10571      10439       8334      12883       3219 
    ## 2012-11-11 2012-11-12 2012-11-13 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
    ##      12608      10765       7336         41       5441      14339      15110 
    ## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
    ##       8841       4472      12787      20427      21194      14478      11834 
    ## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
    ##      11162      13646      10183       7047

'tsum' is a vector containg the values of the total number of steps
taken each day. Then I used 'tsum' to calculate the mean and median:

    mean_steps <- round(mean(tsum),2)
    median_steps <- round(median(tsum),2)

The median steps per day is 1.076510^{4} and the mean number of steps is
1.07661910^{4}. The code below plots a histogram of the total number of
steps taken each day:

    hist(tsum,breaks = 6, xlab = "Steps",
         main = "Histogram of Steps Taken")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

What is the average daily activity pattern?
-------------------------------------------

To calculate I used tapply to consolidate the data into average steps
taken for each interval.

    #create new data frame
    data_3 <- data_2

    #find average of each interval across all days
    taverage <- tapply(data_3[,1],data_3$interval,
                       FUN = mean)

Using the above data, I used ggplot to make a time series plot for each
interval averaged across all days.

    #create the data frame for ggplot
    x <- as.numeric(as.character(names(taverage)))
    y <- taverage
    ggdata_1 <- data.frame(x,y)
    #Plot the data
    g <- ggplot(data = ggdata_1, aes(x = ggdata_1$x,
                                     y = ggdata_1$y,
                                     group =1))
    g + geom_line()+ggtitle("Average Steps by Interval")+
            labs(y = "Average Steps", x ="Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

Lastly, I found the interval at which the max average steps occurred:

    max_interval = names(which.max(taverage))

The interval with the highest average was 835.

Imputing missing values
-----------------------

Below is the code to calculate the number of missing values:

    #Calculate number of missing values and create new data frame
    NAcount <- sum(is.na(data_1[,1]))
    data_4 <- data_1

There are 2304 missing values

My strategy for imputing missing values was to use the average steps for
each interval. So, created a data frame that mapped each interval to its
average steps:

    #Create data frame matching intervals to their averages
    avgDF <- data.frame(names(taverage),taverage)
    names(avgDF) <- c("interval","Interval Average")

Then I merged the above data frame with the original data and replaced
the missing values with the interval averages

    #merge original data with interval averages
    mergetest <- merge(data_4,avgDF, by.x = "interval",by.y = "interval")
    #Map interval averages to steps with NA values
    mergetest[is.na(mergetest$steps),"steps"] <- mergetest[is.na(mergetest$steps),"Interval Average"]

Then I used tapply to get the average steps for each day, and plotted
the frequency of steps taken with a histogram

    #sum the total steps (with imputed values) for each day
    Impsum <- tapply(mergetest[,2],mergetest$date, FUN = sum)
    #plot histogram of Impsum
    hist(Impsum,breaks = 6, xlab = "Steps",
         main = "Histogram of Steps Taken (Imputed)")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)

Lastly, I calculated the new mean and median using the imputed data:

    #calculate mean and median of imputed data
    median_steps_imp <- round(median(Impsum),2)
    mean_steps_imp <- round(mean(Impsum),2)

-   Original median: 1.076510^{4}
-   Imputed median: 1.07661910^{4}
-   Original mean: 1.07661910^{4}
-   Imputed meanK: 1.07661910^{4}

Are there differences in activity between weekdays and weekends?
----------------------------------------------------------------

I used the weekdays function to add a column to the dataframe denoting
dates that were weekends or weekdays

    #change the date from factor to date
    mergetest$date <- as.Date(mergetest$date,format = "%Y-%m-%d")
    #Add a weekday column
    mergetest$weekday <- weekdays(mergetest$date)
    #Find indexes of Saturday and Sunday and replace them with "Weekend"
    wIndex <- mergetest$weekday == "Sunday" |
            mergetest$weekday == "Saturday"
    mergetest$weekday[wIndex] <- "Weekend"
    #Find indexes of non Weekends and replace with "Weekday"
    wIndex2 <- mergetest$weekday != "Weekend"
    mergetest$weekday[wIndex2] <- "Weekday"

Then I extracted the relevant columns, aggregated by interval and
weekday, and finally plotted the results using ggplot

    #Select relevant columns
    weekData <- mergetest[,c(1,2,5)]
    #Aggregate by interval and weekday
    weekData2 <- aggregate(.~interval+weekday,weekData, mean)
    #Plot using ggplot, and separate by weekday factor
    g2 <- ggplot(data = weekData2, aes(x = weekData2$interval,
                                       y = weekData2$steps, 
                                       group =1))
    g2 + geom_line()+facet_grid(rows=vars(weekday))+
            ggtitle("Average Steps by Interval")+
            labs(y = "Average Steps", x ="Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-15-1.png)
