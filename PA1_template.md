---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data
For loading the data, you need to set your working directory in the same location as the file and then use the command "read.csv" as follows:


``` r
tabla <- read.csv("./activity.csv")
```

As you can see, I assigned the table information to the variable "tabla". We can see the content of tabla using "head":


``` r
head(tabla)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

As you can see, the variable contains the require information. To continue working with the table, we will erase the NA values in a new variable called "tablanoNAs" and we will check the content using the same head command.


``` r
tablanoNAs <- na.omit(tabla)
head(tablanoNAs)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

You can confirm the NA deletion using the next command:


``` r
sum(is.na(tabla$steps))
```

```
## [1] 2304
```

``` r
sum(is.na(tablanoNAs$steps))
```

```
## [1] 0
```

You will notice that there are no more NA rows in the steps column. So now we can process our data.

## What is mean total number of steps taken per day?

For this, we need to assign the date column as a factor to calculate the mean by day. We can use the dplyr Package to group the data and assign it to a new variable. In the new table, the column with the sum of steps for each day will be called "Total_steps".


``` r
library(dplyr, warn.conflicts = FALSE)

tablaporfecha <- tablanoNAs %>% group_by(date) %>% summarise(Total_steps = sum(steps))

head(tablaporfecha)
```

```
## # A tibble: 6 × 2
##   date       Total_steps
##   <chr>            <int>
## 1 2012-10-02         126
## 2 2012-10-03       11352
## 3 2012-10-04       12116
## 4 2012-10-05       13294
## 5 2012-10-06       15420
## 6 2012-10-07       11015
```

Once the table is grouped, we can generate an histogram using the base ploting system. Lets modify the breaks in the histogram so we can have a better idea of how is the information distributed across the dates:


``` r
hist(tablaporfecha$Total_steps, xlab = "Total steps", main = "Histogram of the total number of steps taken each day", breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Furthermore, the mean and median of the steps taken by day can be found using the "summary" function:


``` r
summary(tablaporfecha)
```

```
##      date            Total_steps   
##  Length:53          Min.   :   41  
##  Class :character   1st Qu.: 8841  
##  Mode  :character   Median :10765  
##                     Mean   :10766  
##                     3rd Qu.:13294  
##                     Max.   :21194
```

Or it can be calculated using the corresponding function as follows:


``` r
mean(tablaporfecha$Total_steps)
```

```
## [1] 10766.19
```

``` r
median(tablaporfecha$Total_steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
To solve this question, we will group the table but using the 5-minute interval, instead of the date.


``` r
tablaporintervalo <- tablanoNAs %>% group_by(interval) %>% summarise(Mean_steps = mean(steps))

head(tablaporintervalo)
```

```
## # A tibble: 6 × 2
##   interval Mean_steps
##      <int>      <dbl>
## 1        0     1.72  
## 2        5     0.340 
## 3       10     0.132 
## 4       15     0.151 
## 5       20     0.0755
## 6       25     2.09
```

Finally we build the plot using the base plotting system:


``` r
plot(tablaporintervalo$interval, tablaporintervalo$Mean_steps, "l", xlab = "Interval", ylab = "Mean steps", main = "Average number of steps taken by interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

## Imputing missing values



As we know now, there is a total of 2304 missing values in the steps column in the original data. We know this after we ran the command:


``` r
sum(is.na(tabla$steps))
```

[1] 2304

Now, we are going to fill those NAs with the mean value for each interval using the 


``` r
tablallena <- tabla %>% mutate_all(~replace(., is.na(.), tablaporintervalo$Mean_steps))
```

We can compare both tables and notice that the NAs are gone:


``` r
head(tabla)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

``` r
head(tablallena)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

``` r
sum(is.na(tabla$steps))
```

```
## [1] 2304
```

``` r
sum(is.na(tablallena$steps))
```

```
## [1] 0
```

Now, we can build the same histogram, calculate the mean and median of the table without NAs and compare this information against the original.


``` r
tablallenaporfecha <- tablallena %>% group_by(date) %>% summarise(Total_steps = sum(steps))

hist(tablallenaporfecha$Total_steps, xlab = "Total steps", main = "Histogram of the total number of steps taken each day without NAs", breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

``` r
mean(tablallenaporfecha$Total_steps)
```

```
## [1] 10766.19
```

``` r
median(tablallenaporfecha$Total_steps)
```

```
## [1] 10766.19
```

We can see the mean and media do not change to much. However, looking closely to the histogram (next figure), we can see that the amount of data in the middle of the histogram increase. The approach we took for filling the NA values can trick us about the real dispersion of the data when compared to the original file.


``` r
par(mfrow = c(1, 2))

hist(tablaporfecha$Total_steps, xlab = "Total steps", main = "Histogram with NAs", breaks = 10)

hist(tablallenaporfecha$Total_steps, xlab = "Total steps", main = "Histogram NAs replaced", breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

Using the filled up table, we will compare the interval activity in weekdays versus weekends. First, we create a new column which contains the weekday by using the weekdays function. We can check the new table content using summary, and the content of the new column by using the unique function:


``` r
tablallena$weekday <- weekdays(as.Date(tablallena$date, format = "%Y-%m-%d"))

head(tablallena)
```

```
##       steps       date interval weekday
## 1 1.7169811 2012-10-01        0  Monday
## 2 0.3396226 2012-10-01        5  Monday
## 3 0.1320755 2012-10-01       10  Monday
## 4 0.1509434 2012-10-01       15  Monday
## 5 0.0754717 2012-10-01       20  Monday
## 6 2.0943396 2012-10-01       25  Monday
```

``` r
unique(tablallena$weekday)
```

```
## [1] "Monday"    "Tuesday"   "Wednesday" "Thursday"  "Friday"    "Saturday" 
## [7] "Sunday"
```

Then, we will create a factor using this new column by replacing each of the days by either "Weekday" or "Weekend". We can check the replacement of the values using the same unique function:


``` r
tablallena$weekday[tablallena$weekday == "Monday"] <- "Weekday"
tablallena$weekday[tablallena$weekday == "Tuesday"] <- "Weekday"
tablallena$weekday[tablallena$weekday == "Wednesday"] <- "Weekday"
tablallena$weekday[tablallena$weekday == "Thursday"] <- "Weekday"
tablallena$weekday[tablallena$weekday == "Friday"] <- "Weekday"

tablallena$weekday[tablallena$weekday == "Saturday"] <- "Weekend"
tablallena$weekday[tablallena$weekday == "Sunday"] <- "Weekend"

unique(tablallena$weekday)
```

```
## [1] "Weekday" "Weekend"
```

As a final step, we calculate the mean for the intervals by building 2 tables and binding them at the end. Then we can plot the steps by interval for the weekdays and the weekends using ggplot2:


``` r
tablaweekday <- tablallena %>% filter(weekday == "Weekday") %>% group_by(interval) %>% summarise(steps.mean = mean(steps)) %>% cbind(weekday = "Weekday")
tablaweekend <- tablallena %>% filter(weekday == "Weekend") %>% group_by(interval) %>% summarise(steps.mean = mean(steps)) %>% cbind(weekday = "Weekend")

tablaparaplot <- tablaweekday %>% rbind(tablaweekend) %>% group_by(weekday)

library(ggplot2)

ggplot(tablaparaplot) +
  geom_line(aes(interval, steps.mean), color = "blue") +
  facet_grid(vars(weekday)) +
  labs(y = "Average steps by interval", title = "Comparison of average steps on the week vs on the weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

And that would be all. Thank you.
