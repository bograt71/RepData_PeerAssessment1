# Repdata-032 Assignment 1
Bograt71  
20 September 2015  

Introduction
============

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment from the Coursera **[Reproducible Research](https://www.coursera.org/course/repdata)** at John Hopkins University, makes use of data from a personal activity monitoring device to demonstrate the skills learned not only on this course module but also from the previous modules on the Signature Track. 

Device Data Background
----------------------
The device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

- **Dataset**: Activity monitoring data [52K]  

The variables included in this dataset are:  

- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- **date**: The date on which the measurement was taken in YYYY-MM-DD format

- **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Loading and Preprocessing the data
--------

The script uses your existing working directory. Please change this if necessary using the `getwd()` and `setwd()` commands.

The following libraries are required for the analysis:

- lubridate  
- dplyr  
- ggplot2  
- lattice  


```r
library(lubridate)
library(ggplot2)
library(lattice)
library(dplyr)
```

We will also adjust the default number of digits required to display **mean** and **median** values to 12.  


```r
options(digits=12)
```


Four actions are taken to load and prepare the data for analysis:  

1. Download the data from the source  
2. Load and unzip the data using the ```{r}data.csv```  and ```{r}unz``` commands  
3. Processing the headfile  
4. Converting the date string field into factors  


```r
if (!file.exists("./data")) {dir.create("./data")}
fileURL <-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"  
download.file(fileURL,destfile="./data/repdata data activity.zip",method="curl")  
unzip("./data/repdata data activity.zip","activity.csv",exdir="./data")
activityData<-read.csv("./data/activity.csv",header=FALSE,sep=",", na.strings="NA",skip=1) 
cnames<-readLines ("./data/activity.csv",1) 
cnames
```

```
## [1] "\"steps\",\"date\",\"interval\""
```

```r
## split out the names 
cnames<-strsplit(cnames, ",",fixed=TRUE)
cnames
```

```
## [[1]]
## [1] "\"steps\""    "\"date\""     "\"interval\""
```

```r
## remove the \" elements
cnames<-gsub('\\"',"",cnames[[1]])

## Assign the processed names to our data frame (activityData)
names(activityData)<-cnames

## Convert date to factor 
activityData$date<-as.Date(activityData$date,format="%Y-%m-%d")
```

Quickly verify that the data is in the format that we require:


```r
head(activityData)
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

```r
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Data Analysis
===
We are now able to start the analysis of the data.

## What is the mean total number of steps taken per day?

We can use a histogram to illustrate the average of the total number of steps taken each day.  


```r
q1<-activityData%>% group_by(date) %>% summarize(total.steps=sum(steps))
res<-with(q1,hist(total.steps,plot=FALSE))
## determine what the maximum y values are to re-adjust the extent of the axis.
rng<-range(res$counts)
rng
```

```
## [1]  2 28
```

```r
with(q1,hist(total.steps, col="red", main="Histogram for total number of steps taken each day", xlab="Number of steps taken each day.",labels=TRUE,ylim=c(0,30)))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
summary(q1$total.steps)
```

```
##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
##    41.0000  8841.0000 10765.0000 10766.1887 13294.0000 21194.0000 
##       NA's 
##          8
```

From the table we can see that the **mean** value is:

```
## [1] 10766.1886792
```
and the **median** value is:

```
## [1] 10765
```


## What is the average daily activity pattern?

Next level down in our analysis is to investigate the average daily pattern, where we focus on the average number of steps taken at each of the 5 minute intervals. We add a line on the plot to indicate what time period the maximum number of steps occurred.  


```r
q2<-activityData%>% group_by(interval) %>% summarize(average.steps=mean(steps,na.rm=TRUE))
with(q2,plot(average.steps~interval, type="l",main="Average steps taken at each 5 minute interval"),ylab="Avg. number of steps")

## extract the time period with the maximum number of average.steps
maxsteps<- q2[which.max(q2$average.steps),]
## time period with maximum number of steps
maxsteps[1,1]
```

```
## Source: local data frame [1 x 1]
## 
##   interval
## 1      835
```

```r
abline(v=maxsteps[1,1],col="blue",lwd=3)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

## Imputing missing values

The above analyses were done ignoring the fact that there are a number of days where no data was available -`NA's`. This will introduce bias in our analysis and should be explored.

Let's quantify the problem first; how many days are there for which we have no data?


```r
sum(is.na(q1))
```

```
## [1] 8
```

There are 8 days where no data was collected.  

### Determining what strategy to adopt to impute the missing values

As this does not need to be a sophisticated strategy, we can use some simplistic methods.  First off, let's create a function to allow us to apply a function over our data. Source for this idea came from an old newsgroup post by **[Hadley](http://www.mail-archive.com/r-help@r-project.org/msg58289.html)**, though he did use ```dplyr``` as the method to apply over the subset. We'll use ```mutate``` from the ```dplyr``` library instead.    

Create the function:  

```r
impute <- function(x, fun) {
        missing <- is.na(x)
        replace(x, missing, fun(x[!missing]))
}
```

We can now use this function to try a number of strategies, such as ```min```, ```mean``` or ```median```. Each of them impact the data in their own way. Final decision is to use ```mean``` and create a new dataset called ```q4``` to hold these imputed values.


```r
q4<-q3%>% group_by(interval) %>%mutate(steps=impute(steps,mean))
head(q4)
```

```
## Source: local data frame [6 x 3]
## Groups: interval
## 
##             steps       date interval
## 1 1.7169811320755 2012-10-01        0
## 2 0.3396226415094 2012-10-01        5
## 3 0.1320754716981 2012-10-01       10
## 4 0.1509433962264 2012-10-01       15
## 5 0.0754716981132 2012-10-01       20
## 6 2.0943396226415 2012-10-01       25
```
Now let's compare the total number of steps taken each day as before. We need to group our ```q4``` dataframe by date again, into a data frame we'll call ```q5```.


```r
q5<-q4%>% group_by(date) %>% summarize(total.steps=sum(steps))
## generate the data from a plot, but do not plot using the plot=FALSE argument.
res1<-with(q5,hist(total.steps,plot=FALSE))
## work out the range for the y-axis.
rng1<-range(res1$counts)
rng
```

```
## [1]  2 28
```

```r
with(q5,hist(total.steps, col="red", main="Histogram for total number of steps taken each day", xlab="Number of steps taken each day.",labels=TRUE,ylim=c(0,40)))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

We can compare the **mean** and **median** values as before, using either the ```summary``` function or individually with ```mean``` and ```median```.  


```r
summary(q5)
```

```
##       date             total.steps        
##  Min.   :2012-10-01   Min.   :   41.0000  
##  1st Qu.:2012-10-16   1st Qu.: 9819.0000  
##  Median :2012-10-31   Median :10766.1887  
##  Mean   :2012-10-31   Mean   :10766.1887  
##  3rd Qu.:2012-11-15   3rd Qu.:12811.0000  
##  Max.   :2012-11-30   Max.   :21194.0000
```

```r
mean(q5$total.steps,na.rm=TRUE)
```

```
## [1] 10766.1886792
```

```r
median(q5$total.steps,na.rm=TRUE)
```

```
## [1] 10766.1886792
```

We can see that these differ from the previous values, primarily as we've shifted all the ```NA``` values to a mean, the **median** has now shifted to the **mean** value. 

## Are there differences in activity between the weekdays and weekends?

Last part of the assignment is to determine if there is a difference between weekdays and weekends. We create a custom function using the `weekdays` command to determine which day of the week a date is. We create this as a factor with two levels (i.e. it is either a weekday, or it is not - which means it must be a weekend day).


```r
weekday_or_weekend <- function (x) {
        work<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
        factor((weekdays(x) %in% work),levels=c(TRUE,FALSE),labels=c("weekday","weekend"))
}
```

And then using the `mutate` function from the `dplyr` package, apply it across the dataframe. Use the group by function to group into *weekdays* or *weekends* and then summarise the mean number of steps.


```r
q8<-q4 %>% mutate(work.or.week.day=weekday_or_weekend(date))%>% group_by(interval,work.or.week.day) %>% summarize(average.steps=mean(steps))
head(q8)
```

```
## Source: local data frame [6 x 3]
## Groups: interval
## 
##   interval work.or.week.day   average.steps
## 1        0          weekday 2.2511530398323
## 2        0          weekend 0.2146226415094
## 3        5          weekday 0.4452830188679
## 4        5          weekend 0.0424528301887
## 5       10          weekday 0.1731656184486
## 6       10          weekend 0.0165094339623
```

Finally, construct a panel plot using the `lattice` library loaded earlier to create two plots (one above the other), to allows the comparison between the two separate factors.


```r
par(mfrow=c(2,1))
xyplot(average.steps ~ interval | work.or.week.day, data=q9,layout=c(1,2),type="l",xlab="Interval",ylab="Number of steps",main="Difference in activity between weekdays and weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png) 


    
