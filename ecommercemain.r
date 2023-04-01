library(readr)
library(dplyr)
library(ggplot2)
library(DataExplorer)
#
custData <- read_csv('E:/Creig Files/Projects/BDAR/data.csv')

#We'll start by glimpsing the data to get a feel for its size and the class of each variable:
glimpse(custData)
dim(custData)

options(repr.plot.width=8, repr.plot.height=3)
# look for missing values using the DataExplorer package


#graph 0 for missing data

plot_missing(custData)
##Looking at the size of the dataset and the missing value plot, it seems as if we can remove the missing values and still have a good-sized set of data to work on, so let's start by doing that:

custData <- na.omit(custData)
dim(custData)

##we've still got over 400,000 rows of data to work with. One thing that pops out is the InvoiceDate variable. This is a character variable, but we can pull out the data and time information to create two new variables: date and time. We'll also create separate variables for month, year and hour of day.


# separate date and time components of invoice date
custData$date <- sapply(custData$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
custData$time <- sapply(custData$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][2]})



# create month, year and hour of day variables
custData$month <- sapply(custData$date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][1]})
custData$year <- sapply(custData$date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][3]})

custData$hourOfDay <- sapply(custData$time, FUN = function(x) {strsplit(x, split = '[:]')[[1]][1]})

head(custData, n =5)


##pulled out some more useful components from our invoice date column, but let's convert the date variable to the appopriate class so we can do a bit more with it:
custData$date <- as.Date(custData$date, "%m/%d/%Y")


#we can create a new variable that tells us the day of the week, using the wday function from the lubridate package.


library(lubridate)
custData$dayOfWeek <- wday(custData$date, label=TRUE)

##And we'll just add one more column at this stage, we'll calculate the line total by multiplying the Quantity by the UnitPrice for each line:
custData <- custData %>% mutate(lineTotal = Quantity * UnitPrice)

head(custData)


##That's given us a good dataframe to start performing some summary analyses. But before we move on to getting involved with product and customer segmentation, we'll look at some of the bigger features of the dataset. First, we'll turn the appropriate variables into factors:

custData$Country <- as.factor(custData$Country)
custData$month <- as.factor(custData$month)
custData$year <- as.factor(custData$year)
levels(custData$year) <- c(2010,2011)
custData$hourOfDay <- as.factor(custData$hourOfDay)
custData$dayOfWeek <- as.factor(custData$dayOfWeek)

#That done, we can take a look at some of the 'big picture' aspects of the dataset.
#revenue by date graph 1

options(repr.plot.width=8, repr.plot.height=3)
custData %>%
  group_by(date) %>%
  summarise(revenue = sum(lineTotal)) %>%
  ggplot(aes(x = date, y = revenue)) + geom_line() + geom_smooth(method = 'auto', se = FALSE) + labs(x = 'Date', y = 'Revenue (£)', title = 'Revenue by Date')

###It appears as though sales are trending up, so that's a good sign, but that doesn't really generate any actionable insight, so let's dive into the data a bit farther...

###Day of week analysis¶
#revenue by day of week graph 2

custData %>%
  group_by(dayOfWeek) %>%
  summarise(revenue = sum(lineTotal)) %>%
  ggplot(aes(x = dayOfWeek, y = revenue)) + geom_col() + labs(x = 'Day of Week', y = 'Revenue (£)', title = 'Revenue by Day of Week')




weekdaySummary <- custData %>%
  group_by(date, dayOfWeek) %>%
  summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup()

head(weekdaySummary, n = 10)



# day of week graph 3
ggplot(weekdaySummary, aes(transactions, fill = dayOfWeek)) + geom_density(alpha = 0.2)


kruskal.test(transactions ~ dayOfWeek, data = weekdaySummary)


###### try 
library(agricolae)
kruskal(weekdaySummary$transactions, weekdaySummary$dayOfWeek, console = TRUE)

####

###Conclusions from our day-of-the-week summary¶

##revenue by hour of the day
custData %>%
  group_by(hourOfDay) %>%
  summarise(revenue = sum(lineTotal)) %>%
  ggplot(aes(x = hourOfDay, y = revenue)) + geom_col() + labs(x = 'Hour Of Day', y = 'Revenue (£)', title = 'Revenue by Hour Of Day')


## no. of transactions by hour of the day
custData %>%
  group_by(hourOfDay) %>%
  summarise(transactions = n_distinct(InvoiceNo)) %>%
  ggplot(aes(x = hourOfDay, y = transactions)) + geom_col() + labs(x = 'Hour Of Day', y = 'Number of Transactions', title = 'Transactions by Hour Of Day')


#####Country summary¶
##Gives the country summary for all the transactions



countrySummary <- custData %>%
  group_by(Country) %>%
  summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))

head(countrySummary, n = 10)
unique(countrySummary$Country)



countryCustSummary <- custData %>%
  group_by(Country) %>%
  summarise(revenue = sum(lineTotal), customers = n_distinct(CustomerID)) %>%
  mutate(aveCustVal = (round((revenue / customers),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))

head(countryCustSummary, n = 10)




topFiveCountries <- custData %>%
  filter(Country == 'Netherlands' | Country == 'EIRE' | Country == 'Germany' | Country == 'France' | Country == 'Australia')







####Customer segmentation¶
custSummary <- custData %>%
  group_by(CustomerID) %>%
  summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))

head(custSummary, n = 10)




#transactions by customer ID

ggplot(custSummary, aes(revenue)) + geom_histogram() + scale_x_log10() + labs(x = 'Revenue', y = 'Count of Customers', title = 'Histogram of Revenue per customer (Log Scale)')


ggplot(custSummary, aes(transactions)) + geom_histogram() + scale_x_log10() + labs(x = 'Number of Transactions', y = 'Count of Customers', title = 'Histogram of Transactions per customer')
















##custSummaryB <- custData %>%
##  group_by(CustomerID, InvoiceNo) %>%
##  summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>%
##  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
##  ungroup() %>%
##  arrange(revenue) %>%
##  mutate(cumsum=cumsum(revenue))

##head(custSummaryB, n =10)



