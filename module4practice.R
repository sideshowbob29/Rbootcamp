#this section is on linear algebra

A <- matrix(1:9, 3)
B <- matrix(seq(4,36, by = 4), 3)
A
B
A + B
#multiplying each column of matrix A by column 1 of matrix B
A * B[ , 1]

?"%*%"
#multiplying matrix by vector--vector is first column of B
A %*% B[ , 1]

?svd

params <- list(a = list(mn = 7, sd = 3), b = list(mn = 6,sd = 1), 
               c = list(mn = 2, sd = 1))
#sapply applies a function to all the elements in a vector
#"[[" selects a particular element from a list
sapply(params, "[[", 1)


air <- read.csv('../data/airline.csv', stringsAsFactors = FALSE)


#make a table using 2 fields
tbl <- table(air$UniqueCarrier, air$Year)

#creating table using subsetted data
with(air[air$UniqueCarrier == 'UA', ], table(Month, Cancelled))
tbl

DestSubset <- c('LAX','SEA','PHX','DEN','MSP','JFK','ATL','DFW','IAH', 'ORD') 
airSmall <- subset(air, Dest %in% DestSubset)
airSmall$DepDelay[airSmall$DepDelay > 60] <- 60
#split data into 10 smaller dataframes
subsets <- split(airSmall, airSmall$Dest)
length(subsets)
subsets
str(subsets)

airSmallNum <- airSmall[ , c('DepDelay', 'ArrDelay', 'TaxiIn', 'TaxiOut')]
aggregate(airSmallNum, by = list(destination = airSmall$Dest), FUN = median, na.rm = TRUE)

numFlights <- as.data.frame(table(air$Dest))
numFlights

#how to join in R
air2 <- merge(air, numFlights, by.x = 'Dest', by.y = 'Var1',
              all.x = TRUE, all.y = FALSE)

#create a vector that concatenates Origin and Dest
Route <- paste(air$Origin, "_", air$Dest)
View(Route)

#generate a table of count of flights per year
table(air$Year)

#compute the number of NAs in each column of air
#using sapply to create a matrix where is.na has been applied to every element in air;
#this creates of matrix of "TRUE/FALSE"
scratch <-sapply(air, FUN=is.na)
#summing count of TRUEs in each column; TRUE is counted as 1, False as 0; this gives me
#the count of NAs in each column
colSums(scratch)

#merge air dataset with carriers dataset
#first, importing carriers
carriers <- read_csv("C:/Users/jacob/Documents/r-bootcamp-2016-master/r-bootcamp-2016-master/data/carriers.csv")
#need to join air$UniqueCarrier to carriers$Code
air2 <- merge(air, carriers, by.x = 'UniqueCarrier', by.y = 'Code',
              all.x = TRUE, all.y = FALSE)

#Discretize distance into some bins and create a Dist_binned variable. 
#Count the number of flights in each bin.
str(air$Distance)
summary(air$Distance)
#creating new variable of binned distances
air$Dist_binned <- cut(air$Distance, breaks = c(0, 250, 500, 1000, 1500, 2000, 2500, 3000))
#giving labels to my bins
levels(air$Dist_binned) <- c('0-250','250-500','500-1000',
                             '1000-1500','1500-2000','2000-2500','2500-3000')
#creating table with counts of flights in each distance bins
bintable <- table(air$Dist_binned)
write.csv(bintable, "bintable.csv")

#Create a boxplot of delay by binned distance.
boxplot(air$DepDelay ~ air$Dist_binned)

#Sort the dataset and find the shortest flight. 
#Now consider the use of which.min() and why using that 
#should be much quicker with large datasets.

#this creates a vector of row numbers in order of distance
ord <- order(air$Distance, decreasing = FALSE)
#this applies that vector to air; now air will be sorted by distance
air_ordered <- air[ord, ]
head(air_ordered)
#here is a way to get the shortest distance from my ord vector
air$Distance[ord[1]]
#here is another way to get the shortest distance; this saves a stepw
air$Distance[which.min(air$Distance)]

#how to compute chi-square statistic by hand without loops
#this is your observations matrix
y <- matrix(sample(1:10, 12, replace = TRUE), 
            nrow = 3, ncol = 4)
#pretend this is your expected outcomes matrix
e <- matrix(sample(1:10, 12, replace = TRUE), 
            nrow = 3, ncol = 4)
#this produces a matrix of each element of the chi-squared statistic
chimatrix <- (y - e)^2/e
#this sums up all the elements, giving the chi-squared statistic
sum(chimatrix)
#generating the matrix of expected values
expectedmatrix <- outer(rowSums(y), colSums(y), "*")/sum(y)

#For each combination of UniqueCarrier, Month, and DayOfWeek, 
#find the 95th percentile of delay.
#first, creating new field that is unique combos of carrier, month, day of week
air$bin <- paste(air$UniqueCarrier, "_", air$Month, "_", air$DayOfWeek)
#opening dplyr 
library(dplyr)
#creating groups based on bin field
airbins <- group_by(air, bin)
#calculating 95th percentile per bin, ignoring NAs
airbinsummary <- summarize(airbins, pct = quantile(DepDelay,0.95, na.rm=TRUE))

h <- air$DepTime %/% 100
m <- air$DepTime %% 100
h <- as.character(h)
h[nchar(h) == 1] <- paste('0', h[nchar(h) == 1], sep = '')
air$DepTimeChar <- paste(h, m, sep = '-')
head(air$DepTimeChar)
#Create hour and minute fields using strsplit() and sapply() from air$DepTimeChar. 
#What format is the result of strsplit(). Why do you need sapply()?
?strsplit
scratch1 <- strsplit(air$DepTimeChar, "-")
str(scratch1)
typeof(scratch1)
hourmin <- strsplit(air$DepTimeChar, "-")
str(air)
air$hour <-substr(air$DepTimeChar,1,2)
air$min <-substr(air$DepTimeChar,4,5)
View(air)
str(air)
?sapply
sapply(air$DepTimeChar, strsplit)
scratch2 <- sapply((strsplit(air$DepTimeChar, "-")), '[',2, simplify = F)
View(air$DepTimeChar)
typeof(scratch2)
str(scratch2)
sapply(air$DepTimeChar, function(x) strsplit(x), "-")
typeof(air$DepTimeChar)
str(air$UniqueCarrier)
