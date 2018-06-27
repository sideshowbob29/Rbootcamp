search()
getwd() 
cpds2 <- read.csv(file.path('..', 'rbootcamplessons', 'cpds.csv'))
head(cpds)
setwd('C:\\Users\\Your_username\\Desktop\\r-bootcamp-2016')

#this package can read stata and other formats
library(foreign)
vote <- read.dta(file.path('..', 'data', '2004_labeled_processed_race.dta'))
head(vote)
library(help = foreign)

#how to save something to a pdf
pdf('myplot.pdf', width = 7, height = 7)
x <- rnorm(10); y <- rnorm(10)
plot(x, y)
dev.off()

#saving for a Latex or html format
require(xtable)
print(xtable(table(air$UniqueCarrier, air$Cancelled)), type = "html")


install.packages('lmtest') 
getwd()
setwd("C:/Users/jacob/Documents/r-bootcamp-2016-master/r-bootcamp-2016-master/rbootcamplessons")

#saving as a simple text file, no row names, no quotes around character strings
?write.table()
write.table(cpds2,"cpds2", quote = FALSE, row.names = FALSE)

head(air)

pdf('airlineplot.pdf', width = 25, height = 25)
x <- air$ArrDelay; y <- air$DepDelay
plot(x, y)
dev.off()

.libPaths('fields')
install.packages('fields')
library(spam)
install.packages('spam')

hello
