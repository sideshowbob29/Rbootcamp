#trying to extract 5th row from airline dataset 
air5throw <- air[5,]

#extract last row from air
airlastrow <- air[nrow(air),]

#count the number of NAs in the departure delay field of air
sum(is.na(air$DepDelay))
table(is.na(air$DepDelay))

#set all the extreme delays (>300) to NA
air$DepDelayCens <- air$DepDelay
air$DepDelayCens[air$DepDelayCens > 300] <- NA

#how to concatenate a couple fields into a single new field
air$UniqueCarrier[1]
air$FlightNum[1]
paste(air$UniqueCarrier[1],"-",air$FlightNum[1])

#tells user how many rows are in any dataset
noros <- nrow(air)
paste("There are", noros, "rows in the dataset")

#creating a new vector that identifies rows with carrier WN and distances <1000
# then summarizing how many rows meet these conditions
air2$dummy <- air2$UniqueCarrier == 'WN' & air2$Distance<1000
table(air2$dummy)

#creating new subset dataframe only with rows we identified in previous step
air2subset <- subset(air2, dummy=='TRUE')
air2subset

names(air)
table(air$Dest)
class(air$Dest)

#converting character to factor
air3$Dest <- as.factor(air3$Dest)
class(air3$Dest)

library(plyr)
#creating new dataframe that is the count of destination locations
Dest_count <- count(air3$Dest)
print(levels(air3$Dest))

#re-ordering the levels of the destinations based on frequency of count
Dest_count$x<-factor(Dest_count$x, levels=Dest_count$x[order(Dest_count$freq, decreasing=TRUE)])
print(levels(Dest_count$x))
#re-ording the Dest field in my air dataset
air3$Dest<- factor(air3$Dest, levels = levels(Dest_count$x))                 
levels(air3$Dest)
#subsetting my table for only January 2005
air3subset <-subset(air3, Year == 2005 & Month == 1)
#boxplot of Departure Delay by destination
boxplot(DepDelay~Dest, data=air3subset)

#creating row names using a concatenation of several collumns; get an error if row names
#are not unique
rownames(air) <-paste(air$Year, air$Month, air$DayofMonth, air$UniqueCarrier, air$TaxiIn, air$FlightNum, sep="_")
