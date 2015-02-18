# Download the file
if (!file.exists("repdata-data-StormData.csv.bz2")){
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, destfile="repdata-data-StormData.csv.bz2")
}
# Load the storm data
if(!exists("data")){
        data<-read.csv("repdata-data-StormData.csv.bz2", header=TRUE, sep=",", na.strings=" ")
}

# Subset the data to get only important variables
sub_data<-data[,c("BGN_DATE", "COUNTY", "COUNTYNAME", "STATE", "EVTYPE", "LENGTH", "WIDTH", "F", "MAG", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "REFNUM")]

# Create a new column with the property and crop damage considering the magnifying factor.
sub_data$PROPDMGEXP<-as.character(sub_data$PROPDMGEXP)
sub_data$PROPDMGEXP[grep("K", sub_data$PROPDMGEXP)] <- "1000"
sub_data$PROPDMGEXP[grep("M", sub_data$PROPDMGEXP)]<-"1000000"
sub_data$PROPDMGEXP[grep("m", sub_data$PROPDMGEXP)]<-"1000000"
sub_data$PROPDMGEXP[grep("B", sub_data$PROPDMGEXP)]<-"1000000000"
othervalues<-!(sub_data$PROPDMGEXP %in% c("1000", "1000000", "1000000000")) == 1
sub_data$PROPDMGEXP[othervalues==1]<-"1"
sub_data$CROPDMGEXP<-as.character(sub_data$CROPDMGEXP)
sub_data$CROPDMGEXP[grep("K", sub_data$CROPDMGEXP)] <- "1000"
sub_data$CROPDMGEXP[grep("M", sub_data$CROPDMGEXP)] <- "1000000"
sub_data$CROPDMGEXP[grep("B", sub_data$CROPDMGEXP)] <- "1000000000"
othervalues<-!(sub_data$CROPDMGEXP %in% c("1000", "1000000", "1000000000")) == 1
sub_data$CROPDMGEXP[othervalues==1]<-"1"
sub_data$PROPDMG<-as.numeric(sub_data$PROPDMG)
sub_data$PROPDMGEXP<-as.numeric(sub_data$PROPDMGEXP)
sub_data$CROPDMG<-as.numeric(sub_data$CROPDMG)
sub_data$CROPDMGEXP<-as.numeric(sub_data$CROPDMGEXP)
sub_data$property<-sub_data$PROPDMG * sub_data$PROPDMGEXP
sub_data$crop<- sub_data$CROPDMG * sub_data$CROPDMGEXP


# population health
sub_data$EVTYPE<-as.character(sub_data$EVTYPE)
sub_data$FATALITIES<-as.numeric(sub_data$FATALITIES)
sub_data$INJURIES<-as.numeric(sub_data$INJURIES)
sub_data$EVTYPE<-as.character(sub_data$EVTYPE)
fatalities<-aggregate(FATALITIES ~ EVTYPE, sub_data, sum)
injuries<-aggregate(INJURIES ~ EVTYPE, sub_data, sum)
sub_data$health<-sub_data$FATALITIES + sub_data$INJURIES
health<-aggregate(health ~ EVTYPE, sub_data, sum)

# Monetary damages (crop), property damages (PROPDMG)
property<-aggregate(property ~ EVTYPE, sub_data, sum)
crop<-aggregate(crop ~ EVTYPE, sub_data, sum)
sub_data$economic<-sub_data$property + sub_data$crop
economic<-aggregate(economic ~ EVTYPE, sub_data, sum)

# To find the events that cause more damages, I will clean the 4 data frames created to cut off the numeric values that are equal zero.
fatalities<-fatalities[!fatalities$FATALITIES == 0, ]
injuries<-injuries[!injuries$INJURIES == 0, ]
property<-property[!property$property == 0, ]
crop<-crop[!crop$crop == 0, ]
health<-health[!health$health == 0, ]
economic<-economic[!economic$economic == 0, ]

# Top 15 causes
fatalities15<-head(fatalities[order(fatalities$FATALITIES, decreasing=T),],15)
injuries15<-head(injuries[order(injuries$INJURIES, decreasing=T),],15)
property15<-head(property[order(property$property, decreasing=T),],15)
crop15<-head(crop[order(crop$crop, decreasing=T),],15)
health15<-head(health[order(health$health, decreasing=T), ], 15)
economic15<-head(economic[order(economic$economic, decreasing=T), ], 15)

# plot the health15 and economic15 graphics
library(ggplot2)
health_graph<-ggplot(health15, aes(x=EVTYPE, y=health))
health_graph<-health_graph + geom_bar(stat="identity") + xlab("Event type") + ylab("Health damage") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(health_graph)

economic_graph<-ggplot(economic15, aes(x=EVTYPE, y=log10(economic)))
economic_graph<-economic_graph + geom_bar(stat="identity") + xlab("Event type") + ylab("Economic damage") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(economic_graph)


