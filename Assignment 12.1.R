x<-read.csv("C:\\Users\\Celebrity\\Desktop\\Data Analytics File\\Acadgild\\Chicago_Crimes_2012_to_2017.csv")
View(x)
names(x)
str(x)
a<-na.omit(x)
names(a)
b<-cor(a[c(11,12,13,14,18,20,21)])
library(reshape2)
m <- melt(b)
library(dplyr)
m
top <- m%>%select(Var1, Var2, value)%>%filter(value != 1)
top[order(top$value, decreasing = T)[1:10],]
# District~Beat, Ward~District, Ward~Beat, Latitude ~Ward, Latitude~District are top5 attributes with highest correlation
x <- as.data.frame(table(Crimes$Description))
x[order(x$Freq, decreasing = T)[1:3],]
# $500 and Under and  Domestic Battery Simple are the top 3 reasons for having more crime

# Which all attributes have correlation with crime rate?

# The data we have are at a crime incident level, that is, for each recorded each crime incident
# Hence we need to transform it to workable model
crime <- Crimes
head(crime)
table(is.na(crime))

crime$Date <- as.POSIXlt(crime$Date, format= "%m/%d/%Y %H:%M:%S")
crime$`Updated On` <- as.POSIXlt(crime$`Updated On`, format= "%m/%d/%Y %H:%M:%S")

#install.packages("chron")
library(chron)

crime$Time <- time(format(crime$Date,"%H:%M:%S"))
crime$Date <- as.POSIXct(crime$Date)
crime$`Updated On` <- as.POSIXct(crime$`Updated On`)


