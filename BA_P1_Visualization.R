data <- read.csv("/Users/devarshipancholi/Desktop/join_data.csv")
str(data)
View(data)

unique(data$Element.Id) #59344 unique students
unique(data$Id) #822678 unique emails

# On an average there are 13 emails per student!

# Mails sent to females V males.

library(dplyr)
library(ggplot2)
data.Grp <- group_by(data, Element.Id, Gender) 
data.SUM <- summarise(data.Grp, Freq= n())
data.SUMF <- top_n(data.SUM, n= 2)
ggplot(data.SUMF,aes(x = Gender , y = data.SUMF$'Freq')) + geom_bar(stat = "identity") + theme_classic() +labs(x = "Gender",y = "Emails sent", title = paste("Fatal V Non Fatal Accidents") )

# Mails sent to different age groups

Current <- 2019
data$'Age' <- Current - as.numeric(data$Date.Of.Birth)

library(lubridate)
data$time <- as.POSIXct(strptime(data$Date.Of.Birth,"%m/%d/%Y %H:%M"))
data$year <- as.factor(year(data$time))

data$'AgeF' <- 119 - data$year
View(data)

drop(data$Age)
drop(data$AgeF)

data$DOB <- as.Date(as.character(data$Date.Of.Birth), format = "%d/%m/%y")
data$Year <- as.factor(year(data$DOB))
Current <-format(Sys.time(),format='%Y')
data$Age <- as.numeric(Current) - as.numeric(data$Year)
data$AgeF <- data$Age - 1968


View(data)

data$Age <- NULL
data$Year <- NULL
data$DOB <- NULL

data.Grp3 <- group_by(data, Action ) 
data.Sum3 <- summarise(data.Grp3, Freq3= n())
#data.SUMF <- top_n(data.SUM, n= 2)
ggplot(data.Sum3,aes(x = Action , y = Freq3)) + geom_bar(stat = "identity") + theme_classic() + labs(x = "Types of Actions",y = "Number of Emails", title = paste("") ) +theme_bw()


dataframe <- read.csv("/Users/devarshipancholi/Desktop/join_data.csv", strip.white = TRUE)
View(dataframe)

data.usa <- dataframe[dataframe$Citizenship.Coutry == "USA" & dataframe$Prospects == "1" & dataframe$Admit == "1",]
# head(data.usa, 10)
Domestic <- length(data.usa)
View(Domestic)

library(dplyr)
library(ggplot2)
library(data.table)
data.grp0 <- subset(dataframe, Citizenship.Coutry != "")
data.grp1 <- group_by(data.grp0, Citizenship.Coutry)
#View(data.grp1)
data.sum1 <- summarise(data.grp1, Freq1= n())
data.sumf1 <- top_n(data.sum1, n= 15)
na.exclude(ggplot(data.sumf1,aes(x = Citizenship.Coutry , y = Freq1)) + geom_bar(stat = "identity") + theme_classic() + labs(x = "Countries",y = "Emails Interactions", title = paste("") ) + theme_bw())

data.grp2 <- group_by(dataframe, Element.Id)
#View(data.grp1)
data.sum2 <- summarise(data.grp2, Freq2= n())
#data.sumf1 <- top_n(data.sum1, n= 15)



na.exclude(ggplot(data.sum2, aes(x = Element.Id , y = Freq2)) + geom_bar(stat = "identity") + theme_classic() + labs(x = "Countries",y = "Emails Sent", title = paste("") ) + theme_bw())


d2 <- summary(dataframe$Action)
View(d2)

open_rate <- 60
click_rate <- 11
click_to_open <- 18

x <- data.frame("Rate" = c("Open Rate", "Click Rate", "Click to Open"), "Values"= c("60", "11", "18"))
str(x)
View(x)
library(ggplot2)

ggplot(x,aes(x= Rate ,y= Values, fill= Rate)) + geom_bar(stat="identity",position="dodge", width = 0.4) + coord_flip() + theme_bw() + theme(axis.text.y = element_blank())



installed.packages("janitor")
install.packages("janitor")
library(janitor)
library(sqldf)

df1 <- sqldf("select \"Element.Id\" as Element_id, Action, count(Action) as 'Number_of_Emails_Delivered' from dataframe where Action='emailDelivered' group by \"Element.Id\", Action")
View(df1)
str(df1)

sqldf("select Avg(Number_of_Emails_Delivered) from df1")

ggplot(data =df1, aes(x=Element_id, y=Number_of_Emails_Delivered)) + geom_point()

df2 <- aggregate(df1$Element_id, by = list(df1$Number_of_Emails_Delivered), FUN = length)
df3 <- df2[1:32, ]

ggplot(df3, aes(x = Group.1, y = x)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(label = x), vjust = -0.3, size = 3.5) +
  ggtitle("Average Number of Email Delivered") +
  xlab("Number of Email Delivered") +
  ylab("Number oF Student")






