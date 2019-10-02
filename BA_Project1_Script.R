library(readr)
Users <- read.csv("/Users/devarshipancholi/Desktop/CRT_Users.csv")
View(Users)
Emails <- read.csv("/Users/devarshipancholi/Desktop/CRT_emails.csv")
View(Emails)
str(Users)
str(Emails)

Right_Join <- merge(x=Users, y=Emails, by.x= c("Element.Id"), by.y = c("Element.Id"),all.y=TRUE)
str(Right_Join)
View(Right_Join)
na.omit(Right_Join)
View(Right_Join)

library(dplyr)
any(is.na(Right_Join$Action))
any(is.null(Right_Join$Birth.Place))

