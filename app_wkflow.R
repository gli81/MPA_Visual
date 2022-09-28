
###### Import libraries
#rm(list = ls())
library(knitr)
library(ggplot2)
library(kableExtra)
library(lattice)
library(dplyr)
library(rms) #for VIF
library(MASS)


###### Data
test_data <- read.csv("/Users/godwinanguzu/Documents/projects 2022/Marine_project/Rshiny_test/Rshiny_project1/rshiny_test_data.csv", header= T)
head(test_data)

str(test_data)

###### EDA
#test_data$log_storagehighorlow <- log(test_data$storagehandlinghigh__value_)

table(test_data$storagehandlinghigh__unit_)
table(test_data$storagehandlinghigh__value_)


#levels
test_data$storagehandlinghigh__unit_ <- factor(test_data$storagehandlinghigh__unit_)
table(test_data$storagehandlinghigh__unit_)

#combine some values into one
#religion
test_data$value <- ""
test_data[which(test_data$storagehandlinghigh__value_==35| test_data$storagehandlinghigh__value_==50|test_data$storagehandlinghigh__value_==77 | test_data$storagehandlinghigh__value_==65|test_data$storagehandlinghigh__value_==38|test_data$storagehandlinghigh__value_==40|test_data$storagehandlinghigh__value_==50), 'value'] <-  
test_data[which(test_data$storagehandlinghigh__value_ == 60) , 'value'] <- 60
test_data[which(test_data$storagehandlinghigh__value_ == 70) , 'value'] <- 70
test_data[which(test_data$storagehandlinghigh__value_ == 75) , 'value'] <- 75
test_data[which(test_data$storagehandlinghigh__value_ == 80) , 'value'] <- 80
test_data[which(test_data$storagehandlinghigh__value_ == 85) , 'value'] <- 85
test_data[which(test_data$storagehandlinghigh__value_ == 90) , 'value'] <- 90
test_data[which(test_data$storagehandlinghigh__value_ == 93) , 'value'] <- 93
test_data$value <- factor(test_data$value) 
table(test_data$value)

#combine
test_data$category <- ""
test_data[which(test_data$storagehandlinghigh__unit_ == "Degrees Celsius" | test_data$storagehandlinghigh__unit_ == "Degrees Fahrenheit") , 'category'] <- "1"
test_data[which(test_data$storagehandlinghigh__unit_=="Percent (%) Relative Humidity"), 'category'] <- "2"
test_data$category <- factor(test_data$category,levels=c(1,2),labels=c("celcius/fahrenheit","humidity"))
table(test_data$category)


#Drawing simple bar graph
p<-ggplot(data=test_data, aes(x=category, y=value)) +
  geom_bar(stat="identity")
p

write.csv(test_data, "/Users/godwinanguzu/Documents/projects 2022/Marine_project/Rshiny_test/Rshiny_project1/df.csv")




# Horizontal bar plot
#p + coord_flip()


###Create R-shiny app

# Modified from https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/














