
# Project Title:  Hotel Pricing
# NAME: Nishtha Nayar
# EMAIL: nishthanayar1998@gmail.com
# COLLEGE / COMPANY: Shiv Nadar University 

hotels <- read.csv(paste("Cities42.csv")) #Creating data frame; Step 1
summary(hotels) #Summarising entire data; Step 2

#Taking problem to be as Y = F(x1,x2,x3); Step 3
#As we need to figure out what affects room rent
#**Y= RoomRent**; Step 4

#To figure x1,x2,x3 I will try to eliminate variables to get the best
#Will use corrgrams, correlation matrix and lm model; Step 5

#Plotting corrgram
library(corrgram)
corrgram(hotels, lower.panel = panel.shade, upper.panel = panel.pie, 
         text.panel = panel.txt, main= "Corrgram of hotel variables")

#Building rough model by analysing corrgram

model <- lm(RoomRent ~ + Population + CityRank + IsMetroCity + IsTouristDestination + IsWeekend +StarRating
            +Airport+ HotelPincode + FreeBreakfast + HotelCapacity+ HasSwimmingPool, data= hotels)
summary(model)

data <- hotels[,c(3,4,5,6,7,11,12,13,15,18,19,20)] #analysing correlation for the above variables once
cor(data)

#Modifying model to eliminate variables with high p-value and try to increase R squared value

model <- lm(RoomRent ~ Population+ IsMetroCity + IsTouristDestination +StarRating
            +Airport+  HotelCapacity+ HasSwimmingPool, data= hotels)
summary(model)

#Further eliminating 

model <- lm(RoomRent ~ Population+ + IsTouristDestination +StarRating
            +  HotelCapacity+ HasSwimmingPool, data= hotels)
summary(model)

#Using correlation matrix to further determine relation
data <- hotels[,c(11,3,6,12,19,20)]
cor(data)

#Upon analysis of the model, best R squared value, and correlation matrices, we see that 3 important independent variables are:
# StarRating, HasSwimmingPool, HotelCapacity=x1,x2,x3

#Analysing independent variables; Step 6

boxplot(hotels$RoomRent, main= "Boxplot of Room Rent", ylab="Room Price") #Y= RoomRent
table(hotels$StarRating)
boxplot(hotels$StarRating, main ="Boxplot of Star Rating", ylab="Stars", col="gold")
table(hotels$HasSwimmingPool)
boxplot(hotels$HotelCapacity,main ="Boxplot of Hotel Capacity", ylab="Capacity", col="lavender")

#Analysing through scatter plots; Step 7

plot(hotels$RoomRent, hotels$StarRating, xlab= "Room Rent", ylab = "Star Rating",
     main= "Scatterplot of Room Rent vs Star Rating",
     col= "brown", cex =0.8)

plot(hotels$RoomRent, hotels$HasSwimmingPool, xlab= "Room Rent", ylab = "Has Swimming Pool",
     main= "Scatterplot of Room Rent vs Presence of Swimming Pool",
     col= "black")

plot(hotels$RoomRent, hotels$HotelCapacity, xlab= "Room Rent", ylab = "Hotel Capacity",
     main= "Scatterplot of Room Rent vs Hotel Capacity",
     col= "blue")

#Plotting Corrgram; Step 8

data1 <- hotels[,c(11,12,19,20)]
corrgram(data1, lower.panel = panel.shade, upper.panel = panel.pie, 
         text.panel = panel.txt, main= "Corrgram of hotel variables")

#Variance-Covariance matrix; Step 9

var(data1)
cov(data1)