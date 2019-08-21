# Analysis of Airline Ticket Pricing
# NAME: Nishtha Nayar
# EMAIL: nishthanayar1998@gmail.com
# COLLEGE: Shiv Nadar University
airlines.df <- read.csv(paste("SixAirlinesData.csv")) #Creating data frame
summary(airlines.df) #Getting summary of data
table(airlines.df$Airline) #Getting information about the airline companies
table(airlines.df$Airline,airlines.df$Aircraft) #Aircrafts with each airlines
boxplot(airlines.df$FlightDuration, xlab = "All airlines", ylab= "Duration", main= "Boxplot of flight duration") #Boxplot of flight duration
table(airlines.df$TravelMonth) #Getting information about travel month
table(airlines.df$IsInternational) #Getting information about international/domestic

#Graph to see number of Economy and Premium Seats
library(lattice)
barchart(airlines.df$SeatsEconomy + airlines.df$SeatsPremium ~ airlines.df$Airline, main= "Distribution of seats", col = c("blue", "pink"),
          xlab = "Airline", ylab= "Seats")
legend("topright", legend =  c("Economy", "Premium"), fill= c("blue", "pink")) 

#Distribution of Pitch in Economy for different airlines
barchart(airlines.df$PitchEconomy ~ airlines.df$Airline,
         main="Pitch in Economy",
         xlab= "Airlines", ylab="Pitch in Economy"
         ) 

#Distribution of Pitch in Premium for different airlines
barchart(airlines.df$PitchPremium ~ airlines.df$Airline,
         main="Pitch in Premium",
         xlab= "Airlines", ylab="Pitch in Premium"
) 

#Distribution of Width for different airlines
barchart(airlines.df$WidthEconomy + airlines.df$WidthPremium ~ airlines.df$Airline,
         main="Width in Different Airlines",
         xlab= "Airlines", ylab="Width", col= c("red", "blue")) 
legend( "topleft", legend=c("Economy", "Premium"), fill = c("red", "blue"))


#Distribution of price for different airlines
barchart(airlines.df$PriceEconomy + airlines.df$PricePremium ~ airlines.df$Airline,
         main="Price in Different Airlines",
         xlab= "Airlines", ylab="Price", col= c("green", "yellow")) 
legend( "topleft", legend=c("Economy", "Premium"), fill = c("green", "yellow"))


#Boxplot to compare prices of premium and economy
boxplot(airlines.df$PriceEconomy,airlines.df$PricePremium, main="Prices for 2 categories"
        , xlab= "Category", ylab="Price")
axis(side=1, at=c(1,2),labels=c("Economy", "Premium")) 

#Relative price distribution
boxplot(airlines.df$PriceRelative ~ airlines.df$Airline, main="Relative price",
         ylab="Relative Price", xlab="Airline") 

#Distrubution of total no. of seats
boxplot(airlines.df$SeatsTotal, main="Total seats distribution") 

#Distribution of Fraction Premium seats for airlines
boxplot(airlines.df$FractionPremiumSeats ~ airlines.df$Airline, main="Fraction of Premium Seats in Airlines",
        ylab="Fraction", xlab="Airline") 


#Distribution of Pitch Difference
barchart(airlines.df$PitchDifference ~ airlines.df$Airline,
         xlab= "Airline", ylab= "Pitch Difference", main ="Pitch difference in Airlines")


#Distribution of Width Difference
barchart(airlines.df$WidthDifference ~ airlines.df$Airline,
         xlab= "Airline", ylab= "Width Difference", main ="Width difference in Airlines")

#Creating corrgram of data set
library(corrgram)
corrgram(airlines.df, lower.panel = panel.shade, upper.panel = panel.pie, 
         text.panel = panel.txt, main= "Corrgram of Airlines variables")

#Creating correlation matrix
air_data <- airlines.df[, 6:18]
cor(air_data)


#Creating covariance matrix
cov(air_data)

#Creating Scatterplots 
plot(airlines.df$PriceEconomy, airlines.df$PitchDifference,
      xlab= "Price of Economy Seat", ylab = "Pitch Difference",
     main = "Relation between Pitch Diff and Price of Economy Seat")

plot(airlines.df$PriceEconomy, airlines.df$WidthDifference,
     xlab="Price of Economy Seat", ylab = "Width Difference",
     main= "Relation between Width Diff and Price of Economy Seat",
     col= "purple")

plot(airlines.df$PricePremium, airlines.df$PitchDifference,
     xlab= "Price of Premium Seat", ylab = "Pitch Difference",
     main = "Relation between Pitch Diff and Price of Premium Seat",
     col="blue")

plot(airlines.df$PricePremium, airlines.df$WidthDifference,
     xlab= "Price of Premium Seat", ylab = "Width Difference",
     main = "Relation between Width Diff and Price of Premium Seat",
     col="red")

plot(airlines.df$PriceRelative, airlines.df$PitchDifference,
     xlab= "Relative Price", ylab = "Pitch Difference",
     main = "Relation between Pitch Diff and Relative Price",
     col="blue")

plot(airlines.df$PriceRelative, airlines.df$WidthDifference,
     xlab= "Relative Price", ylab = "Width Difference",
     main = "Relation between Width Diff and Relative Price",
     col="cyan")

#Performing t-tests
#Null hypothesis= Relative price and Pitch Difference have no relation
t.test(airlines.df$PriceRelative, airlines.df$PitchDifference)

#Null hypothesis: Relative price and Witdh Difference have no relation
t.test(airlines.df$PriceRelative, airlines.df$WidthDifference)


#Forming Regression Model
model <- lm(PriceRelative ~ PitchDifference + WidthDifference + PitchEconomy +WidthEconomy + PitchPremium + WidthPremium+PriceEconomy+FlightDuration+PriceEconomy, data=airlines.df)
summary(model)
coef(model)