#Read Files
setwd("C:/Users/jberthet001/Desktop/AiA/Airbnb/AirbnbApp")
cal <- read.csv('calendar.csv')
listing <- read.csv('listings.csv')
review <- read.csv('reviews.csv')
#Save & Read as Rdata file
# saveRDS(list, file = "listingRdata.rds")
# listing <- readRDS("listingRdata.rds")

#Remove dollar signs and turn into integer
dollarInt <- function(x) {
  as.integer(gsub('\\$', '', x))
}

PercentageInt <- function(x) {
  as.integer(sub("%", "", x))/100
}

#Data must be data.frame
columnsDollarInt <- listing[,c("price", "weekly_price", "monthly_price", "cleaning_fee", "security_deposit", "extra_people")]   #Columns to convert
colsPercentageInt <- listing[,c("host_response_rate", "host_acceptance_rate")]
cleandf1 <- as.data.frame(sapply(columnsDollarInt, dollarInt))
cleandf2 <- as.data.frame(sapply(colsPercentageInt, PercentageInt))

finalDF <- cbind(cleandf1, cleandf2)

#Function to replace all the old columns w/ cleaned ones
for (i in names(finalDF)) {      #replace w/ newest table
  listing[i] <- finalDF[i]
}

list2 <- cbind(listing, east_northing)
names(list2)


#Linear Model. Family = poisson because price has poisson shape
hist(listing$price, 1000)
mod1 = glm(price ~ cleaning_fee
           + weekly_price
           + monthly_price
           + security_deposit
           + extra_people
           + host_acceptance_rate
           + host_response_rate,
           data = listing, family = poisson)
#linear model summary: modeling price: cleaning_fee and weekly_price closely associated with price. Weekly & monthly prices doesn't explain price well. A positive host_response_rate is associated with price to a slight degree.
plotSummary(mod1)

test2.2.2

