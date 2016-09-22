#Read Files
setwd("C:/Users/jberthet001/Desktop/AiA/Airbnb/AirbnbApp")
cal <- read.csv('calendar.csv')
listing <- read.csv('listings.csv')
review <- read.csv('reviews.csv')
#Save & Read as Rdata file
saveRDS(list, file = "listingRdata.rds")
listing <- readRDS("listingRdata.rds")

#Remove dollar signs and turn into integer
dollarInt <- function(x) {
  as.integer(gsub('\\$', '', x))
}

PercentageInt <- function(x) {
  as.integer(sub("%", "", x))
}

columnsDollarNum <- listing[,c("price", "weekly_price", "monthly_price", "cleaning_fee", "security_deposit", "extra_people")]   #Columns to convert
colsPercentageNum <- listing[,c("host_response_rate", "host_acceptance_rate")]
cleandf1 <- as.data.frame(sapply(columnsDollarInt, dollarInt))
cleandf2 <- as.data.frame(sapply(colsPercentageNum, PercentageInt))

finaldf <- cbind(cleandf1, cleandf2)
#Linear Model. Family = poisson because price has poisson shape
hist(finaldf$price, 1000)
mod1 = glm(price ~ cleaning_fee + weekly_price + monthly_price + security_deposit + extra_people + host_acceptance_rate + host_response_rate, data = finaldf, family = poisson)


