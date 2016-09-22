#Read Files
setwd("C:/Users/jberthet001/Desktop/AiA/Airbnb/AirbnbApp")
cal <- read.csv('calendar.csv')
list <- read.csv('listings.csv')
review <- read.csv('reviews.csv')

#Remove dollar signs and turn into integer
dollarInt <- function(x) {
  as.integer(gsub('\\$', '', x))
}

columnsDollarInt <- list[,c("price", "weekly_price", "monthly_price", "cleaning_fee", "security_deposit", "extra_people")]

newdf = []
for (col in columnsDollarInt) {
  # newdf$col <- dollarInt(col)
  newdf$col <- as.data.frame(dollarInt(col))
}
head(newdf)


list$price <- as.integer(gsub('\\$', '', list$price))
list$weekly_price <- as.integer(gsub('\\$', '', list$weekly_price))
list$monthly_price <- as.integer(gsub('\\$', '', list$monthly_price))
list$cleaning_fee <- as.integer(gsub('\\$', '', list$cleaning_fee))
list$security_deposit <- as.integer(gsub('\\$', '', list$security_deposit))
list$extra_people <- as.integer(gsub('\\$', '', list$extra_people))
columns = list[,c("price", "weekly_price")]
dollarInt(columns)
```


```{r}
#Linear Model
mod1 = glm(price ~ review_scores_rating + square_feet, data = list, family = gaussian)
```

str(list$price)
str(list$review_scores_rating)
list_mod1 <- list[,c("price", "review_scores_rating", "square_feet")]
dim(complete.cases(list_mod1))

Testing


