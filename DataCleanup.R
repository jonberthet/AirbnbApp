---
  title: "R Notebook"
output: html_notebook
---

  This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*.
Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.
When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

```{r}
#Read Files
cal <- read.csv('calendar.csv')
list <- read.csv('listings.csv')
review <- read.csv('reviews.csv')
```

```{r}
#Remove dollar signs and turn into integer
columns = list[,c("price", "weekly_price")]
dollarInt <- function(x) {
  as.integer(gsub('\\$', '', x))
}
```

dollarInt(list$price)
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


