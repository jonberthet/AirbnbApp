#Prints all 4 model plots in 1 screen
plotSummary <- function(model){
  par(mfrow=c(2,2))
  plot(model)
  par(mfrow=c(1,1))
}

#Turns $x.xx into integer
dollarInt <- function(x) {
  as.integer(gsub('\\$', '', x))
}

#Turns xx% into integer
PercentageInt <- function(x) {
  as.integer(sub("%", "", x))/100
}
