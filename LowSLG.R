
library(ggplot2)

setwd("/home/akara/workspace/R/CHAAHK/")
argss = commandArgs(trailingOnly = TRUE)
output <- read.csv(paste("output/", argss, ".csv", sep=""))
lowSLG <- output[output$SustainabilityOrLackOfGrowth < .5,]

makePlots <- function(x) {
  p <- ggplot(lowSLG, aes_string(x=x[1])) + 
    geom_histogram(bins=15, fill="blue", linetype=1, color="black")  +
    theme(text = element_text(size=20), panel.background = element_rect(fill="grey"))
  ggsave(paste(argss, "/figures/5-", x[2], ".png", sep = ""), width = 9, height = 5.5, units = "in")
  return(p)
}

params <- c("disturbanceRemovalChance", "costPromotiveRes", "costPromotiveIncRate", "costDemotiveRes", 
	    "costDemotiveIncRate", "fecundityPromotiveRes", "fecundityPromotiveIncRate",
	    "fecundityDemotiveRes", "fecundityDemotiveIncRate")

numbers <- c(18, 24, 23, 26, 25, 20, 19, 22, 21)
params.numbered <- cbind(params, numbers)
apply(params.numbered,1,makePlots)
