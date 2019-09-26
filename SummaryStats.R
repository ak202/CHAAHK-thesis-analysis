library(ggplot2)

setwd("/home/akara/workspace/R/CHAAHK/")
argss = commandArgs(trailingOnly = TRUE)
output <- read.csv(paste("output/", argss, ".csv", sep=""))

pops <- c(output$MaxPop, output$MinPop, output$FinalPop)
factors <- factor(c(rep("max", 30000), rep("min", 30000), rep("final", 30000)))
pops <- data.frame(pops, factors)

boxs <- ggplot(pops, aes(y=pops,x=factors)) + geom_boxplot(size=1.3) +
  scale_y_continuous(breaks = c(0,5,10,35,100,350,1000,2000,3000), trans = "log") +
  labs(x = "Census Description (log scale)", y = "Number of Groups") +
  scale_x_discrete(labels = c('Final Count','Maximum Groups','Post-Collapse')) +
  theme(text = element_text(size=20), panel.background = element_rect(fill="grey"))
ggsave(paste(argss, "/figures/5-11.png", sep = ""), width = 9, height = 5.5, units = "in")

histogram <- ggplot(output, aes(x=SustainabilityOrLackOfGrowth)) + geom_histogram(binwidth=.25, fill="orange", linetype=1,
		 color="black")  +
  labs(x = "Sustainability or Lack of Growth", y = "Count") +
  theme(text = element_text(size=20), panel.background = element_rect(fill="grey"))
ggsave(paste(argss, "/figures/5-12.png", sep = ""), width = 9, height = 5.5, units = "in")
