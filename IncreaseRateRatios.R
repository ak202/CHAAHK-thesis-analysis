library(ggplot2)

setwd("/home/akara/workspace/R/CHAAHK/")
argss = commandArgs(trailingOnly = TRUE)
output <- read.csv(paste("output/", argss, ".csv", sep=""))

costratio <- output$costPromotiveIncRate/output$costDemotiveIncRate
fecundityratio <- output$fecundityPromotiveIncRate/output$fecundityDemotiveIncRate
output <- data.frame(output, costratio, fecundityratio)

# 5.27
IRRatios.max <- ggplot() + geom_point(aes(x=costratio, y = MaxPop, color=fecundityratio), data=output, shape=1) +
  scale_x_continuous(breaks = c(.0001, .001, .01,0.1,1,10,100,1000,10000), trans="log") +
  scale_y_continuous(breaks = c(0,5,10,35,100,350,1000,2000,3000), trans = "log") +
  scale_colour_gradient(breaks = c(.0001, 0.01,1,100,10000), low = "yellow", high = "blue", trans="log", limits = c(.0001,10000)) +
  labs(x = expression("C"["p"]*"l/C"["d"]*"l"), y = "MaxPop", color = expression("F"["p"]*"l/F"["d"]*"l")) +
  theme(text = element_text(size=20), panel.background = element_rect(fill="grey"))
ggsave(paste(argss, "/figures/5-27.png", sep = ""), width = 9, height = 5.5, units = "in")

# 5.28
IRRatios.min <- ggplot() + geom_point(aes(x=costratio, y = MinPop, color=fecundityratio), data=output, shape=1) +
  scale_x_continuous(breaks = c(.0001, .001, .01,0.1,1,10,100,1000,10000), trans="log") +
  scale_y_continuous(breaks = c(0,5,10,35,100,350,1000,2000,3000), trans = "log") +
  scale_colour_gradient(breaks = c(.0001, 0.01,1,100,10000),low = "yellow", high = "blue", trans="log", limits = c(.0001,10000)) +
  labs(x = expression("C"["p"]*"l/C"["d"]*"l"), y = "MinPop", color = expression("F"["p"]*"l/F"["d"]*"l")) +
  theme(text = element_text(size=20), panel.background = element_rect(fill="grey"))
ggsave(paste(argss, "/figures/5-28.png", sep = ""), width = 9, height = 5.5, units = "in")

# 5.29
IRRatios.fin <- ggplot() + geom_point(aes(x=costratio, y = FinalPop, color=fecundityratio), data=output, shape=1) +
  scale_x_continuous(breaks = c(.0001, .001, .01,0.1,1,10,100,1000,10000), trans="log") +
  scale_y_continuous(breaks = c(0,5,10,35,100,350,1000,2000,3000), trans = "log") +
  scale_colour_gradient(breaks = c(.0001, 0.01,1,100,10000),low = "yellow", high = "blue", trans="log", limits = c(.0001,10000)) +
  labs(x = expression("C"["p"]*"l/C"["d"]*"l"), y = "FinalPop", color = expression("F"["p"]*"l/F"["d"]*"l")) +
  theme(text = element_text(size=20), panel.background = element_rect(fill="grey"))
ggsave(paste(argss, "/figures/5-29.png", sep = ""), width = 9, height = 5.5, units = "in")

# 5.30
IRRatios.slg <- ggplot() + geom_point(aes(x=costratio, y = SustainabilityOrLackOfGrowth, color=fecundityratio), data=output, shape=1) +
  scale_x_continuous(breaks = c(.0001, .001, .01,0.1,1,10,100,1000,10000), trans="log") +
  scale_y_continuous(breaks = c(0,1,2,3,4)) +
  scale_colour_gradient(breaks = c(.0001, 0.01,1,100,10000),low = "yellow", high = "blue", trans="log", limits = c(.0001,10000)) +
  labs(x = expression("C"["p"]*"l/C"["d"]*"l"), y = "SLG", color = expression("F"["p"]*"l/F"["d"]*"l")) +
  theme(text = element_text(size=20), panel.background = element_rect(fill="grey"))
ggsave(paste(argss, "/figures/5-30.png", sep = ""), width = 9, height = 5.5, units = "in")

