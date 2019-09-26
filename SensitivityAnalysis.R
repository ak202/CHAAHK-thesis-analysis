
setwd("/home/akara/workspace/R/CHAAHK/")
argss = commandArgs(trailingOnly = TRUE)
output <- read.csv(paste("output/", argss, ".csv", sep=""))

params <- c("disturbanceRemovalChance", "costPromotiveRes", "costPromotiveIncRate", "costDemotiveRes", 
	    "costDemotiveIncRate", "fecundityPromotiveRes", "fecundityPromotiveIncRate",
	    "fecundityDemotiveRes", "fecundityDemotiveIncRate")

# as per Saltelli et al 2008 pg. 21, but rounds parameter values in order to "fix" them in place
FirstOrderSI <- function(i.input, i.output, df, partitions) {
	rounded <- (max(df[,i.input])/partitions)*round(df[,i.input]/(max(df[,i.input])/partitions))
	values.fixed  <- unique(rounded)
	mean.fixed <- function(value.fixed) {
		mean(df[rounded == value.fixed,i.output])
	}
	num <- var(sapply(values.fixed, mean.fixed))
	dem <- var(df[,i.output])
	return(round(num/dem,3))
}

MaxPop <- sapply(params, FirstOrderSI, df=output, i.output="MaxPop", partitions=20)
MinPop <- sapply(params, FirstOrderSI, df=output, i.output="MinPop", partitions=20)
FinalPop <- sapply(params, FirstOrderSI, df=output, i.output="FinalPop", partitions=20)
SLG <- sapply(params, FirstOrderSI, df=output, i.output="SustainabilityOrLackOfGrowth", partitions=20)

# Table 5.2
FO.All <- data.frame(MaxPop, MinPop, FinalPop, SLG)
FO.All <- FO.All[ c(1,7,6,9,8,3,2,5,4), ]
Symbol <- c("$Drc$", "$F_{p}i$", "$F_{p}r$", "$F_{d}i$", "$F_{d}r$", "$C_{p}i$", "$C_{p}r$", "$C_{d}i$", "$C_{d}r$")
FO.All <- data.frame(Symbol, FO.All)
rownames(FO.All) <- c("Disturbance Removal Chance", "Fecundity Promotive Increase Rate", "Fecundity Promotive Resilience", "Fecundity Demotive Increase Rate", "Fecundity Demotive Resilience", "Cost Promotive Increase Rate", "Cost Promotive Resilience", "Cost Demotive Increase Rate", "Cost Demotive Resilience")
write.csv(FO.All, paste( argss, "/tables/FOindices.csv", sep=""), quote=FALSE)

