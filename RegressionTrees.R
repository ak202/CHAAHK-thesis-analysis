library(rpart)
library(rpart.plot)

setwd("/home/akara/workspace/R/CHAAHK/")
argss <- commandArgs(trailingOnly = TRUE)
data <- read.csv(paste("output/", argss, ".csv", sep=""))

ctrl <- rpart.control(cp = .0025)

rpart.getr2 <- function(rpartresult) {
  tmp <- printcp(rpartresult)
  rsq.val <- 1-tmp[,c(3,4)]  
  return(rsq.val[nrow(rsq.val),])
}

result.MaxPop <- rpart(MaxPop ~ disturbanceRemovalChance + 
                   fecundityPromotiveIncRate     +           
                   fecundityPromotiveRes         +       
                   fecundityDemotiveIncRate      +
                   fecundityDemotiveRes          +          
                   costPromotiveIncRate          +                 
                   costPromotiveRes              +                 
                   costDemotiveIncRate           +      
                   costDemotiveRes, data = data, control = ctrl)   
importance.MaxPop <- result.MaxPop$variable.importance
importance.MaxPop <- 100/sum(importance.MaxPop)*importance.MaxPop
#importance.max
#rpart.getr2(result.max)

result.MinPop <- rpart(MinPop ~ disturbanceRemovalChance + 
                   fecundityPromotiveIncRate     +           
                   fecundityPromotiveRes         +       
                   fecundityDemotiveIncRate      +
                   fecundityDemotiveRes          +          
                   costPromotiveIncRate          +                 
                   costPromotiveRes              +                 
                   costDemotiveIncRate           +      
                   costDemotiveRes, data = data, control = ctrl)   
importance.MinPop <- result.MinPop$variable.importance
importance.MinPop <- 100/sum(importance.MinPop)*importance.MinPop
#importance.min
#rpart.getr2(result.min)

result.FinalPop <- rpart(FinalPop ~ disturbanceRemovalChance + 
                   fecundityPromotiveIncRate     +           
                   fecundityPromotiveRes         +       
                   fecundityDemotiveIncRate      +
                   fecundityDemotiveRes          +          
                   costPromotiveIncRate          +                 
                   costPromotiveRes              +                 
                   costDemotiveIncRate           +      
                   costDemotiveRes, data = data, control = ctrl)   
importance.FinalPop <- result.FinalPop$variable.importance
importance.FinalPop <- 100/sum(importance.FinalPop)*importance.FinalPop
#importance.fin
#rpart.getr2(result.fin)

result.SLG <- rpart(SustainabilityOrLackOfGrowth ~ disturbanceRemovalChance + 
                   fecundityPromotiveIncRate     +           
                   fecundityPromotiveRes         +       
                   fecundityDemotiveIncRate      +
                   fecundityDemotiveRes          +          
                   costPromotiveIncRate          +                 
                   costPromotiveRes              +                 
                   costDemotiveIncRate           +      
                   costDemotiveRes, data = data, control = ctrl)
importance.SLG <- result.SLG$variable.importance
importance.SLG <- 100/sum(importance.SLG)*importance.SLG
#importance.slg
#rpart.getr2(result.slg)

# Figures 5.14-5.16
png(paste(argss, "/figures/5-13-14-15-16.png", sep = ""), width = 10, height = 8, units = "in", res=300)
rpart.plot(result.SLG, varlen=3, tweak=1.25)
dev.off()

# Table 5.4
importance.all <- data.frame(importance.MaxPop, importance.MinPop, importance.FinalPop, importance.SLG)
importance.all <- round(importance.all, 3)
colnames(importance.all) <- substr(colnames(importance.all), 12,100)
Symbol <- c("$Drc$", "$F_{p}i$", "$F_{p}r$", "$F_{d}i$", "$F_{d}r$", "$C_{p}i$", "$C_{p}r$", "$C_{d}i$", "$C_{d}r$")
importance.all <- data.frame(Symbol, importance.all)
rownames(importance.all) <- c("Disturbance Removal Chance", "Fecundity Promotive Increase Rate", "Fecundity Promotive Resilience", "Fecundity Demotive Increase Rate", "Fecundity Demotive Resilience", "Cost Promotive Increase Rate", "Cost Promotive Resilience", "Cost Demotive Increase Rate", "Cost Demotive Resilience")

write.csv(importance.all, paste(argss, "/tables/TreeImportance.csv", sep = ""), quote=FALSE)

