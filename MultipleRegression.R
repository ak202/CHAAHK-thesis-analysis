setwd("/home/akara/workspace/R/CHAAHK/")
argss = commandArgs(trailingOnly = TRUE)
data <- read.csv(paste("output/", argss, ".csv", sep=""))

fit.MaxPop <- lm(MaxPop ~ disturbanceRemovalChance + 
                   fecundityPromotiveIncRate     +           
                   fecundityPromotiveRes         +       
                   fecundityDemotiveIncRate      +
                   fecundityDemotiveRes          +          
                   costPromotiveIncRate          +                 
                   costPromotiveRes              +                 
                   costDemotiveIncRate           +      
                   costDemotiveRes, data = data)   
tval.MaxPop <- summary(fit.MaxPop)$coef[,"t value"]
slope.MaxPop <- summary(fit.MaxPop)$coef[,"Estimate"]
Rsquare.MaxPop <- summary(fit.MaxPop)$r.squared

fit.MinPop <- lm(MinPop ~ disturbanceRemovalChance + 
                   fecundityPromotiveIncRate     +           
                   fecundityPromotiveRes         +       
                   fecundityDemotiveIncRate      +
                   fecundityDemotiveRes          +          
                   costPromotiveIncRate          +                 
                   costPromotiveRes              +                 
                   costDemotiveIncRate           +      
                   costDemotiveRes, data = data)   
tval.MinPop <- summary(fit.MinPop)$coef[,"t value"]
slope.MinPop <- summary(fit.MinPop)$coef[,"Estimate"]
Rsquare.MinPop <- summary(fit.MinPop)$r.squared


fit.FinalPop <- lm(FinalPop ~ disturbanceRemovalChance + 
                   fecundityPromotiveIncRate     +           
                   fecundityPromotiveRes         +       
                   fecundityDemotiveIncRate      +
                   fecundityDemotiveRes          +          
                   costPromotiveIncRate          +                 
                   costPromotiveRes              +                 
                   costDemotiveIncRate           +      
                   costDemotiveRes, data = data)   
tval.FinalPop <- summary(fit.FinalPop)$coef[,"t value"]
slope.FinalPop <- summary(fit.FinalPop)$coef[,"Estimate"]
Rsquare.FinalPop <- summary(fit.FinalPop)$r.squared

fit.SLG <- lm(SustainabilityOrLackOfGrowth ~ disturbanceRemovalChance + 
                   fecundityPromotiveIncRate     +           
                   fecundityPromotiveRes         +       
                   fecundityDemotiveIncRate      +
                   fecundityDemotiveRes          +          
                   costPromotiveIncRate          +                 
                   costPromotiveRes              +                 
                   costDemotiveIncRate           +      
                   costDemotiveRes, data = data)   
tval.SLG <- summary(fit.SLG)$coef[,"t value"]
slope.SLG <- summary(fit.SLG)$coef[,"Estimate"]
Rsquare.SLG <- summary(fit.SLG)$r.squared

rnames <- c("Disturbance Removal Chance", "Fecundity Promotive Increase Rate", "Fecundity Promotive Resilience", "Fecundity Demotive Increase Rate", "Fecundity Demotive Resilience", "Cost Promotive Increase Rate", "Cost Promotive Resilience", "Cost Demotive Increase Rate", "Cost Demotive Resilience")

tval.all <- data.frame(tval.MaxPop, tval.MinPop, tval.FinalPop, tval.SLG)
tval.all <- round(tval.all, 3)
colnames(tval.all) <- substr(colnames(tval.all), 6,100)
rownames(tval.all) <- c("Intercept", rnames)
Symbol <- c("","$Drc$", "$F_{p}i$", "$F_{p}r$", "$F_{d}i$", "$F_{d}r$", "$C_{p}i$", "$C_{p}r$", "$C_{d}i$", "$C_{d}r$")
tval.all <- data.frame(Symbol, tval.all)
write.csv(tval.all, paste(argss, "/tables/tvals.csv", sep=""), quote=FALSE)

slope.all <- data.frame(slope.MaxPop, slope.MinPop, slope.FinalPop, slope.SLG)
slope.all <- round(slope.all, 3)
colnames(slope.all) <- substr(colnames(slope.all), 7,100)
rownames(slope.all) <- c("Intercept", rnames)
Symbol <- c("","$Drc$", "$F_{p}i$", "$F_{p}r$", "$F_{d}i$", "$F_{d}r$", "$C_{p}i$", "$C_{p}r$", "$C_{d}i$", "$C_{d}r$")
slope.all <- data.frame(Symbol, slope.all)
write.csv(slope.all, paste(argss, "/tables/slopes.csv", sep=""), quote=FALSE)

Rsquare.all  <- t(data.frame(Rsquare.MaxPop, Rsquare.MinPop, Rsquare.FinalPop, Rsquare.SLG))
Rsquare.all <- round(Rsquare.all, 3)
colnames(Rsquare.all) <- "$R^2$"
rownames(Rsquare.all) <- substr(rownames(Rsquare.all), 9,100)
write.csv(Rsquare.all, paste(argss, "/tables/Rsquares.csv", sep=""), quote=FALSE)

