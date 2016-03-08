# Project
# Packages
library (ggplot2)
library (psych)
library (Rmisc)
library (sem)
library (psych)
library (car)
library (gvlma)
library (sandwich)
library (msm)
library (leaps)
library (MASS)
library (rpart)
library (party)
library (ran)
library (tree)

# Load dataset
setwd ("C:/Users/Victor/Dropbox/NEU/Project/Dissertation")
# setwd ("C:/Users/nogueiracortezgome.v/Dropbox/NEU/Project/Dissertation")

Data <- read.csv("Data.csv")
row.names(Data) <- Data$Airport
Data$Airport <- NULL
attach (Data)
summary(Data)

# Information about the variables
plot(Data)

png(filename = "HUB_relationships.png",width = 1200, height = 1200, units = "px", pointsize = 30, bg = "white", res = NA, restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"))
par(mfrow = c(4,4))
plot(NPAS,HUB)
plot(AREA,HUB)
plot(CPOP,HUB)
plot(CSIZ,HUB)
plot(GDP,HUB)
plot(M50,HUB)
plot(M100,HUB)
plot(M200,HUB)
plot(AIO,HUB)
plot(TQUAL,HUB)
plot(NRUN,HUB)
plot(NAIRL,HUB)
plot(OPER,HUB)
plot(NDES,HUB)
plot(IDES,HUB)
plot(NTER,HUB)
dev.off()

png(filename = "NPAS_relationships.png",width = 1200, height = 1200, units = "px", pointsize = 30, bg = "white", res = NA, restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"))
par(mfrow = c(4,4))
plot(HUB,NPAS)
plot(AREAIR,NPAS)
plot(CPOP,NPAS)
plot(CITYA,NPAS)
plot(GDP,NPAS)
plot(M50,NPAS)
plot(M100,NPAS)
plot(M200,NPAS)
plot(AIO,NPAS)               
plot(ACCON,NPAS)             
plot(NRUN,NPAS)              
plot(NAIRL,NPAS)   
plot(OPER,NPAS)
plot(NDES,NPAS)
plot(IDES,NPAS)
plot(NTER,NPAS)
dev.off()

png(filename = "NPAS_relationships with line.png",width = 2400, height = 2700, units = "px", pointsize = 35, bg = "white", res = NA, restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"))
par(mfrow = c(5,4))
plot(NPAS~HUB,data=Data);abline(NPASHUB,col="red")
plot(NPAS~NHUB,data=Data);abline(NPASNHUB,col="red")
plot(NPAS~AREAIR,data=Data);abline(NPASAREAIR,col="red")
plot(NPAS~CPOP,data=Data);abline(NPASCPOP,col="red")
plot(NPAS~CITYA,data=Data);abline(NPASCITYA,col="red")
plot(NPAS~GDP,data=Data);abline(NPASGDP,col="red")
plot(NPAS~M50,data=Data);abline(NPASM50,col="red")
plot(NPAS~M100,data=Data);abline(NPASM100,col="red")
plot(NPAS~M200,data=Data);abline(NPASM200,col="red")
plot(NPAS~AIO,data=Data);abline(NPASAIO,col="red")
plot(NPAS~ACCON,data=Data);abline(NPASACCON,col="red")
plot(NPAS~NRUN,data=Data);abline(NPASNRUN,col="red")
plot(NPAS~NAIRL,data=Data);abline(NPASNAIRL,col="red")
plot(NPAS~OPER,data=Data);abline(NPASOPER,col="red")
plot(NPAS~NDES,data=Data);abline(NPASNDES,col="red")
plot(NPAS~IDES,data=Data);abline(NPASIDES,col="red")
plot(NPAS~NTER,data=Data);abline(NPASNTER,col="red")
dev.off()

png(filename = "TQUAL_relationships.png",width = 1200, height = 1200, units = "px", pointsize = 30, bg = "white", res = NA, restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"))
par(mfrow = c(4,4))
plot(HUB,TQUAL)
plot(NPAS,TQUAL)
plot(AREA,TQUAL)
plot(CPOP,TQUAL)
plot(CSIZ,TQUAL)
plot(GDP,TQUAL)
plot(M50,TQUAL)
plot(M100,TQUAL)
plot(M200,TQUAL)
plot(AIO,TQUAL)
plot(NRUN,TQUAL)
plot(NAIR,TQUAL)
plot(OPER,TQUAL)
plot(NDES,TQUAL)
plot(IDES,TQUAL)
plot(NTER,TQUAL)
dev.off()	

# Plot of Regression of City Population on GDP
plot(GDP,CPOP)
abline(lm(CPOP~GDP))
title("Regression of City Population on GDP")

# Plot of Airport-city connectivity vs. Number of daily operations with names
plot(OPER,ACCON, main = "Airport-city connectivity vs. Number of daily operations",
     xlab = "Number of daily operations", ylab="Airport-city connectivity",pch=18, col="blue")
text(OPER, ACCON, row.names(Data),cex=0.6,pos=4,col="red")

# Histograms
p1 	<-	qplot(NPAS,binwidth = 10000000)+ggtitle("Number of passengers")
p2	<-	qplot(AREAIR,binwidth = 1000)+ggtitle("Area covered by airport")
p3	<-	qplot(CPOP,binwidth = 1000000)+ggtitle("Population of metr. area")
p4	<-	qplot(CITYA,binwidth = 100)+ggtitle("Area of main city")
p5	<-	qplot(GDP,binwidth = 100000)+ggtitle("Metropolitan area GDP")
p6	<-	qplot(M50,binwidth = 0.1)+ggtitle("Cities in 50 mi radius")
p7	<-	qplot(M100,binwidth = 1)+ggtitle("Cities in 100 mi radius")
p8	<-	qplot(M200,binwidth = 1)+ggtitle("Cities in 200 mi radius")
p10	<-	qplot(NRUN,binwidth = 0.5)+ggtitle("Number of runways")
p11	<-	qplot(NAIRL,binwidth = 2)+ggtitle("Number of airlines")
p12	<-	qplot(OPER,binwidth = 100)+ggtitle("Number of daily operations")
p13	<-	qplot(NDES,binwidth = 10)+ggtitle("Number dom. destinations")
p14	<-	qplot(IDES,binwidth = 2)+ggtitle("Number int. destinations")
p15	<-	qplot(NTER,binwidth = 0.5)+ggtitle("Number of terminals")
p16     <-      qplot(ACCON,binwidth = 0.1)+ggtitle("Airport-city connectivity")

# Multiplot with histograms
PA <- multiplot(p1 ,p2,p3,p4,p5,p6,p7,p8, cols=2)
PB <- multiplot(p10,p11,p12,p13,p14,p15, cols=2)
png(filename = "Histograms2.png",width = 2400, height = 2700, units = "px", pointsize = 15, bg = "white", res = 200, restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"))
PC <- multiplot(p1 ,p2,p3,p4,p5,p6,p7,p8,p10,p11,p12,p13,p14,p15,p16, cols=3)
dev.off()

# Density plots
qplot(ACCON, geom = "density",group=HUB, color=HUB)
qplot(CITYA, geom = "density",group=HUB, color=HUB)
qplot(ACCON, geom = "density",group=NHUB, color=NHUB)
qplot(OPER, geom = "density",group=HUB, color=HUB)
qplot(NRUN, geom = "density",group=HUB, color=HUB)
qplot(NAIRL, geom = "density",group=HUB, color=HUB)
qplot(NDES, geom = "density",group=HUB, color=HUB)
qplot(IDES, geom = "density",group=HUB, color=HUB)
qplot(CPOP, geom = "density",group=HUB, color=HUB)
qplot(GDP, geom = "density",group=HUB, color=HUB)
qplot(NPAS, geom = "density",group=HUB, color=HUB)

# Q-Q plot for ACCON
qqnorm(ACCON) + qqline(ACCON)

# Shapiro-Wilk test for ACCON
shapiro.test(ACCON)

# Principal components analysis
fa.parallel(Data, fa = "pc", n.iter = 1000)
pc3 <- principal(Data,nfactors = 3)
pc3
rc <- principal(Data,nfactors=10,rotate="varimax")
rc

# Exploratory factor analysis
# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors with varimax rotation 
fit <- factanal(Data, 3, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(Data),cex=.7) # add variable names
# Iterated principal axis
fa.parallel(corma, n.obs = 51, fa = "fa", n.iter = 100, main = "Scree plots with parallel analysis")
fa(corma,nfactors = 3,rotate = "none",fm = "pa")
fa(corma,nfactors = 3,rotate = "varimax",fm = "pa")
fa.promax <- fa(corma,nfactors = 3,rotate = "promax",fm = "pa")
# Finding the scores
fsm <- function(oblique){
        if (class(oblique)[3]=="fa"&is.null(oblique$Phi)){
                warning("Object doesn't look like oblique EFA")
        }else{
                        P <- unclass(oblique$loading)
                        F <- P%*% oblique$Phi
                        colnames(F) <- c("PA1","PA2","PA3")
                        return (F)
                }
}
round(fsm(fa.promax),2)
factor.plot(fa.promax,labels = row.names(fa.promax$loadings))
# Plot containing the original variables
plot(fa.promax$loadings,type = "n")
text(fa.promax$loadings,labels=names(Data),cex=.7) 
# Diagram
fa.diagram(fa.promax,simple = FALSE)

# Multidimensional scaling
Datat <-t(Data)
# euclidean distances between the rows
dt <- dist(Datat)
d <- dist(Data)
fitt <- cmdscale(dt,eig=TRUE, k=2,add = TRUE) # k is the number of dim
fitt # view results
fit <- cmdscale(d,eig=TRUE, k=2,add = TRUE) # k is the number of dim
# plot solution 
x <- fitt$points[,1]
y <- fitt$points[,2]
factor.plot(fit$points,labels = rownames(fit$points))
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",main ="Metric MDS")
text(x, y, labels = row.names(Datat), cex=.7)
# Scaling the data
scale_data <- scale(Data)
sDatat <-t(scale_data)
# euclidean distances between the rows
dt <- dist(sDatat)
fitt <- cmdscale(dt,eig=TRUE, k=2,add = TRUE) # k is the number of dim
fitt # view results
# plot solution 
x <- fitt$points[,1]
y <- fitt$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",main ="Metric MDS")
text(x, y, labels = row.names(Datat), cex=.7)


# Regression
# Linear regression 
# NPAS as dependent variable
par(mfrow = c(1,1))
NPASHUB	<-lm(NPAS~HUB,Data)		
summary(NPASHUB)							
plot(NPAS~HUB,data=Data);abline(NPASHUB,col="red")

NPASNHUB<-lm(NPAS~NHUB,Data)		
summary(NPASNHUB)							
plot(NPAS~NHUB,data=Data);abline(NPASNHUB,col="red")

NPASAREAIR<-lm(NPAS~AREAIR,Data)		
summary(NPASAREAIR)							
plot(NPAS~AREAIR,data=Data);abline(NPASAREAIR,col="red")

NPASCPOP<-lm(NPAS~CPOP,Data)		
summary(NPASCPOP)							
plot(NPAS~CPOP,data=Data);abline(NPASCPOP,col="red")

NPASCITYA<-lm(NPAS~CITYA,Data)		
summary(NPASCITYA)							
plot(NPAS~CITYA,data=Data);abline(NPASCITYA,col="red")

NPASGDP	<-lm(NPAS~GDP,Data)		
summary(NPASGDP)							
plot(NPAS~GDP,data=Data);abline(NPASGDP,col="red")

NPASM50	<-lm(NPAS~M50,Data)		
summary(NPASM50)							
plot(NPAS~M50,data=Data);abline(NPASM50,col="red")

NPASM100<-lm(NPAS~M100,Data)		
summary(NPASM100)							
plot(NPAS~M100,data=Data);abline(NPASM100,col="red")

NPASM200<-lm(NPAS~M200,Data)		
summary(NPASM200)							
plot(NPAS~M200,data=Data);abline(NPASM200,col="red")

NPASAIO	<-lm(NPAS~AIO,Data)		
summary(NPASAIO)							
plot(NPAS~AIO,data=Data);abline(NPASAIO,col="red")

NPASACCON<-lm(NPAS~ACCON,Data)		
summary(NPASACCON)							
plot(NPAS~ACCON,data=Data);abline(NPASACCON,col="red")

NPASNRUN<-lm(NPAS~NRUN,Data)		
summary(NPASNRUN)							
plot(NPAS~NRUN,data=Data);abline(NPASNRUN,col="red")

NPASNAIRL<-lm(NPAS~NAIRL,Data)		
summary(NPASNAIRL)							
plot(NPAS~NAIRL,data=Data);abline(NPASNAIRL,col="red")

NPASOPER<-lm(NPAS~OPER,Data)		
summary(NPASOPER)							
plot(NPAS~OPER,data=Data);abline(NPASOPER,col="red")

NPASNDES<-lm(NPAS~NDES,Data)		
summary(NPASNDES)							
plot(NPAS~NDES,data=Data);abline(NPASNDES,col="red")

NPASIDES<-lm(NPAS~IDES,Data)		
summary(NPASIDES)							
plot(NPAS~IDES,data=Data);abline(NPASIDES,col="red")

NPASNTER<-lm(NPAS~NTER,Data)		
summary(NPASNTER)							
plot(NPAS~NTER,data=Data);abline(NPASNTER,col="red")

# Save correlation matrix to file
write.csv(correlations,"Correlations.csv")





#===============================================================================
# Multiple linear regression
linear1 <- lm(NPAS ~ NHUB + CPOP + GDP + ACCON + NRUN + NAIRL + OPER + NDES + IDES + NTER, Data)
summary(linear1)
        # Check normality assumption
        qqPlot(linear1, llabels = row.names(Data), id.method = "identify", simulate = TRUE, main = "Q-Q Plot")
        # Residplot function
        residplot <- function (fit, nbreaks = 10){
                z <- rstudent (fit)
                hist(z, breaks = nbreaks, freq=FALSE,
                     xlab = "Studentized Residual",
                     main = "Distribution of Errors")
                rug(jitter(z), col = "brown")
                curve(dnorm(x, mean = mean(z), sd = sd(z)),
                      add = TRUE, col = "blue", lwd = 2)
                lines(density(z)$x, density(z)$y,
                      col = "red", lwd = 2, lty = 2)
                legend("topright",
                       legend = c("Normal Curve", "Kernel Density Curve"),
                       lty = 1:2, col = c("blue","red"),cex = .7)
        }
        # Check linearity
        crPlots(linear1)
        # Check independence of errors
        durbinWatsonTest(linear1)
        
        linear1a <- lm(log(NPAS) ~ NHUB + CPOP + GDP + ACCON + NRUN + NAIRL + OPER + NDES + IDES + NTER, Data)
        summary(linear1a)        

# Model 1 violates some of the assumptions for OLS models. In order to meet those assumptions, a square root 
# transformation will be performed to create a model in accordance to the results from spreadLevelPlot() function
linear2 <- lm(sqrt(NPAS) ~ NHUB + CPOP + GDP + ACCON + NRUN + NAIRL + OPER + NDES + IDES + NTER, Data)
summary(linear2) 
        # Check normality assumption
        qqPlot(linear2, llabels = row.names(Data), id.method = "identify", simulate = TRUE, main = "Q-Q Plot")
        # Residplot function
        residplot(linear2)
        # Check independence of errors
        durbinWatsonTest(linear2)
        # Check linearity
        crPlots(linear2)
        ncvTest(linear2)
        gvlma(linear2)
        plot(linear2)
        spreadLevelPlot(linear2)
        relweights(linear2)
        
linear3 <- lm(sqrt(NPAS) ~ CPOP + GDP + NAIRL + OPER + NDES + NTER, Data)
summary(linear3) 
        # Check normality assumption
        qqPlot(linear3, llabels = row.names(Data), id.method = "identify", simulate = TRUE, main = "Q-Q Plot")
        # Residplot function
        residplot(linear3)
        # Check independence of errors
        durbinWatsonTest(linear3)
        # Check linearity
        crPlots(linear3)
        ncvTest(linear3)
        spreadLevelPlot(linear3)
        gvlma(linear3)
        plot(linear3)
        relweights(linear3)

# Deleting some predictors according to their significance from the p-test on summary() function
linear4 <- lm(sqrt(NPAS) ~ NAIRL + OPER + NDES -1, Data)
summary(linear4)
        #Check normality assumption
        qqPlot(linear4, llabels = row.names(Data), id.method = "identify", simulate = TRUE, main = "Q-Q Plot")
        # Residlinear5nction
        residplot(linear4)
        # Check independence of errors
        durbinWatsonTest(linear4)
        # Check linearity
        crPlots(linear4)
        gvlma(linear4)
        spreadLevelPlot(linear4)
        relweights(linear3)


linear5 <- lm(NPAS ~ NAIRL + I(OPER^2) + NDES -1, Data)
summary(linear5)
        # Check normality assumption
        qqPlot(linear5, llabels = row.names(Data), id.method = "identify", simulate = TRUE, main = "Q-Q Plot")
        # Residplot function
        residplot(linear5)
        # Check independence of errors
        durbinWatsonTest(linear1)
        # Check linearity
        crPlots(linear5)
        gvlma(linear5)        
        spreadLevelPlot(linear5)
        relweights(linear5)

        
        
        
             
#=============================================================================== 
linear6 <- lm(sqrt(NPAS) ~ NAIRL + OPER + NDES -1, Data)
summary(linear6)
        #Check normality assumption
        qqPlot(linear6, llabels = row.names(Data), id.method = "identify", simulate = TRUE, main = "Q-Q Plot")
        # Residlinear5nction
        residplot(linear6)
        # Check linearity
        crPlots(linear6)
        ncvTest(linear6)
        gvlma(linear6)
#====================== 
linear7 <- lm(sqrt(NPAS) ~ CPOP + GDP + ACCON + NRUN + NAIRL + OPER + NDES + IDES + NTER, Data)
summary(linear7)
# Check normality assumption
qqPlot(linear7, llabels = row.names(Data), id.method = "identify", simulate = TRUE, main = "Q-Q Plot")
# Residplot function
residplot(linear7)
# Check linearity
crPlots(linear7)
gvlma(linear7)
#====================== 
linear8 <- lm(NPAS ~ NAIRL + OPER + NDES, Data)
pdf("linear3graphs.pdf")
summary(linear3)
# Check normality assumption
qqPlot(linear3, llabels = row.names(Data), id.method = "identify", simulate = TRUE, main = "Q-Q Plot")
# Residplot function
residplot(linear3)
# Check linearity
crPlots(linear3)
# Scatterplot
Relevant <- data.frame(NPAS, NAIRL, OPER, NDES)
scatterplotMatrix(Relevant, spread = FALSE, lty.smooth=2,main ="Scatter Plot Matrix")
dev.off()
#====================== 
linear9 <- lm(sqrt(NPAS) ~ NHUB + NAIRL + I(OPER^2) + NDES -1, Data)
summary(linear9)
        #Check normality assumption
        qqPlot(linear5, llabels = row.names(Data), id.method = "identify", simulate = TRUE, main = "Q-Q Plot")
        # Residlinear5nction
        residplot(linear5)
        # Check linearity
        crPlots(linear5)
        gvlma(linear5)
#======================         
linear10 <- lm(sqrt(NPAS) ~ CPOP + GDP + NAIRL + OPER + NDES + NTER, Data)
summary(linear10) 
        # Check normality assumption
        qqPlot(linear8, llabels = row.names(Data), id.method = "identify", simulate = TRUE, main = "Q-Q Plot")
        # Residplot function
        residplot(linear8)
        # Check linearity
        crPlots(linear8)
        ncvTest(linear8)
        gvlma(linear8)
        plot(linear8)
        relweights(linear8)
#======================         
        #linear2b <- lm(NPAS ~ I(NAIRL^2) + I(OPER^2) + I(NDES^2) -1, Data)
        #summary(linear2b)
#linear2d <- lm(NPAS ~ I(NAIRL^2) + OPER + NDES -1, Data)
        #summary(linear2d)
                #linear2e <- lm(NPAS ~ NAIRL + OPER + I(NDES^2) -1, Data)
        #summary(linear2e)
                #linear2f <- lm(log(NPAS) ~ NAIRL + OPER + NDES, Data)
        #summary(linear2f)
                linear4 <- lm(NPAS ~ NAIRL + OPER + NDES -1, Data)
        summary(linear4)
        # Check normality assumption
        qqPlot(linear4, llabels = row.names(Data), id.method = "identify", simulate = TRUE, main = "Q-Q Plot")
        # Residplot function
        residplot(linear4)
        # Check linearity
        crPlots(linear4)
        linear3 <- lm(NPAS ~ NHUB + CPOP + GDP + ACCON + NRUN + IDES + NTER, Data)
        summary(linear3)
                linear4 <- lm(NPAS ~ NHUB + CPOP + ACCON + NRUN + IDES, Data)
        summary(linear4)
                linear5 <- lm(NPAS ~ NHUB + NRUN + IDES, Data)
        summary(linear5)
#===============================================================================        
        

        
        
        
        
        
# High leverage points
        hat.plot <- function (fit){
                p <- length(coefficients(fit))
                n <- length(fitted(fit))
                plot(hatvalues(fit),main = "Index plot of Hat values")
                abline(h = c(2,3)*p/n, col = "red", lty = 2)
                identify(1:n, hatvalues(fit), names(hatvalues(fit)))
                
        }
        hat.plot(linear3)

# Inlfuential observations
cutoff <- 4/(nrow(Data)-length(linear5$coefficients)-2)
plot(linear5, which = 4,cook.levels=cutoff)
abline(h=cutoff,lty=2,col="red")
        
influencePlot(linear5, id.method = "identify",main="Influence Plot", sub = "Circle size is proportional to Cook's distance")

# Akaike Information Criterion
AIC(linear3,linear4,linear5)
        
# Fitting values
Datafitted2a<-fitted(linear2a,Data)
Datacomp<-data.frame(Datafitted,Datafitted2,Datafitted2a,Data$NPAS)
Datacomp$Difference <- Datacomp$Datafitted2a - Datacomp$Data.NPAS							

# All subsets regression
leaps <- regsubsets(NPAS ~ NHUB + CPOP + GDP + ACCON + NRUN + NAIRL + OPER + NDES + IDES + NTER, data = Data, nbest=3)
plot(leaps, scale = "adjr2")

subsets(leaps,statistic = "cp", main = "Cp plot for all subsets regression")
abline (1, 1, lty=2, col = "red")

stepAIC(linear3, direction = "backward")
#===============================================================================


# Hub analysis
# Data visualization
# Plot of Airport-city connectivity vs. Number of daily operations with categories: Hub/Non-hub
index <- c(0,1)
values <- c("Non-hub","Hub")
Data$HUBCATEGORY <- values[match(Data$HUB,index)]
qplot(OPER,ACCON,data=Data,color=HUBCATEGORY,main="Airport-city connectivity vs. Number of daily operations with categories: Hub/Non-hub",size=OPER) + geom_smooth(method=lm,se=TRUE) + ylab("Connectivity") + xlab("OPER")

# Plot of Passenger number vs. Population
qplot(CPOP,NPAS,data=Data,color=HUBCATEGORY, main = "Passenger number vs. Population with categories: Hub/Non-hub") + geom_smooth(method=lm,se=TRUE) + ylab("Passengers") + xlab("Population")

# Plot of Passenger number vs. Number of airlines
qplot(NAIRL,NPAS,data=Data,color=HUBCATEGORY, main = "Passenger number vs. Number of airlines with categories: Hub/Non-hub", size=NAIRL) + geom_smooth(method=lm,se=TRUE) + ylab("Passengers") + xlab("Number of airlines")

# Plot of GDP vs. Passenger number
qplot(NPAS,GDP,data=Data,color=HUBCATEGORY, main = "GDP vs. Passenger number with categories: Hub/Non-hub", size=GDP) + geom_smooth(method=lm,se=TRUE) + ylab("GDP") + xlab("Passengers")

# Plot of GDP vs. Airport-city connectivity
qplot(ACCON,GDP,data=Data,color=HUBCATEGORY,main = "GDP vs. Airport-city connectivity with categories: Hub/Non-hub") + geom_smooth(method=lm,se=TRUE) + ylab("GDP") + xlab("Airport-City connectivity") + title(main = "GDP vs. Airport-city connectivity")

# Plot of GDP vs. Number of airlines
qplot(NAIRL,GDP,data=Data,color=HUBCATEGORY,main = "GDP vs. Airport-city connectivity with categories: Hub/Non-hub") + geom_smooth(method=lm,se=TRUE) + ylab("GDP") + xlab("Airport-City connectivity") + title(main = "GDP vs. Airport-city connectivity")

# Plot of GDP vs. Number of operations
qplot(OPER,GDP,data=Data,color=HUBCATEGORY,main = "GDP vs. Airport-city connectivity with categories: Hub/Non-hub") + geom_smooth(method=lm,se=TRUE) + ylab("GDP") + xlab("Airport-City connectivity") + title(main = "GDP vs. Airport-city connectivity")

# Plot of GDP vs. Number of domestic destinations
qplot(NDES,GDP,data=Data,color=HUBCATEGORY,main = "GDP vs. Airport-city connectivity with categories: Hub/Non-hub") + geom_smooth(method=lm,se=TRUE) + ylab("GDP") + xlab("Airport-City connectivity") + title(main = "GDP vs. Airport-city connectivity")

# Plot of GDP vs. Number of international destinations
qplot(IDES,GDP,data=Data,color=HUBCATEGORY,main = "GDP vs. Airport-city connectivity with categories: Hub/Non-hub") + geom_smooth(method=lm,se=TRUE) + ylab("GDP") + xlab("Airport-City connectivity") + title(main = "GDP vs. Airport-city connectivity")

# Plot of GDP vs. M200
qplot(M200,GDP,data=Data,color=HUBCATEGORY,main = "GDP vs. Airport-city connectivity with categories: Hub/Non-hub") + geom_smooth(method=lm,se=TRUE) + ylab("GDP") + xlab("Airport-City connectivity") + title(main = "GDP vs. Airport-city connectivity")

# Plot of Passengers vs. M200
qplot(M200,NPAS,data=Data,color=HUBCATEGORY,main = "GDP vs. Airport-city connectivity with categories: Hub/Non-hub") + geom_smooth(method=lm,se=TRUE) + ylab("GDP") + xlab("Airport-City connectivity") + title(main = "GDP vs. Airport-city connectivity")
#===================

# Decision trees
CART <-rpart(factor(HUBCATEGORY) ~ CPOP + GDP + NAIRL + OPER + NDES, data=Data, method="class",control = rpart.control(minsplit=30, cp=0.001))
plot(CART, uniform=T, main="Classification Tree for Data")
text(CART, use.n=TRUE, all=T, cex=.5)

CART <-rpart(factor(HUBCATEGORY) ~ NAIRL + OPER + NDES, data=Data, method="anova")
plot(CART, uniform=TRUE, main="Classification Tree for Data",asp=0.7)
text(CART, use.n=TRUE, all=TRUE, cex=0.8)


CART <- ctree(factor(HUBCATEGORY) ~ NAIRL + OPER + NDES, data=Data)
plot(CART)

CART <- randomForest(HUB ~ ACCON + NRUN + CPOP + GDP + NAIRL + OPER + NDES, data=Data)
plot(CART)

CART <- randomForest(factor(HUBCATEGORY) ~ NAIRL + OPER + NDES, data=Data)
plot(CART)

ResCART <- getTree(CART, 490)
plot (ResCART)

index <- c(0,1)
values <- c("Non-hub","Hub")
Data$HUBCATEGORY <- values[match(Data$HUB,index)]
PAP <- tree(factor(HUBCATEGORY) ~ NPAS + NAIRL + OPER + NDES + IDES + NTER, data=Data)
plot(PAP);text(PAP)

PAP <- cforest(factor(HUBCATEGORY) ~ NAIRL + OPER + NDES, data=Data)
plot(PAP)
pt <- party:::prettytree(PAP@ensemble[[1]], names(PAP@data@get("input"))) 
pt
nt <- new("BinaryTree")
nt@tree <- pt
nt@data <- PAP@data
nt@responses <- PAP@responses
nt
plot(nt)

