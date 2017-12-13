options(warn=-1)
#Miss out the first this many weeks (because of pandemic/lack of data):
notthese <- 156#6 months? - 24
#Predict this many weeks:
thisfar <- 28

#FLUID
#Load data:
fluid1c <- read.csv("XfluIDFrance.csv", header=TRUE)#enough time points
#21=USA - problem with space (#)?
cname="France"

#Extract cases:
toMatch <- c("ILI CASES", "ILI OUTPATIENTS", "SARI CASES", "SARI DEATHS$", "SARI INPATIENTS", "SARI DEATHS INPATIENTS")#All usable
#toMatch <- c("ILI CASES", "SARI CASES")
#toMatch <- c("DEATHS", "PATIENTS$")
fluidc <- fluid1c[grep(paste(toMatch, collapse="|"), fluid1c$MEASURE_CODE),]

#fluidc <- data.frame(fluidc$YEAR_CODE,fluidc$WEEK_CODE,fluidc$MEASURE_CODE,fluidc$AGEGROUP_CODE,fluidc$DataValue)
#Why doesn't work? - get "fluidc." in column names
keeps <- c("YEAR_CODE","WEEK_CODE","MEASURE_CODE","AGEGROUP_CODE","DataValue")
fluidc <- fluidc[keeps]
names(fluidc)[1:5] <- paste(c("year", "week", "case", "age", "value"))#case

fluidc$week <- as.numeric(fluidc$week)
fluidc$year <- as.numeric(fluidc$year)
fluidc$value <- as.numeric(as.character(fluidc$value))
#fluisc <- fluidc[order(fluidc$week),]
#fluisc <- order(fluidc$year),]#Retains sub-order from previous? #Col numbers retain old order

#print(fluidc)

#Separate out ILI and SARI CASES only:
cases <- c("ILI CASES", "SARI CASES")#Include sub-cases?
fluidis <- fluidc[grep(paste(cases, collapse="|"), fluidc$case),]#"ILI/SARI"
#ILI+SARI:
fluid1is <- aggregate(fluidis$value, by=list(fluidis$year, fluidis$week), FUN=sum, na.rm=TRUE)
names(fluid1is)[1:3] <- paste(c("year", "week", "value"))

fluid1is <- fluid1is[order(fluid1is$week),]
fluid1is <- fluid1is[order(fluid1is$year),]
#Disguard pandemic period:
fluid1is <- fluid1is[-c(1:notthese),]

lf <- length(fluid1is$value)

#Plot ILI=SARI:
#plot(fluid1is$value, type="l")

##ACF and PACF in same figure:
par(mfrow=c(2,1))
acf(fluid1is$value)
pacf(fluid1is$value)

##Difference series - not necessary here?
#dfluid <- diff(fluid1is$value)
#ndifffluid <- length(dfluid)
#plot(1:ndifffluid,dfluid,type="l")
#fluid1is$value <- c(0,dfluid)
##ACF/PACF for dfluid

#ARIMA:
fluid.fit <- arima(fluid1is$value, order=c(1,0,1), seasonal=list(order=c(0,1,0), period=52), include.mean=FALSE)
#fluid.fit <- auto.arima(fluid1is$value, seasonal=list(order=c(1,1,0), period=52))#, include.mean=FALSE)
#fluid.fit <- sarima.for(fluid1is$value,2,1,0,1,1,0,1,52) 
print(fluid.fit)

#Predictions:
fluid.pred <- predict(fluid.fit, n.ahead=thisfar)
ymax <- max(fluid.pred$pred)+2*max(fluid.pred$se)
par(mfrow=c(1,1))
plot(fluid1is$value, type="l", xlim=c(0,lf+thisfar), ylim=c(0,ymax), xlab="Time (weeks)", ylab="Cases", main=cname)
lines(fluid.pred$pred, col="blue")
lines(fluid.pred$pred+2*fluid.pred$se, col="red")
lines(fluid.pred$pred-2*fluid.pred$se, col="red")


