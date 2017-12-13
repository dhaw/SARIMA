#flunet <- read.csv("FluNetInteractiveReportAfrican.csv",skip=3)

#To do:
#Extract first n characters from country names?
#For loops: for (i in 1:n-1) //
#Where no data: NaNs rather than 0s
#Only retaining those with non-zero ILI and SARI?

options(warn=-1)
#FLUID
#Load all data:
listcsv <- dir(pattern = "Xfluid*")#DynamicFLUID*")
fluid1 <- read.csv(listcsv[1])
for (k in 1:length(listcsv)){
  fluidNext <- read.csv(listcsv[k], header=TRUE)
  fluid1 <- rbind(fluid1, fluidNext)
}

#Rename first column:
names(fluid1)[1]<-paste("COUNTRY")

#Extract cases:

toMatch <- c("ILI CASES", "ILI OUTPATIENTS", "SARI CASES", "SARI DEATHS$", "SARI INPATIENTS", "SARI DEATHS INPATIENTS")#All usable
#toMatch <- c("ILI CASES", "ILI OUTPATIENTS", "SARI CASES", "SARI DEATHS$", "SARI INPATIENTS", "SARI DEATHS INPATIENTS")#All usable
#toMatch <- c("ILI CASES", "SARI CASES")
#toMatch <- c("DEATHS", "PATIENTS$")
fluid <- fluid1[grep(paste(toMatch, collapse="|"), fluid1$MEASURE_CODE),]
fluid$DataValue <- as.numeric(as.character(fluid$DataValue))#[2:nrow(fluid1)]
#print(fluid)
cases <- toMatch

#Sum by 4-category:
dataFull <- aggregate(fluid$DataValue, by=list(fluid$COUNTRY, fluid$YEAR_CODE, fluid$MEASURE_CODE, fluid$AGEGROUP_CODE), FUN=sum, na.rm=TRUE)
names(dataFull)[1:5]<-paste(c("country", "year", "case", "age", "value"))
#print(dataFull)

########Single plot, one box per country, stacked bars by case: ########

fluBar <- function(dataFull, cases){
  #All ages, ILI+SARI:
  #All ages:
  dataAllage <- dataFull[grep("ALL", dataFull$age), ]
  #ILI+SARI:
  countryYear <- aggregate(dataAllage$value, by=list(dataAllage$country, dataAllage$year, dataAllage$case), FUN=sum, na.rm=TRUE)#Warnings here?##
  names(countryYear)[1:4] <- paste(c("country", "year", "case", "value"))#case
  #print(countryYear)
  
  #Cases:##
  #cases <- droplevels(countryYear$case)
  #cases <- levels(cases)
  lc <- length(cases)
  
  #Vector of all years:
  years <- unique(countryYear$year)
  years <- years[order(years)]
  yearVec <- seq(min(years), max(years), by=1)#In case any universally missing
  ly <- max(years)-min(years)+1
  
  #Plot bar graph for each country:
  nameC <- levels(countryYear$country)
  numC <- length(nameC)
  
  #toplot <- array(0,c(ly, numC*lc))#Columns are time series##One slice per case, array
  toplot <- array(0,c(ly, numC, lc))#Columns are time series##One slice per case, array
  for (i in 1:numC){
    thisC1 <- countryYear[grep(nameC[i], countryYear$country), ]
    for (j in 1:lc){##
      thisC <- thisC1[grep(cases[j], thisC1$case), ]
      #Quicker/more elegant way to do this bit? ##1
      otherYears <- setdiff(yearVec, thisC$year)
      lother <- length(otherYears)
      if (length(lother)>0){##
        sameC <- rep(as.character(thisC[1,1]), lother)
        sameCase <- rep(as.character(cases[j]), lother)
        noVals <- rep(0, lother)
        sameC <- cbind(sameC, otherYears, sameCase, noVals)
        colnames(sameC)[1:4] <- paste(c("country", "year", "case", "value"))
        thisC <- rbind(thisC, sameC)
      }##
      #toplot[, (lc*(i-1)+j)] <- as.numeric(as.character(thisC[order(thisC$year), 4]))
      toplot[, i, j] <- as.numeric(as.character(thisC[order(thisC$year), 4]))
    }##
  }
  ##Miss out countries with all zeros:#Why was this giving an error? Summing non-numbers?
  Csum <- apply(toplot,2,sum)
  C0 <- which(Csum==0)
  if (length(C0)>0){
    toplot <- toplot[, -C0, ]
    notthese <- nameC[C0]#Countries missed out
    nameC <- nameC[-C0]
    numC=length(nameC)
    #colnames(toplot) <- paste(nameC)
  }
  
  yearsAll <- seq(min(years),max(years),1)#Same as "years"?
  numY=length(yearsAll)
  Y1 <- numY-7;
  #Full plot:
  numC1 <- numC-1
  par(mar=c(1,4,1,1))
  lcol <- c(ceiling(numC/2))
  par(mfrow=c(lcol,2))#par(mfrow=c(numC,1))
  #if (numC>1){
  for (i in 1:(numC-2)){
    #barplot(t(as.matrix(toplot[Y1:numY,((i-1)*lc+1):(i*lc)])), ylab=nameC[i], lwd=10, lend="square", xaxt="n")#Log or not?, log="y" #Numbering then labels:, xaxt="n", ann=FALSE #axes=FALSE, frame.plot=TRUE)
    barplot(t(as.matrix(toplot[Y1:numY, i, ])), ylab=nameC[i], lwd=10, lend="square", xaxt="n", col=terrain.colors(lc))
  }
  #barplot(t(as.matrix(toplot[Y1:numY,((numC-2)*lc+1):(numC*(lc-2))])), ylab=nameC[numC-1], lwd=10, lend="square")
  #barplot(t(as.matrix(toplot[Y1:numY,((numC-1)*lc+1):(numC*lc)])), ylab=nameC[numC], lwd=10, lend="square")
  barplot(t(as.matrix(toplot[Y1:numY, (numC-1), ])), ylab=nameC[numC-1], lwd=10, lend="square", col=terrain.colors(lc))
  barplot(t(as.matrix(toplot[Y1:numY, numC, ])), ylab=nameC[numC], lwd=10, lend="square", col=terrain.colors(lc))
  #}
  #title("Cases", outer=TRUE)
  #legend("topright", legend=cases)
  plot.new()
  par(mfrow=c(1,1))
  #pie(rep(1,lc), labels=cases, col=terrain.colors(lc))
}

#Correlations:
fluCor <- function(numC, toplot, Y1, numY){
  CC <- matrix(0,numC,numC)
  for (i in 1:(numC-1)){#i=1:numC-1, j=i+1:numC - why can't i index to x-1 in R?
    for (j in (i+1):numC){
      CC[i,j] <- cor(toplot[Y1:numY,i], toplot[Y1:numY,j])#Ignore NAs - from Y1 is cack handed
    }
  }
  CC <- CC+t(CC)+diag(numC)
  print(CC)
  #plot(yearsAll, toplot[,numC], type="h", log="y", ylab=nameC[numC])
}

########One plot per country, one box per age group, stacked bars by case: ########

fluBarAges <- function(dataFull, cases){
  #Age groups, ILI+SARI:
  #Age groups (inc. unknown):
  dataAges <- dataFull[dataFull$age!="ALL", ]
  #ILI+SARI:
  countryYearAge <- aggregate(dataAges$value, by=list(dataAges$country, dataAges$year, dataAges$case, dataAges$age), FUN=sum, na.rm=TRUE)#Warnings here?
  names(countryYearAge)[1:5] <- paste(c("country", "year", "case", "age", "value"))
  
  #Cases:##
  lc <- length(cases)
  
  #Vector of all years:#Put outside of function?
  years <- unique(countryYearAge$year)
  years <- years[order(years)]
  yearVec <- seq(min(years), max(years), by=1)#In case any universally missing
  ly <- max(years)-min(years)+1
  
  #Plot bar graph for each country:#Put next 2 lines outside of function?
  nameC <- levels(countryYearAge$country)
  numC <- length(nameC)
  
  for (i in 1:numC){
    toMatch <- c(nameC[i])
    yearAge <- countryYearAge[grep(paste(toMatch, collapse="|"), countryYearAge$country),]
    yearAge$value <- as.numeric(as.character(yearAge$value))
    #yearAge$age <- as.factor(yearAge$age)
    
    nameA <- levels(yearAge$age)
    nameA <- sort(nameA)
    nameA <- nameA[2:length(nameA)]
    numA <- length(nameA)
    
    #toplot <- array(0,c(ly, numA*lc))#Columns are time series
    toplot <- array(0,c(ly, numA, lc))#Columns are time series
    for (j in 1:numA){#j=11 comes out empty for some reason - column header?
      a <- nameA[j]
      thisA1 <- yearAge[grep(nameA[j], yearAge$age), ]
      for (k in 1:lc){
        thisA <- thisA1[grep(cases[k], thisA1$case), ]
        #Quicker/more elegant way to do this bit? ##1
        otherYears <- setdiff(yearVec, thisA$year)
        lother <- length(otherYears)
        if (lother>0){
          sameA <- rep(as.character(a, lother))
          sameCase <- rep(as.character(cases[k]), lother)
          noVals <- rep(0, lother)
          sameA <- cbind(otherYears, sameCase, sameA, noVals)
          colnames(sameA)[1:4] <- paste(c("year", "case", "age", "value"))
          thisA <- rbind(thisA[, 2:5], sameA)
        }
        #toplot[, (lc*(j-1)+k)] <- as.numeric(as.character(thisA[order(thisA$year), 4]))
        toplot[, j, k] <- as.numeric(as.character(thisA[order(thisA$year), 4]))
      }
    }
    #Option to miss out age groups with all zeros - here
    Asum <- apply(toplot,2,sum)
    A0 <- which(Asum==0)
    if (length(A0)>0){
      toplot <- toplot[, -A0, ]
      notthese <- nameA[A0]#Age groups missed out
      nameA <- nameA[-A0]
      numA=length(nameA)
      colnames(toplot) <- paste(nameA)
    }
    
    yearsAll <- seq(min(years),max(years),1)#Same as "years"?
    numY=length(yearsAll)
    Y1 <- numY-7
    #Full plot:
    numA1 <- numA-1
    par(mar=c(1,4,1,1))
    if (numA>0){
      par(mfrow=c(numA,1))
      for (j in 1:(numA-1)){
        #barplot(t(as.matrix(toplot[Y1:numY,((j-1)*lc+1):(j*lc)])), ylab=nameA[i], lwd=10, lend="square", xaxt="n") #Log or not?, log="y" #Numbering then labels:, xaxt="n", ann=FALSE #axes=FALSE, frame.plot=TRUE)
        barplot(t(as.matrix(toplot[Y1:numY, j, ])), ylab=nameA[i], lwd=10, lend="square", xaxt="n", col=terrain.colors(lc))
      }
      #barplot(t(as.matrix(toplot[Y1:numY,((numA-1)*lc+1):(numA*lc)])), ylab=nameA[numA], lwd=10, lend="square")
      barplot(t(as.matrix(toplot[Y1:numY, numA, ])), ylab=nameA[numA], lwd=10, lend="square", col=terrain.colors(lc))
      title(nameC[i], outer=TRUE)
      #legend("topright", legend=cases)
      
    }
  }
  plot.new()
  par(mfrow=c(1,1))
  #pie(rep(1,lc), labels=cases, col=terrain.colors(lc))
}

##Miss out countries with all zeros:#Why was this giving an error? Summing non-numbers?
#Csum <- apply(toplot,2,sum)
#C0 <- which(Csum==0)
#toplot <- toplot[, -C0]
#notthese <- nameC[C0]#Countries missed out
#nameC <- nameC[-C0]
#numC=length(nameC)
##colnames(toplot) <- paste(nameC)
##rownames(toplot) <- paste(years)
##print(toplot)


##Miss out age groups with all zeros:
#Asum <- apply(toplot,2,sum)
#A0 <- which(Asum==0)
#toplot <- toplot[, -A0]
#notthese <- nameA[A0]#Age groups missed out
#nameA <- nameA[-C0]
#numA=length(nameA)
#colnames(toplot) <- paste(nameA)