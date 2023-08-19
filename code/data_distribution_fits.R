

#R functions to read, analyze, plot and detect patterns in data along with neural and computer algorithms to model complex data.    

#Copyright (C) 2023 Shyam Srinivasan 

#This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details.

#You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.



#This function contains various function to analyze distribution fits and so on.

#a fuction to get a nice fancy plot
#input: the x vector, the y vector, xlabel, ylabel, title
fplot <- function(x,y,xlabel='x',ylabel='y',title='',origin=F){
  norm <- 1
  if(!origin) {
    plot(x,y,frame.plot = FALSE,main = title, xlab = xlabel,ylab = ylabel,
         xlim = c(min(x),ceiling(max(x) * 10)/10),ylim = c(min(y),max(y)),
         pch=1,cex=2,cex.lab=1.5,cex.axis=1.5)
  }
  else {
    plot(x,y,frame.plot = FALSE,main = title, xlab = xlabel,ylab = ylabel,
         xlim = c(0,ceiling(max(x) * 10)/10),ylim = c(0,max(y)),
         pch=1,cex=2,cex.lab=1.5,cex.axis=1.5)
    
  }
}

#this function draws or adds a line plot to an existing plot
#input: the x vector, the y vector, color, and line width
flines <- function(x,y,color="red",lwidth=2,op=1){
  lines(x,y,col = color, lwd = lwidth)
}

#plot the relative cumulative frequency
#input: a vector of the daata
#output: the relative cumulative frequency as 2 columns: x, rel. freq(x)
RelCumFreq <- function(vec,op=1){
  splitlist <- split(vec,vec) #produces a list with each element containing a list of occurances
  #of that element. Split automatically produces a sorted list
  freqx <- sapply(splitlist, length) # now make a frequency list
  cumfreqx <- cumsum(freqx)/length(vec)
  x <- as.numeric(names(freqx)) # gets the unique x values
  res <- cbind(x,cumfreqx) # return as 2 columsn, data, relative frequency
  row.names(res) <- NULL
  res
}

#will do a non-linear squares fit for a gamma function. 
#input is the data, the distribution, param guesses
#dist - 1, normal, 2 - gamma, 3 - lognormal
#output is the fit by NLS
NlsFitCDF <- function(data,dist,params,op=1){
  #assumes that your data is in columnar format
  #listargs <- list(data,params[1],params[2])
  return(switch (dist,
                 NlsFitNormalCDF(data,params,op),
                 NlsFitGammaCDF(data,params,op),
                 NlsFitLogNormalCDF(data,params,op)
  ))
  return(FALSE)
}

#will do a non-linear squares fit for a normal function. 
#input is scale and shape guesses
#output is the fit by NLS
NlsFitNormalCDF <- function(data,params,op=1){
  #assumes that your data is in columnar format
  x <- data[,1]
  y <- data[,2]
  fit <- nls(y~pnorm(x,mean = a,sd = b),start = list(a = params[1], b = params[2]),trace = T)
  fit
}


#will do a non-linear squares fit for a gamma function. 
#input is scale and shape guesses
#output is the fit by NLS
NlsFitGammaCDF <- function(data,params,op=1){
  #assumes that your data is in columnar format
  x <- data[,1]
  y <- data[,2]
  fit <- nls(y~pgamma(x,shape = a,scale = b),start = list(a = params[1], b = params[2]),trace = T)
  fit
}

#will do a non-linear squares fit for a gamma function. 
#input is scale and shape guesses
#output is the fit by NLS
NlsFitLogNormalCDF <- function(data,params,op=1){
  #assumes that your data is in columnar format
  x <- data[,1]
  y <- data[,2]
  fit <- nls(y~plnorm(x,meanlog = a,sdlog = b),start = list(a = params[1], b = params[2]),trace = T)
  fit
}


#computes the coeeficient of determination
#if dist - 1, normal, 2 - gamma, 3 - lognormal
#output: {r2,sstot,ssreg,ssres}
ComputeCoeffDetCDF <- function(data,dist,params,op=1){
  ybar <- mean(data[,2]) # mean of the data
  sstot <- sum((data[,2]-ybar)^2) # total sum of squares
  #fit the fn, get params, and then the fn based CDF values
  fit.fn <- NlsFitCDF(data,dist,params,op)
  params <- summary(fit.fn)$parameters
  fn <- switch (dist,
                pnorm(data[,1],mean = params[1], sd = params[2]),
                pgamma(data[,1],shape = params[1], scale = params[2]),
                plnorm(data[,1],meanlog = params[1], sdlog = params[2])
  )
  ssreg <- sum((fn-ybar)^2) #regression sum of squares
  ssres <- sum((data[,2]-fn)^2) # residual sum of squares
  c(R2=1-ssres/sstot,sstot=sstot,ssreg=ssreg,ssres=ssres,"params"=params[1:2])
}


#this function reads in raw data from a prespecified file and outputs
#input: filename is a string
# the paramters and graph fits
CalculateCDFs <- function(filename,dist="gamma",op=1){
  synsizes <- read.csv(filename) # read in the data
  l1syns <- as.numeric(as.vector(synsizes$raw.data[3:119])) # get PSD sizes
  #calculate rel frequency histogram
  relcum <- RelCumFreq(l1syns)
  #now the Gamma distribution
  res1 <- ComputeCoeffDetCDF(relcum,2,c(1.03,.079)) # find R2, and params
  print("Gamma")
  print(res1)
  #now draw the plot
  if ( tolower(dist) == "gamma") {
    fplot(relcum[,1],relcum[,2],"PSD area","relative frequencies",title="Gamma Distribution CDF")
    flines(relcum[,1],pgamma(relcum[,1],shape = 1.45519,scale = .042),color = "red",
           lwidth = 2)
    text(.33,.23,bquote(R^2 == .(round(res1[1],3))))
  }
  #the log normal distribution
  res1 <- ComputeCoeffDetCDF(relcum,3,c(-3.03,.79)) # find r2, and params
  print("Log Normal")
  print(res1)
  #now draw the plot
  if ( tolower(dist) == "lognormal") {
    fplot(relcum[,1],relcum[,2],"PSD area","relative frequencies",title="Log Normal Distribution CDF")
    flines(relcum[,1],plnorm(relcum[,1],meanlog = -3.06, sdlog = .93),color = "blue",
           lwidth = 2)
    text(.33,.23,bquote(R^2 == .(round(res1[1],3))))
  }
}

PlotPCDensity <- function(filename,op=1){
  pc.df <- ReadData(filename)
  fstripchart(pc.df[,c(1,2)],"jitter",rndfact = 5)
}

PlotPCWidth <- function(filename,op=1){
  layer.df <- ReadData(filename)
  tst <- split(as.vector(layer.df[,c(2,3,4)]),layer.df[,1])
  print(tst)
  layermean <- lapply(tst,function(df) {apply(df,2,mean)})   
  HorzBarPlot(unlist(layermean),rndfact = 5)
  
}

GetMeanPCWidth <- function(data.df,op=1){
  tst <- split(as.vector(data.df[,c(2,3,4)]),layer.df[,1])
  #print(tst)
  layermean <- lapply(tst,function(df) {apply(df,2,mean)})   
  layermean
}

GetStrPCWidth <- function(data.df,op=1){
  tst <- split(as.vector(data.df[,c(2,3,4)]),layer.df[,1])
}

#given the layer1a filename, will produce a data frame fstripchart format
#op=2, calculates length as fraction of total
GetLayer1a <- function(name,op=1){
  data <- ReadData(name,sepe = "\t")
  cnames <- names(data)
  if( op == 2) { # calculate the length as a fraction of total
    sumvec <- apply(data,1,sum)
    data <- data/sumvec
  }
  namvec <- rep(cnames,each=length(data[[1]]))
  nosvec <- as.vector(as.matrix(data))
  data.df <- data.frame(namvec,nosvec)
  if (op == 1) return(data.df)
  fstripchart(data.df,method="jitter",rndfact = 10,tickno = 2)
}

