#R functions to read, analyze, plot and detect patterns in data along with neural and computer algorithms to model complex data.    

#Copyright (C) 2023 Shyam Srinivasan 

#This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details.

#You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.



#file contains several statistics funtions, some of them imported from graphs.R

#function definitions and tips
#qnorm(p=t,sd=std), for a normal distirbution with mean 0 and sd=std, this gives the value of the t'th percentil. 0<t<1.

#some functions for generating correlated vectors
#Good resource: https://www.gaussianwaves.com/2014/07/generating-multiple-sequences-of-correlated-random-variables/

#this function takes in a vector length, and gets the correlaction between vectors taht 
#are generated under different conditions
#cond: 1 - base vector is fixed, 
#op =1 : brute forece way
#op = 2, y = r.x +(1-r^2)^.5 * rnorm()
getCorrVecs <- function(veclen,cond=1,basev=1,r=.3,op=1) {
  if(length(veclen)>1) basevec <- veclen
  else  basevec <- rbinom(veclen,1,.8)
  veclen <- length(basevec)
  if (op==1){
    vec1 <- basevec + basevec*runif(veclen,-r,r)
    vec2 <- basevec + basevec*runif(veclen,-r,r)
    cat(vec1,'\n',vec2,'\n')
    cor(vec1,vec2)
    res <- list(vec1,vec2)
  }
  if(op==2){
    vec1 <- basevec*r + sqrt(1-r^2)*rnorm(veclen,0,1)
    res<-list(basevec,vec1)
    cat('\n',fitlinearlaw(basevec,vec1))
  }
  res
}

#this function generates a correlation matrix
#size: matrix of size*size
#rho: the correlation between vectors, where each vector is a column
genCorMat <- function(size,rho=.7,op=1){
  cormat <- matrix(0,nrow = size,ncol = size)
  #set the diagonal to one
  diag(cormat) <- 1
  #set the upper and lower triangular matrices to cor
  cormat[upper.tri(cormat)] <- rho
  cormat[lower.tri(cormat)] <- rho
  cormat
}

#given a vector y generates n correlated vectors to it; uses the cholesky decomposition
#p : numebr of correlated vectors or can be a matrix, if p is a matrix, it has to be in
#the form where the rows are vectors, and the no of cols is the length of each vector
#vecsize : size of vectors
#rho : amount of correlation
#dist: the type of distribution to use to generate correlated vectors
#params: the parameters of this distribution
#op=1, vectors in rows, 2 - vectors in cols
#retop=1, return the correlated matrix, 2- the correlated vector, the corr matrix result, 
#the correlation matrix
genCorrMat <- function(p,vecsize=p,vec=c(),rho=0.7,dist=1,params=c(0,1),retop=1,op=1){
  if (length(dim(p)) == 2) {
    novecs <- nrow(p)
    rmat <- p
    vecsize <- ncol(p)
  }
  else {
    #tmpvec <- rnorm(vecsize,mean=5,sd=1)
    novecs <- p
    #generate n random vectors of size vecsize, here the rows are the vectors
    rmat <- matrix(rnorm(novecs*vecsize,mean=params[1],sd=params[2]),nrow = novecs)
    #rmat[1,] <- tmpvec
  }
  cormat <- genCorMat(novecs,rho = rho) #generate the correlation matrix
  tmpc <- chol(cormat) #use cholesky decompostion to get the sqrt
  
  #cat(str(tmpc),str(rmat))
  resvecs <- t(tmpc) %*% rmat #since the rows are the vectors you have to transpose the sqrt
  if (retop==2) res <- computeCorMat(t(resvecs),op=op) #compute the correlation matrix of the rows
  switch(retop,t(resvecs),list(t(resvecs),res,cormat))
} 

#given a vector vec, this generates n-1 correlated vectors
# where they are all correlated according to some rho. other param defns, see getCorrMat
# dist: 1 - gaussian, 2 - exponential, 3 - uniform
#op=1, use the mean and std of vec, 2 - use params
#retop: the type of numbers you resturn, 1 - whatevet the function gives, 2 - integers
genCorrVecs <- function(vec,n=10,rho=0.7,dist=1,params=c(0,1),retop=1,op=1){
  #the first vector is the given vector
  len <- length(vec)
  if(dist ==1 ){#the distribution is gaussian
    if(op==1){
      avg <- mean(vec)
      std <- sd(vec)
    }
    else {
      avg <- params[1]
      std <- params[2]
    }
    #cat('\n corrvecs',len,n,len*(n-1),avg,std)
    mat <- rnorm(len*(n-1),mean = avg,sd = std)#generate the random vectors
    mat <- t(matrix(c(vec,mat),ncol = n))#cast as a matrix and switch to having vectors as rows
  }
  if(dist == 2 ){#exponential distribution
    if(op==1){
      avg <- mean(vec)
    }
    else {
      avg <- params[1]
    }
    mat <- rexp(len*(n-1),rate = 1/avg)#generate the random vectors
    mat <- t(matrix(c(vec,mat),ncol = n))#cast as a matrix and switch to having vectors as rows
  }
  if(dist == 3 ){#uniform distribution
    if(op==1){
      a <- min(vec)
      b <- max(vec)
    }
    else {
      a <- params[1]
      b <- params[2]
    }
    mat <- runif(len*(n-1),min = a,max = b)#generate the random vectors
    mat <- t(matrix(c(vec,mat),ncol = n))#cast as a matrix and switch to having vectors as rows
  }
  genCorrMat(p=mat,rho=rho) # now, generate the correlation matrix and return
  
}

#generates correlated vectors that fall within a specific range
#range: the range within which the correlations hsould fall
genCorrVecsSpec <- function(vec,n=10,rho=0.7,dist=1,params=c(0,1),retop=1,range=0.05,op=1){
  no <- 0
  res.mat <- cbind(vec)
  while(no < n) {
    cor.vec <- genCorrVecs(vec,n=n,rho=rho,dist=dist,params=params,retop=retop,op=op)
    cormat <- cor(cor.vec)
    posns <- getClosestValRange(cormat[1,-1],target = rho,range = range,op=2,retop=2)
    #cat('\nGV posns',posns,'\t',length(posns),':',cormat[1,-1],range)
    if(length(posns) > 0){
      res.mat <- cbind(res.mat,cor.vec[,posns+1])
      no <- no + length(posns)
    }
  }  
  res.mat[,1:n]
}


#given a matrix, where each col or vec is a unit, it calculates the correlation between
#the cols or rows
#op= 1, cols, 2: rows
computeCorMat <-function(cmat,op=1){
  #orient the matrix according to whether you want correlations of rows or coumns
  if(op==1) mat <- cmat 
  else mat <- t(cmat)
  #res <- sapply(1:ncol(mat),function(x,mat){#go throught i and j 
  #  tst <- sapply(1:ncol(mat),function(y){
  #    cor(mat[,x],mat[,y])
  #  })  
  #  tst
  #},mat)
  cor(mat)
}



#gets the stats of a vector
getStats <- function(vec,op=1){
  getstatscol(vec = vec,op = op)
}

#given a list of vectors or a dataframe and cols, will get the mean, Sd and sem
getStatsLst <- function(lst,op=1){
  mn <- sapply(lst,mean) 
  std <- sapply(lst,sd)
  sem <- std/sqrt(sapply(lst,length))
  res <- list(mn,std,sem)
  names(res) <- c('mean','sd','sem')
  res
}

#generates shuffled responses given a bunch of neuron rates or items
#value.vecs: list of neuronal responses or item values
#n: no of shuffled responses
#size: is the size of 
#op: type of shuffling: 1 - just random permutation shuffling
#2 - no need to shuffle keep the value vecs as they are
#3 - composite of random shuffle and value vecs
genShuffledResponses <- function(value.vecs,size=c(),n=100,op=1){
  resp.size <- length(value.vecs[[1]])
  allvals <- unlist(value.vecs)
  #cat('\nlength',length(allresponses))
  res.ls <- lapply(1:n, function(i){
    #just select resp.size values from allresponses randomly
    sample(allvals,resp.size,replace = F)
  })
  res.ls
}



computeSizePower <- function(mu,mu0,sd,alpha=.05,beta=.2){
  #mu=115
  #mu0=120
  #sd=24
  #alpha=0.05
  #beta=0.20
  n=(sd*(qnorm(1-alpha)+qnorm(1-beta))/(mu-mu0))^2
  size = ceiling(n)
  z=(mu-mu0)/sd*sqrt(n)
  pow=pnorm(abs(z)-qnorm(1-alpha))
  c(size,pow)
}

#this function computes the compound poisson gamma distribution. Actually the CDF since 
#the pdf for a Gamma disribution can give values greater than 1.
#strength: the x value for which the probability has to be evaluated.
#l = the lambda poisson parameter
#a,b - shape andd scale parameters for the gamma distibution
#n - specifies the number of inputs, for example glomeruli, so that l will become l*n
#iter - the no of iterations foew hich we shoulld calculate this, empirically 10*l*n seems good
compoundPoissonGamma <-function(strength,l,a,b,n=1,iter=10,op=1){
  noiter <- l*n*iter #calcaulte the no of iterations over which the dist. should be summed
  poisval <- dpois(0:noiter,l*n) #the poisson probabilities for each iteration
  #gamma cdf for each iteration value
  gammaval <- sapply(0:noiter,function(x) pgamma(strength,a*x,scale = b))  
  res <- poisval*gammaval #the compound cdf
  sum(res,na.rm = T)
}

#estimates the compund poisson gamma cdf for a range of strength or x values
#strengthvec: the range of values for estimating the probability. If a single no, evaluates
#it for a range from 0 to sternghvec step stp. e.g.P(strength=s/l,a,b)
#l = the lambda poisson parameter
#a,b - shape andd scale parameters for the gamma distibution
#n - specifies the number of inputs, for example glomeruli, so that l will become l*n
#iter - the no of iterations foew hich we shoulld calculate this, empirically 10*l*n seems good
compoundPoissonGammavec <-function(strengthvec,l,a,b,n=1,iter=10,stp=.01){
  if (length(strengthvec) == 1) seqiter <- seq(0,strengthvec,stp)
  else seqiter <- strengthvec
  #the probabiliy 
  rateprob <- sapply(seqiter,function(x) compoundPoissonGamma(strength = x,l,a,b,n,
                                                              iter = iter))
}

#l: the poisson parameter
#a,b: gamma shape and scale factors
#maxs: is the maxs strength for which we should calculate the probability, P(strength=maxs)
#op=1,get the gaussian parameters
convertCompPoisGamma2Gaussian <-function(l,a,b,n=1,stp=.01,maxs=4,op=1){
  #generate a compound poisson gamma vec
  tmp <- compoundPoissonGammavec(maxs,l=l,a = a,b=b,n=n,stp = stp)
  #now, fit it to a Gaussian and return the params
  res <- NlsFitCDF(cbind.data.frame(seq(0,maxs,stp),tmp),1,op=1)
}

#l: the poisson parameter
#a,b: gamma shape and scale factors
#maxs: is the maxs strength for which we should calculate the probability, P(strength=maxs)
#top: specifies the top % wanted
#op=1,get the gaussian parameters
convertCompPoisGamma2GaussianTopn <-function(l,a,b,n=1,stp=.01,maxs=4,top=.1,op=1){
  #generate a compound poisson gamma vec
  tmp <- compoundPoissonGammavec(maxs,l=l,a = a,b=b,n=n,stp = stp)
  #now, fit it to a Gaussian and return the params
  res <- FindMeanVarTopnCDF(cbind.data.frame(seq(0,maxs,stp),tmp),topn = top,op = 1)
  #res <- NlsFitCDF(cbind.data.frame(seq(0,maxs,stp),tmp),1,op=1)
}
#will fit x and y vectors to a power law of the form y = a x^b, and will return a,b
# and the model
#return: the model fit
#op 1: return the values of a and b, 2 - return the lm model fit,, op = 3, summary of fit
#4 - get 95 % confidence interval of the fit, i.e the slope
#5 - get R2 for custom fit
fitpowlaw <- function(x,y,params=c(0,0),op=1){
  #this works by taking log and converting to a log-log plot where the equation becomes
  # log10(y) = log10(a) +b*log10(x), where cof1 = log10(a), cof2 = b
  #first fit the model
  powmod <- lm(log10(y)~log10(x)) # logy = log(a) + b.log(x)
  cof <- coef(powmod)
  cof[1] <- 10^cof[1] # so, a = 10^cof1
  slopvar <- names(cof)[2]
  names(cof) <- c('a','b')
  switch(op,cof,powmod,summary(powmod),confint(powmod,slopvar,level = .95),
         GetR2Custom(data.frame(log10(x),log10(y)),params=c(log10(params[1]),params[2])))
}


#will fit x and y vectors to a power law of the form y = a b^x, and will return a,b
# and the model
#return: the model fit.
#op 1: return the values of a and b,2 - return the lm model fit, op = 3, summary of fit
#4 - get 95 % confidence interval of the fit, i.e the slope
#5 - get R2 for custom fit
fitexplaw <- function(x,y,params=c(1,1),op=1){
  #this works by taking log and converting to a log-log plot where the equation becomes
  # log10(y) = log10(a) +(b/log(10))*x, where cof1 = log10(a), cof2 = (b/log(10))
  #first fit the model
  posns <- c(clearVecInf(y),getVecNAInf(y)) #get the posns of y that are Inf 
  vecy <- y[setdiff(1:length(y), posns)]
  vecx <- x[setdiff(1:length(y), posns)]
  #cat('\nbefore fitting exp',y,'cleared pos',posns,'vec',y[setdiff(1:length(y), posns)],'x',x,'e',length(posns))
  expmod <- lm(log10(vecy)~(vecx)) # logy = log(a) + x.log(b) 
  cof <- coef(expmod)
  slopvar <- names(cof)[2]
  cof[1] <- 10^cof[1] # so, a = 10^cof1
  cof[2] <- log(10)*cof[2] # and, b = log(10) * cof2
  names(cof) <- c('a','b')
  
  switch(op,cof,expmod,summary(expmod),confint(expmod,slopvar,level = .95),
         GetR2Custom(data.frame(x,log10(y)),params=c(log10(params[1]),params[2]/log(10))))
  #GetR2Custom(data.frame(x,log10(y)),params=c(log10(cof[1]),cof[2]/log(10))))
}

#will fit x and y vectors to a power law of the form y = a e^(bx), and will return a,b
# and the model
#return: the model fit.
#op 1: return the values of a and b,2 - return the lm model fit, op = 3, summary of fit
#4 - get 95 % confidence interval of the fit, i.e the slope
#5 - get R2 for custom fit
fitexpolaw <- function(x,y,params=c(1,1),op=1){
  #this works by taking log and converting to a log-log plot where the equation becomes
  # log(y) = log(a) +(b*x), where cof1 = log10(a), cof2 = (b/log(10))
  #first fit the model
  expmod <- lm(log(y)~(x)) # logy = log(a) + x.log(b) 
  cof <- coef(expmod)
  #cat('coefficients are ',cof)
  slopvar <- names(cof)[2]
  cof[1] <- exp(1)^cof[1] # so, a = 10^cof1
  cof[2] <- cof[2] # and, b =  cof2
  names(cof) <- c('a','b')
  switch(op,cof,expmod,summary(expmod),confint(expmod,slopvar,level = .95),
         GetR2Custom(data.frame(x,log(y)),params=c(log(params[1]),params[2]/log(10))))
  #GetR2Custom(data.frame(x,log10(y)),params=c(log10(cof[1]),cof[2]/log(10))))
}



#will fit x and y vectors to a power law of the form y =a+bx, and will return a,b
# and the model. If the vectors are empty return(0)
#return: the model fit
#op 1: return the values of a and b,2 - return the lm model fit, op = 3, summary of fit
#4 - get 95 % confidence interval of the fit, i.e the slope
#5 - get R2 for custom fit, 6 - plot the points
fitlinearlaw <- function(x,y,params=c(),op=1){
  #cat('\nfitlinearlaw',x,y)
  if(length(x)==0 || length(y)==0) return(c(F,F))
  y <- unlist(y) #convert y to a vector if it is a DF
  #first fit the model
  linmod <- lm((y)~(x)) # y = ax + b 
  cof <- coef(linmod)
  slopvar <- names(cof)[2]
  names(cof) <- c('a','b')
  #GetR2Custom(data.frame(log10(x),log10(y)),params=c(log10(params[1]),params[2]))
  res <- switch(op,cof,linmod,summary(linmod),confint(linmod,slopvar,level = .95),
         GetR2Custom(data.frame(x,y),params=c(params[1],params[2])))
  res
  #GetR2Custom(data.frame(x,y),params=c(cof[1],cof[2])))
}

#given a list of params, with each list item, will create the appropriate function
#params.ls: the function parameters as a list 
#op: 1, a power law function, 2 - linear function
makeFnEqnList <- function(params.ls,op=1){
  fn.ls <- switch(op,
                  lapply(params.ls, function(y){#ax^b
                    fn <- function(x) y[1]*x^y[2]
                  }),
                  lapply(params.ls, function(y){#ax + b
                    fn <- function(x) y[1] + y[2]*x
                  })
  )
  fn.ls
}


#given a two-column matrix, will do a linear fit and draw the plot
#dat: 2-column matrix of the data
#cols: the columns to be compared and plotted
#op= fixop = 1  
drawLinearPlot <-function(dat,cols=c(1,2),ticknox=5,ticknoy=5,xlabel='',ylabel='',fixx=1,
                          fixy=c(),op=1){
  cat(str(dat))
  pars <- fitlinearlaw(dat[,cols[1]],dat[,cols[2]])
  if(is.null(fixx) && is.null(fixy)) { #no limits specified
    fploteq(dat[,cols[1]],dat[,cols[2]],eq = function(x) pars[1]+pars[2]*x,xlabel = xlabel,
            ylabel = ylabel,ticknox = ticknox,ticknoy = ticknoy)
  }
  else { #limits specificied
    fploteq(dat[,cols[1]],dat[,cols[2]],eq = function(x) pars[1]+pars[2]*x,xlabel = xlabel,
            ylabel = ylabel,ticknox = ticknox,ticknoy = ticknoy,fixx = fixx,fixy = fixy,fixop = op)
  }
  res <- fitlinearlaw(dat[,cols[1]],dat[,cols[2]],op=3)
  res1 <- fitlinearlaw(dat[,cols[1]],dat[,cols[2]],op=4) #CI
  vars <- c('inter','slope','r2','fstat','CI l','CI r','p-value')
  res <- c(pars,res$r.squared,res$fstatistic[1],res1[1],res1[2],res$coefficients[2,4])
  rbind(vars,res)
}



#given a list of x and y data, will return the best fit eqn based on the fit
#op=1, linear fit, 2 - power law, 3 - exponential
getFitEqn <- function(dat,op=1){
  #based on the condn do the fit
  if (op == 1) dfit <- fitlinearlaw(dat[,1],dat[,2])
  if (op == 2) dfit <- fitpowlaw(dat[,1],dat[,2])
  if (op == 3) dfit <- fitexplaw(dat[,1],dat[,2])
  switch(op,function(x) dfit[2]*x + dfit[1],function(x) dfit[1]*x^dfit[2],
         function(x) dfit[2]*x + dfit[1]) #return the fit as a fn eqn
}

#given a list of data fits all of them and returns as a list of eqns
getFitEqnList <- function(dat.lst,op=1) {
  lapply(dat.lst,function(x) getFitEqn(x,op))
}

#given a list of x and y data, will return the best fit eqn based on the fit
#op=1, linear fit, 2 - power law, 3 - exponential
getFit <- function(dat,op=1){
  #based on the condn do the fit
  if (op == 1) dfit <- fitlinearlaw(dat[,1],dat[,2])
  if (op == 2) dfit <- fitpowlaw(dat[,1],dat[,2])
  if (op == 3) dfit <- fitexplaw(dat[,1],dat[,2])
  dfit #return the fit
}

#given a list of data fits all of them and returns as a df
getFitList <- function(dat.lst,op=1) {
  res <- sapply(dat.lst,function(x) getFit(x))
  data.frame(t(res))
}

#will fit acccording to fitlinearlaw and then plot it
#x,y: the x and y poinnts.
#params: same as fitinearlaw
#fop: fitlinearlaw options  1: return the values of a and b,2 - return the lm model fit, op = 3, summary of fit
#4 - get 95 % confidence interval of the fit, i.e the slope
#5 - get R2 for custom fit, 6 - plot the points
#op=1
plotLinearFit <- function(x,y,params=c(),fop=1,graphparams=c(),op=1){
  res <- fitlinearlaw(x=x,y=y,params = params,op = fop)
  #fploteq(x,y,eq = function(x) res[[1]] + res[[2]]*x)
  graphpar <- c(list(x,y,eq = function(x) res[[1]] + res[[2]]*x),graphparams)
  #cat('\npLF',str(graphpar))
  pltfit <- do.call(fploteq,graphpar)
  res
}

#draws a curvee or line as specified by eq from xst to xend
fiteqplot <- function(eqn,xst,xend,linewidth){
  curve(x*x,from=xst,to=xend,col="red",add=TRUE,lwd=linewidth)
}

#plot the relative cumulative frequency
#prec: the precision over which two nos are treated as the same
#input: a vector of the daata
#output: the relative cumulative frequency as 2 columns: x, rel. freq(x)
RelCumFreq <- function(vec,prec=0,op=1){
  splitlist <- split(vec,vec) #produces a list with each element containing a list of occurances
  #of that element. Split automatically produces a sorted list
  freqx <- sapply(splitlist, length) # now make a frequency list
  cumfreqx <- cumsum(freqx)/length(vec)
  x <- as.numeric(names(freqx)) # gets the unique x values
  res <- cbind(x,cumfreqx) # return as 2 columsn, data, relative frequency
  row.names(res) <- NULL
  res
}

#gets the rel cum frequency list of a bunch of 
#prec: the precision over which two nos are treated as the same
#input: a vector of the daata
#output: the relative cumulative frequency as 2 columns: x, rel. freq(x)
RelCumFreqList <- function(veclst,prec=0,op=1){
  res <- lapply(veclst,function(x){
    RelCumFreq(vec=x,prec = prec,op=op)
  })
  res
}

#generates 0 mean gaussian noise for n numbers. 
#n - length of noise vector
#noise: noise, the SD of the gaussian
#noisedist: the noisedistribution 1: gaussian, 2 - uniform
#op:1
genNoise <-function(n=1,noisedist=1,noise=0.1,noisetype=1,op=1){
  if(noisedist==0) return(rep(0,n))
  res <- switch(noisedist,rnorm(n,mean = 0,sd = noise),
                runif(n,min = 0-noise,max = 0+noise) )
  res
}

#generates 0 mean gaussian noise for rows*cols matrix. returns a matix with size:rows*cols
#n - length of noise vector
#noise: noise, the SD of the gaussian
#noisedist: the noisedistribution 1: gaussian, 2 - uniform
#op:1
genNoiseMat <-function(rows,cols,noisedist=1,noise=0.1,noisetype=1,op=1){
  res <- matrix(genNoise(n=rows*cols,noisedist = noisedist,noise = noise,noisetype = noisetype),nrow = rows)
  res
}

#generates noise for a list of matrices
#noise: noise, the SD of the gaussian
#noisedist: the noisedistribution 1: gaussian, 2 - uniform
#op=1: return a list of bnoise mattrices; 2: add noise to the list of matrices
genNoiseMatList <- function(matlst,noisedist=1,noise=0.1,noisetype=1,op=2){
  reslst <- lapply(matlst, function(x){
    noisemat <- genNoiseMat(rows = nrow(x),cols = ncol(x),noisedist = noisedist,
                     noise = noise,noisetype = noisetype) 
    if(op==1) res <- noisemat  #only return noise matrices
    else res <- x*(1+noisemat) #return the noise added matrices
    res
  })
  reslst
}


# Function to calculate mean numerically from the CDF
calculateMeanFromCDF <- function(cdf.fn, lower = -Inf, upper = Inf, step = 0.01) {
  # Define the PDF as the derivative of the CDF
  pdf.fn <- function(x) {
    (cdf.fn(x + step) - cdf.fn(x)) / step  # Numerical differentiation
  }
  
  # Numerical integration for the mean
  mean.val <- integrate(function(x) x * pdf.fn(x), lower, upper)$value
  return(mean.val)
}

#computes the mean from the CDF
computeMeanCDF <- function(cdf.fn,lower,upper,no.steps=10,op=1){
  step <- (upper-lower)/no.steps
  range.probs <- sapply(seq(lower,upper,step),function(x) x*(cdf.fn(x+step) - cdf.fn(x) ) )
  avg <- sum(range.probs)
  avg
}

#computs the probability, given the CDF of a distribution
#cdf.fn: is the cdf function
#x: the value for which we need to calculate the probability, basically the x
#delta: specifies the F(x+dx) - F(x). usually about 0.01 * mean
computeProbCDF <- function(cdf.fn,x,delta,op=1){
  prob <- cdf.fn(x+delta) - cdf.fn(x-delta)
  prob
}

#finds the mean and variance from a CDF function by first calcualting the PDF
#by usinga difference function
#data: in CDF form, x,F(x)
ConvertCDF2PDF <- function(data,op=1){
  tmp <- slidevec(data[,2],slide=1,op=1)
  probden <- data[,2]-tmp #the proability is the difference
}

#finds the mean and variance from a CDF function by first calcualting the PDF
#by usinga difference function
#data: in CDF form, x,F(x)
FindMeanVarCDF <- function(data,op=1){
  probden <- ConvertCDF2PDF(data) #the proability is the difference
  #cat('\nprobden: probden',probden)
  mn <- sum(probden*data[,1]) #mean
  std <- sqrt(sum(probden*data[,1]^2)-mn^2) #std: since var=E(X^2)-(E[X])^2
  c(mn,std)
}

#finds the mean and variance for a bunch of CDFs which are all given by the columns of 
#data.df
#output is a list of means and std
FindMeanVarCDFDf <-function(data.df,op=1){
  len <- length(data.df)
  res <- sapply(2:len,function(x,datac) FindMeanVarCDF(datac[,c(1,x)],op),data.df)
  data.frame(res)
}

#finds the mean and variance from a CDF function by first calcualting the PDF
#by usinga difference function
#data: in CDF form, x,F(x)
#topn: specifies the topn items in terms of percentage
#op=1, return mean and SD, 2 - return CDF
VarTopnCDF <- function(data,topn=1,op=1){
  #cat(topn,'FindMeanVarTopnCDF\n',topn,'\n')
  cdftopn <- data[,2][data[,2]>=1-topn*.01] #get the topn CDF, topn has to be converted to a fraction
  datatopn <- data[,1][data[,2]>=1-topn*.01] #get the topn nos
  #now do the conversion
  #cat('befre',cdftopn)
  cdftopn <- (1/(1-cdftopn[1]))*(cdftopn-cdftopn[1])
  datatopn <- datatopn-datatopn[1]
  probtopn <- ConvertCDF2PDF(data.frame(datatopn,cdftopn))
  #cat(cdftopn,'\n',datatopn,'\n',probtopn,'\n')
  mn <- sum(probtopn*datatopn) #mean
  std <- sqrt(sum(probtopn*datatopn^2)-mn^2) #std: since var=E(X^2)-(E[X])^2
  switch(op,c(mn,std),data.frame(datatopn,cdftopn))
}

FindMeanVarTopnCDF1 <- function(data,topn=.3,op=1){
  #cat(topn,'FindMeanVarTopnCDF\n')
  probden <- ConvertCDF2PDF(data) #the proability is the difference
  cdftopn <- data[,2][data[,2]>=1-topn] #get the topn CDF
  
  probtopn <- probden[data[,2]>=1-topn] #get the topn PDF
  datatopn <- data[,1][data[,2]>=1-topn] #get the topn nos
  #cat(cdftopn,'\n',datatopn,'\n',probtopn,'\n')
  mn <- sum(probtopn*datatopn) #mean
  std <- sqrt(sum(probtopn*datatopn^2)-mn^2) #std: since var=E(X^2)-(E[X])^2
  c(mn,std)
}

#finds the mean and variance for a bunch of CDFs which are all given by the columns of 
#data.df
#output is a list of means and std
#op=1, mean and sd, 2- get cdf as list of dfs
FindMeanVarTopnCDFDf <-function(data.df,topn=5,op=1){
  len <- length(data.df)
  #cat('FindMeanVarTopnCDFDf\n',len)
  #print(data.df)
  res <- lapply(2:len,function(x,datac) FindMeanVarTopnCDF(datac[,c(1,x)],topn=topn,
                                                           op=op),data.df)
  if (op==1) names(res) <- c(1,2)
  switch(op,data.frame(res),res)
}


#computes the fit params to the distribution specified
#data: the data that is to be fitted
#params: specified at the time of the call
#dist: the distribution in question that is to be fitted
#1 - normal, 2 - gamma, 3 - lognormal, 4 - exponential, 5 - uniform disttribution, 6 - top half normal distribution
# 7 -mixture gaussian gamma model
computeFitParams <-function(data,params=c(),dist=1){
  if(length(params)==0){
    res <- FindMeanVarCDF(data)
    mn <- res[1]
    sdev <- res[2]
    if(dist==1) {
      res1 <- mn
      res2 <- sdev
    }
    if(dist==2){
      res1 <- mn^2/sdev^2
      res2 <- sdev^2/mn
    }
    if(dist==3){
      res1 <- log(mn/sqrt(1+(sdev^2/mn^2) ))
      res2 <- sqrt(log(1+(sdev^2/mn^2) ))
    }
    if(dist==4){
      res1 <- 1/mn
      res2 <- 0
    }
    if(dist==5){#uniform; get min and max of data, not rel cum freq data
      #cat('nhere',min(data),'\n',data,'\n')
      res1 <- min(data[,1])
      res2 <- max(data[,1])
    }
    if(dist==6){#top half of normal distribution
      res1 <- 0.75 #assume you want the top 25th percentile
      res2 <- sdev #first guess
    }
    if(dist==7){#mixture gaussian gamma model
      init.values <- c(
        mu = mn,          # Initial guess for mean of Gaussian
        sigma = sdev,         # Ensure sigma starts as a positive value
        alpha = 2,               # Ensure alpha is positive
        beta = 1 / mn,    # Ensure beta is positive
        pi = 0.5                 # Initial mixing proportion
      )
      cat('\nfitparams',init.values)
      return(init.values)
    }
    
    #cat('computefitparams',res1,res2,'\n')
    return(c(res1,res2))
  }
  params  
}

#converts the normal dist parameters to shape and scale params
convertNorm2Gamma <- function(mn,std,op=1){
  c((mn/std)^2,(std^2)/mn)
}

#converts Gamma distribution poarameters to normal distribution parameters
convertGamma2Norm <- function(shap,scal,op=1){
  c(shap*scal,scal*sqrt(shap))
}

#this retuirns a statistics function with the required params
#which can then be used by any polot function
statsFun <- function(dist=1,param,op=1){
  params <- as.vector(param) #get rid of named vectors as that screws things up
  #names(params) <- ''
  #cat(str(param),str(params))
  fnx <- function(x) switch(dist,do.call(pnorm,as.list(c(q=x,c(params)))),
                            do.call(pgamma,as.list(c(q=x,shape=params[1],scale=params[2]))),
                            do.call(plnorm,as.list(c(q=x,c(params)))),
                            do.call(pexp,as.list(c(q=x,c(params)))))
  plfn <- function(x) sapply(x,fnx) # so that it will apply to a vector
  plfn
}

#will do a non-linear squares fit for a gamma function. 
#input is the data, the distribution, param guesses
#data: a vector of the data, if 2 cols, then data frame of Rel. Cum. Freq.
#dist - 1, normal, 2 - gamma, 3 - lognormal, 4- exponential, 5 uniform, 6 - top half normal,
#7 - gaussian gamma mixture model, 8, fit scubic spline, 9, Iso cubic spline
#params: specifies a guess for the distribution fit, and if it is fittype=2, this is the one
#used for plotting
#graphparams: the gra[hical parameeters as a vector or list
#fittype: 1 output is the fit by NLS, 2- method of moments, 3 - mle, 4 - use the params
#op=1,just the fits, =2 fit plot, =3 fit stats , 4- plot fit function
NlsFitCDF <- function(data,dist=1,params=c(),graphparams=c(),fittype=1,markersize=0.7,op=2){
  if(is.vector(data)) reldata <- RelCumFreq(data) #get the RelCuFreq
  else reldata <- data
  #get fit params for initial guess
  fitparams <- computeFitParams(reldata,params=params,dist=dist) 
  #cat(data)
  #cat(length(data),'fitparams',fitparams)
  #assumes that your data is in columnar format
  resfit <- switch (dist, #get the best fit params
                 NlsFitNormalCDF(reldata,fitparams,op = fittype),
                 NlsFitGammaCDF(reldata,fitparams,op = fittype),
                 NlsFitLogNormalCDF(reldata,fitparams,op = fittype),
                 NlsFitExponentialCDF(reldata,fitparams,op = fittype),
                 NlsFitUniformCDF(reldata,fitparams,op = fittype),
                 NlsFitTopNormalCDF(reldata,fitparams,op = fittype),
                 #NLSFitMixtureModelCDF(reldata,fitparams,op=fittype)
                 NLSFitMixtureModelCDF(data,op=fittype),
                 NLSFitSplineCDF(data,op=fittype),
                 NLSFitIsoCDF(data,op=fittype)
  )
  #cat(str(fit))
  #resfit <- summary(fit)$coefficients #extract the best fit params
  #cat('\nresfit',str(resfit),dist!=8 || dist!=9)
  bestfit <- checkCond(dist<8,resfit[,1],resfit[[1]])
  #bestfit <- resfit[,1]
  #cat('\nbestfit',bestfit)
  names(bestfit) <- NULL # turn off the names since we need this in the fn call
  if (op ==1 ) return(bestfit) #if op=1, just return the fit params
  if (op ==3 ) return(resfit) #if op=3, just return the fits
  
  if(fittype == 4) bestfit <- params #if you want to plot the user defined params NlsFitTopNormalCDF
  #cat('best fit',bestfit,'\n')
  fnx <- function(x) switch(dist,do.call(pnorm,as.list(c(q=x,c(bestfit)))),
                            do.call(pgamma,as.list(c(q=x,shape=bestfit[1],scale=bestfit[2]))),
                            do.call(plnorm,as.list(c(q=x,c(bestfit)))),
                            do.call(pexp,as.list(c(q=x,c(bestfit)))),
                            do.call(punif,as.list(c(q=x,min=bestfit[1],max=bestfit[2]))),
                            do.call(pnorm,as.list(c(q=x+bestfit[1],sd=c(bestfit[2])))) - pnorm(q=bestfit[1]),  #subtract the CDF from the a'th percentile pdf.
                            bestfit[5] * gaussian.cdf(x, bestfit[1], bestfit[2]) + 
                              (1 - bestfit[5]) * gamma.cdf(x, bestfit[3], bestfit[4]),
                            resfit[[2]]) 
  plfn <- checkCond(dist < 8,function(x) sapply(x,fnx),resfit[[2]]) # so that it will apply to a vector
  #plfn <- function(x) sapply(x,fnx) # so that it will apply to a vector
  if (op == 4) return(plfn) #return the plot fitting function
  #adds the user-specified params to the default ones
  graphpar <- c(list(reldata[,1],reldata[,2],plfn,fixy=c(0,1),markersize=markersize,
                     logs=F),graphparams)
  pltfit <- do.call(fploteq,graphpar)
  #pltfit <- fploteq(reldata[,1],reldata[,2],plfn,ticknoy=50,fixy=c(0,1),ticknox = 10,logs=F)
  bestfit
}

#this plots the cum freq. lst of the given data. No fitting done
cumFreqPlot <- function(dat,graphparams=c(),op=1){
  #clacualte the cumultative frequency list and then plot the returned DF
  dat.cum <- RelCumFreq(dat)
  #fploteq.DF(dat.cum)
  do.call(fploteq.DF,c(list(dat.cum),graphparams))
}

#plot the average fit for a list of vectors
#dist: 1, normal, 2 - gamma, 3 - lognormal, 4 - expoontial
#op=1, aggrgate all the the lists into one and fit that
#op=2, take the mean of the fits of every vector and plot that
plotListAvgFit <- function(mat,dist=2,op=1){
  resplst <- computeLinMatCum(mat)
  #for op=2, you have to get the average of the individual fits
  fits.df <- convertNestedListsDF(NlsFitCDFList(resplst,dist = dist,op=1))
  fits.mean <- apply(fits.df,1,mean)
  cat('means',fits.mean)
  tmpfn <- switch(op,NlsFitCDF(unlist(resplst),dist = dist,op=4),
                  statsFun(dist=dist,params = fits.mean))
  #get the fit function
  relcumlst <- RelCumFreqList(resplst)
  #now plot it, after getting 
  fploteq(relcumlst,eq = tmpfn,plottype = 8,markersize = .75)
  
}

#plots the cumulative frequency fit for a given vector, where the params 
#and fit are specified
plotCumFreqFit <- function(dat.vec,fitop,params,op=1){
  
}

#same as earlier function but you can do it for lists
#will do a non-linear squares fit for a gamma function. 
#input is the data, the distribution, param guesses
#data: a vector of the data, if 2 cols, then data frame of Rel. Cum. Freq.
#dist - 1, normal, 2 - gamma, 3 - lognormal, 4- exponential, 5 - uniform
#params: specifies a guess for the distribution fit
#graphparams: the gra[hical parameeters as a vector or list
#output is the fit by NLS
#fittype: 1 output is the fit by NLS, 2- method of moments, 3 - mle, 4 - user defined params
#colorop= 1, points in grey, curves in black, 2 - all diff colors
#op=1,just the fits, =2 fit plot, =3 fit stats , 4- plot fit function
NlsFitCDFList <- function(data,dist=1,params=c(),graphparams=c(),fittype=1,colorop=1,op=2){
  if(op==1 || op==3){ #if they want  a fit return that
    fitlist <- lapply(data,function(x){ #get the best fit params
      #cat(length(x),'\n')
      tmp <- NlsFitCDF(x,dist = dist,params = params,graphparams = graphparams,
                       fittype = fittype,op=op)
    })
    return(fitlist)
  }
  fnxlist <- lapply(data,function(x){#get the plot fit functions
    tmp <- NlsFitCDF(x,dist = dist,params = params,graphparams = graphparams,
                     fittype = fittype,op=4)
  })
  if(op==4) return(fnxlist)
  #get the relative cumulative frequenxy data
  relcumlist <- lapply(data,function(x){#get the best fit functions
    if(is.vector(x)) reldata <- RelCumFreq(x) #get the RelCuFreq
    else reldata <- x
    reldata
  })
  #adds the user-specified params to the default ones
  if(colorop==1) plottype <- 8
  else plottype <- c() #go with default all colors
  graphpar <- c(list(x=relcumlist,eq=fnxlist,fixy=c(0,1),
                     logs=F,plottype=plottype),graphparams) #plottype 8 points in grey, curves in black
  #cat(str(graphpar))
  #print(graphpar[[1]][[1]])
  pltfit <- do.call(fploteq,graphpar)
  #pltfit <- fploteq(reldata[,1],reldata[,2],plfn,ticknoy=50,fixy=c(0,1),ticknox = 10,logs=F)
  #fitlist
}

mleFnGamma <- function(data,params,op=1){
  -sum(dgamma(data,shape = params[1],scale = params[2]))
}

#function for the mle of a truncated normal function. Might not need it
llTNormal <- function(data,op=1){
  
}

#will do a non-linear squares fit for a normal function. 
#input is scale and shape guesses
#output returns the fit
#op=1, NLs, 2 - method of moments, 3 - mle
NlsFitNormalCDF <- function(data,params,op=1){
  #assumes that your data is in columnar format
  x <- data[,1]
  y <- data[,2]
  #cat(x,'\n',y,'\n',params,'\n')
  fit <- nls(y~pnorm(x,mean = a,sd = b),start = list(a = params[1], b = params[2]),
             trace = F,algorithm = 'port',lower=c(params[1]*(1-.05),params[2]*(1-.05)),
             upper = c(params[1]*(1+.05),params[2]*(1+.05)))
  fit <- summary(fit)$coefficients #return the fit coeeficients and errors
  fit
}


#will do a non-linear squares fit for a gamma function. 
#params: input is scale and shape guesses
#data: data in RElcumFreq format of 2 columns
#output is the fit by NLS
#op=1, use NLS, 2 - use method of moments
NlsFitGammaCDF <- function(data,params,op=1){
  #assumes that your data is in columnar format
  x <- data[,1]
  y <- data[,2]
  if(op==1){#nls fit
    #fixed error or check so that b is never 0 so you never get NANs, algorithm has to be port
    #this is done by using the lower option of the pgamma function: liam
    #               start = list(a = params[1], b = params[2]),

    fit <- nls(y~pgamma(x,shape = a,scale = b),
               start = list(a = params[1], b = params[2]),
               lower = list(a = 0, b = .001),trace = F,algorithm = 'port')
    fit <- summary(fit)$coefficients #return the fit coeeficients and errors
  }
  if(op==2 || op == 4){#method of moments
    fit <- data.frame(params,params)    
  }
  if(op==3){#mle, do the optim fit and then return the parameters
    tmp <- optim(params,mleFnGamma,data=data[,2],method = 'BFGS')
    fit <- data.frame(tmp$par,tmp$counts)
  }
  fit
}

#will do a non-linear squares fit for a gamma function. 
#input is scale and shape guesses
#output is the fit by NLS
NlsFitLogNormalCDF <- function(data,params,op=1){
  #assumes that your data is in columnar format
  x <- data[,1]
  y <- data[,2]
  fit <- nls(y~plnorm(x,meanlog = a,sdlog = b),start = list(a = params[1], b = params[2]),
             lower=list(a=0.00001,b=0.00001),upper = list(a=10000,b=10000),trace = F,algorithm = 'port')
  fit <- summary(fit)$coefficients #return the fit coeeficients and errors
  fit
}

#will do a non-linear squares fit for a gamma function. 
#input is scale and shape guesses
#output is the fit by NLS
NlsFitExponentialCDF <- function(data,params,op=1){
  #assumes that your data is in columnar format
  x <- data[,1]
  y <- data[,2]
  #we re avoiding negative values here, lambda > 0 
  fit <- nls(y~pexp(x,rate = a),start = list(a = params[1]),lower=list(a=0),trace = F,algorithm='port')
  fit <- summary(fit)$coefficients #return the fit coeeficients and errors
  fit
}


#will do a non-linear squares fit for a uniform function. 
#input is min and max
#output is the fit by NLS
NlsFitUniformCDF <- function(data,params,op=1){
  #assumes that your data is in columnar format
  x <- data[,1]
  y <- data[,2]
  #cat('Nlsfiruniformcdf',params,'\n')
  fit <- nls(y~punif(x,min = a, max = b),start = list(a = params[1],b=params[2]),lower=list(a=-Inf,b=-Inf),trace = F,algorithm = 'port')
  fit <- summary(fit)$coefficients #return the fit coeeficients and errors
  fit
}

#will do a non-linear squares fit for the top part of a normal function. 
#a is the percentile part, from 50 to 99 % or 0.5 to 0.99, b is the sd 
#output is the fit by NLS
NlsFitTopNormalCDF <- function(data,params,op=1){
  #assumes that your data is in columnar format
  x <- data[,1]
  y <- data[,2]
  #cat('Nlsfiruniformcdf',params,'\n')
  # fit <- nls(y~ pnorm(x,sd=b) - pnorm(a,sd = b),start = list(a = params[1],b=params[2]),
  #            lower=list(a=0.5,b=0),algorithm = 'port',trace = F)
  fit <- nls(y~ pnorm(x+qnorm(.85,sd=1),sd=b)-pnorm(q = .85,sd=b),start = list(b=params[2]),
             lower=list(b=0),algorithm = 'port',trace = F)
  fit <- summary(fit)$coefficients #return the fit coeeficients and errors
  fit
}


#given a vector does a fit to a nonlinear Mixture Gaussian Gamma model
#vec is the data, a vector of values
# Function to compute Gaussian CDF
gaussian.cdf <- function(x, mu, sigma) {
  pnorm(x, mean = mu, sd = sigma)
}

# Function to compute Gamma CDF
gamma.cdf <- function(x, alpha, beta) {
  pgamma(x, shape = alpha, rate = beta)
}

# Main function to fit a Gaussian-Gamma mixture model using relative cumulative frequency
NLSFitMixtureMod.nls <- function(data) {
  # Get relative cumulative frequencies using your function
  rel.cum.freq.data <- RelCumFreq(vec)
  x.vals <- rel.cum.freq.data[, 1] # Extract x values
  rel.cum.freq <- rel.cum.freq.data[, 2] # Extract cumulative frequencies
  
  # Adjusted initial parameter estimates
  init.values <- list(
    mu = mean(vec),          # Initial guess for mean of Gaussian
    sigma = sd(vec),         # Ensure sigma starts as a positive value
    alpha = 2,               # Ensure alpha is positive
    beta = 1 / mean(vec),    # Ensure beta is positive
    pi = 0.5                 # Initial mixing proportion
  )
  
  # Nonlinear least squares fitting function with adjusted bounds
  fit.model <- nls(
    formula = rel.cum.freq ~ (pi * gaussian.cdf(x.vals, mu, sigma)) + 
      ((1 - pi) * gamma.cdf(x.vals, alpha, beta)),
    start = init.values,
    algorithm = "port",
    lower = c(mu = -Inf, sigma = 0.01, alpha = 0.1, beta = 0.1, pi = 0.01),
    upper = c(mu = Inf, sigma = Inf, alpha = Inf, beta = Inf, pi = 0.99)
  )
  
  # Extract estimated parameters
  params <- summary(fit.model)$parameters
  return(params)
}


# Log-likelihood function for Gaussian-Gamma mixture model
logLikelihoodNormGamma <- function(params, x, rel.cum.freq) {
  mu <- params[1]
  sigma <- params[2]
  alpha <- params[3]
  beta <- params[4]
  pi <- params[5]
  
  # Calculate the combined CDF using the Gaussian and Gamma components
  model.cdf <- pi * gaussian.cdf(x, mu, sigma) + (1 - pi) * gamma.cdf(x, alpha, beta)
  
  # Calculate the sum of squared errors for the fit to the cumulative frequencies
  sse <- sum((model.cdf - rel.cum.freq)^2)
  
  # Add a penalty to discourage extreme pi values near 0 or 1
  penalty <- 50 * (pi - 0.5)^2  # Adjust the factor 10 to control the strength of the penalty
  
  # Return negative of the likelihood plus the penalty to minimize in optim
  return(sse + penalty)
}

# Function to fit a spline to the cumulative frequency data
# vec: the vector or data you want to fit 
NLSFitSplineCDF <- function(vec,fitparams=c(),op=1) {
  # Get relative cumulative frequency data
  rel.cum.freq.data <- RelCumFreq(vec)
  x.vals <- rel.cum.freq.data[, 1]
  cum.freq <- rel.cum.freq.data[, 2]
  
  # Adjust endpoints: add (min(x) - small value, 0) and (max(x) + small value, 1)
  epsilon <- 1e-6 # A very small value for adjustment
  x.adjusted <- c(min(x.vals) - epsilon, x.vals, max(x.vals) + epsilon)
  cum.freq.adjusted <- c(0, cum.freq, 1)
  
  # Fit a cubic spline to the adjusted empirical CDF
  spline.fit <- smooth.spline(x.adjusted, cum.freq.adjusted)
  
  # Define a function that uses the spline fit to evaluate the CDF at new points
  cdf.spline <- function(x) {
    y <- predict(spline.fit, x)$y
    # Ensure that the values are bounded between 0 and 1
    pmin(pmax(y, 0), 1)
  }
  # Return the fitted spline and the CDF function
  list(spline.fit = spline.fit, cdf.spline = cdf.spline)
}


# Function to fit a monotonic CDF using isotonic regression
NLSFitIsoCDF <- function(vec,fitparams=c(),op=1) {
  # Get relative cumulative frequency data
  rel.cum.freq.data <- RelCumFreq(vec)
  x.vals <- rel.cum.freq.data[, 1]
  cum.freq <- rel.cum.freq.data[, 2]
  
  # Fit an isotonic regression to ensure monotonicity
  iso.fit <- isoreg(x.vals, cum.freq)
  
  # Define a function that uses the isotonic fit to evaluate the CDF at new points
  cdf.iso <- function(x) {
    y <- approx(iso.fit$x, iso.fit$y, xout = x, rule = 2)$y
    # Ensure that the values are bounded between 0 and 1
    pmin(pmax(y, 0), 1)
  }
  
  # Return the fitted isotonic regression and the CDF function
  list(iso.fit = iso.fit, cdf.iso = cdf.iso)
}

# Main function to fit a Gaussian-Gamma mixture model using relative cumulative frequency
NLSFitMixtureModelCDF <- function(vec,fitparams=c(),op=1) {
  if(length(fitparams)==0){
    # Get relative cumulative frequencies using your function
    rel.cum.freq.data <- RelCumFreq(vec)
    x.vals <- rel.cum.freq.data[, 1] # Extract x values
    rel.cum.freq <- rel.cum.freq.data[, 2] # Extract cumulative frequencies
  } else {
    x.vals <- vec[,1]
    rel.cum.freq <- vec[,2]
  }
  # Initial parameter estimates
  if (length(fitparams)==0) init.values <- c(
    mu = mean(vec[vec < median(vec,na.rm = T)],na.rm = T),          # Initial guess for mean of Gaussian
    sigma = sd(vec[vec < median(vec,na.rm = T)],na.rm = T),         # Ensure sigma starts as a positive value
    alpha = 2,               # Ensure alpha is positive
    beta = 1 / mean(vec,na.rm = T),    # Ensure beta is positive
    pi = 0.5                 # Initial mixing proportion
  ) else init.values <- fitparams
  
  #cat('\ninit.values',init.values)
     
  # Fit the model using optim
  fit.model <- optim(
    par = init.values,
    fn = logLikelihoodNormGamma,
    x = x.vals,
    rel.cum.freq = rel.cum.freq,
    method = "L-BFGS-B", # Bounded optimization method
    lower = c(mu = -Inf, sigma = 0.01, alpha = 0.1, beta = 0.1, pi = 0.01),
    upper = c(mu = Inf, sigma = Inf, alpha = Inf, beta = Inf, pi = 0.99),
    control = list(fnscale = 1) # We multiply by -1 to maximize the likelihood
  )
  
  # Extract the fitted parameters
  fitted.params <- fit.model$par
  #print(fit.model)
  names(fitted.params) <- c("mu", "sigma", "alpha", "beta", "pi")
 
  res <- cbind(fitted.params,fitted.params) 
  res
}


# Function to calculate log-likelihood of a Gaussian fit
logLikelihoodGaussian <- function(data) {
  mu <- mean(data)
  sigma <- sd(data)
  sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))
}

# Function to calculate log-likelihood of a Gamma fit
logLikelihoodGamma <- function(data) {
  fit <- fitdistrplus::fitdist(data, "gamma")
  shape <- fit$estimate["shape"]
  rate <- fit$estimate["rate"]
  sum(dgamma(data, shape = shape, rate = rate, log = TRUE))
}

# Function to find the best change point
findChangePoint <- function(vec, minCp = 180, maxCp = 300) {
  vec <- cleanNAVec(vec)
  bestCp <- NULL
  bestLl <- -Inf
  
  for (cp in seq(minCp, maxCp, by = 1)) {
    # Data before and after the candidate change point
    dataBefore <- vec[vec <= cp]
    dataAfter <- vec[vec > cp]
    
    # Calculate the combined log-likelihood
    llGaussian <- logLikelihoodGaussian(dataBefore)
    #llGamma <- logLikelihoodGaussian(dataAfter)
    llGamma <- logLikelihoodGamma(dataAfter)
    totalLl <- llGaussian + llGamma
    
    # Track the best change point
    #cat('\ntat',totalLl,bestLl,llGamma,llGaussian,'\ncp',cp,dataBefore)
    if (totalLl > bestLl) {
      bestLl <- totalLl
      bestCp <- cp
    }
  }
  
  return(list(bestCp = bestCp, bestLl = bestLl))
}


#check the goodness of fit for a distribution
#op=1, chi square test, 2 - ks test, 3 - cramer vonmisses, 4 - do all three
checkGoodnessFit <- function(dat,fitfn,breaks=50,params=c(),op=1){
  
  if(op==1 || op ==4){
    tmp <- hist(dat,breaks = breaks,probability = T)
    tmp1 <- fitfn(tmp$breaks[-1])-fitfn(tmp$breaks[1:(length(tmp$breaks)-1)])
    cat(tmp$density[1:10],'\n',tmp1[1:10],'\n',
        fitfn(.5*tmp$breaks[1:(length(tmp$breaks)-1)])[1:10],sum(tmp$density))
    tmp1 <- tmp1/10#sum(tmp1)
    #cat(length(tmp$counts),length(tmp1))
    res.chi <- chisq.test(tmp$counts,p=tmp1,rescale.p = T)
    #lines(density(dat))
    lines(tmp$breaks[-1],tmp1*.5)
  }
  if(op==2 || op==4){
    res.ks <- ks.test(dat,rgamma(length(dat),shape = params[1],scale = params[2]))
  }
  switch(op,res.chi,res.ks,res.chi,list(res.chi,res.ks))
}


#gets the coeff of determination, r2 for a custom fit given by params
#computes the coeeficient of determination
#data is in the form of a 2 column matrix or df
#params is of the form c(a,b) where y = a + b*x
#output: {r2,sstot,ssreg,ssres}
GetR2Custom <- function(data,params,op=1){
  ybar <- mean(data[,2]) # mean of the data
  sstot <- sum((data[,2]-ybar)^2) # total sum of squares
  #get the fn values based on the params
  fn <- params[1] + params[2]*data[,1]
  ssreg <- sum((fn-ybar)^2) #regression sum of squares
  ssres <- sum((data[,2]-fn)^2) # residual sum of squares
  c(R2=1-ssres/sstot,sstot=sstot,ssreg=ssreg,ssres=ssres,"params"=params[1:2])
}

#computes the coeeficient of determination
#data is in the form of a 2 column matrix or df
#if dist - 1, normal, 2 - gamma, 3 - lognormal
#output: {r2,sstot,ssreg,ssres}
ComputeCoeffDetCDF <- function(data,dist,params,op=1){
  ybar <- mean(data[,2]) # mean of the data
  sstot <- sum((data[,2]-ybar)^2) # total sum of squares
  #fit the fn, get params, and then the fn based CDF values
  fit.fn <- NlsFitCDF(data,dist,params,op=2)
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

#check whether the data fulfills the constraints imposed by the particular distribution
#data: a list of vectors that need to be fitted and checked if they fulfill the constraints required to be maxiumum
#entropy distributions
#type of fit for NLsfitCDF
#fittype: 1 output is the fit by NLS, 2- method of moments, 3 - mle
#dist - 1, normal, 2 - gamma, 3 - lognormal, 4- exponential
checkDataDistConstraint <- function(data,dist=1,fittype=1,op=1){
  fitlst <- NlsFitCDFList(data = data,dist = dist,fittype = fittype,op=1)
  #cat(str(fitlst))
  #now, test for each of the distributions
  #normal, means are similar and so are the standard deviations
  if(dist>=1 && dist <= 3){
    fit.df <- transposeDF(convertNestedListsDF(fitlst)) #convert to DFF and trasnpose it
    fit.df <- insertColDf(data.df = fit.df,newcol = 1:length(fitlst),posn = 1)
    rownames(fit.df) <- 1:length(fitlst) #set the colnames as indices
    if(dist==1){#normal check for constraint
      mn <- sapply(data,function(x) mean(x)) #the mean
      std <- sapply(data,function(x) sd(x)) #the std
      fit.df <- insertColDf(data.df = fit.df,newcol = mn,posn = 4)
      fit.df <- insertColDf(data.df = fit.df,newcol = std,posn = 5)
      names(fit.df) <- c('index','mean','sd','mean','std')
    }
    if(dist==2){#gamma check for constraint
      ab <- sapply(fitlst,function(x) x[1]*x[2])#shape*scale
      mn <- sapply(data,function(x) mean(x)) #the mean
      lgmean <- sapply(data,function(x) mean(log(x))) #log mean
      dglogfn <- sapply(fitlst,function(x) digamma(x[1]) + log(x[2])) #the function
      fit.df <- insertColDf(data.df = fit.df,newcol = mn,posn = 4)
      fit.df <- insertColDf(data.df = fit.df,newcol = ab,posn = 5)
      fit.df <- insertColDf(data.df = fit.df,newcol = lgmean,posn = 6)
      fit.df <- insertColDf(data.df = fit.df,newcol = dglogfn,posn = 7)
      names(fit.df) <- c('index','scale','shape','mean','k*theta','mean(log)','digamma+log')
    }
    
  }
  #cat(names(fit.df),1:length(fitlst))
  fit.df 
}

#for a distribution, any distribution, brek the distribution into n intervals and return the 
#the points within the distribution that form these intervals
#distvec: the vector
#n: the values of the intervals
genCumulativeList <-function(distvec,n,op=1){
  cumlist <- RelCumFreq(vec = distvec)
  #cat(ceiling(seq(0,length(distvec),length(distvec)/n)),'\n',cumlist[1:length(cumlist[,1])/n,1],'\n')
  if(n>length(cumlist[,1])) res <- cumlist[,1]/cumlist[length(cumlist[,1]),1]
  else res <- cumlist[ceiling(seq(0,length(cumlist[,1]),length(cumlist[,1])/n)),1]/cumlist[length(cumlist[,1]),1]
  res <- rev(1-res)
  res
}

#plot a cumulative histogram plot for a vector without the fitted line
#used for plotting
#graphparams: the gra[hical parameeters as a vector or list
#op=1,default
plotCumulativeHist <- function(dat.vec,graphparams=c(),markersize=1,op=1){
  if(is.vector(dat.vec)) reldata <- RelCumFreq(dat.vec) #get the RelCuFreq
  else reldata <- dat.vec
  
  #print(reldata)
  #adds the user-specified params to the default ones
  graphpar <- c(list(reldata[,1],reldata[,2],fixy=c(0,1),markersize=markersize,
                     logs=F),graphparams)
  do.call(fploteq,graphpar)
  
}


#a discrete form of the normal distribution
#x gives the position for which the probability needs to be calculated. It will return the probability of (OR)
# calculate the density between the interval x[1] and x[2]
#being found in ther interval xposn < x < xposn + delta, where xposn falls on a delta boundary
#delta: the interval over which the PDF is calculated
#wifth: of 95 % of the distribution,
#mn and sd: parameters of the distribution
#op=2- density along delta boundaries, 1 - between interevals
discreteNormal <- function(x,mn=0,std=1,width=c(),delta=0.1,op=1){
  norm.width <- qnorm(p=0.975,sd = std) - qnorm(p=0.025,sd = std) #width of 95 % of a standard normal or one with sd=std
  if(length(width)==0) adj.sd <- 1 #adjust the sd, depending on the width
  else  adj.sd <- width/norm.width 
  if(op==1){#calculate the normal between the range specified
    prob <- pnorm(q=x[2],mean = mn,sd = adj.sd) - pnorm(q=x[1],mean = mn,sd = adj.sd)
  }
  if(op==2){#if we calculate the density along delta boundaries
    no <- floor(width/delta) #no of points to consider
    xposn <- floor(x/delta)  * delta #the xposn detects which delta number you fall in from the the origin
    #consider odd and even number of points separately, as probabilities should be symmetric around 0.
    prob <- pnorm(q=xposn+delta,mean = mn,sd = adj.sd) - pnorm(q=xposn,mean = mn,sd = adj.sd)
  } 
  prob
}
#result: one of the things while sampling this distribution is to make sure that the delta for sampling matches the delta here.
#if the delta for sampling is twice the delta here then you are sampling only half the points, so summing over the whole 
#distribution will be 1/2. If the delta for sampling is half the delta, then you are sampling twice and the sum will be 2.

#iterrate through and get the probability vector, which can then be used like a hash function
#gets a table of the discrete normal values
#distrange: the range of values for which we should get the normal distribution. c(start,finish)
discreteNormalRange <- function(distrange,mn=0,std=1,width=c(),delta=0.1,op=1){
  #setup stuff here
  norm.width <- qnorm(p=0.975,sd = std) - qnorm(p=0.025,sd = std) #width of 95 % of a standard normal or one with sd=std
  if(length(width)==0) adj.sd <- 1 #adjust the sd, depending on the width
  else  adj.sd <- width/norm.width 
  #iterrate through and get the probability vector, which can then be used like a hash function
  #make sure you iterate from mean towards both ends
  if(distrange[1] < mn && distrange[2]>mn) {#the mn is inbertween the distrange
    valrange <- sort(unique(c(distrange[1],seq(mn,distrange[1],-delta),seq(mn,distrange[2],delta),distrange[2] )) )
  }
  else{#the distrange does not include the mean
    valrange <- seq(0,distrange[2],delta)
    posns <- findIndBelowThresh(valrange,t=distrange[1])
    #cat('\ndNF',valrange,delta,':',posns,':',valrange[posns[length(posns)]:length(valrange)])
    valrange <- sort(unique(c(distrange[1],valrange[posns[length(posns)]:length(valrange)],distrange[2])) )
  }
  #cat('\ndNrange',valrange)
  res <- sapply(2:length(valrange),function(i) {
    prob <- pnorm(q=valrange[i],mean = mn,sd = adj.sd) - pnorm(q=valrange[i-1],mean = mn,sd = adj.sd)
  })
  names(res) <- valrange[-1]
  #cat('\n')
  res
}



#function that gives the cdfprobability or the percentile for a given value.
#val: the value for which the percentile is sought
#dist: #dist - 1, normal, 2 - gamma, 3 - lognormal, 4- exponential, 5 - uniform
#params: of the distribution, defauly (0,1) ofthe standard normal
#prec: of the result. 3 implies a 1000 points, 4 10000 points
#avg:breaks down the data set of size 10^prec into avg sized data sets, and averages their results
reverseQuantile <- function(val,dist=1,params=c(0,1),prec=3,avg=1,op=1){
  dat <- switch(dist,do.call(rnorm,as.list(c(n=10^prec,mean=params[1],sd=params[2]))),
                      do.call(rgamma,as.list(c(n=10^prec,shape=params[1],scale=params[2]))),
                      do.call(rlnorm,as.list(c(n=10^prec,meanlog=params[1],sdlog=params[2]))),
                      do.call(rexp,as.list(c(n=10^prec,rate=params))),
                      do.call(runif,as.list(c(n=10^prec,min=params[1],max=params[2]))))
  if(avg>1){
    size <- floor((10^prec)/avg)
    res.vec <- sapply(1:avg, function(y){
      #cat('\niter',y,'\t',((y-1)*size + 1),'\t',(y*size))
      datx <- dat[((y-1)*size + 1):(y*size)]
      ecdf(x = datx)(val)
    })
    #cat('size',size,' results: ',res.vec)
    res <- mean(res.vec)
  }
  else res <- ecdf(x = dat)(val)
  res
}
#Result: you arre better of not averaging and just using a bigger precision


#function that plots a vector as a sorted list
#vec: the vector to be plotted
#graphparams: the gra[hical parameeters as a vector or list
#op=1
plotSortedVector <- function(vec,stddev=c(),xlab='vector',ylab='responses',markersize=0.4,
                             graphparams=c(),op=1){
  #ranking the vector so that you have it for sd
  vec.rank <- getMatSortPosns(cbind.data.frame(res2),decrease = F)
  vec.sort <- vec[vec.rank]
  xno <- 1:length(vec)
  #adds the user-specified params to the default ones
  graphpar <- c(list(x=xno,y=vec.sort,markersize=markersize,logs=F),graphparams,
                stddev=list(stddev[vec.rank]),xlabel=xlab,ylabel=ylab)
  do.call(fploteq,graphpar)
  
}



#the error function for a gaussian
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

#the complimentary error function for a gaussian
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)

#the inverse for the complimentary error function
erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)

#the inverse for the complimentary error function
erfcinv <- function(x) qnorm((x + 1)/2)/sqrt(2)




#generates a random matrix
#op =1 , integers
#op=2, real
#range : range between which the numbers are generated bounds inclusive
genRandomMat <- function(rows,cols,range=c(0,11),op=1){
  mat <- matrix(sample(range[1]:range[2],rows*cols,replace = T),nrow = rows)
  mat
}

#fit a c
#generates numbers in the topn fraction of the GAussian
getTopnGaussian <- function(mu=0,sigma=1,topn=.1,n=2000){
  top <- qnorm(1-topn,mean = mu,sd = sigma)
  tmp <- rnorm(n,mean = mu,sd=sigma)
  res <- tmp[tmp>top]
  #cat(length(res),'\n')
  #tmp1 <- dnorm(res)
  #cat(tmp1,'\n',res,'\n',res*tmp1,sum(res*tmp1))
  res-top
}

#given topn, generates the difference between two lists of topn gaussians
#result is the cumulative frequency of the differene list
#turnds out the difference is a distribution whose mean is 0 and sd the same as the sd
#of the tail of the GAussian
getDiffTopnGaussian <- function(mu=0,sigma=1,topn=.1,n=2000){
  tmp1 <- getTopnGaussian(mu=mu,sigma = sigma,topn=topn,n=n)
  tmp2 <- getTopnGaussian(mu=mu,sigma = sigma,topn=topn,n=n)
  if (length(tmp1) < length(tmp2)) res <- RelCumFreq(tmp1-tmp2[1:length(tmp1)])
  else res <- RelCumFreq(tmp2-tmp1[1:length(tmp2)])
  res
}  

#this gets the statistics for the top n fraction of the Gaussian distribution
# The stats are not for the adjusted GAussian. so the topn pdf value does not start from 0
#mu - mean, sigma - sd, topn - top n fraction e.g. 10 % is .1
#op= 1, adjusted topn so that the probability disttribution starts from 0, 2 - non 
#adjusted top i.e. the mean will be above topn percentile point 
getTopnGaussianStats <- function(mu=0,sigma=1,topn=.1,op=1){
  top <- qnorm(1-topn,mean = mu,sd = sigma)
  #have to divide the pdf by topn to normalize it so that the pdf sums to 1
  #calculate EX
  mu2 <- integrate(function(x) x*dnorm(x,mean = mu,sd=sigma)/topn,lower = top,upper = Inf)
  #cat(mu+10,top)
  #calculate EX2
  x2 <- integrate(function(x) x^2*dnorm(x,mean = mu,sd=sigma)/topn,lower = top,upper = Inf)
  var <- x2$value-(mu2$value^2) #var = EX 2-(Ex)^2
  #cat(mu2$value,var,'\n')
  switch(op,c(mu2$value-top,var^.5),c(mu2$value,var^.5))
}

#get topn difference. Given a guassian distribution, will generate the 'n' numbers
#from this distribution and turn everything to 0 except the topn fraction
#n: the number or size of the vector
#op=1, return the top 10%, 2 - return the top 10% adjusted to start from 0
getNormalVecTopn <-function(n,mu=0,sigma=1,topn=.1,op=2){
  vecn <- rnorm(n,mean = mu,sd=sigma) #get n number from this gaussian distribution
  top <- qnorm(1-topn,mean = mu,sd = sigma) #purge everything except top n
  #cat('top %',top,'\n')
  vecn.adj0 <- vecn-top # get the top n % starting from 0
  vecn.adj0[vecn.adj0<0] <- 0
  vecn[vecn < top] <- 0 #now, get the top 10 % without normalizaing to 0
  switch(op,vecn,vecn.adj0)
}

#calculatd the absolute value difference of two random variables
#returns the sum of the actual difference and absolute difference
#n: the number or size of the vector
#diff: the difference between the two distributions
#op=1, sum of difference, 2 - sum of absolute value of differences, 
# 3 , cdf of abs diff of only those elements that are non-zero for both distributions
# 4, cdf of absolute difference d, i.e. fraction of absolute differences below d
# 5, cdf of absolute difference where one of the populations is 0
calculateNormalTopnDiff <-function(n,mu=0,sigma=1,topn=.1,dif=0,op=1){
  vec1 <- getNormalVecTopn(n,mu= mu,sigma = sigma,topn = topn)
  vec2 <- getNormalVecTopn(n,mu= mu,sigma = sigma,topn = topn)
  difv <- vec1-vec2
  if(op>2){#what's the difference
    absdiffv <- abs(difv) #get the absolute value of the difference
    #get all the elements that are positive as those are the ones of interest
    vec1n <- which(vec1>0)
    vec2n <- which(vec2>0)
    commonvec <- intersect(vec1n,vec2n) #those that are positive for both
    diffvec <- union(setdiff(vec1n,vec2n),setdiff(vec2n,vec1n))
    nodiffv <- absdiffv[commonvec]
    nodiffvec <- absdiffv[diffvec]
    nod <- nodiffv[nodiffv<=dif] #all the CDF elements
    nolen <- length(nodiffv)
    if(nolen == 0) prob <- 0 #check to make sure that the 
    else prob <- length(nod)/nolen
  }
  #cat('\nvector',nod,':',commonvec)
  #cat('total',nolen,',',length(nod),',',length(absdiffv[absdiffv<=dif]),'\n')
  switch(op,sum(difv),sum(abs(difv)),prob,length(absdiffv[absdiffv<=dif])/n,
         length(nodiffvec[nodiffvec<=dif])/length(nodiffvec))
}

#same options as above, this function gets expected value of each of the distributions
sumNormalTopnDiff <- function(n,mu=0,sigma=1,topn=.1,stp=.1,op=1){
  #summ them up 
  upper <- mu + sigma*5 #sum to 10 SD away
  seqrates <- seq(0,upper,stp)
  cdfs <- sapply(seqrates,function(x) 
    calculateNormalTopnDiff(n=n,mu=mu,sigma = sigma,topn=topn,dif=x,op=op))
  #this is not the right way to do it. res <- sum(1-cdfs)
  res <- FindMeanVarCDF(data.frame(seqrates,cdfs))
  res[1]
}

inver <- function(){
  res <- sapply(resseq,function(x) ptailnorm(x))
  res1 <- sapply(resseq,function(x) calculateNormalTopnDiff(10000,dif = x,op=3))
  
  
}

#probability density function of a standard normal distribution 
#eps is in terms of z, as in z = x-u/sigma
phi <- function(eps){
  (1/sqrt(2*pi))*exp(-.5*eps^2)
}

#the cumulative densiry funttion of standard normal distribution
#x is in terms of z again, as in z = x-u/sigma
phicdf <- function(x){
  .5*(1+erf(x/sqrt(2)))
}

#this gets the statistics for the trauncated part of the Gaussian distribution b/w a and b 
#mu - mean, sigma - sd, topn - top n fraction e.g. 10 % is .1
#op= 1, adjusted topn so that the probability disttribution starts from 0, 2 - non 
#adjusted top i.e. the mean will be above topn percentile point 
getTruncGaussianStats <- function(mu=0,sigma=1,a,b,op=1){
  alpha <- (a-mu)/sigma
  #if(is.finite(b)) beta <- (b-mu)/sigma
  beta <- (b-mu)/sigma
  z <- phicdf(beta)-phicdf(alpha)
  #to help deal with the case where b is infininity
  var.t <- sigma^2*(1+ ((alpha*phi(alpha)-ifelse(is.finite(beta),beta,0)*phi(beta))/z)-
                      ((phi(alpha)-phi(beta))/z)^2 )
  mean.t <- mu + ((phi(alpha)-phi(beta))/z)*sigma
  cat(alpha,beta,phicdf(alpha),'\n')
  #cat(phi(alpha),phi(beta),z,'\n')
  #op=2, subtract alpha so that the mean starts from a
  switch(op,c(mean.t-a,var.t^.5),c(mean.t,var.t^.5))
}

#commputes the probability of x given that the distribution is the tail of gauassian 
#wherein the tail is for the topn fraction, so topn=.1 is top 10%
#x - the value for which the probability should be calculated, here x is the value above
#the (1-topn) fraction. Not the value for the whole GAussian.
#op: 1 : x is above the topn threshold, 2 : x is over the whole gaussian, so we have to 
#normalize to be above threshold d
computeProbTopn <-function(x,mu=0,sigma=1,topn=.1,op=1){
  probd <- dnorm(x,mean = mu,sd=sigma) #op=2, when x is over the whole gaussian
  if(op==1) topnx <- qnorm(1-topn,mean=mu,sd = sigma)
  else topnx <- 0
  #cat('topnx',topnx,x-.9,dnorm(x-.9+topnx,mean=mu,sd = sigma),probd,'\n')
  #add topnx to start it from topn and divide by topn to sum pdf to 1
  prodtopn <- dnorm(x+topnx,mean=mu,sd = sigma)/topn #if op=1
  switch(op,prodtopn,probd)
}

#the PDF of a truncated Gaussian, where the limits are b/w a and b
#op:1, x is the offset from a or it normalizes as if a starts from 0, 
#2 - x has to be normalized to get the offset from a
computePDFTruncGaussian <-function(x,mu=0,sigma=1,a,b,op=1){
  eps <- (switch(op,x+a,x)-mu)/sigma
  alpha <- (a-mu)/sigma
  beta <- (b-mu)/sigma
  z <- phicdf(beta)-phicdf(alpha)
  phi(eps)/(sigma*z)
}

#the PDF of a truncated Gaussian, where the limits are b/w a and b
#op:1, x is the offset from a or it normalizes as if a starts from 0, 
#2 - x has to be normalized to get the offset from a
computeCDFTruncGaussian <-function(x,mu=0,sigma=1,a,b,op=1){
  eps <- (switch(op,x+a,x)-mu)/sigma
  alpha <- (a-mu)/sigma
  beta <- (b-mu)/sigma
  z <- phicdf(beta)-phicdf(alpha)
  (phicdf(eps)-phicdf(alpha))/z
}

#here, mu and sigma are the mean and sd of the original untruncated Gaussian
#It treats the tail distribution as if it starts from 0. So, mu does not matter so much as 
#sigma
dtailnorm <-function(x,mu=0,sigma=1,topn=.1,op=1){
  a <- qnorm(1-topn,mean = mu,sd = sigma)
  computePDFTruncGaussian(x=x,mu=mu,sigma =sigma,a=a,b=Inf,op=1)
}

#here, mu and sigma are the mean and sd of the original untruncated Gaussian 
#in order to ffind the mean and sd of the truncated gaussian use truncGaussianStats or
#getTopnGaussianStats
#It treats the tail distribution as if it starts from 0. So, mu does not matter so much as 
#sigma
ptailnorm <-function(x,mu=0,sigma=1,topn=.1,op=1){
  a <- qnorm(1-topn,mean = mu,sd = sigma) #get the point a where topn starts
  #here x is counted from 0, in the sense the tail is normalized to starrt from 0
  computeCDFTruncGaussian(x=x,mu=mu,sigma =sigma,a=a,b=Inf,op=1)
  #if you want the tail to start from a, you should just add that in the calling function
}

#this function generates n numbers from the tail of a gaussian
#here, mu and sigma are the mean and sd of the original untruncated Gaussian 
#in order to ffind the mean and sd of the truncated gaussian use truncGaussianStats or
#getTopnGaussianStats 
#It treats the tail distribution as if it starts from 0. So, mu does not matter so much as 
#sigma
rtailnorm <-function(n,mu=0,sigma=1,topn=.1,op=1){
  a <- qnorm(1-topn,mean = mu,sd = sigma) #get the point a where topn starts
  #basically generatea a number between 0 and 1 and then get the inverse truncated gaussian
  x <- rnorm(n,mean=mu,sd=sigma)
  cat('x',x)
  #here x is counted from 0, in the sense the tail is normalized to starrt from 0
  computeCDFTruncGaussian(x=x,mu=mu,sigma =sigma,a=a,b=Inf,op=1)
  #if you want the tail to start from a, you should just add that in the calling function
}



#here, mu and sigma are the mean and sd of the original Gaussian
dfoldnorm <-function(x,mu=0,sigma=1,op=1){
  #a <- qnorm(1-topn,mean = mu,sd = sigma)
  coeff <- 1/sqrt(2*pi*sigma*sigma)
  ex1 <- coeff*exp(-(x-mu)^2/(2*sigma*sigma))
  ex2 <- coeff*exp(-(x+mu)^2/(2*sigma*sigma))
  ex1+ex2
}

#here, mu and sigma are the mean and sd of the original Gaussian
pfoldnorm <-function(x,mu=0,sigma=1,op=1){
  #a <- qnorm(1-topn,mean = mu,sd = sigma)
  erf1 <- erf((x+mu)/(sigma*sqrt(2)))
  erf2 <- erf((x-mu)/(sigma*sqrt(2)))
  .5*(erf1+erf2)
}

#function to compute the sum after n years, given a starting sum of s, andd a recurrent
#addition of ra, and an interest rate of r
#ra: the reccurent addition every year denoted by s(1+ra/100)^n, since your salary might 
#go up each year
computePrincipal <- function(s,n,ra,r=5,op=1){
  tmp <- sapply(1:n,function(x,ri,i,nt){
    sp <- 2*s*((1+(ri/100))^x) #calculate the principal for that year
    prin <- sp*((1+(i/100))^(nt+1-x))
  },ra,r,n)
  sum(tmp)
}



# Log-likelihood function for Gaussian-Gamma mixture model
logLikelihoodNormGamma.old <- function(params, x, rel.cum.freq) {
  mu <- params[1]
  sigma <- params[2]
  alpha <- params[3]
  beta <- params[4]
  pi <- params[5]
  
  # Calculate the combined CDF using the Gaussian and Gamma components
  model.cdf <- pi * gaussian.cdf(x, mu, sigma) + (1 - pi) * gamma.cdf(x, alpha, beta)
  
  # Calculate the log-likelihood for the observed data
  # We use sum of squared errors to fit to the cumulative frequencies
  -sum((model.cdf - rel.cum.freq)^2)
}