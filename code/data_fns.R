#R functions to read, analyze, plot and detect patterns in data along with neural and computer algorithms to model complex data.    

#Copyright (C) 2023 Shyam Srinivasan 

#This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details.

#You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.


#contains functions to manipulate data structures and read/write data to files

#declaring global types that can be used in all files: Data type declarations
#1 for vector, 2 for matrix, 3 for dataframe, 4 for list
const.DataType <- list(vector=1,matrix=2,dataframe=3,list=4)


#given  filename open the, the file and return the data read as a data structure
#op = 1, read with header, = 0, no header
ReadData <- function(filename,sepe=" ",op=1){
  #read in the file with the seperator being " " by default
  mydata <- read.table(filename, header=op, sep=sepe)
  mydata 
}

#given a filename, opens the file and writes the data frame to the file
#op=1, no rowname,=2 write row names before every row
WriteData <- function(mydata,filename,sepe=" ",op=1){
  #write to file
  write.table(mydata,filename,sep=sepe,row.names = switch(op,F,T))
}

#function to extract the x and y coords of a vector of points from 
# a matrix mydata. cols is a vector of the column numbers you want, and n is the no
# of rows that are to be read
ReadCols <- function(data,cols,n=nrow(data)) {
  # get the first n numbers
  size <- nrow(data)
  #x1 <- data[seq(1,n,1),c(xc,yc)]
  x1 <- data[seq(1,n,1),cols]
  x1
}

#function to extract the column col , and n is the no
# of rows that are to be read
ReadCol <- function(data,col,n=nrow(data)) {
  # get the first n numbers
  x1 <- data[seq(1,n,1),c(col)]
  x1
}

#this function slides vector left or right by the required number of steps
#if slide is 1, data[1] becomes data[2] 
#data - vector, slide - no of positions to slide it by, op = default value
#op=0, dont pad out the vector, 1 = pad out the vector with zeros
slidevec <- function(data,slide,op=0){
  #if slide is negative, slide the list to the right
  if (slide < 0) {
    slidedata <- data[-(1:-slide)]
    if (op==1){#if op is 1, pad out the vector with zeros
      slidedata <- c(slidedata,rep(0,-slide))
    } 
    return(slidedata)
  }
  #if slide is positive, move the list to the left
  slidedata <- head(data,-slide)
  if (op==1) {
    slidedata <- c(rep(0,slide),slidedata)
  }
  slidedata
}

#same as slide vecs, only this time you slide multiple vectors. If slide is positive
# the vector is shifted to the right, e.g. data[2] becomes data[1]
slidevecs <- function(data,slide,op=0){
  #make a dummy empty matrix
  cols <- ncol(data) #get number of columns
  dummy <- as.data.frame(matrix(0,ncol=cols,nrow=abs(slide))) #the dummy row
  #if slide is negative, slide the list to the right
  if (slide < 0) {
    #if slide is positive, move the list to the left
    slidedata <- data[c(seq(1-slide,nrow(data))),]
    if (op==1){#if op is 1, pad out the vector with zeros
      names(dummy) <- names(slidedata)
      slidedata <- rbind(slidedata,dummy)
    } 
    return(slidedata)
  }
  slidedata <- data[c(seq(1,nrow(data)-slide,1)),] #lose the nth element
  if (op==1) {
    print(slide)
    names(dummy) <- names(slidedata)
    slidedata <- rbind(dummy,slidedata)
  }
  slidedata
}

#gets the posns and lengths of consecutive sequences of non-zero numbers
#greater than equal to thresh
#op=1, return posns, op=2, return lengths, op = 3, return both as dataframe
GetConsecSeqStats <- function(data,thresh=1,op=1) {
  #first trheshold the list
  threshlst <- data
  threshlst[threshlst < thresh] <- 0
  threshlst[threshlst >= thresh] <- 1
  #get the rle for this list
  rlthr <- rle(threshlst)
  #calculate the lengths first
  rlens <- split(rlthr$lengths,rlthr$values)
  #now find the posns by doing a cumulative list
  rlcum <- cumsum(rlthr$lengths)
  #and split the posns according into 0 annd 1 arrays
  rlpos <- split(rlcum,rlthr$values)
  #adjust the posn 
  rlposn <- rlpos$`1`-rlens$`1`+1
  if (op == 1) {return(rlposn)}
  if (op == 2) {return(rlens$`1`)}
  return(as.data.frame(cbind("posns"=rlposn,"lengths"=rlens$`1`)))
}

#given a vector will chunkify the array into the number of chunks
#specified by chunk
#output: returns the chunked out vector
ComputeChunkedVec <- function(vec,chunksize,op=1) {
  #if the structure is a vector, length is good, if not, use nrow
  if(is.vector(vec)) {norows <- length(vec)} else {norows <- nrow(vec)}
  #if(is.atomic(vec)) {norows <- length(vec)} # no of elems in this vector
  #if(is.list(vec)) {norows <- nrow(vec)} # no off rows in this list
  #chunkedvec <- split(vec,ceiling(seq_along(vec)/chunksize))
  chunkedvec <- split(vec,ceiling(seq(1,norows,1)/chunksize))
}

#given a list will compute a funtion on the specified columns
computelistcol <- function(lst,fn,cols=1,op=1){
  # reduce the list to only the cols
  tmp <- lst[,cols]
  res <- apply(tmp,2,fn) # now, apply fn over the columns of the list
  res
}

#given a data frame will compute the function on the specified dimension
computedataframe <- function(data,fn,...){
  res <- apply(data,2,fn,...)
}

#given a data frame will compute the function on the specified dimension, for a 
#particular column(s)
computedataframecol <- function(data,fn,col,...){
  res <- apply(data[c(col)],2,fn,...)
}

#fn that calculates the mean and standard deviation for the specified vector
#op=1, all stats,2-mean,3-mean and sd, 4-mean and sem
getstatscol <- function(vec,op=1){
  resm <- mean(vec)
  resstd <- sd(vec)
  ressem <- resstd/sqrt(length(vec))
  res <- c(resm,resstd,ressem)
  switch(op,res,res[1],res[c(1,2)],res[c(1,3)])
}

#function to interleave multiple vectors
interleaveVec <- function(a,b,op=1){
  #join the two vectors as rows
  #now generate the merge by using apply
  #the trick is that when you generate a matrix from a vector
  # it does so columnwise, and the reverse is also true
  if(length(a) > length(b)) { 
    #if the length of a is greater, it should start and end with a
    tmp <- a[-(length(a))]
    tmp1 <- a[length(a)]
    res <- as.vector(rbind(tmp,b))
    return(c(res,tmp1))
  }
  if(length(a) < length(b)) {
    #if the length of b is greater, it should start and end with b
    tmp <- b[-(length(b))]
    tmp1 <- b[length(b)]
    res <- as.vector(rbind(tmp,a))
    return(c(res,tmp1))
  }
  res <- c(rbind(a,b)) #both of equal length, so cool
}

#given two vectors, will generate a new vector which contains
#b[n],a[n] times and so on
GenVecRep <- function(vec,op=1){
  tmp <- rep(vec[1],vec[2])
}

#given a data frame, will generate a set of vectors, where each vector is the element
#in the first column, repeated the second col # of times
GenVecDF <- function(gendf,op=1){
  tmp <- apply(gendf,1,GenVecRep)
  res <- Reduce(c,tmp)
  names(res) <- c(1:length(res))
  res
}

#given a sequence of continuous nos c(1,1,1,2,2,2,1,1,3,4) generates a data frame such that the first column is the #, and 
#the second column is the no of occurences of that number, the third column is the 
#position
GenDFVec <- function(vec,op=1){
  #do the rle, and then make a data frame of length and values
  tmp <- rle(vec) #returns # as col1 and freq as col2, rle gets elements and their freq
  tmp1 <- c(1,cumsum(tmp[[1]])[-length(tmp[[1]])] +1) #this finds their start posn
  res <- cbind(tmp[[2]],tmp[[1]],tmp1)
  colnames(res) <- c('number','freq','posn')
  row.names(res) <- NULL
  data.frame(res)
  #GenVecDF(tmp1)
}

#given a vetor, it only reports those consecutive sequences above a certain
# thresh no: output is a matrix
FilterConseqSec <- function(vec,thresh,op=1){
  #get a df of value and occurences
  tmp <- GenDFVec(vec)
  #for each non-ve no, check if it occurs more than thresh times
  tmp[,1] <- ifelse((tmp[,1] >= 1) & (tmp[,2] < thresh),0,tmp[,1])
  tmp
}

#given a vetor, it only reports those consecutive sequences above a certain
# thresh no: output is a matrix
FilterConseqSecDF <- function(vecdf,thresh,op=1){
  tmp <- vecdf
  #for each non-ve no, check if it occurs more than thresh times
  tmp[,1] <- ifelse((tmp[,1] >= 1) & (tmp[,2] < thresh),0,tmp[,1])
  tmp
}

#Vector version
#this function does statistiscs or something similar, on the m frames, before or 
#after the specified frame
#vec: to be processed
#pos: the posn to start 
#m: the number of time frames to analyze
#dirn:dirn, -1 for back, 1 for forward, 0 for both
#fn to be used for analysis, ...- arguments for fn
IsNoThere <- function(pos,vec,m,no,dirn=1){
  #gets the indices to check
  tmp <- switch(dirn+2,(pos-m):pos,(pos-m):(pos+m),pos:(pos+m))
  if( tmp[1] < 0 )  tmp <- switch(dirn+2,0:pos,0:(pos+m)) #set to 0 if negative no
  #check if the values of these indices match no
  res <- which(vec[tmp] == no)
  normres <- res + tmp[1] # add to the start of list of indices
  if(length(res) > 0) c(length(normres),normres[1]) else c(0,0)
}

#average the m frames before and/or after the pos
#posvec: the vector of positions to be considered
#m: the number of frames to process
#oper: the kind of operation to do, 1 - mean, 2 -sd
#dirn: -1, m frames before, 0- both dirs, 1 - after
ProcFramesPos <- function(pos,vec,m,oper,dirn){
  #gets the indices to check
  tmp <- switch(dirn+2,(pos-m):pos,(pos-m):(pos+m),pos:(pos+m))
  if( tmp[1] < 0 )  tmp <- switch(dirn+2,0:pos,0:(pos+m)) #set to 0 if negative no
  #now to the operation, oper = 1, get mean
  if (oper == 1) res <- c(mean(vec[tmp]))
  #now to the operation, oper = 3, get vector
  if (oper == 3) res <- vec[tmp]
  #cat(oper,res)
  res
}

#average the m frames before and/or after the posns specified by posvec in the vector vec
#posvec: the vector of posns
#vec: the vector whose values are to be considred
#m: the number of frames to process
#oper: the kind of operation to do, 1 - mean, 2 -sd,3 - the vector
#dirn: -1, m frames before, 0- both dirs, 1 - after
ProcFramesPosvec <- function(posvec,datavec,m,oper,dirn){
  #gets the inidices to the left
  tmp1 <- switch(dirn+2,(posvec-m),(posvec-m),posvec)
  tmp2 <- switch(dirn+2,posvec,(posvec+m),(posvec+m)) # and to the right
  #if less than 0, set to 0
  tmp1[tmp1 < 0] <- 0
  #if greater than length of vector, set to lenght
  tmp2[tmp2 > length(datavec)] <- length(datavec)
  #now, apply this 
  res <- apply(rbind(tmp1,tmp2),2,function(x,vec)
    {switch(oper,mean(vec[x[1]:x[2]]),sd(vec[x[1]:x[2]]),vec[x[1]:x[2]])},
    datavec)
  #cat(oper,res)
  res
}


# a template versions
ProcessAroundTimeTemplate <- function(pos,vec,m,no,dirn,fn,...){
  tmp <- switch(dirn+2,(pos-m):pos,(pos-m):(pos+m),pos:(pos+m))
  res <- which(vec[tmp] == no)
  #print(res)
  if(length(res) > 0) c(res[1]+pos,length(res)) else c(0,0)
}


#given a sequence check if a particular number occurs, its posn, and freq
CheckNoStats <- function(vec,no,op=1){
  posn <- which(vec == no) #gives the positions where no occurs
  freq <- length(posn)
  res <- list(posn=posn,freq=freq)
}

#this function compacts the RLE data frame of #, freq, posn so that adjacent 
#1s or 0s are made part of the same list, thresh specifies the threshold below
#which the non-zeros, should be 0
CompactDFthresh <- function(gendf,thresh,op=1){
  tmp <- gendf
  #for each non-ve no, check if it occurs less than thresh times
  tmp[,1] <- ifelse((tmp[,1] >= 1) & (tmp[,2] < thresh),0,tmp[,1])
  vec <- GenVecDF(tmp) #generate the vector again
  res <- GenDFVec(vec) # again generate the RLE DF for this
  row.names(res) <- NULL
  res
}

#given a df, this will compact it so that if adjacent entries are the same #,
#it will join all of them and update the frequency
CompactDF01 <- function(data,op=1){
  #assign unique id to non zero #s and 0s, 0s are -ve, >0 are +ve
  #1st term assigns -ve nos to 0s
  idval <- -(cumsum(data[,1])*!data[,1]) + cumsum(!data[,1])*data[,1]
  #kludgy fix, if the 1st value being 1
  if (data[1,1] == 1) idval <- idval + 1
  idlsts <- split(data[,2],idval) # split the freqs by index, so consecutive
  #print(idlsts)
  #freqs of the saame number and index are assigned to the same spilt list
  freqsum <- sapply(idlsts,sum) #sum up the freq' lists for each index
  ind <- as.numeric(names(freqsum)) #convert to #s
  indl <- ind[ind <= 0] ##s less than 0
  indg <- ind[ind > 0] #greater than 0
  indlist <- interleaveVec(rev(indl),indg) #make a vector
  idfreq <- freqsum[as.character(indlist)] #now get the freq for these indices
  res <- unlist(lapply(indlist,function(x) {min(which(idval==x))})) #their values
  tmp1 <- c(1,cumsum(idfreq)[-length(idfreq)] +1)
  res1 <- cbind(data[,1][res],idfreq,tmp1) #voila
  colnames(res1) <- c('number','freq','posn')
  row.names(res1) <- NULL
  res1
}

#generic version will work for lists besides 0 and 1. though not for -ve nos
#given a df, this will compact it so that if adjacent entries are the same #,
#it will give the same frequency
#op = 1, 0 and +ve nos, op = 2, all integers
CompactDF <- function(data,op=1){
  abdata <- if (op==2) abs(data[,1]) else data[,1]
  #assign unique id to non zero #s and 0s, 0s are -ve, >0 are +ve
  #1st term assigns -ve nos to 0s, 2nd terms assigns increasing +ve indices to +ve nos
  idval <- -(cumsum(abdata)*!abdata) + cumsum(!abdata)* cummax(abdata)*!(!abdata)
  idlsts <- split(data[,2],idval) # split the freqs by index, so consecutive
  #freqs of the saame number and index are assigned to the same split list
  freqsum <- sapply(idlsts,sum) #sum up the freq' lists for each index
  uniqid <- as.character(unique(idval)) #delete duplicates from idval list
  #for each index value, get the posn of its 1st occurence in idval
  res <- Get1stPosn(as.numeric(uniqid),idval)
  #now compute the posn in the vector
  tmp1 <- c(1,cumsum(freqsum[uniqid])[-length(freqsum[uniqid])] +1)
  res1 <- cbind(data[,1][res],freqsum[uniqid],tmp1) 
  colnames(res1) <- c('number','freq','posn')
  row.names(res1) <- NULL
  res1
}


#this function approximates the vector so that if there is an 'apprrox' # of
#0s between 1s, then it is read as as 1. by default approx is 1
#op=1, return vec, 2 - return df
ApproxVec <- function(vec,approx=1,op=1){
  #generate the dF first
  tmp <- GenDFVec(vec)
  #do the approximation
  #if the no of 0s is less than approx, set it to 1
  tmp[,1] <- ifelse((tmp[,1] == 0) & (tmp[,2] <= approx),1,tmp[,1])
  return(switch(op,GenVecDF(tmp),tmp))
}

#given an values list valst and vector vec, gets the first posn of value in vec
Get1stPosn <- function(valst,vec,op=1){
  res <- unlist(lapply(valst,function(x) {min(which(vec==x))}))
}

#this does statistics factor-wise on a data fram
#data is the data frame
#col is the column number to be used for factoring
#the processing is done column wise
ListStatsByFactDF <- function(data,fcol,cols,op=1){
  #res <- subset(data,data[[col]])
  #res <- data[data[[col]],]
  factnames <- levels(data[[fcol]])
  #print(mydata[mydata[[fcol]]==,])
  #lapply over the factors, calls statsDF on each individual factor data.frame
  #iterates over the factnames vector: makea different fun
  tst <- lapply(factnames,function(name,mydata,colums) 
    {StatsDF(mydata[mydata[[fcol]]==name,],colums)},data,cols)
  names(tst) <- factnames
  tst
}

#given a df will get the averages and sems of the columns
StatsDF <- function(mydata,cols,op=1){
  data <- mydata[,cols] # get tje relevant cols
  #if vector convert to data frame for apply 
  if(!is.data.frame(data)) data <- data.frame(data) 
  mn <- apply(data,2,mean)
  sd <- apply(data,2,sd)
  se <- sd/sqrt(nrow(data))
  data.frame(rbind(mn,se,sd))
}

#this is a testfn 
testfn <- function(vec,m){
  res <- fn(data[1],data[2])
  res
}

#function to load a directory in a system independent way
ReturnPath <- function(vecstr,op=1){
  #assumes the default path to be the homedirectory
  #check if it is windows or unix first
  tmp <- Sys.info() # get the system info
  #check if it is windows or unix
  if( grepl("windows",tmp["sysname"],ignore.case = TRUE) ) os = c("C:","Users") else os = "home"
  #set the user
  user <- tmp["user"]
  vecpath <- c(os,user,vecstr)
  #print(vecpath)
  res <- do.call(file.path,as.list(vecpath))
  res
}

#this function, given a data frame and two lists of indices, will switch indices 
#in list1 and list2
#op = 1, dont change the column names, 2 - switch the column names too
switchDfcols <- function(data,lst1,lst2,op=1){
  res <- data #makea copy of data
  #exchange the data in the columns
  res[,lst1] <- data[,lst2]
  res[,lst2] <- data[,lst1]
  if (op == 1) return(res)
  #now change the column names too for op=2
  if (op == 2)
    names(res)[lst1] <- names(data)[lst2]
  names(res)[lst2] <- names(data)[lst1]
  res
}

#this function gets the best cross- correlation for two sets of numbers
#op= 2, absolute correlation, 1 - correlation with sign, 4 - best lag for abs correltion
#3- best lag for correlation with sign
getBestCorr <- function(vec1,vec2,op=1){
  corr <- ccf(vec1,vec2,plot = F)
  #get the corr coeff for all lags, alternate corr are absolute
  corrcoeff <- switch((op %% 2) + 1,abs(corr$acf),corr$acf) 
  max <- max(corrcoeff) #get either straight or abs. val
  if(op <= 2) return(max)
  posns <- which(corrcoeff==max) #get the posn of max
  lagno <- corr$lag[posns] # get the corresponding lag max
  #cat(posns,max,lagno,corr$lag,corr$acf)
  lagno
}

#this function takes a data frame and compacts it so that choice (0 or 1) entries whose
#freq is below thresh freq are set to the previous element
#the data frame only has entries which are either 0 or 1
SetDfThresh <- function(data.df,thresh,choice=0,op=1){
  chposns <- which(data.df[,1]==choice & data.df[,2]<=thresh) #the posns where the cond is met
  val <- 1 - choice # the value to be set to
  prevposns <- chposns -1 #the posn before
  prevposns[prevposns<0] <- 0 # if its the ffirst posn, set to 0
  nextposns <- chposns + 1
  nextposns[nextposns>length(data.df[,1])] <- length(data.df[,1])
  #set the data frame so that the prev and next posns' freq is greater than thresh 
  data.df[chposns,1][data.df[prevposns,2]>thresh & data.df[nextposns,2]>thresh] <- val # set the value
  #cat(prevposns,'\t',nextposns)
  #print(data.df)
  CompactDF01(data.df)
}

#this function given posns of elements, and the length, will get all those posns as a 
#vector
getPosnsVec <- function(posns,lens,op=1){
  res <- sapply(c(1:length(posns)),function(i,pos,len){
    c(pos[i]:(pos[i]+len[i]-1)) #since the length includes the first element
  },posns,lens)
  unlist(res) # convert the list into a vector
}

#given a vector thta is supposed to be made of similar elemens, checks if it is made of different elemenst and gets their frequency
#op=1: get the number that occuurs most often, 2 = get the positions of this number, 3-get the frequency of the number that occures most often
checkVectorSimilar <- function(vec,op=1){
  res <- table(vec)
  nofreq <- which(res==max(res)) #get the freq of the number that occurs the most number of times
  theno <- as.numeric(names(res)[nofreq])
  #cat('\ncheckvec',as.numeric(names(res)[pos]),str(res))
  switch(op,theno,which(theno==vec),nofreq) #freq, and number that occures most often
}


#given two data frames, will match them to produce a data frame, where all their entries by column are
#in columns 1 and 2. Bycol means, it will take the first col from df1 and match it to the the first col from df2
#skip: cols to skip
#bycol=1, do it by col, 2 by row
#op: 1 - all entries, 2 - filter out 0s
matchDataFrames <- function(df1,df2,bycol=1,skip=c(),op=1){
  if(length(skip)==0){
    data1 <- df1
    data2 <- df2
  } else {
    data1 <- df1[-skip]
    data2 <- df2[-skip]
  }
  #now, match them
  dat.ls <- lapply(1:ncol(data1),function(i){#col by col
    cbind.data.frame(data1[,i],data2[,i])  
  })
  res <- joinListDFs(dat.ls)
  res
}


#converting a list of lists into a data frame, and also eliminates NULL
#a data frame can also be considered as a list so will also work for a list of data frames
#which it will concatenate vertically by adding more rows.
#lists is a nested list structure, 
#dfnames: the names of the rownames
#header that you might want
convertNestedListsDF <-function(lists,header=c(),dfnames=c(),op=1){
  #cat('convertnnested','\n')
  lst <- lists[lapply(lists,is.null)==F]
  if(length(lst) == 0) return(NULL) #if list is empty nothing to do
  if (isNestedList(lst)){#it is a nested list
    res <- lists[lapply(lists,is.null)==F] #jettison all the NULL list elements
    len <- length(res[[1]]) #pick the 1st element as an example for length
    #print('nested list')
  }
  else {#it's a single list
    len <- length(lst)
    res <- list(lst)
  }
  for (i in 1:len){#extract andd add each column to the data frame
    #res.df[[i]] <- unlist(do.call(rbind,res)[,i])
    tmp <- sapply(res, function(x){
      return(x[i])})
    # print(tmp)
    tmp <- unlist(tmp)
    if(i==1) res.df <- data.frame(tmp) #first time, initialize data frame
    else res.df[,i] <- tmp
  }
  #assign names to columns and rows
  if(length(names(lists))==0) names(res.df) <- 1:ncol(res.df)
  else names(res.df) <- names(lists)
  if(length(dfnames)==0) {#dfnames is empty
    #cat('\n',str(dfnames),str(lists),names(lists[[1]]))
    if(length(lists[[1]])>0)  
      if(length(names(lists[[1]]))==0) rownames(res.df) <- 1:length(lists[[1]])
      else rownames(res.df) <- modifyDuplicateStrs(names(lists[[1]]))
    else rownames(res.df) <- 1:length(lists[[1]])
  }
  else rownames(res.df) <- dfnames
  res.df
}

#given a list of names, will check for duplicate names, and will add a number to the second, third occurence
#sep: The seperator to be added before the number like str.1, str.2, ... here sep='.'
modifyDuplicateStrs <- function(vecstr,sep='.',op=1){
  #algo: for each lement, get all the occurences
  #if only single occurence leave alone, else get all posns and add the posn no to the string end
  #cat('\nmodydup')
  newvec <- sapply(1:length(vecstr), function(i) {
    posn <- which(vecstr==vecstr[i])
    if(length(posn) == 1) res <- vecstr[i]
    else {
      res <- paste(vecstr[i],as.character(which(posn==i)),sep = sep)
    }
    res
  })
  newvec 
}

#join a list of DFs all which have different columns. 
#lstdfs: list of DFs
#op: 1, join one on top of each other; 2 - join one next to each other
joinListDFs <- function(lstdfs,op=1){
  lens <- sapply(lstdfs, function(x) ncol(x))
  #check that all DFs have the same number of cols
  #cat('\njlDFs',str(lens) )
  if( sum(lens - rep(mean(lens),length(lens)) )!= 0) {
    cat('\nDfs do not have the same number of cols')
    return(F)
  }
  #cat('\n',str(lstdfs))
  res <- switch(op,do.call(rbind.data.frame,lstdfs),do.call(cbind.data.frame,lstdfs))
  res
}

#concatenates a list of DFs using the operations specified by op
#op= 1, add corresponding columns across the dfs, 2 - multiply
concatListDfs <- function(lst.dfs, op = 1) {
  # Determine the operation based on 'op'
  if (op == 1) {
    operation <- `+`
  } else if (op == 2) {
    operation <- `*`
  } else {
    stop("Invalid operation specified. Use op = 1 for addition, op = 2 for multiplication.")
  }
  
  # Use Reduce to apply the operation across all dataframes
  # Use Reduce to sum the dataframes
  res.df <- Reduce(function(x, y) {
    if(!identical(dim(x), dim(y))) {
      stop("Dataframes have different dimensions.")
    }
    #cat('\n',str(x),str(y))
    x + y
  }, lst.dfs)
  
  res.df
}


#joins the list given in lst
#not sure how this works. Will discovver when we need it.
joinLists <- function(lst,level=1,op=1){
  #cat('\nJoin list',level)
  len <- length(lst)
  flatlst <- c()
  for (i in seq_len(len)) {
    flatlst <- c(flatlst,lst[[i]])
  }
  if(class(flatlst[[1]]) == class(list(1)) && level > 1) flatlst <- joinLists(flatlst,level=level-1,op=op)
  flatlst
}

# This takes in a list of lists of vector, and from each list takes the element corresponding to name
# and joins each such element from each of the lists into one long vector
# lists: a list of list of vectors
# name: the name of the vector to be chosen from every list
concatenateVectorsByName <- function(lists, name) {
  concatenated_vector <- do.call(c, lapply(lists, function(list) {
    # If the name exists in the list, return it; otherwise, return an empty numeric vector
    if (!is.null(list[[name]])) {
      list[[name]]
    } else {
      numeric(0)  # Important for op = 2, ensuring consistent vector lengths when concatenating
    }
  }))
  return(concatenated_vector)
}

#tmp1 <- res5$`./mouse163`$mouse163$summary$freqlist
#res5$`./mouse163`[[1]][[1]]$freqlist

#concatenates a list of lists of Vectors, where each list contains names vectors. It concatenates
#those vectors that have common names across the lists 
#lists: the list of lists
#op:1 - common names, 2 - all names
concatenateListsVec <- function(lists, op = 1) {
  # Check if the input is non-empty and correctly structured
  if (length(lists) == 0 || !all(sapply(lists, is.list))) {
    stop("Input must be a non-empty list of lists.")
  }
  # Determine the set of names to be processed based on op
  list_names <- lapply(lists, names)
  if (op == 1) {
    # Common names across all lists
    names_to_use <- Reduce(intersect, list_names)
  } else if (op == 2) {
    # All names across all lists
    names_to_use <- unique(unlist(list_names))
  } else if (op == 3) {
    # Only uncommon names
    names_to_use <- setdiff(unique(unlist(list_names)), Reduce(intersect, list_names))
  } else {
    stop("Invalid value for op. Use 1 for common names, 2 for all names, 3 for uncommon names.")
  }

  # Create the new list with concatenated vectors for the selected names
  combined_list <- setNames(lapply(names_to_use, function(name) concatenateVectorsByName(lists, name)), names_to_use)
  
  return(combined_list)
}


#join a list of DFs all which have different columns. 
#only applies when all the columns are of the same type: preffereably numeric
#lstdfs: list of DFs
joinUnevenDFs <- function(lstdfs,op=1){
  #cat('\njoinUn',length(lstdfs),isDataType(lstdfs))
  if(isDataType(lstdfs)==3) return(lstdfs)
  lens <- sapply(lstdfs, function(x) ncol(x))
  #cat('\njoin',str(lens) )
  maxlen <- max(lens)
  res <- lapply(lstdfs, function(x) {
    #cat('\n',maxlen-ncol(x),ncol(x))
    data.matrix(padColsDF(x,nocols = maxlen-ncol(x),val=.0001,posn=ncol(x)) ) 
  })
  #cat(str(res))
  res <- do.call(rbind,res)
  res
}

#takesa data frame and pads it with the required number of cols
#op:1, just add cols to the end, 2- insert cols at posn
padColsDF <- function(dat.df,nocols=1,val=0,posn=1,op=1){
  #cat('\npad',nocols)
  if(nocols==0) return(dat.df) #nothing to insert
  #make the matrix to be inserted
  insertcols <- matrix(data = rep(0,nrow(dat.df)*nocols),nrow = nrow(dat.df))
  res <- cbind.data.frame(dat.df,insertcols) #add it to the end of the data frame
  #cat(str(insertcols),nocols,'\t',order(c(1:ncol(dat.df),posn-(nocols:1)/(2*nocols)) ),'\n')
  res <- res[order(c(1:ncol(dat.df),posn-(nocols:1)/(2*nocols)) )] #reorder the cols
  res
}

#given a list, will pad out all elements to match the element with highest number so that you can make a DF
#extra:the value to be used for padding
#op=1, just padding, 2: include number of elements in the first column, 3 - no of elements in last column
convertNestedUnevenListsDF <-function(lst,padding=0.0001,op=1){
  lenvec <- sapply(lst,function(x) length(x))
  maxlen <- max(lenvec) #get the maximum size
  newlst <- lapply(lst,function(x){
    switch(op,c(x,rep(padding,maxlen-length(x))),c(length(x),x,rep(padding,maxlen-length(x))),c(x,rep(padding,maxlen-length(x)),length(x)))
  })
  newlst.df <- transposeDF(convertNestedListsDF(newlst))
  newlst.df
}


#comapres if a vector of values are the same, returns T if they are, F otherwise
areValsIdentical <- function(vec,op=1){
  #cat('\nident:',vec)
  #iterate over the vector, if there is even one F, then it will return F
  res <- sapply(2:length(vec),function(x) identical(vec[1],vec[x]))
  if(sum(res) == (length(vec)-1)) return(T)
  F
}



#this function takes a long list, groups it into smaller sub-list  based on list names and then converts the individual
#elements of these lists into data frames. Each individual element can be a vector
#data.lst : a list, where each list element is a list of vectors or something
#sel:the selection criterion for grouping, 1 = names
#op: element number to be used for the df
getGroupedListsDf <- function(data.lst,sel=1,op=1){
  sel.crit <- switch(sel,names(data.lst))
  #group the lements
  grp.lst <- split(data.lst,sel.crit)
  #now, extract the DFs
  res.ls <- lapply(grp.lst, function(x){
    res <- lapply(x, function(y){
      #cat('\n',str(grp.lst),str(x))
      vec <- y[[op]]
    })
    #put it into a dF
    #cat('\n',str(res))
    res.df <- convertNestedListsDF(res)
    row.names(res.df) <- 1:getLength(res.df)
    names(res.df) <- 1:length(res.df)
    res.df
  })
  res.ls
}


#removes empty elements in a list, an empty element is one that has no elements inside it 
removeListEmpty <- function(lst,op=1){
  #go through the elements of the list and check if they are empty
  res <- sapply(lst, function(x) {
    #cat(str(x))
    length(x)==0 #check if the list has no elements
  })
  res.ls <- lst[(which(res == F))]
  cat('\nremove out of ',names(lst),'valid ones are ',names(lst)[which(res == F)],' with length ',length(res.ls),'\n')
  res.ls
}

#given a bunch of lists, will join them to produce a bigger list, without 
#lst: the nested list to flatten
#level: the depth from the top upto which you want to flatten the list
flattenLists <- function(lst,level=1,op=1){
  #cat('\nJoin list',level)
  #recursie function: flatten at the top level, and then keep recursing down.
  len <- length(lst)
  flatlst <- c()
  flatlst.names <- c()
  for (i in seq_len(len)) { #make a sequentual list of children
    flatlst <- c(flatlst,lst[[i]])
    #if the list has no name, just take the child's name or add parent + child
    if(length(names(lst)) == 0) flatlst.names <- c(flatlst.names,names(lst[[i]]) )
    else flatlst.names <- c(flatlst.names,paste(names(lst)[i],names(lst[[i]]),sep = ','))
  }
  names(flatlst) <- flatlst.names
  #cat('\nflattenlists',str(flatlst))
  if(class(flatlst[[1]]) == class(list(1)) && level > 1) flatlst <- flattenLists(flatlst,level=level-1,op=op)
  flatlst
}



#function that flattens a 2 dimensional data frame so that each x,y element becomes 
#a list element and the name of the list element is the name of the x,y 
#dat.df: the 2 dimensional matrix
flattenDFtoList <- function(dat.df,op=1){
  len <- ncol(dat.df)
  pairs <- lapply(1:len^2, function(x){
    #cat('\t',x,floor((x-1)/odorno)+1,(x %% odorno+1) )
    c(floor((x-1)/len)+1,(x %% len+1))
  })
  #print(pairs)
  rown <- rownames(dat.df)
  #now, go through the list while skipping paraffin oil
  res.ls <- lapply(pairs, function(x){
    dat.df[x[1],x[2]]
  })
  names(res.ls) <- sapply(pairs, function(x){
    paste(rown[x[1]],rown[x[2]],sep = ',')
  })
  res.ls
}

#function that does the reverse, converrts a list into a df. List can have only one elem
#a list element and the name of the list element is the name of the x,y, which in 
#the df will be at row x and col y. 
#sel:1, the element from the list 
#sep: the seperator for determing the split, default ','
#op=1
makeDfFromList <- function(lst,sel=1,sep=',',op=1){
  #vals <- names(lst)
  res <- sapply(names(lst), function(i) strsplit(i,split = sep))
  vals <- unique(unlist(res)) #get all the atomic vals
  if(isStringNo(vals[1])) vals <- sortStringNos(vals) #if string no, sort it
  #create a square matrix
  mat <- matrix(data = rep(0,length(vals)^2),nrow = length(vals) )
  for(i in (1:length(vals))){
    for(j in 1:length(vals)) {
      tmp <- paste(vals[i],sep,vals[j],sep = '')
      mat[i,j] <- lst[[tmp]][sel]
    }
  }
  colnames(mat) <- vals
  rownames(mat) <- vals
  mat
}

#makes a mapping of repeated values from cola to colb
#cola and colb, the two cols for mapping cola values to colb
makeMapDfCols <- function(dat.df,cola=1,colb=2,op=1){
  vals <- unique(dat.df[,cola]) #get the unqie values,
  #then return the corresponding values ffrom colb. Assumes that cola and colb values have an exact on-to-on mapping
  res <- sapply(vals, function(i) {
    pos <- which(dat.df[,cola] == i)
    dat.df[pos[1],colb]
  })
  res
}


#test if this list is a nested list
#if there is one sub-list, it is a nested list
isNestedList <- function(lst,op=1){
  if (typeof(lst)!=typeof(list())) return(F) #if not a list return false
  res <- lapply(lst, function(x){#hceck if each element itself is a list
    if (is.null(x)) return(NULL)
    typeof(x)==typeof(list()) #typeof recognizes a list and dataframe as the same thing
  })
  res <- unlist(res)
  #cat('isnested',res,'\n')
  is.element(T,res) #if there is single sub-list list element, return T
}

#given a list will make 'n' copies of the list
#op = 1: repeat the whole list 'n' times
#2 : repeat the first element 'n' times, then the second element 'n' times,...
repList <- function(lst,n,op=1){
  if(op==1) res.lst <- lapply(1:n,function(i) lst)
  else {
    res.lst <- lapply(1:length(lst), function(i) #repList(lst[[i]],n = n,op=1))
      lapply(1:n,function(j) lst[[i]])  
    )
  }
  res.lst
}


#merges logical lists of the same length by ANDing or ORing all the lists
#op = 1, return a vector
#op = 2, return a list
mergeLogicalLists <- function(lists,op=1){
  if(length(lists) == 1) {
    #cat(isDataType(lists),class(lists),is.vector(lists))
    if(isDataType(lists[[1]])==4) return(switch(op,lists[[1]][[1]],lists[[1]]))
    else return(switch(op,lists[[1]],lists)) #reached the end so just return the last list
  }
  res <- mapply("&",lists[[1]],lists[[2]]) #do an AND of the first two elements
  #replace them with the result
  tlist <- lists
  tlist[[1]] <- NULL
  tlist[[1]] <- list(c(res))
  #now, call mergeLocal again with the new list
  #cat(str(tlist))
  mergeLogicalLists(tlist,op)  
}



#function that loooks at a series of data frames gets the specified column from each as a 
#list
#...: data frames as input
#cols: the columns to be considered from each data frame, by default the second column
getDFColsAsLists <-function(...,cols=c(2),cnames=c()){
  data <- list(...) #convert the data frames into a list
  #if only 1 column value, then get the same column from all data frames
  colp <- rep(cols,ifelse(length(cols)==1,length(data),length(cols)))
  #get the column values from all data frames
  clst <- lapply(1:length(data),function(x,data,colp) {
    cat(x,str(data),colp,'\n')
    data[[x]][,colp[x]]
  },data,colp)
  #set the names for each of the list elements
  if (length(cnames) == 0 ) cnames <- 1:length(data)
  names(clst) <- as.character(cnames) #convert to strings
  clst
}

#measuring the number of NAs in a structure, percentage wise. 
#op: 1 - overall structure, 2 - no of columns with an NA, 3 - no of rows with an NA
countNA <- function(dat.df,op=1){
  
  
}


#this cleans up all occurances of NA in a list
cleanNA <- function(data,op=1){
  if(is.data.frame(data)) return(data[complete.cases(data),])
  data[complete.cases(data)]
}

#this cleans NA from a matrix or data frame
#op=1, turn NA to 0
cleanNAMat <- function(dat,op=1){
  tmp <- dat
  tmp[!is.finite(tmp)] <- 0
  tmp
}

#this cleans NA from a vector
cleanNAVec <- function(dat,op=1){
  tmp <- dat
  #a[!is.finite(a)] <- 0
  tmp[!is.finite(tmp)] <- 0
  tmp
}


#if there are two vectors with NAs in them, cleans them up
#op=1, aad them, = 2 get avg
addVecCleanNA <-function(vec1,vec2,op=1){
  vec1.na <- which(is.na(vec1))
  vec2.na <- which(is.na(vec2))
  napos <- union(vec1.na,vec2.na)
  nonapos <- setdiff(1:length(vec1),union(vec1.na,vec2.na))
  res.na <- sapply(napos, function(x){
    if(is.na(vec1[x]) && is.na(vec2[x])) tmp <- NA
    if(is.na(vec1[x]) ) tmp <- vec2[x]*2
    else tmp <- vec1[x]
    tmp
  })
  res.nona <- vec1[nonapos] + vec2[nonapos]
  #cat('here',union(vec1.na,vec2.na),'\n',setdiff(1:length(vec1),union(vec1.na,vec2.na)))
  #vec3 <- vec1()
  #cat('\n',vec1[order(c(napos,nonapos))])
  res <- 1:length(vec1)
  res[napos] <- res.na
  res[nonapos] <- res.nona
  switch(op,res,res/2)
}

#when two vectors are about to be concatenated
#checks if the data type falls into a particular category and returns a vector of that type
#current data types checked are integer, double, character, and lofical
castVecType <- function(origvec,addvec,op=1){
  res <- c(origvec,addvec)
  #cat(str(origvec))
  if(typeof(origvec) == typeof(1:3)) res <- as.integer(res)
  if(typeof(origvec) == typeof(1.0)) res <- as.double(res)
  if(typeof(origvec) == typeof('1')) res <- as.character(res)
  if(typeof(origvec) == typeof(T)) res <- as.logical(res)
  #if(typeof(origvec) == typeof('1')) as.character(res)
    # introduce more types as you encounter them
  res
}


#inserts an element into a list at posn
#elem: the element or list of elements to be inserted
#lst: the lst in which to insert it
#posn: the posn at which to insert, if its greater than list length, adds to end
insertElemList <- function(lst,elem,posn,op=1){
  lstlen <- length(lst)
  if(posn>lstlen) newlst <- c(lst,elem)
  else newlst <- c(lst[1:(posn-1)],elem,lst[(posn):lstlen])
  newlst
}


#inserts an element into a list at posn
#elem: the element or list of elements to be inserted
#lst: the lst in which to insert it
#c(stpos,endpos): the posns to replace
replaceElemList <- function(lst,elem,stpos,endpos,op=1){
  lstlen <- length(lst)
  #cat('\nreplaceElemlst',1:(stpos-1),'stpos',stpos,'endpos',endpos,lstlen)
  #if the endpos is the last posn, then also check if the stpos is > 1, otherwise stpos -1 will be wrong
  newlst <- c()
  if(endpos<lstlen && stpos>1) {
    newlst <- c(lst[1:(stpos-1)],elem,lst[(endpos+1):lstlen])
    return(newlst)
  }
  #now check extreme posns are at the start or end of list
  if(endpos>=lstlen && stpos>1) {
    newlst <- c(lst[1:(stpos-1)],elem)
    return(newlst)
  }
  if(stpos==1 && endpos<lstlen) {
    newlst <- c(elem,lst[(endpos+1):lstlen])
  }
  else newlst <- elem #both extreme condns fail, so this is the new list
  newlst
}


#this function inserts a row into a data frame
#data.dt : data frame:
#newrow : data row to be inserted
#posn: to be inserted, e.g. 4 means put it in posn 4, and the old posn 4 will become 5,
#default is posn 1
#op: 1 - default, just add the row at the particular posn, op=2, do it at the end
insertRowDf <- function(data.df,newrow,newname='',posn=1,op=1){
  #cat('new data ',nrow(data.df),ncol(data.df),'\n')
  if(nrow(data.df)==0) {#no data in this data. frame so make a new data.frame
    new.df <- rbind.data.frame(newrow)
    rownames(new.df) <- newname
    #cat('\ninset',str(new.df) )
    return(new.df)
  }
  if(op==2) posn <- nrow(data.df)+1 #if op==2, add to the end
  #convert to a mtrix to deal with factors, and then convert it back
  tmp.df <- lapply(1:length(data.df),function(x,dat.df,newdat){
    #cat(class(dat.df[,x]),'\n')
    if(class(dat.df[,x])=='factor') tmp <- as.factor(c(as.character(dat.df[,x]),newdat[x])) 
    else tmp <- castVecType(dat.df[,x],newdat[x])
    #you also have to fix it so that the data types are not changed for the othe columns
    tmp
  },data.df,newrow)
  #convert to a data frame
  res.df <- convertNestedListsDF(tmp.df)
  #reorder the rows, so that newrow takes position posn, also fixed error with 1 col DFs
  norows <- nrow(res.df)
  if(ncol(res.df)==1) res.df <- as.data.frame(unlist(res.df[order(c(1:(norows-1),posn-.5)),]))
  else res.df <- res.df[order(c(1:(norows-1),posn-.5)),]
  #res.df <- res.df[order(c(1:(norows-1),posn-.5)),]
  #renumber the row names, and retain the old ones if they exist
  newnames <- c(row.names(data.df),newname)[order(c(1:(norows-1),posn-.5))]
  #print(res.df)
  row.names(res.df) <- newnames
  names(res.df) <- names(data.df)
  res.df
}


#preserves the column types.
#orig.df: the original df whose col types are to be preserved in the new.df
#op:1 - default: type(orig.df[,i]) = type(new.df[,i])
#orientop: 1 - along columns
preserveColsType <- function(orig.df,new.df,op=1){
  #get a vector of the types of orig.df
  col.types <- sapply(1:ncol(orig.df), function(i){
    if(typeof(origvec) == typeof(1:3)) res <- as.integer
    if(typeof(origvec) == typeof(1.0)) res <- as.double
    if(typeof(origvec) == typeof('1')) res <- as.character
    if(typeof(origvec) == typeof(T)) res <- as.logical
    if(class(dat.df[,x])=='factor') res <- as.factor
  })
  #now apply it
  for (i in ncol(new.df)) {
    res.df[,i] <- col.types[i](new.df[,i])
  }
  res.df
}

#this function inserts a column into a data frame
#data.dt : data frame:
#newrow : data col to be inserted
#posn: to be inserted, e.g. 4 means put it in posn 4, and the old posn 4 will become 5,
#default is posn 1
#op: 1 - default, just add the col at the particular posn, op=2, do it at the end
insertColDf <- function(data.df,newcol,posn=1,colname='',op=1){
  #cat('new data ',newrow,'\n')
  if(op==2) posn <- ncol(data.df)+1 #if op==2, add to the end
  #assign to a temp df and add the new column
  res.df <- data.df
  #slight issue with inserting a column if it is a matrix as opposed to a df
  if(isDataType(data.df)==2){#special processing for a matrix
    res.df <- cbind(res.df,newcol)
    colnames(res.df)[ncol(data.df)+1] <- colname
    #reorder the cols, so that new col takes position posn
    res.df <- res.df[,order(c(1:(ncol(res.df)-1),posn-.5))]
  }
  else{
    res.df[ncol(data.df)+1] <- newcol
    colnames(res.df)[ncol(data.df)+1] <- colname
    #reorder the cols, so that new col takes position posn
    res.df <- res.df[order(c(1:(ncol(res.df)-1),posn-.5))]
  }
  #renumber the col names
  #colnames(res.df) <- 1:ncol(res.df)
  res.df
}

#given a data frame or matrix, will rerder the cols
#colorder: a vector containing the ordering of the cols. e.g., c(2,3,1) will put the 1st col. 2nd, the 2nd col third...
reorderDFCols <- function(dat.df,colorder,op=1){
  neworder <- getMatSortPosns(cbind(1:ncol(dat.df),colorder),col=2,op=1)
  #cat('\nmrDC',colorder,':',neworder)
  newdf <- dat.df[,neworder]
  newdf
}


#this function swaps two rows in a data frame, data.df
#row1 and row2 are the rows to be swapped
#op=1, reorder the row names, 2 - leave row names as they are
swapDfRows <- function(data.df,row1,row2,op=1){
  rows <- 1:nrow(data.df)
  rows[row1] <- row2
  rows[row2] <- row1
  res <- data.df[rows,]
  if (op==1) return(res)
  row.names(res) <- row.names(data.df)
  #cat(rows,str(as.character(rows)),str(row.names(res)),str(row.names(data.df)))
  res
}

#this function swaps two cols in a data frame, data.df
#col1 and col2 are the cols to be swapped
#op=1, reorder the col names
swapDfCols <- function(data.df,col1,col2,op=1){
  cols <- 1:ncol(data.df)
  cols[col1] <- col2
  cols[col2] <- col1
  res <- data.df[,cols]
  #if (op==1) return(res)
  #colnames(res) <- colnames(data.df)
  #cat(rows,str(as.character(rows)),str(row.names(res)),str(row.names(data.df)))
  res
}


#this function converts a particular column to numeric or character strings
#data.dt : data frame:
#cols : the cols to be changed
#op: 1 - change to numeric, 2 - change to character, 3 - factor
#orientop: specifies whether you want to change cols or rows 1 - cols, 2 - rows, but then 
#will return a DF with the original rows as cols, because, see Note
#Note: orientation does not work for rows as DF requires that aall elements of a column
#have the same data type.
ConvertDfCols <- function(data,cols,op=1,orientop=1){
  if(orientop==2) data.df <- transposeDF(data)
  else data.df <- data
  #convert to a mtrix to deal with numberes, and then convert it back
  typ <- switch(op,as.numeric,as.character,as.factor)
  tmp.df <- lapply(1:length(data.df),function(x,dat.df,changecols){
    if(x %in% changecols) { #factors should first be converted to character before conversion
      if(class(dat.df[,x])=='factor') tmp <- sapply(as.character(dat.df[,x]),typ)
      else tmp <- sapply(dat.df[,x],typ)
    }
    else tmp <- dat.df[,x]
    #cat(x,' is now ',class(tmp))
    tmp
  },data.df,cols)
  #convert to a data frame and rename the column names
  res.df <- as.data.frame(tmp.df,stringsAsFactors = F) #do not automatically convert strings to factors
  #cat(str(res.df),str(tmp.df))
  names(res.df) <- names(data.df)
  row.names(res.df) <- row.names(data.df)
  #if(orientop==2) res.df <- transposeDF(res.df) #if the orientation is along the rows
  res.df
}

#same function as above except this time, you convert multiple columns at the same time
#data.df : the data frame or matrix
#cols: the cols to be changed
#types: same size as columns specifying the types that you want to convert to 
#1 - change to numeric, 2 - change to character, 3 - factor
#orientop: specifies whether you want to change cols or rows 1 - cols, 2 - rows
ConvertDfMultipleCols <- function(data.df,cols,types,op=1,orientop=1){
  dat <- data.df
  cat(types)
  dat[,cols] <- lapply(1:length(cols), function(x,data.df,cols,types) {
    cat(types)
    tmp <- ConvertDfCols(data.df = data.df,cols=cols[x],op=types[x],orientop = orientop)
    tmp[,cols[x]]
    },data.df,cols,types)
  dat
}

#this function combines a list of vectors into a matrix and gets the sum
#op =1, get the sum of each row, 2 - sum of only the positive values,
#4- get the mean ofthe positive values, 3 - mean of all values, 5 - # of positive values
getVecListStats <- function(vec.lst,op=1){
  res.mat <- sapply(vec.lst,function(x) x) #convert into matrix form
  #get the matrices of only positive values
  respos.mat <- sapply(vec.lst,function(x) setVecThresh(x,thresh = 0,op=1)) 
  #get matrix where all postive values are 1
  resbin.mat <- sapply(vec.lst,function(x) setVecThresh(x,thresh = 0,op=2))
  res <- apply(res.mat,1,sum) #the sum of all values 
  #get a binary matrix of all positive values
  res.pos <- apply(respos.mat,1,sum)
  avg <- res/ncol(res.mat) #average of all values
  avg.pos <- cleanNAVec(res.pos/apply(resbin.mat,1,sum)) #average of only positive values
  switch(op,res,res.pos,avg,avg.pos,apply(resbin.mat,1,sum))
}


#given a list of matrices or data frames, gives one matrix by combining all the columns of the matrices
#of the list
#op=1, add each matrix to the other by adding as columns, 2 - do by adding as rows one on top of each otehr
combineListMat <- function(lst,op=1){
  if(op==2) {#if you are goingt to stack them vertically you need to make sure colnames match
    namex <- colnames(lst[[1]])
    tlst <- lapply(lst, function(x) {
      names(x) <- namex
      x
    })
  }
  switch(op,do.call(cbind,lst),do.call(rbind,tlst)) #combines all the mat(rices by column
  #can also do Reduce(cbind,lst)
}

#given a list of lists, where each sublist contains two lists of matrices, this function
#returns a list of 2 lists, which takes the nth element and puts that into a list
#op=1, combine as cols, 2 - combines as rows
combineListsofMat <- function(lst,op=1){
  n <- length(lst[[1]]) #get the number of subelements of a list
  res <- lapply(1:n,function(x) {
    switch(op,do.call(cbind,lapply(lst,function(y,i) y[[i]],x)),
           do.call(rbind,lapply(lst,function(y,i) y[[i]],x)) )
  })
}

#repeat a vector 'n' number of times to produce a matrix of n rows or cols
#op=1, n rows, 2 - n cols
repVecMat <- function(vec,n,op=1){
  #repeat the vector n times and assumble into matric by rows.
  res <- matrix(rep(vec,n),nrow = n,byrow = T)
  res <- switch(op,res,t(res))
  res
}

#gets the correlation of a list of vectors. sets NAs to 0
corListVectors <- function(lst1, lst2,op=1) {
  # Efficiently compute correlation using vapply()
  res <- sapply(seq_along(lst1), function(i) {
    x <- lst1[[i]]; y <- lst2[[i]]
    
    # If scalaar, can't do correlation
    if (length(x) == 1 || length(y) == 1 || sd(x) == 0 || sd(y) == 0) cor.res <- 0
    else {    
      # Compute correlation, replace NA with 0
      cor.res <- cor(x, y, use = "pairwise.complete.obs")
      if(length(cor.res) != 1) cat('\nnot1',i)
      cor.res <- ifelse(is.na(cor.res), 0, cor.res)
    }
    cor.res
  })#, numeric(1))  # Predefine output type for efficiency
  names(res) <- names(lst1) #set the names to the names of lst1 
  res
}

#function adds a list of vectors
#vec.lst: list of vectors
#op=1 sum, =2 positive nos are set to 1.
addListVectors <- function(vec.lst,op=1){
  res <- sapply(1:length(vec.lst[[1]]),function(i) sum(unlist(lapply(vec.lst,'[[',i) )) )
  switch(op,res,setAboveThreshOne(res))
}

#function adds a list of vectors
#vec.lst: list of vectors
#op=1 sum, =2 positive nos are set to 1.
meanListVectors <- function(vec.lst,op=1){
  #res <- sapply(1:length(vec.lst[[1]]),function(i) mean(unlist(lapply(vec.lst,'[[',i) )) )
  res <- rowMeans(rbind.data.frame(vec.lst),na.rm = T)
  switch(op,res,setAboveThreshOne(res))
}


#given a list of matrices, gives one matrix by summing all the matrices
#of the list
sumListMat <- function(lst,op=1){
  #res <- sapply(1:length(vec.lst[[1]]),function(i) mean(unlist(lapply(vec.lst,'[[',i) )) )
  Reduce('+',lst)
}

#this takes a list of data frames, and gets the indicated cols from each data frame to assemble into one data frame
#column is common to all the data frames
#cols: the cols to be chosen from each list element data frame
combineListOfDfs <-function(lst,factorcol = c(1),cols=c(2),op=1){
  #gets all the entries into a data frame format
  dfcols <- do.call(cbind,lapply(lst,function(x) x[,cols]))
  dat.df <- cbind(lst[[1]][,factorcol],dfcols) #put the common col at the start
  dat.df
  #make the code more genereic and extract the numbers from 375:375, arrane the cols in descending sort
}

#takesa data frame and pads it 
#op:1, just add cols to the end, 2- insert cols at posn
padRowsValsDF <- function(dat.df,vals,col=1,padval=0,op=1){
  if(nocols==0) return(dat.df) #nothing to insert
  #make the matrix to be inserted
  insertcols <- matrix(data = rep(0,nrow(dat.df)*nocols),nrow = nrow(dat.df))
  res <- cbind.data.frame(dat.df,insertcols) #add it to the end of the data frame
  #cat(str(insertcols),nocols,'\t',order(c(1:ncol(dat.df),posn-(nocols:1)/(2*nocols)) ),'\n')
  res <- res[order(c(1:ncol(dat.df),posn-(nocols:1)/(2*nocols)) )] #reorder the cols
  res
}

#takes a vector and pads it out to lenfth n
#vec: vector to be apadded
#n: length of the padded vector
#pad: the value with which to pad
padVec <- function(vec,n,pad=0,op=1){
  #cat('\npadVec',length(vec),n,n-length(vec))
  if(length(vec) > n) return(vec[1:n])
  #cat('\npadVec',length(vec),n,n-length(vec))
  res <- c(vec,rep(pad,n-length(vec)))
  res
}

#takes a vector of strings and pads it out to lenfth n
#vec: vector to be apadded
#n: length of the padded vector
#pad: the value with which to pad
padVecStr <- function(vec,n,pad='na',op=1){
  #cat('\npadVec',length(vec),n,n-length(vec))
  if(length(vec) > n) return(vec[1:n])
  #cat('\npadVec',length(vec),n,n-length(vec))
  res <- c(vec,rep(pad,n-length(vec)))
  res
}

#this takes a list of DFs where all DFs are combined by taking the values of the designated column(default 1) and their value
#across data frames. If some DF contains more rows, then the values of all other data frames are padded accordingly
#factor col: col. values on which each DFs elements are added to the big DF
#cols: the cols to be chosen from each list element data frame
combineListOfUnevenDfs <-function(lst,factorcol = c(1),cols=c(2),padval=0,op=1){
  #to expand all the DFs, you need to get the values across all DFs
  vals <- unique(unlist(lapply(lst,function(x) x[,1])))
  #cat('\ncLOUD',vals)
  vals.vec <- seq(min(vals),max(vals),1)
  dfs.lst <- lapply(lst, function(x) {
    #first create a DF with all the padded vals
    mat.df <- as.data.frame(matrix(data = rep(padval,length(vals.vec)*(length(cols)+1)),ncol = length(cols)+1))
    mat.df[,factorcol] <- vals.vec #populate the factor col
    posns <- sapply(x[,factorcol], function(y) which(vals.vec == y)) #get the posns of rows that need to be populated 
    mat.df[posns,cols] <- x[,cols] 
    mat.df
  })
  #now that all of them are the same length, make them into a DF
  res.df <- combineListOfDfs(dfs.lst,factorcol = factorcol,cols = cols)
  res.df
}

#takes a vector or a matrix and shuffles the entries. IF it is a matrix, it onyl 
#shuffles all the values inside each individual column
shuffleData <- function(dat,op=1){
  dattype <- isDataType(dat)
  len <- ifelse(dattype == 1,length(dat),nrow(dat)) #generate a random vector of the same length with no repeats
  if( dattype == 1 ) data <- as.matrix(dat) #if it is a vector, convert to matrix,
  else data <- dat                          #and convert back once the apply is done
  res <- sapply(1:ncol(data), function(x) {
    shuffle <- sample(1:len,len) #shuffle the vector
    data[shuffle,x]
  })
  if(dattype == 1) return(as.vector(res))
  res #return shuffled data
}

#given a list of matrices or DFs. gets the selected columns as one long vector. Can also get
#it as a lsit of vectors
getColMatList <- function(lstmat,selcol=1,op=1){
  #iterate throigh each list an get the specified col
  res.ls <- lapply(lstmat, function(x) x[,selcol])
  switch(op,res.ls,unlist(res.ls))
}


#given a list, wjhere each list itself is a list of dta frames, this returns a list of 
#data frames, where the first data frame containsthe columns from the first data frame in each list, and so on
#lst: list of list of data frames
#selcol: the selected column
#factor col: the col or cols that dont change across any of the DFs
getSelColsListDfs <- function(lst,selcol=2,factorcol=1,op=1){
  #iterate tghrough the data frames across lists
  res.ls <- lapply(1:length(lst[[1]]),function(x){
    #get the factor cols and bind them with the selected cols from all lists
    dfs <- cbind(lst[[1]][[x]][,factorcol],sapply(lst,function(y) y[[x]][,selcol]))
    #get the proper names for each col, do factors, then results
    fnames <- colnames(lst[[1]][[x]])[factorcol] #names of factor cols
    #prepare names of all the results
    dnames <- sapply(1:length(lst),function(y) paste(colnames(lst[[1]][[x]])[selcol],as.character(y),sep = ''))
    names.vec <- rep('',ncol(dfs)) #initialize the names array
    names.vec[factorcol] <- fnames #assign the factor names, and then results names
    names.vec[(ncol(dfs)-length(lst)+1):ncol(dfs)] <- dnames
    #colnames(dfs)[factorcol] <- fnames
    colnames(dfs) <- names.vec
    dfs
  })
  names(res.ls) <- names(lst[[1]]) #assign the names from the first list
  res.ls
}

#given a list of data frames, will get the mean, sd, and sem of the all numeric cols
getStatsColsListDfs <- function(lst,op=1){
  res.ls <-lapply(lst,function(x) {
    avg <- apply(x[,c(2:ncol(x))],1,mean)
    std <- apply(x[,c(2:ncol(x))],1,sd)
    se <- std/sqrt(ncol(x)-1)
    cbind(x,cbind(avg,std,se))
  })
  res.ls
  res.ls
}


#given a data frame, extracts the numeric value from colnames and changes the col
#names accordingly
#factorcol: ignore factor names
changeDfNames <- function(dat.df,factorcol=1,op=1){
  dfnames <- colnames(dat.df)
  dfnum <- sapply(dfnames[-factorcol],function(x) strsplit(x,'\\D+')[[1]][1])
  as.numeric(dfnum)
}

#checks if the current.df row elements and the names of elements in current.vec match 
#op=1: just check for matching lengths
#op=2: check if the names of vec and entries of first row of df match
#op=3: chekc if the names of vec are present within the entries of first row of df
checkDfVecMatch <- function(current.df, current.vec, op=1) {
  if (op == 1) {
    # Check if the number of elements and rows are the same
    #cat('\ncheckdfg',nrow(current.df),'sfks',length(current.vec))
    return(length(current.vec) == nrow(current.df))
  } else if (op == 2) {
    # Check if the names of vector elements match the entries in the first column of the dataframe
    return(all(names(current.vec) == current.df[,1]))
  } else if (op == 3){
    #chekc if the names of vec are present within the entries of first row of df
    # cat('\ncheckdfg',current.df[,1],'sfks',names(current.vec),':',
    #     ( names(current.vec) %in% as.character(current.df[,1])))
    return( all(names(current.vec) %in% as.character(current.df[,1])) )
  } else {
    stop("Invalid operation specified. Use op = 1 for count check, op = 2 for name match check.")
  }
}

#function that checks based on op=1, check that the number of elements in the 
#frequency vector and the rows in the dataframe are the same, 
#op =2, check that the names of the vector elements and the entries in the first column of the dataframe match.
#op=3: chekc if the names of vec are present within the entries of first row of df
#freq.lst = list of frequency vectors
#lst.dfs = list of data frames where each row corresponds to the elements  of the frequency vectors
checkListVecDF <- function(lst.dfs, freq.lst, op = 1) {
  # Apply the check.pair function to each item and collect results
  results <- sapply(names(lst.dfs), function(df.name) {
    current.df <- lst.dfs[[df.name]]
    current.vec <- freq.lst[[df.name]]
    checkDfVecMatch(current.df, current.vec, op=op)
  })
  # Return the results as a named vector
  #cat('\nresults',str(results),'\n',names(lst.dfs),names(freq.lst))
  results
}



#this function given a frequency list and  data frame of the means and sems of the 
#items in the frequency list, will give you the total stats
computeTotalStats <- function(freq.vec, data.df,op=1) {
  # Ensure the names in the dataframe are aligned with the frequency vector
  aligned.df <- data.df[match(names(freq.vec), data.df[,1]), ]
  # Calculate total value as mean * frequency
  total.value <- aligned.df[,2] * freq.vec
  # Calculate total standard deviation as SEM * sqrt(frequency)
  total.sd <- (aligned.df[,3] * sqrt(freq.vec))^2
  # Create a result dataframe
  result.df <- data.frame(
    name = names(freq.vec),total = total.value,totalsd2 = total.sd)
  result.df
}

#takes a list of frequency vectors and their corresponding data framees containing 
#the means and sems of each freqeuncy vector and gets the mean and sem for the whole populatio
computeListTotalStats <- function(freqvec.lst,lst.dfs,op=1){
  if(all(checkListVecDF(lst.dfs = lst.dfs,freq.lst = freqvec.lst,op=3)) == F) return(F) 
  #first compute the total stats
  res.totaldfs <- lapply(names(freqvec.lst), function(x) 
    computeTotalStats(freq.vec = freqvec.lst[[x]],data.df = lst.dfs[[x]]))
  rows <- getListDFsColRange(lst.dfs = lst.dfs,n=1) #get a range of elements 
  res.ls <- lapply(rows, function(i) {
    freqsum <- sum(cleanNAVec(getNElem(freqvec.lst,i)))
    avg <- sum(cleanNAVec(getListDfsMRowNElem(res.totaldfs,i,2)) )/freqsum
    sem <- sqrt(sum(cleanNAVec(getListDfsMRowNElem(res.totaldfs,i,3))) )/freqsum
    #cat('\nres:',i,cleanNAVec(getListDfsMRowNElem(res.totaldfs,i,3)),freqsum )
    c(i,avg,sem)
  })
  
  #print(res.ls)
  res.df <- do.call(rbind,res.ls)
  res.df <- res.df[complete.cases(res.df),] #remove rows with NA in it
  colnames(res.df) <- c('item','avg','sem')
  row.names(res.df) <- 1:nrow(res.df)
  res.df
}



#partition the df or matrix into 2 categories required for generating test and training samples 
#col: the col based on which you should divide
#splitno: gives the percentage of entries in the first category, e.g., training
#op=1 : split it so that splitno % is taken from every stimulus
#op=2 : choose splitno % stimuli randomly
splitData <- function(dat.df,col=ncol(dat.df),splitno=80,op=1){
  if(op==2){#choose splitno stim randomly
    train.vec <- sample(1:nrow(dat.df),floor(nrow(dat.df)*(splitno/100)))
  }
  if(op==1){
    stim <- unique(dat.df[,col]) #get the stim in this set
    train.lst <- lapply(stim,function(x){#now choose the train no of trial for each stimulus
      posns <- which(x==dat.df[,col])
      #cat('\npos',posns)
      nostim <- floor(length(posns)*(splitno/100))
      posns[1:nostim]
    })  
    #get the training vectors, and training set is these vectors, and test is eerything else
    train.vec <- unlist(train.lst)
  }
  #cat('\nop=2',train.vec,';',odors)
  train.df <- dat.df[train.vec,]
  test.df <- dat.df[-train.vec,]
  list(train.df,test.df)
}


#splits the data into chunks 
#dat: the data a vector, chunk- size of chunk, 
#op = 1, return split data as a list, 2= just return the chunks as contiguous sequences
splitData2chunks <-function(dat,chunk=length(dat)/10,op=1){
  #cat('data length',length(dat))
  #take care so that if its a data frame we split it up accordingly
  no.chunks <- floor(length(dat)/chunk)
  seq.chunks <- rep(c(1:no.chunks),each=chunk) #make all chunks except the last one
  #figure out the length of the last chunk
  tmp <- length(dat)/chunk - no.chunks 
  if(tmp > 0) last <- length(dat) - (no.chunks*chunk)
  else last <- 0
  #cat('remainder',tmp,',',last,',',ceiling(last))
  chunks <- c(seq.chunks,rep(no.chunks+1,last))
  switch(op,split(dat,chunks),chunks)
}

#col1: on whcih the opereation has to be done
#colfact: the column of factors on which col1 should be operated
#op = 1, mean, 2 - sd, 3 -sem, 4 - do nothing
getColStats <-function(col1,colfact,op=1){
  tmp <- split(col1,colfact) #split col1 based on colfact
  switch(op,sapply(tmp,function(x) mean(x)),
         sapply(tmp,function(x) sd(x)),
         sapply(tmp,function(x) sd(x)/sqrt(length(x))),col1)# get the mean of the split lists and put them back
}

#Given a data frame whose first column is factors, this will average the values for all the 
#other columns for each of the factors
#factor: specifies the columns that are characters
#factorcol: the factor column on which the rest of the df should be averaged
#op = 1, mean, 2 - sd, 3 -sem
getDfAvg <-function(data.df,factorcol=1,op=1){
  coltype <- sapply(1:length(data.df),function(x) class(data.df[,x]))
  #cat('coltype',coltype,'\n')
  coltype <- which(coltype==class(1))
  #cat('coltype',coltype,'\n')
  res.ls <- lapply(coltype,function(x) getColStats(data.df[,x],data.df[,factorcol],op=op))
  res.df <- convertNestedListsDF(res.ls) #convert lists to df
  names(res.df) <- names(data.df)[coltype]
  res.df[names(data.df)[factorcol]] <- row.names(res.df) #the first col contains the factors
  len.df <- length(res.df)
  row.names(res.df) <- 1:nrow(res.df)
  res.df[c(len.df,1:len.df-1)]
}

#get statistics on the data frames
#dat.df: the data frame
#op=1:mean,sd,sem; 2:mean; 3:sd, 4:sem
getDfStats <- function(dat.df,start=1,op=1){
  numind <- 1:length(dat.df[1,])
  mn <- apply(dat.df[,numind],2,mean)
  std <- apply(dat.df[,numind],2,sd)
  sem <- std/(length(dat.df[,1])**.5)
  res <- rbind.data.frame(mn,std,sem)
  names(res) <- names(dat.df)[numind]
  row.names(res) <- c('mean','sd','sem')
  switch(op,res,res[1,],res[2,],res[3,])
}


#Given a data frame whose first column is factors, this will split it into lists, where each list contains the 
#data frame of just one factor
#other columns for each of the factors
#factor: specifies the columns that are characters
#factorcol: the factor column on which the rest of the df should be averaged
#op = 1, mean, 2 - sd, 3 -sem
getDfSplit.old <-function(data.df,factorcol=1,op=1){
  #split the df into lists bassed on the col type
  #res.ls <- lapply(coltype,function(x) getColStats(data.df[,x],data.df[,factorcol],op=op))
  res.ls <- split(data.df,data.df[,factorcol])
  res.ls
}

#Given a data frame whose first column is factors, this will split it into lists, where each list contains the 
#data frame of just one factor
#other columns for each of the factors
#factor: specifies the columns that are characters
#factorcol: the factor column on which the rest of the df should be averaged
#cols: c() - get all columns, number = get these cols alone
#op = 1, mean, 2 - sd, 3 -sem
getDfSplit <-function(data.df,factorcol=1,cols=c(),op=1){
  #split the df into lists bassed on the col type
  #res.ls <- lapply(coltype,function(x) getColStats(data.df[,x],data.df[,factorcol],op=op))
  #first convert the factors col so that it does not carry the baggage of earlier factors, convert to string
  res.ls <- split(data.df,as.character(data.df[,factorcol]))
  if(isDataType(data.df)==2){#its a matrix
    res.ls <- lapply(res.ls, function(x) {
      res <- matrix(x,ncol=ncol(data.df),byrow=F) #refactor the vector into a matrix
      colnames(res) <- colnames(data.df)
      res
    })
  }
  if(is.null(cols)) return(res.ls)
  res.ls <- lapply(res.ls, function(x) x[,cols])
  res.ls
}


#given a list of data frames, most likely the result of the getDfSplit call, will give 
#the mean, sd, and other things
getDfSplitAvg <- function(df.lst,factorcol=1,op=1){
  
}

#this function gets the nth subelement of every list element
getNElem <- function(lst,n,op=1){
  sapply(lst,function(x) x[n]) #goes thru the list and gets the nth element
}

#given a list of data frames gets the Nth element of the Mth row from all dataframes
getListDfsMRowNElem <- function(lst.dfs,m,n,op=1) {
  # Use lapply to iterate over the list of dataframes
  result <- lapply(lst.dfs, function(df) {
    # Check if the dataframe has enough rows
    if (nrow(df) >= m) {
      return(df[m, n])  # Return the second element of the nth row
    } else {
      return(NA)  # Return NA if the nth row does not exist
    }
  })
  # Convert the list to a vector
  res <- unlist(result)
  names(res) <- names(lst.dfs)
  res
}

#given a list of dataframes, gets the nth column of each and returns a list of 
#values arranged in sorted order, without duplicates
getListDFsColRange <- function(lst.dfs,n,op=1){
  # Extract the nth column from each dataframe in the list
  res.ls <- lapply(lst.dfs, function(df) {
    if (ncol(df) >= n) {
      return(df[,n])
    } else {
      return(NULL)  # Return NULL if the dataframe does not have an nth column
    }
  })
  res <- sort(unique(cleanNAVec(unlist(res.ls))))
  res
}

#this function takes a data.lst and means which is the same length as data.lst and returns
#an data frame with 2 columsn. The 1st are the unpacked elements of data.lst, and the 2nd
#is means, such that means[i] is repeated as many times as the length of length(data.lst[[i]])
#op=1, return a df of the x and y coords, 2 - a df of y and x coords
makeXYDf <- function(data.lst,means,op=1){
  #generates a second list, where the corresponding entry in means is 
  #reperated as many times as the length of the list element
  res <- lapply(1:length(data.lst),function(x){
    tmp <- rep(means[x],each=length(data.lst[[x]]))
  })
  switch(op,data.frame(unlist(data.lst),unlist(res)),
         data.frame(unlist(res),unlist(data.lst)))
}


#gien a matrix will rotate it based on op
#op: 1, rotate once to the left. 2 - rotate once to the right
rotateMatrix <- function(mat,op=1){
  #rotate to the left: take the first row, make it the first column, 2nd row 2nod col and ...
  #the top elem of row 1 is the first element of the column
  nr <- nrow(mat); nc <- ncol(mat)
  newmat <- matrix(rep(0,prod(nr,nc)),nrow = nc,ncol = nr)
  #rotate so that the 1st col, is the first row but in reverse
  switch(op,for (i in seq_wrap(1,nr)) {
    newmat[,i] <- rev(mat[i,])  
  }, #right rotation: 1st row is the first col but in reverse
  for (i in seq_wrap(1,nc)) {
    newmat[i,] <- rev(mat[,i])  
  })
  #names: left rotation - columns reverse, right rotation - rows reverse
  colnames(newmat) <- switch(op,rownames(mat),rev(rownames(mat)))
  rownames(newmat) <- switch(op,rev(colnames(mat)),colnames(mat)) 
  newmat
}

#transpose a data frame
#op:1 transpose row and col names, too; 2 - don't transpose row names, 3 - dont transpose rownames, but include them as an extra col
transposeDF <- function(dat.df,op=1){
  res <- data.frame(t(as.matrix(dat.df)),stringsAsFactors = F) #leave strings as they are.
  #make sure the col and row names are transposed too
  colnames(res) <- rownames(dat.df)
  #cat('\n transpose noyo',colnames(dat.df),'op ',op)
  if(op==1) rownames(res) <- colnames(dat.df)
  #cat('\n',str(colnames(dat.df)))
  if(op==3) {
    res <- insertColDf(res,newcol = colnames(dat.df),posn = ncol(res)+1 )
  }
  res
}

#transpose a list made of 'n' items. so that we have a list of length n, where the first list is a list of first ones,
#second is second ones.
transposeList <- function(dat.ls,op=1){
  noitems <- length(dat.ls[[1]])
  res.ls <- lapply(1:noitems, function(i) unlist(lapply(dat.ls,'[[',i)) )
  names(res.ls) <- names(dat.ls[[1]])
  res.ls
}


#given a vector index, returns the x,y indices of a matrix with r rows and c cols
#assuminng number is bycol=T,i.e., the elements are counted by going across each col
#returns c(row posn,col posn)
#r - no of cols or rowws, depending on bycol=T or byrow=T
#if op = 2, byrow = T matrix, i.e. the vector elements are counted by going down each row
#first
getMatrixInd <- function(vecindex,c=3,op=1){
  rowind <- floor((vecindex-1)/c) + 1 #havve to do inddex - 1 and +1 in the end to handle
  colind <- ((vecindex-1) %% c) + 1  # 1 based indices rather than 0 based indices
  switch(op,c(rowind,colind),c(colind,rowind))
}

#given a matrix element and its r,c postion will return the vector position according
#to bycol= T matrix for op = 1
#if op = 2, byrow = T matrix, i.e. the vector elements are counted by going down each row
#first
getVectorInd <- function(rowind,colind,r=3,c=3,op=1){
  switch(op,(rowind-1)*r + colind,(colind-1)*r + rowind)
}

#gets the indices of the diagonal elements, in order
#op=1, get it in matrix form c(r,c); 2 - in vector form, i.e., a flattened vector
getDiagInd <- function(r=3,c=3,op=1){
  delems <- lapply(1:r, function(i) c(i,i))
  delems.vec <- sapply(delems,function(i) getVectorInd(i[1],i[2],r = r,c = c,op=2))  
  switch(op,delems,delems.vec)
}


#function that given the size of the square matrix, converts the list index into 
#a matrix element position. e.g., index i from 1..n would become x,y. We are going
#bycol=T, which means that you count across columns
#op=1,bycol=T, 2
convertMatVec <- function(len,op=1){
  #gives the pair x,y that would convert to position i
  pairs <- lapply(1:len^2, function(x){
    getMatrixInd(vecindex = x,c = len,op=op)
  })
  pairs
}


#function shrinks a matrix of nx m by a factor of a x b. Have to ensure that n and m are divisible by a and b
#shrinkfactors: c(a,b). if empty will shrink by half i.e c(2,2)
#op=1, matrix, 2 - an array
shrinkMat <- function(mat,shrinkfactors=c(),op=1){
  #get the dimensions of the matrix
  dims <- dim(mat)
  #now check if it is divisible by the shrink facctors
  if(is.null(shrinkfactors)) shrink <- sapply(dims, function(x) getLowestDivisor(x))
  else shrink <- shrinkfactors
  dims <- dims/shrink #the new dimensions
  mat.new <- createNewDataStruct(dims) 
  #shrink by the first dimension, has to be done in a for loop
  for(i in 1:dims[1])
    for(j in 1:dims[2]){
      #do it submatrix by submatrix size given by shrink
      #and indexed by i*shrink1,j*shrink2
      #cat('\n',shrink,',',i,',',j,':',(((i-1)*shrink[1])+1):(i*shrink[1]),',,',(((j-1)*shrink[2])+1):(j*shrink[2]))
      mat.new[i,j] <- sum(mat[(((i-1)*shrink[1])+1):(i*shrink[1]),(((j-1)*shrink[2])+1):(j*shrink[2])])
    }
  res <- mat.new/prod(shrink)
  res
}


#scales the vector so the minimum and maximum values are stretced by scale factor
#which implies the values in between will also be changed accordingly
#op=1, scale while keeping the min the same, 2 - keeping the center the same
scaleVec <- function(vec,scale.fac=1,op=1){
  vec.min <- min(vec)
  newvec <- sapply(vec, function(x) vec.min + (x-vec.min)*scale.fac)
  if(op==1) return(newvec)
  cur.range <- max(vec) - min(vec) 
  newmin <- mean(vec) - (cur.range*scale.fac)/2
  newvec <- newvec - (vec.min - newmin)
  newvec
}

#scales the vector to be in the following ranage
#vec: the vector to be scaled
#rangeval: the range to which t should be scaled given as c(rangemin,rangemax)
scaleVecRange <- function(vec,rangeval,op=1){
  cur.min <- min(vec); cur.max <- max(vec)
  scale.fac <- (rangeval[2] - rangeval[1])/(cur.max - cur.min)
  newvec <- (vec-cur.min)*scale.fac + rangeval[1]
  newvec
}

#function that computes the minimum, maximum, or mean for specified columns of a 
#data frame based on the value of the op parameter:
getDfBasics <- function(dat.df, cols, op = 1) {
  # Define the function to apply based on the option
  stat.func <- switch(op,
                      `1` = min,   # op = 1 -> minimum
                      `2` = max,   # op = 2 -> maximum
                      `3` = mean,  # op = 3 -> mean
                      stop("Invalid option for op. Use 1 (min), 2 (max), or 3 (mean)."))
  
  # Apply the function to the specified columns
  result <- sapply(dat.df[, cols, drop = FALSE], stat.func, na.rm = TRUE)
  result
}

#function works on a list of data frames, where each data frame has the same columns but 
#different values. The function first combines the data frames by their columns, computes the 
#specified operation (minimum, maximum, or mean) for the given columns, and returns the result.
getListDfsBasic <- function(list.dfs, cols, op = 1) {
  # Combine all data frames in the list by their rows
  combined.df <- do.call(rbind, list.dfs)
  
  # Call the getDfBasics function on the combined data frame
  result <- getDfBasics(combined.df, cols, op)
  result
}

#function that scales all the columns of a list of data frames. Each column from each 
#data frame is just the same variable. So, normalization is done on a column across all data frames
#scalefac: the range of over which to normalize. if scale fac is c(min,max), 
#it adjusts the numbers to go from min to max
#op=1: normalize by minimnum and max value
#cols: the cols that have to be examined. If only one number, it is the startin col
#and cols is col:ncol(dat.df)
scaleListDfs <- function(lst.dfs,scalefac=c(0,100),cols=c(),op=1){
  allcols <- condVal(length(cols)==0,1:ncol(lst.dfs[[1]]),condVal(length(cols)==1,cols:ncol(lst.dfs[[1]]),cols))
  nocols <- ncol(lst.dfs[[1]]) #no of cols
  no.dfs <- length(lst.dfs)
  #get the minimum and maximum for each column and normalize
  comb.df <- do.call(rbind,lst.dfs)
  min.vec <- getDfBasics(comb.df,cols = allcols,op=1)
  max.vec <- getDfBasics(comb.df,cols = allcols,op=2)
  #cat('\nst',cols,allcols,'\n',min.vec,str(comb.df))
  range.vec <- max.vec - min.vec
  norm.mat <- rbind(min.vec,max.vec,range.vec)
  #now, go through each data frame, and for each column scale the elements
  #so that they fall within a range from 0 to 1 and then you can scale it
  #first, determine the scaling factors
  scale.f <- scalefac[2] - scalefac[1]; scale.m <- scalefac[1]
  comb.df[,allcols] <- sweep(sweep(comb.df[, allcols], 2, min.vec, "-"), 2, range.vec, "/") * scale.f + scale.m
  res.ls <- splitDfByLen(comb.df,len.seq = sapply(lst.dfs,nrow))
  return(res.ls)
  res.ls <- lapply(1:no.dfs, function(i){
    #for each df normalize all columns: scale.min + ((val-min)/range)*scale.range
    allcols <- sapply(1:ncol(lst.dfs[[i]]), function(j){
      res <- ((lst.dfs[[i]][,j] - norm.mat[1,j])/norm.mat[3,j] * scale.f) + scale.m
    })
    res.struct <- matchDataStruct(lst.dfs[[i]],allcols) #ensures that the 2 structures match
  })
  #now splut comb.df back into 3. 
  res.ls
}

#given a dataframe dat.df, and length sequence, will break dat.df on the basis of this. 
splitDfByLen <- function(dat.df,len.seq=c(),op=1){
  res.ls <- split(dat.df,unlist(lapply(1:length(len.seq),function(i) rep(i,len.seq[i]))))
  res.ls
}

#function condVal, which takes three arguments: condition, yesval, and noval. The function
#returns yesval if condition is TRUE, and noval otherwise.
condVal <- function(condition, yesval, noval) {
  if (condition) {
    return(yesval)
  } else {
    return(noval)
  }
}

#function that will take a structure dat.df which can be a matrix or data frame, and a second strcture, 
#moddat.df which is some processed form of dat.df. The function should ensure that moddat.df has the
# same columnnames and rownames as dat.df, and is of the same type as dat.df
matchDataStruct <- function(dat.df, moddat.df,op=1) {
  # Ensure column names are the same
  if (!is.null(colnames(dat.df))) {
    colnames(moddat.df) <- colnames(dat.df)
  }
  # Ensure row names are the same
  if (!is.null(rownames(dat.df))) {
    rownames(moddat.df) <- rownames(dat.df)
  }
  # Ensure the type matches
  if (is.data.frame(dat.df) && !is.data.frame(moddat.df)) {
    moddat.df <- as.data.frame(moddat.df)
  } else if (!is.data.frame(dat.df) && is.data.frame(moddat.df)) {
    moddat.df <- as.matrix(moddat.df)
  }
  moddat.df
}

#takes a data frame dat.df and returns a vector containing the indices of all numeric columns in ascending order
getNumericColsDf <- function(dat.df) {
  # Check if each column is numeric
  numeric.indices <- which(sapply(dat.df, is.numeric))
  
  # Return the indices in ascending order (default behavior of `which`)
  numeric.indices
}


#scales a matrix or data frames so that in any column the values go from min to max as specified by
#scalefac: the range of over which to normalize. if scale fac is c(min,max), 
#it adjusts the numbers to go from min to max
#op=1: normalize by minimnum and max value
#cols: the cols that have to be examined. If only one number, it is the startin col
#and cols is col:ncol(dat.df)
scaleDF <- function(dat.df,scalefac=c(0,100),cols=c(),op=1){
  allcols <- condVal(length(cols)==0,1:ncol(lst.dfs[[1]]),condVal(length(cols)==1,cols:ncol(lst.dfs[[1]]),cols))
  #get the scaling range and the minimum
  scale.f <- scalefac[2] - scalefac[1]; scale.m <- scalefac[1] 
  min.vec <- getDfBasics(dat.df,cols = allcols,op=1)
  max.vec <- getDfBasics(dat.df,cols = allcols,op=2)
  range.vec <- max.vec - min.vec
  data.df <- dat.df
  data.df[,allcols] <- sweep(sweep(dat.df[, cols], 2, min.vec, "-"), 2, range.vec, "/") * scale.f + scale.m
  data.df
}


#scales all the other columns of the matrix so that they have the same mean as the column
#specified by col
#scalemean: scale to a specified mean
#scaletype: scale to mean, max, or min
#op = T, all entries should be positive, F - doesnt matter
scaleMat <- function(mat,col=1,scalemean=0,scaletype=1,pos=T,op=1){
  rmat <- mat
  if (is.null(dim(mat))) { #if it is a 
    rmat <- as.data.frame(mat) #if its a vector, dimensionalize it
  }
  if (scalemean>0) colmean <- scalemean
  else colmean <- switch(scaletype,mean(rmat[,col]),max(rmat[,col]),min(rmat[,col])) 
  #claculate the mean
  res <- sapply(1:ncol(rmat),function(x){
    tmp <- rmat[,x]
    #first take care of positive entries
    tmpmin <- min(tmp)
    if(tmpmin <0 && pos) tmp <- tmp - tmpmin
    tmpmean <- switch(scaletype,mean(tmp),max(tmp),min(tmp))
    tmp <- tmp *(colmean/tmpmean)
    #mat[,x] <- tmp
  })
  if (ncol(res) == 1) return(as.vector(res))
  res
}

#normalizes the matrix so that every row has no negative numbers. 
#Basically for each row adds the most -ve number to it, and then scales all the entries 
#so that all rows have the same mean.
#scalemean: scale to a specified mean
#op = 1, by row, 2 - by col
normalizeMat <- function(mat,scalemean=1,changemean=F,op=1){
  if (is.null(dim(mat))) { #if it is a 
    rmat <- as.data.frame(mat) #if its a vector, dimensionalize it
  }
  if(op==1) rmat <- mat
  else rmat <- transposeDF(mat)
  res.df <- sapply(1:nrow(rmat), function(x){
    rowmin <- min(rmat[x,])
    newrow <- unlist(rmat[x,] - rowmin) #add or reduce so that the lowest value is 0
    #cat(scalemean,mean(newrow),'\t',unlist(newrow))
    if(changemean) newrow <- newrow * (scalemean/mean(newrow))
    newrow
  })
  switch(op,transposeDF(res.df),res.df)  
}

#converts the matrix of real numbers into integers
#mat: the matrix or df
#op: conversion type- 1 - floor, 2 - ceiling, 3 - round
convertMatInteger <- function(mat,op=1){
  res.mat <- mat
  fun <- switch(op,floor,ceiling,round)
  for (i in 1:ncol(mat)) {
    #cat('\ndoing col ', i)
    #got through each col and turn it into integers
    res.mat[,i] <- fun(res.mat[,i])
  }
  res.mat
}

#creates a new matrix or array structure of the prescribed nunmber of dimensioms
#op=1, create a matrix or array, 2 -create avecotr
createNewDataStruct <-function(dims,op=1){
  res <- rep(0,prod(dims))
  res <- array(res,dim = dims) #if its 2 dimensions its a matrix otherwise an array
  switch(op,res,as.vector(res))
}



#function that converts vector intoa matrix of dimensions n x m
convertVec2Mat <- function(vec,norows=c(),nocols=c()){
  matrix(vec,nrow = norows)
}

#takes a vector and refactors it so that only the elements of the current vector function
#as factors without any of the old remenants
reFactorVec <- function(vec,op=1){
  as.factor(as.character(vec))
}

#this function gets the length of the vector or matrix
#mat: mat or vector
getLength <- function(mat,op=1){
  if(isDataType(mat) == 1) len <- length(mat) #if a vector 
  else len <- nrow(mat)
  len
}

#given a vecctor will get sorted result of the posns of the elements in the original vector 
#a clever way of doing it.
#vec: the vector
#op=1,increasing rank, 2 - decreasing rank
getVecRank <- function(vec,decrease=T,op=2){
  elems <- sort(unique((vec))) #get the unique elements in sorted order
  posns <- split(seq_along(vec),vec) #the posns of every element
  res <- unlist(posns[as.character(elems)])
  names(res) <- sort(vec)
  switch(op,res,rev(res))
}

#given a vector/mat will return another vector or df of the entries of the vector ranked in ascending or descending order
#decrease: T: rank 1 is the max, F: rank 1 is the min
#col: on which the ranking should be done
getMatRank <- function(mat,col=1,decrease=T,op=1){
  #sort and return as a matrix or df
  tmp <- getMatSortPosns(mat,col = col,decrease = F,op = 2)
  #now, just take the sorted posns vector, add an descending vector and sort that vector
  list.vec <- getLength(tmp):1
  ntmp <- insertColDf(as.data.frame(tmp),newcol = list.vec,posn = 2)
  res <- getMatSortPosns(ntmp,col = 1,decrease = F,op=2) #this should have the ranks now
  #we dont need the first two columns, we can just get rid of them and return the rest
  if(!decrease) res[,3] <- 1 + length(res[,3]) - res[,3] #if rank 1 is min
  row.names(res) <- 1:getLength(tmp)
  switch(op,res[,3],res[,-c(1,2)])
}

#given a matrix or df. will sort the matrix and return the sorted positions in ascending order
#sortDF
#col: the col along which to sort the matrix
#op=1, return the sorted positions, 2 - the sorted matrix, 3 - sorted matrix without
#posn vector
getMatSortPosns <-function(mat,col=1,decrease=F,op=1){
  if(isDataType(mat) == 1) cmat <- as.matrix(mat) #if a vector convert to a matrix
  else cmat <- mat
  res <- cbind(1:nrow(cmat),mat)
  res <- res[order(res[,col+1],decreasing = decrease),] #now sort by the column specified, add 1 to account for the row we added
  #the first column are the sorted posns, the second onwards is the original matrix
  switch(op,res[,1],res,res[,-1])
}

#gets the posns of the vector based on op
#sortop=1, use the sorting function, 2 - use sort string nos
#the sorted posns vector gives the sorted vector, and the position of the these elements in the original vector
#op=1, return the sorted positions, 2 - the sorted vector
getVecSortPosns <-function(vec,decrease=F,sortop=2,op=1){
  #cat('\n',str(vec))
  #convert the matrix or data frame to avector by using as.vector
  res <- cbind.data.frame(1:length(vec),as.vector(vec)) #the first col is going to give you the new sorted order
  #print(res[1:10,])
  #get the sorted posns
  if(sortop==1) sortpos <- res[order(res[,2],decreasing = decrease),]
  if(sortop==2) sortpos <- res[sortStringNos(vec,decrease=decrease,op=2),]
  names(sortpos) <- c('sorted','values')
  #res <- vec[sortpos] #now sort by the column specified, add 1 to account for the row we added
  switch(op,sortpos[,1],sortpos[,2])
}

#gets the posns of the vector or matrix based on op
#sortop=1, use the sorting function, 2 - use sort string nos
#the sorted posns vector gives the sorted vector, and the position of the these elements in the original vector
#op=1, return the sorted positions, 2 - the sorted vector
#funop: the option of which function to call 1: vector function, 2 - matrix, 3 - determine automatically
getSortPosns <-function(vec,col=1,decrease=F,sortop=2,funop=3,op=1){
  #determine if it is a matrix or vector and call appropriate function
  if((funop==3) && (length(nrow(vec))==0) ) funtype <- 1 #vector
  if((funop==3) && (length(nrow(vec))>0) ) funtype <- 2 #vector
  if(funop<3) funtype <- funop
  res <- switch(funtype,getVecSortPosns(vec,decrease = decrease,sortop = sortop,op = op),
                getMatSortPosns(vec,decrease = decrease,col = col,op = op))
  res
}

#gets the sorted vector and posns of the sorted item either in 
#change this to just call the vector functions
#decrease, T - decreasing or F - increasing orrder
#op: 1 - vals, 2 - posns, 3 - names in sorted order, 4 - all of them as 3 lists
getSortedVec <- function(vec,decrease=T,op=1){
  #sortmat <- getMatSortPosns(vec,decrease = decrease,op = 2) old code
  res <- switch(op,getVecSortPosns(vec,decrease = decrease,op = 2),
         getVecSortPosns(vec,decrease = decrease,op = 1),
         names(vec)[getVecSortPosns(vec,decrease = decrease,op = 1)],
         list(getVecSortPosns(vec,decrease = decrease,op = 2),
              getVecSortPosns(vec,decrease = decrease,op = 1),
              names(vec)[getVecSortPosns(vec,decrease = decrease,op = 1)])
  )
  #cat('\nsorted',str(sortmat))
  #res <- switch(op,sortmat[,2],sortmat[,1],names(vec)[sortmat[,1]],
  #              list(sortmat[,2],sortmat[,1],names(vec)[sortmat[,1]]))
  res
}



#rank order rows so that the lowest rows are the rows with ones in the earliest columns
orderMatRowsPrecedence <- function(mat,thresh=0,op=1){
  matones <- setMatNonzero(mat,val = thresh)
  #cat(str(mat),'\nOrderMat',isDataType(mat))
  if(isDataType(mat)==1) print(mat)
  rowvals <- sapply(1:nrow(mat),function(i) {
    posns <- which(mat[i,] > thresh)
    sort(posns)[1]
  })
  #cat('\n',rownames(mat))
  getMatSortPosns(cbind(rowvals,1:length(rowvals),rownames(mat)),op=3)
  
}

#function that converts a list of numbers to a list of strings or characters. 
#lst: the list of vectors of numbers
convertListNos2Str <- function(lst,op=1){
  #len <- length(lst)
  res.ls <- lapply(lst, function(x){
    as.character(x)
  })
  res.ls
}

# Function to fill missing values in a vector
# vector: that we want to fill
# full_names: the names of vector elements with no missing entries
fill_missing <- function(vector, full_names, fill_value=0,op=1) {
  existing_names <- names(vector)
  missing_names <- setdiff(full_names, existing_names)
  vector[missing_names] <- fill_value
  vector[full_names]  # Return with correct order
}


#function that takes a list of vectors, where each vector might have missing named entries, 
#and pads the missing values with fill_value
#op=1, returns as a list, 2: return as a DF
ConvertListWMissEntry <- function(vec.lst, value = 0, op = 1) {
  # Determine the complete range of names across all vectors
  all_names <- unique(unlist(lapply(vec.lst, names)))
  min_name <- min(as.integer(all_names))
  max_name <- max(as.integer(all_names))
  full_names <- as.character(min_name:max_name)
  
  # Apply the fill function to each vector in the list
  filled_vectors <- lapply(vec.lst, fill_missing, full_names, value)
  
  # Depending on op, return a list or a data frame
  if (op == 1) {
    res <- filled_vectors
  } else if (op == 2) {
    # Combine filled vectors into a data frame
    res <- as.data.frame(do.call(rbind, lapply(filled_vectors, function(x) as.numeric(x))))
    names(res) <- full_names
  } else res <- vec.lst
  res
}


#converts a list with an unequal number of elements into a data frame by converting 
#everything to a list of characters/strings and then padding them all with 'pad' to 
#get the same length.
convertUnequalListToDf <- function(dat.lst,pad='na',op=1){
  #lengths of list vectors
  lens <- sapply(dat.lst, function(x) length(x))
  #test if all the list elements are of equal length
  if(all(lens==lens[1])) return(convertListToDF(dat.lst = dat.lst))
  #they are not the same length
  maxlen <- max(lens)
  dat.strlst <- convertListNos2Str(dat.lst)
  res.ls <- lapply(dat.strlst, function(x) padVecStr(x,n = maxlen,pad = pad))
  res.df <- convertListToDF(res.ls)
  res.df
}

#given a list with each list element a vector
#default: do not convert strings to factors.
#coltype: 1 = keep the first column as a string, 2 - change it to numeric
#op=1
convertListToDF <- function(dat.lst,coltype=2,op=1){
  #cat(names(dat.lst),coltype)
  lst.names <- switch(coltype,names(dat.lst),convertVecString2Nos(names(dat.lst)) )
  #do it with a for loop
  res.df <- cbind.data.frame(dat.lst[[1]],stringsAsFactors=F)
  if(isDataType(dat.lst[[1]]) == 3)
    name.vec <- names(dat.lst[[1]])
  #cat('\nconL2DF',names(dat.lst[[1]]),':',isDataType(dat.lst[[1]]),str(dat.lst[[1]]))
  for (i in seq_wrap(2,length(dat.lst)) ) {
    res.df <- cbind.data.frame(res.df,dat.lst[[i]],stringsAsFactors=F)
    if(isDataType(dat.lst[[i]]) == 3)
      name.vec <- c(name.vec,names(dat.lst[[i]]))
  }  
  #cat('\nconverLtoD')
  if(isDataType(dat.lst[[1]]) == 3)
    colnames(res.df) <- name.vec
  else
    colnames(res.df) <- names(dat.lst)
  #res <- joinListDFs(lst.dfs)
  res.df
}

#given a list with each list element a vector
#coltype: 1 = keep the first column as a string, 2 - change it to numeric
#op=1
convertListToDF.old <- function(dat.lst,coltype=2,op=1){
  lst.names <- switch(coltype,names(dat.lst),convertVecString2Nos(names(dat.lst),op=1) )
  lst.dfs <- lapply(1:length(dat.lst),function(i){
    tmp <- rep(lst.names[i],length(dat.lst[[i]]))
    #cat('\n',i,':',tmp,dat.lst[[i]])
    cbind.data.frame(tmp,dat.lst[[i]])
  })
  res <- joinListDFs(lst.dfs)
  res
}

#given a Matrix or Df with x,y,z or any other coordinates along the columns, will calculate the center
#headrow: the row for the default values
computeMatCenter <- function(mat,cols=c(),headrow=1,op=1){
  res.df <- mat[headrow,]
  #given a dataframe will compute the mean of the cols
  for (i in cols) {
    res.df[1,i] <- mean(mat[,i])
  }
  res.df
}

#function takes a DF or matrix and gives all the rows where the prescribed col
#is above thresh
#op=1- return a sorted matrix by col, 2 - non-sorted cols
getThreshDF <- function(mat.df,thresh=0,col=1,op=1){
  #sort based on the column
  tmp <- getMatSortPosns(mat = mat.df,col=col,op=2)
  vec <- tmp[,col+1] #now get the valyues over which to threshold
  posns <- which(vec>thresh) #and the posns that are above thresh
  #cat('\nposn',posns,':::')
  res <- tmp[posns,] #filter out the other posns
  if(isDataType(res)==const.DataType$vector) res <- t(matrix(res)) #make sure it is a matrix; convert vec to matrix
  if(op==2) res <- getMatSortPosns(res,col=1,op=3) #the original unsorted matrix
  #cat('\nhere2',str(res))
  if(nrow(res)==1) t(matrix(res[,-1]))
  else res[,-1]
}

#for each unique entry in col, will get all the values of col2 that are associated with this value
#col: column for calculating the unique values
#target: the target col for doing the list
listDF <-function(mat.df,col=1,target=2,op=1){
  #cat('sDF\n')
  vals <- unique(mat.df[,1]) #get the vals over which to sum
  #get all the vals for the position and sum all the rows
  res.ls <- lapply(vals, function(x){
    posns <- which(mat.df[,1]==x)
    res <- unlist(mat.df[posns,target])
    names(res) <- posns
    res
  })
  names(res.ls) <- vals
  res.ls
}  
  
#sums all the elements in the subsequent cols based on the values in 
#col column
sumDF <- function(mat.df,col=1,op=1){
  #cat('sDF\n')
  vals <- unique(mat.df[,1]) #get the vals over which to sum
  #get all the vals for the position and sum all the rows
  res.ls <- lapply(vals, function(x){
    posns <- which(mat.df[,1]==x)
    #if more than one element sum all the matrix posns
    if(length(posns) > 1) tmp <- apply(mat.df[posns,],2,sum)
    else tmp <- mat.df[posns,]
    c(x,tmp[-1]) #val, and all the values except val
  })
  res.df <- transposeDF(convertNestedListsDF(res.ls)) #convert to DF
  colnames(res.df) <- colnames(res.df)
  row.names(res.df) <- 1:length(vals)
  res.df
}

#mean of all the elements in the subsequent cols based on the values in 
#col column
meanDF <- function(mat.df,col=1,op=1){
  vals <- unique(mat.df[,1]) #get the vals over which to sum
  #get all the vals for the position and sum all the rows
  res.ls <- lapply(vals, function(x){
    posns <- which(mat.df[,1]==x)
    #if more than one element sum all the matrix posns
    if(length(posns) > 1) tmp <- apply(mat.df[posns,],2,mean)
    else tmp <- mat.df[posns,]
    c(x,tmp[-1]) #val, and all the values except val
  })
  res.df <- transposeDF(convertNestedListsDF(res.ls)) #convert to DF
  colnames(res.df) <- colnames(res.df)
  #cat('\nmDF',str(res.df))
  row.names(res.df) <- 1:length(vals)
  res.df
}

#mean of all the elements in the subsequent cols based on the values in 
#col column
sdDF <- function(mat.df,col=1,op=1){
  vals <- unique(mat.df[,1]) #get the vals over which to sum
  #get all the vals for the position and sum all the rows
  res.ls <- lapply(vals, function(x){
    posns <- which(mat.df[,1]==x)
    #if more than one element sum all the matrix posns
    if(length(posns) > 1) {
      tmp <- apply(mat.df[posns,],2,sd)
      tmp <- tmp/(length(posns)^0.5)
    }
    else tmp <- mat.df[posns,]
    c(x,tmp[-1]) #val, and all the values except val
  })
  res.df <- transposeDF(convertNestedListsDF(res.ls)) #convert to DF
  colnames(res.df) <- colnames(res.df)
  #cat('\nmDF',str(res.df))
  row.names(res.df) <- 1:length(vals)
  res.df
}

#will renormalize the value of every row so that each column value is the column value divided by the sum of the row
#dat.df: the DF whose rows are to be renormalized
renormDF <- function(dat.df) {
  # Calculate the row sums
  row.sums <- rowSums(dat.df)
  # Renormalize each row
  res.df <- dat.df / row.sums
  res.df
}

#gets the minimum of each column of matrix
getmin <- function(mat){
  #sapply(1:ncol(mat),function(x) min(mat[,x]))
  switch(isDataType(mat),min(mat),apply(mat,2,min),apply(mat,2,min),apply(mat,2,min))
}

#gets the sd of each column of matrix
getsd <- function(mat){
  switch(isDataType(mat),sd(mat),apply(mat,2,sd),apply(mat,2,sd),apply(mat,2,sd))
  #sapply(1:ncol(mat),function(x) sd(mat[,x]))
}

#gets the maximum of each column of matrix
getmax <- function(mat){
  switch(isDataType(mat),max(mat),apply(mat,2,max),apply(mat,2,max),apply(mat,2,max))
  #apply(mat,2,max)
  #sapply(1:ncol(mat),function(x) max(mat[,x]))
}




#given a matrix or vector, find the posns that over a certain value t
#op =1 , return the posns, 2 - the values
findIndAboveThresh <-function(mat,t=1,op=1){
  if(isDataType(mat) == 1) cmat <- as.matrix(mat) #if a vector convert to a matrix
  else cmat <- mat
  #now just go col. by col.
  res <- lapply(1:ncol(cmat), function(x) {
    posn <- which(cmat[,x] > t) #get all the rows poistns
    resindx <- cbind.data.frame(posn,rep(x,length(posn))) #put them in row,col format
  })
  res <- combineListMat(res,op = 2)
  colnames(res) <- c('row','col')
  if(isDataType(mat) == 1) {
    res <- res[,1]
  }
  res
}

#given a matrix or vector, find the posns that are below a certain value t
#op =1 , return the posns, 2 - the values
findIndBelowThresh <-function(mat,t=1,op=1){
  if(isDataType(mat) == 1) cmat <- as.matrix(mat) #if a vector convert to a matrix
  else cmat <- mat
  #now just go col. by col.
  res <- lapply(1:ncol(cmat), function(x) {
    posn <- which(cmat[,x] < t) #get all the rows poistns
    #cat('\nfindblow',posn)
    resindx <- cbind.data.frame(posn,rep(x,length(posn))) #put them in row,col format
  })
  res <- combineListMat(res,op = 2)
  colnames(res) <- c('row','col')
  if(isDataType(mat) == 1) {
    res <- res[,1]
  }
  res
}

#given a vector, find the lowest posn that is at or over a certain value t
#op =1 , return the posns, 2 - the values
findThreshPos <-function(vec,t=1,op=1){
  vect <- sort(vec) #sort it just for fun
  pos <- Find(function(x) x >= val,vect)
  switch(pos,vect[pos]) #return the 
}

#finds the position of val in the vector, where vec is a sorted vector of values
#It returns the value of the first element which is greater than val.
findPosVal <-function(val,vec,op=1){
  vect <- sort(vec) #sort it just for fun
  pos <- Find(function(x) x > val,vect)
  if(is.numeric(pos) == F) return(length(vec)+1)
  #cat('\tfindpos', val,pos,' pos:',which(vec==pos),is.numeric(pos),':')
  which(vec==pos)[1] #return the 
}

#given a list of elements, will pick out all the elements that are the same
#and report their frequency, and the total number of such groups
#op=1, gets the number of subgroups in this list, 2 - number of times they are repeated,
# 3 - df of values and freq
findVecSubGrps <- function(vec,op=1){
  res <- rle(vec)
  switch(op,res$values,res$lengths,data.frame(res$values,res$lengths))
}

#given a list of data vectors, finds out the number of subgroups at a certain height. 
#Default is 40
#op=1, gets the number of subgroups in this list, 2 - number of times they are repeated,
# 3 - df of values and freq
findDataSubGrps <- function(mat,h=40,op=1){
  clust <- getClust(mat) #gets the classification
  ctree <- cutree(clust,h=h) #get the tree elements at the certain height
  findVecSubGrps(ctree,op=op) #return the number of sub groups
}



#inserts the value val at the specified pos in the vector vec
insertValPos <- function(val,vec,pos,op=1){
  #use seqwrap for inserting in the first and last positions.
  vect <- c(vec[seq_wrap(1,(pos-1))],val,vec[seq_wrap(pos,length(vec))])
  vect
}


#given two vectors will filter the second one by the first. It will get all those posns
#for the first one which are above thresh, and then get the corresponding values for the
#second vector, while setting all other posns in the second vector to 0
#vec1: the vector to be filtere
#vec2: the vector to filter it with
#thresh: choose those posn in in vec2 above thresh
filterWithVec <- function(vec1,vec2,thresh=0,op=1){
  posns <- which(vec2<=thresh) #get possn below or at thresh
  res <- vec1 #set these posns to 0
  res[posns] <- 0
  res
}

#fileters the second matrix by the first
#mat1: matrux to be filtered
#mat2: matrix to filter it with
filterWithMat <- function(mat1,mat2,thresh=0,op=1){
  res <- sapply(1:ncol(mat1), function(x){
    tmp <- setVecThresh(mat2[,x],thresh = thresh,op=2) #filter the second mat
    tmp1 <- mat1[,x]*tmp #get all those filtered position in mat1
    #cat(tmp,'\t',tmp1,'\n')
  })
  res
}


#given two lists of vectors will filter the second one by the first. 
#for the corresponding vectors, it will use filterWithVec 
#vec1.lst: the list of vectors to be filtered
#vec2.lst: the list of vectors to filter with
#thresh: choose those posn in in vec2.lst above thresh
filterListWithList <- function(vec1.lst,vec2.lst,thresh=0,op=1){
  if(length(vec1.lst) != length(vec2.lst)) return(F) #the lists should be of the same length
  #pick the corresponding vectors, and filter
  res <- lapply(1:length(vec1.lst), function(x){
    vec <- filterWithVec(vec1 = vec1.lst[[x]],vec2 = vec2.lst[[x]],thresh = thresh,op = op)
  })
  res
}


#function that gets the rows where there are at least n cols with vals over thresh
#the return is data frame irrespective of the input
#dat.df : the data frame
#n: the rwo has at least n number of nonzero values
#ret: 1- the data frame, 2 return where each col is a list element
#op=1, row has at least n nonzero, 2 - row has exactly n nonzero values
#op=3, row has at least n nonzero only n cols, 4 - row has exactly n nonzero values only n cols,
#op=5, row had at least n nonzeros, return nocols cols
getRowsDFNonzeros <- function(data.df,n=1,thresh=0,cols=0,op=1){
  datatype <- isDataType(data.df) #get the data type so that we can restore it at the end
  if (datatype==2) dat.df <- data.frame(data.df) #for single cell results, works better if it is a data frame
  else dat.df <- data.df
  if (n > ncol(dat.df)) len <- ncol(dat.df)
  else len <- n
  #get the non-zero responses for every row
  res <- sapply(1:nrow(dat.df), function(x){ 
    tmp <- length(which(dat.df[x,]>thresh))
  })
  res <- switch(op,dat.df[which(res>=n),],dat.df[which(res==n),],dat.df[which(res>=n),],dat.df[which(res==n),],dat.df[which(res>=n),])
  if(nrow(res)==0) return(NULL) #empty so return that
  if(op>2){#we only want  the first n responses
    #cat('\n',n,str(res))
    nocols <- ifelse(op==5,cols,n) #the nocols to return, needed for op=5  
    res.filter <- lapply(1:nrow(res), function(x){
      padVec(getAboveThresh(res[x,]),n = nocols)[1:nocols] #gets the first n sig. responses
    }) 
    #cat(str(res.filter))
    res.filter <- convertNestedListsDF(res.filter) #put it back in df form
    rownames(res.filter) <- rownames(res)
    res <- res.filter
  }
  if(datatype==2) res <- as.matrix(res) #the input was a matrix, so return one
  res
}

#function that gets the rows where there are at least n cols with vals over thresh
#The result is a list, where each data frame gives rows with 1 col, 2 cols, 3 cols ...that have over thresh vals
#dat.df : the data frame
#ret: 1- the data frame, 2 return where each col is a list element
#op=1, row has at least n nonzero, 2 - row has exactly n nonzero values
#op=3, row has at least n nonzero only n cols, 4 - row has exactly n nonzero values only n cols
getRowsDFNonzerosAll <- function(dat.df,thresh=0,op=1){
  len <- ncol(dat.df)
  res <- lapply(1:len, function(x){
    tmp <- getRowsDFNonzeros(data.df = dat.df,n = x,thresh = thresh,op = op)  
  })
  res
}

#generates a DF from a list where the 1st column is the list element name, and the seocnd
#is the length of the vector elment
genDfFromList <- function(lst,op=1) {
  # Get the names of the list elements
  list.names <- names(lst)
  #cat('\nlist names',list.names)
  # Calculate the number of elements in each list element
  list.lengths <- sapply(lst, length)
  # Create a data frame with the desired structure
  res.df <- data.frame(index = list.names, no.children = list.lengths)
  res.df
}

#convert the cols of a DF into a list
#if op = 2, convert the rows to lists
convertDfToList <- function(dat.df,op=1){
  switch(op,as.list(dat.df),as.list(transposeDF(dat.df)) )
}

#It takes a dataframe lst and an option op to convert either rows (op=1) or columns (op=2) into list elements.
#lst: the list to be converted
convertDfRowsToList <- function(dat.df, op = 1) {
  # Convert rows to list elements
  res <- lapply(1:nrow(dat.df), function(i) {
    row <- unlist(dat.df[i,])
    names(row) <- colnames(dat.df)
    row
  })
  names(res) <- rownames(dat.df) 
  res
}


#given a df will tell give you the first N non-zero values
#n: no of non zero values
#pad: vales for padding
#op=1, get the frist n non-zero values, 2 - get first n, if less than n pad with pad value
#op=3, get all the responses, i.e. no of responses, and pad it out
getDfColNVals <- function(dat.df,n=2,pad=0,op=1){
  res.df <- dat.df
  len <- switch(op,n,n,ncol(dat.df))
  res <- sapply(1:nrow(res.df), function(x){
    if(sum(res.df[x,])>0 ) {
      tmp <- getAboveThresh(vec=unlist(res.df[x,]),val=0) #only non-zero values
      tmp <- padVec(tmp,n=len,pad=pad)
    } 
    else tmp <- rep(0,len)
  })
  #res <- switch(op,res,tabulate(res))
  res
}


#given a df will tell you if the first non-zero col is also the peak col or not. 1 if first col, 2 if second or other col
#op=1, just get the ranks for each row , 2 - get a tabulated rank, i.e. no. of times for the
#col, second col and so on
getDfColDecPeak <- function(dat.df,op=1){
  res.df <- dat.df
  res <- sapply(1:nrow(res.df), function(x){
    if(sum(res.df[x,])>0 ) {
      tmp <- getAboveThresh(vec=unlist(res.df[x,]),val=0) #only non-zero values
      rank <- getVecRank(tmp) #decreasing rank order
      rank <- ifelse(rank[1]==1,1,2) #if first element is highest, 1 else 2
    } 
    else rank <- 0
    rank  
  })
  res <- switch(op,res,tabulate(res))
  res
}

#given a df will get the cols with the highest rank
#op=1, just get the ranks for each row , 2 - get a tabulated rank, i.e. no. of times for the
#col, second col and so on
getDfColPeak <- function(dat.df,op=1){
  res.df <- dat.df
  res <- sapply(1:nrow(res.df), function(x){
    if(sum(res.df[x,])>0 ) rank <- which(res.df[x,]==max(res.df[x,])) #gets the col with highest rank
    else rank <- 0 #if all the cols are 0
  })
  res <- switch(op,res,tabulate(res))
  res
}





#determines the data structure type
#returns 1 for vector, 2 for matrix, 3 for dataframe, 4 for list
isDataType <- function(dat,op=1){
  #cat('\isdatatype',class(dat),class(matrix(1)),class(matrix(1)) == class(dat))
  if(is.vector(dat)==T && is.list(dat)==F ) return(const.DataType$vector) #lists are vectors and lists
  #matrices are matrix or array
  #if(class(matrix(1)) == class(dat)) return(const.DataType$matrix)
  if(length(intersect(class(dat),class(matrix(1))) ) > 0) return(const.DataType$matrix) 
  if(class(data.frame(1)) == class(dat)) return(const.DataType$dataframe)
  if(class(list(1)) == class(dat)) return(const.DataType$list)
}

#comapres if a vector of values are the same, returns T if they are, F otherwise
areValsIdentical <- function(vec,op=1){
  #cat('\nident:',vec)
  #iterate over the vector, if there is even one F, then it will return F
  res <- sapply(2:length(vec),function(x) identical(vec[1],vec[x]))
  if(sum(res) == (length(vec)-1)) return(T)
  F
}

#match two mtrices to see if they have the same number of columns and names
#
#mat1 and mat2: the two matrices
isMatchMatrices <-function(mat1,mat2,op=1){
  condn <- (nrow(mat1)!=nrow(mat2) || ncol(mat1)!=ncol(mat2)) #the condition to check
  #cat('\n',condn,is.null(condn),condn || is.null(condn))
  if(is.na(condn)) return(F) #if one of them is empty you get a null
  if(condn) return(F) #check if they have the same number of elements
  #check if they have same names, scrambled is ok
  if(identical(sort(unique(rownames(mat1))),sort(unique(rownames(mat2)))) ) return(T)
  F
}

#given a matrix and a vector, will see if the vec has the same number of elems as the matrix cols
#and if the names are identical
#mat and vec: tje ma,triix and vector
#col: T, check against cols, F - check against rows
isMatchMatVecNames <- function(mat,vec,col=T,op=1){
  if(col==F) rmat <- t(mat)
  else rmat <- mat
  if(length(mat)==0 || length(vec)==0 ) return(NULL) #the elements are empty
  if(ncol(rmat) != length(vec)) return(F) #same number of elements?
  #check if they have same names, scrambled is ok
  if(identical(sort(unique(colnames(rmat))),sort(unique(names(vec)))) ) return(T)
  T
}

#function that only picks those elements whose correlation is close
#mat1 and mat2: the two matrices to be compared
#prec: the amount within which the two numbers should match
#op=1, the elements, 2 : the elements and values from both matrices 
pickMatchedVals <-function(mat1,mat2,prec=.1,op=1){
  if(isMatchMatrices(mat1,mat2) == F) return(NULL) #if the matrices are not matched return nothing
  rnames1 <- rownames(mat1)
  rnames2 <- rownames(mat2)
  res <- sapply(rnames1, function(x) {
    tmp <- sapply(rnames2, function(y){
      #cat(x,y,'\n')
      abs(mat1[x,y]-mat2[x,y])
    })
  })
  #gets all the elements that satisfy the match
  elems <- getBelowThresh(unlist(flattenDFtoList(res)),val = prec)
  #cat('\npMV ',length(elems))
  if(length(elems)==0) return(NULL) #empty no match, return
  matvalues <- getMatPairedValues(mat1,mat2,elems) #get the values from both matrices
  #cat('\npMV ',colnames(matvalues),', ...',names(elems))
  #make sure that the names match, if they do, we are good to go
  if(isMatchMatVecNames(matvalues,elems) == T) res <- rbind(elems,matvalues)
  else res <- list(elems,matvalues)
  #names(res) <- names(matvalues)
  res
}


#given a bunch of paired names for 2 comparable matrices, gets their values for each matrix
#mat1 and 2:the two matrices
#vec:the vector of values or names that have to be retrieved for each matrix
getMatPairedValues <-function(mat1,mat2,vec,op=1){
  res <- sapply(names(vec), function(x){
    c(getNamedVal(mat1,x),getNamedVal(mat2,x))
  })
  #cat('\ngMPV match ',length(res))
  if(length(res)) colnames(res) <- names(vec) #if not empty
  #cat('getPaired',str(res),'\n')
  res
}


#given a combination name, example, 'rownam1,colname2' will get the corresponding value from the matrix
#mat: the matrix
#separator: the string separator
getNamedVal <- function(mat,name,separator=',',op=1){
  res <- unlist(strsplit(name,split = separator))
  #cat(res)
  mat[res[1],res[2]]
}



#get the top n percentile of population
#op=1, set other entries to 0, 2 - delete all other entries
getTopNPercentile.old <-function(pop,topn,op=1){
  topnos <- pop[pop > quantile(pop,prob=1-topn/100)]
  polst <- pop
  polst[polst < min(topnos)] <- 0
  switch(op,polst,topnos)
}


#Given a matrix or DF, will calculate all the values that fall within the percentage (Int stands for intervals)
#ranges iven by a to b, and get the corresponding values for col. dec.col
#dec.col: the column on which we want the deciles 
#res.col: the column of results that we want.
#The return is a list by the deciles numbered 1,2,...10
#iter: gives the decile size, 10, would give 10 blocks from 0 to 100, 20 would give 5 blocks
#op=1, vector, 2 - a matrix
getMatIntPercentVals <- function(mat.swc,dec.col,res.col,iter=10,op=1){
  dec.vec <- unlist(mat.swc[,dec.col])
  res.dec <- lapply(seq(0,100-iter,iter), function(i){
    dec.posns <- getIntPercentage(dec.vec,i,i+iter,op=2)
    #if(i==90) cat('\nposns',dec.posns)
    res <- mat.swc[dec.posns,res.col]
  })
  names(res.dec) <- seq(0,100-iter,iter)
  res.dec
}

#Given a matrix or DF, will calculate all the values that fall within the percentage (Int stands for intervals)
#ranges iven by a to b, and get the corresponding values for col. dec.col
#dec.col: the column on which we want the deciles 
#res.col: the column of results that we want.
#The return is a list by the deciles numbered 1,2,...10
#iter: gives the decile size, 10, would give 10 blocks from 0 to 100, 20 would give 5 blocks
#op=1, vector, 2 - a matrix
getMatIntPercentileVals <- function(mat.swc,dec.col,res.col,iter=10,op=1){
  dec.vec <- unlist(mat.swc[,dec.col]) 
  dec.vec <- c(dec.vec,max(dec.vec)+1) # do this to include the 100th percentile or highest no
  len <- length(dec.vec)
  res.dec <- lapply(seq(0,100-iter,iter), function(i){
    dec.posns <- getIntPercentile(dec.vec,i,i+iter,op=2)
    #if(i==90) cat('\nposns',dec.posns)
    res <- mat.swc[dec.posns,res.col]
  })
  names(res.dec) <- seq(0,100-iter,iter)
  res.dec
}


#where vec is the vector of numbers, and range.vals gives the minimum and maximum values of the vector
# onto whic the numbers of vec is mapped
# op: 1- return mapped vector, 2 - matrix of original and mapped vector
mapVecToRange <- function(vec, range.vals, op = 1) {
  # Extract min and max from range.vals
  min.range <- range.vals[1]; max.range <- range.vals[2]
  
  # Compute the min and max of the input vector
  min.vec <- min(vec); max.vec <- max(vec)
  
  # Scale the values to the new range
  mapped.vec <- (vec - min.vec) / (max.vec - min.vec) * (max.range - min.range) + min.range

  res <- switch(op,mapped.vec,cbind(vec, mapped.vec))  
  res
}



#function that gets the population interval between percentages a and b, e.g., a=80, b= 90. B/w 80 and 90% 
#it is inclusive of a
#op: 1 - the values, 2 - the positions
getIntPercentage <- function(pop,a,b,prec=0.001,op=1){
  #algo: get the extremes and the range of values, then take the values that fall within the fractions a and b
  extremes <- getExtrema(pop)
  range.pop <- diff(extremes) + extremes[2]*prec # prec helps in include the 100 % number
  perc.a <- extremes[1] + range.pop*(a/100)
  perc.b <- extremes[1] + range.pop*(b/100)
  #cat('\nthe percentages are',perc.a,perc.b)
  res <- switch(op,pop[pop >= perc.a & pop < perc.b],
                which(pop >= perc.a & pop < perc.b))
  res
}

#function that gets the population interval between percentiles a and b, e.g., a=80, b= 90. B/w 80 and 90% 
#it is inclusive of a, but not b
#op: 1 - the values, 2 - the positions
getIntPercentile <- function(vec,a,b,prec=0.001,op=1){
  # Compute percentile thresholds
  lower <- quantile(vec, a / 100, na.rm = TRUE)
  upper <- quantile(vec, b / 100, na.rm = TRUE)
  
  # Efficiently return either values or positions using switch
  res <- switch(op, vec[vec >= lower & vec < upper],  # Case op = 1: Return values
         which(vec >= lower & vec < upper) # Case op = 2: Return positions
  )
  res
}


#get the top n percentage of population: the faster function
#topn, the top n fraction, eg.g 10 is top 10 percentile
#op=1, set other entries to 0, 2 - delete all other entries, 3 - get their positions
getTopNPercentage <-function(pop,topn,op=1){
  low <- min(pop)
  high <- max(pop)
  thresh <- (high-low)*topn/100 #calculate the threshold percentage
  polst <- pop
  polst[polst < thresh] <- 0
  posns <- which(pop >= thresh)
  topnos <- pop[posns]
  switch(op,polst,topnos,posns)
}

#get the top n percentile of population: the faster function
#topn, the top n fraction, eg.g 10 is top 10 percentile
#op=1, set other entries to 0, 2 - delete all other entries, 3 - get their positions
#zero = 1, leave zeros in judging top N, 2 - take zeros out
getTopNPercentile <-function(pop,topn,zero=1,op=2){
  st.time <- Sys.time()
  #border condition, topn is 0, nothing to return
  if(topn==0) return(c())
  popv <- cleanNAVec(pop) #get rid of NAs and Infs, set them to 0
  #t0.time <- Sys.time() - st.time
  #condn to check if thresh calculation should include zeros or only non-zero nos
  if(zero==2) thresh <- ceiling(length(which(popv!=0))*topn/100) # take out all 0s
  else thresh <- ceiling(length(popv)*topn/100)
  #t1.time <- Sys.time() - st.time
  #get the topn from sorted vector
  res.pos <- getVecSortPosns(popv,decrease = T,op = 1) #sorted posns of elements #sortvec.lst[[1]]
  res <- popv[res.pos] #re-arrange so you have a sorted vector
  names(res) <- names(popv)[res.pos] #sortvec.lst[[3]] #re-assign original names in sorted order
  #t2.time <- Sys.time() - st.time
  #take the topn and set everything else to 0
  res[(thresh+1):length(res)] <- 0
  switch(op,res,res[1:thresh],res.pos[1:thresh])
}

#get the top n percentile of population: the faster function
#topn, the top n fraction, eg.g 10 is top 10 percentile
#decile: gets the nos from topn to topn-decile
#op=1, set other entries to 0, 2 - delete all other entries, 3 - get their positions
#zero = 1, leave zeros in judging top N, 2 - take zeros out
getTopNDecile <-function(pop,topn,decile=c(),zero=1,op=2){
  #border condition, topn is 0, nothing to return
  if(topn==0) return(c())
  if(length(decile)==0) dec <- floor(topn * 0.1)
  else dec <- decile
  popv <- cleanNAVec(pop) #get rid of NAs and Infs, set them to 0
  sortvec.lst <- getSortedVec(popv,op=4) 
  #condn to check if thresh calculation should include zeros or only non-zero nos
  if(zero==2) {
    thresh <- ceiling(length(which(popv!=0))*topn/100) # take out all 0s
    topend <- thresh - ceiling(length(which(popv!=0))*dec/100)
  }
  else {
    thresh <- ceiling(length(popv)*topn/100)
    topend <- thresh - ceiling(length(popv)*dec/100)
  }
  #get the topn from sorted vector
  res <- sortvec.lst[[1]]
  names(res) <- sortvec.lst[[3]] #re-assign original names in sorted order
  #cat('\nget',seq_wrap(1,topend),c(thresh+1):length(res))
  notposns <- c(seq_wrap(1,topend),c(thresh+1):length(res))
  res[notposns] <- 0
  
  switch(op,res,res[(topend+1):thresh],sortvec.lst[[2]][(topend+1):thresh])
}


#given two vectors, compares the overlap between them for different deciles
#vecs: list of vectors
#deciles: the deciles for which we need to do the calculation
getVecOverlapDeciles <- function(vecs,deciles=seq(25,100,25),decsize=25,op=1){
  #cat('\n',str(vecs))
  #get the overlap for each of the deciles
  res.ls <- lapply(deciles,function(x){
    cell.lst <- lapply(vecs,function(y){
      #cat('\ntopdec',x-1,x-1+decsize)
      topdec <- getTopNDecile(y,topn = x,decile = decsize,zero = 2,op = 3)
      #topdec <- setdiff(getTopNPercentile(y,topn = x,zero = 2,op = 3),getTopNPercentile(y,topn = x-decsize,zero = 2,op = 3) )
      #cat('\n',x,getTopNDecile(y,topn = x,decile = decsize,zero = 2,op = 3),':',topdec)
      topdec
    })
    common <- Reduce(intersect,cell.lst)
  })
  sapply(res.ls,length)
}

#sorts the list according to the names of the list
#op=1, just sort it alphabetially, 2 = sort as numbers
sortList <- function(lsts,op=1){
  
}


#get the bottom n percentile of population: the faster function
#topn, the top n fraction, eg.g 10 is top 10 percentile
#op=1, set other entries to 0, 2 - delete all other entries, 3 - get their positions
getBottomNPercentile <-function(pop,bottomn,op=1){
  sortpop <- sort(pop,decreasing = F) #sort it in descending ordes
  #cat(sortpop)
  thresh <- ceiling(length(pop)*bottomn/100) #calculate the no in the bottomn percentil
  botnos <- head(sortpop,thresh) #and get the bottomn from the sorted list
  #topnos <- pop[pop > quantile(pop,prob=1-topn/100)]
  #cat('\ntopnos',str(topnos),thresh,sortpop)
  polst <- pop
  polst[polst > max(botnos)] <- 0
  posns <- which(pop <= max(botnos))
  switch(op,polst,botnos,posns)
}


#gets the number that marks the beginning of the top N percentile. 
#e.g., if it is 80, gets the number that marks the 80th percentile
#op=1, get the number that marks the beginning, 2 - get the number that is one step lower than the topn percentile
getTopNPercentileThresh <-function(pop,topn,op=1){
  sortpop <- sort(pop,decreasing = T) #sort it in descending ordes
  #cat(sortpop)
  thresh <- length(pop)*topn/100 #calculate the no in the topn percentil
  topnos <- head(sortpop,thresh) #and get the topn from the sorted list
  #cat('\ngetTOPPTh',head(sortpop,thresh+1),'\t',topnos,'\t',thresh)
  switch(op,min(topnos),min(head(sortpop,thresh+1)))
}

#gets the number that marks the end of the bottom N percentile. 
#e.g., if it is 20, gets the number that marks the end of the 20th percentile
#op=1, get the number that marks the end, 2 - get the number that is one step higher than the bottomn percentile
getBottomNPercentileThresh <-function(pop,bottomn,op=1){
  sortpop <- sort(pop,decreasing = F) #sort it in descending ordes
  #cat(sortpop)
  thresh <- length(pop)*bottomn/100 #calculate the no in the topn percentil
  bottomnos <- head(sortpop,thresh) #and get the bottomn from the sorted list
  #cat('\nbottom',bottomnos,'\nthresh',thresh,'\n',sortpop,'\n',bottomn)
  #cat('\ngetTOPPTh',head(sortpop,thresh+1),'\t',topnos,'\t',thresh)
  switch(op,max(bottomnos),max(head(sortpop,thresh+1)))
}


#gets the top N numbers in a population
#op=1, get the top N nos, 2 = get their posns
getTopNNos <-function(pop,topn,op=1){
  sortpop <- sort(pop,decreasing = T) #sort it in descending ordes
  switch(op,sortpop[c(1:topn)],match(sortpop[c(1:topn)],pop))
}

#gets the top N numbers for a matric or data frame
#col = 1 topn for the col, 2 - topn for the rows
#op=1, get the top N nos, 2 = get their posns
getTopNNosMat <- function(mat,topn,col=1,op=1){
  data.mat <- switch(op,mat,t(mat)) #if row format , transpose the matrix
  #now get the top elements for each col
  res <- sapply(1:ncol(data.mat), function(x){
    getTopNNos(data.mat[,x],topn = topn,op=op)
  })
}

#given two vectors, will cover the topN percentiles of both
#topn: the topn percentile
#op: 1 =- corr, 2 - cosine, 10 - union of topn positions
compareTopNPercentiles<-function(vec1,vec2,topn=10,op=1){
  #get the topn percentile for both, then look at the correlation of the union of the cells
  top1 <- getTopNPercentile(pop = vec1,topn = topn,op=3)
  top2 <- getTopNPercentile(pop = vec2,topn = topn,op=3)
  both <- union(top1,top2)
  if(op!=10) res <- switch(op,cor(vec1[both],vec2[both]),cosine(vec1[both],vec2[both]))
  else res <- both
  res
}

#given a vector, generates a new vector with ones in those posns, and 0s elsewhere
#posns: vector of posns
#n: length of vector
genVecOnes <- function(n,posns,op=1){
  basevec <- rep(0,n)
  basevec[posns] <- 1
  basevec
}

#given a vector will generate 'n' copies, where the copies are stored in each col of a matrix
genVecCopies <- function(vec,n,op=1){
  veclen <- length(vec)
  #cat('\n n ',n,'\n::',vec)
  res <- matrix(rep(vec,n),byrow = F,ncol = n)
  res
}

#sets all the values below threshold to 0
#op=1, leave val> thresh as it is, 2 - set them to 1
setVecThresh <- function(vec,thresh=0,op=1){
  res <- vec
  res[res <= thresh] <- 0
  res1 <- res
  res1[res1 > thresh] <- 1
  switch(op,res,res1)
}

#sets the threshold of values before val to 0 and the rest to 1
#op=1, leave val> thresh as it is, 2 - set them to 1
setAboveThreshOne <- function(vec,val=0,op=2){
  res <- vec
  res[res <= val] <- 0
  res1 <- res
  res1[res1 > val] <- 1
  switch(op,res,res1)
}

#gets values that are all above thresh, leave the rest
#op=1, 
getAboveThresh <- function(vec,val=0,op=2){
  res <- vec
  pos <- which(res > val)
  res[pos]
}

#gets values that are all below thresh, leave the rest
#op=1, 
getBelowThresh <- function(vec,val=0,op=2){
  res <- vec
  pos <- which(res < val)
  res[pos]
}


#function that returns a vector where all values below 0 are 0
threshVal <- function(pop,val=0,op=1) {
  res <- pop
  res[res < val] <- 0
  #cat('\nthresh',res,val)
  res
}

#function that returns a vector where all values above val are set to val
threshUpperVal <- function(pop,val=1,op=1) {
  res <- pop
  res[res > val] <- val
  res
}

#same function that ensures that values above a certain threshold are set to threshold without using a conditional
threshWithHill <- function(val,thresh=1,n=10,op=1){
  val + satFn(val,thresh,n=n) * (thresh-val) 
}


#function that returns a vector where all values below thresh are set to target 
threshValTarget <- function(pop,val=0,target=0,op=1) {
  res <- pop
  res[res < val] <- target
  res
}

#function that returns a vector where all values below or equal to thresh are set to target 
threshAtValTarget <- function(pop,val=0,target=0,op=1) {
  res <- pop
  res[res <= val] <- target
  res
}


#function that returns a mat where all values below val are 0
threshValMat <- function(mat,val=0,op=1) {
  res <- mat
  res[res < val] <- 0
  res
}

#function that sets nonzero positions of the matrix to 1,everything else to 0
#set value below/equal val to 0, and above val to 1
setMatNonzero <- function(mat,val=0,op=1) {
  res <- mat
  res[res <= val] <- 0
  res[res > val] <- 1
  res
}


#function that returns a DF which gives the Row, Col positions of all elements over the threshold val
threshValMatPosns <- function(mat,thresh=0,op=1) {
  res <- mat
  res <- lapply(1:nrow(mat),function(x){
    vec <- which(mat[x,]>thresh) #get all posn over thresh
    posns <- data.frame(rep(x,length(vec))) # generate two colsm 1st col: the rows
    posns[,2] <- vec #secon col, the colns
    posns #return the 2-col data frame
  })
  res <- convertNestedListsDF(res) #concatenate the list of data frames
  colnames(res) <- c('row','col')
  res
}

#function calculates the nu,mber of entries that are not zeroes
#thresh: threshold, 
#op=1
countNonZeroes <- function(mat,thresh=0,op=1){
  #is the vector non-empty * no of nonzeroes
  #cat('\ncounting nonzeroes',length(mat))
  count <- as.numeric(length(mat)>0)*length(which(mat!=thresh))
  #cat('\ncounting done')
  count
}

#function calculates the nu,mber of entries that are not zeroes
#thresh: threshold, 
#op=1
countZeroes <- function(mat,thresh=0,op=1){
  #is the vector non-empty * no of zeroes
  count <- as.numeric(length(mat)>0)*length(which(mat==thresh))
  count
}


#function calculates the nu,mber of entries above threshold
#thresh: threshold, 
#op=1
countAboveThresh <- function(mat,thresh=0,op=1){
  count <- length(which(mat>thresh))
  count
}

#function calculates the nu,mber of entries that are threshold
#thresh: threshold, 
#op=1
countAtThresh <- function(mat,thresh=0,op=1){
  count <- length(which(mat==thresh))
  count
}

#count no of times elem occurs in vec
countElem <- function(vec,elem,op=1){
  #cat('\n',str(vec),elem)
  length(which(vec==elem))
}



#sets the threshold according to the rules of setVecThresh
#op=1, leave val> thresh as it is, 2 - set them to 1
setListThresh <- function(veclst,thresh=0,op=1){
  lapply(veclst,function(x) setVecThresh(x,thresh = thresh,op = op))
}


#function that calculates the mismatch between two vectors in terms of how their values are ranked
#e.g., if vec1 is ranked in ascending order: 10,2,23,43,51,... and vec2 is also the same, then the 
#output is 1. 0 indicates not a single match
#op: type of match. 1 - absolute match of rank order, 2 - group match, i.e., if the top percent of
#both are the same, 3 - same as 2 except group size is based on the lower sparsity of the 2 vectors
#topsize: the size of the top group
calcVecsMatch <- function(vec1,vec2,top=10,op=1){
  #get the ranked posns of all the elements in vec1
  vec1.ranked <- getSortPosns(vec1,decrease = T)
  vec2.ranked <- getSortPosns(vec2,decrease = T)
  if(op==3) topn <- min(countNonZeroes(vec1),countNonZeroes(vec2))
  else topn <- top
  res <- switch(op,vecMatchAbsolute(vec1.ranked,vec2.ranked),
                vecMatchTopN(vec1.ranked,vec2.ranked,top = topn,op=1),
                vecMatchTopN(vec1.ranked,vec2.ranked,top = topn,op=2))
  res
} 

#function that tells you how many elements of vec1 match vec2
vecMatchAbsolute <- function(vec1,vec2,op=1){
  res <- length(vec1)-countNonZeroes(vec1-vec2)
  res
}

#function that tells you how many of the top N numbers of vec1 and vec2 are common 
#op: 1 - top %, 2 - the top is decided by the size of the more sparsely coded vector
vecMatchTopN <- function(vec1,vec2,top=10,op=1){
  if(op==1) topn <- ceiling((top*length(vec1))/100)
  if(op==2) topn <- top
  #cat('\n=',op,'\t',topn,'\t',vec1[topn],'\t',vec2[topn])
  res <- intersect(vec1[1:topn],vec2[1:topn])
  length(res)
}  




#this function joins two data frames x and y as long as the # of their columns match, the 
#colnames will be set to  x's colnames
joinDF <- function(x,y,op=1){
  #cat('jDF\n')
  tmpy <- y
  colnames(tmpy) <- colnames(x)
  #cat(str(tmpy),str(x))
  #res <- merge(x=x,y=tmpy,all.x = T, all.y = T)  
  res <- rbind.data.frame(x,tmpy)  
  #rownames(res) <- c(row.names(y),row.names(x)) #got to make the names unique
  res
}

#given two lists will create a new one with an equal number of elements
mergeLists <- function(lst1,lst2,op=1){
  Map(c,lst1,lst2)
}

#given a number of lists, will concatenate all of them so that they are all elements of a single data frame
#with the values being in column 2 and the name of the data list from which the value came in the first col
#vec.lst: a list of vectors to be joined
#vecnames: if specified these are the names of each list
joinListOfVecs <- function(vec.lst,vecnames=c(),op=1){
  tvec.lst <- vec.lst #assign tmp variable and set the names of the list elements
  #if names not specified 
  if (is.null(vecnames) && is.null(names(tvec.lst))) names(tvec.lst) <- 1:length(tvec.lst)
  if (!is.null(vecnames)) names(tvec.lst) <- vecnames #if the names are given
  #make a long vector of the factors or names columnrep
  names.vec <- sapply(1:length(tvec.lst), function(x){ #will return a list of the lengths dont match
    namex <- rep(names(tvec.lst)[x],length(tvec.lst[[x]]))
    namex
  })
  names.vec <- unlist(names.vec) #make into a vector
  val.vec <- sapply(1:length(tvec.lst), function(x){#now, get the values
    tvec.lst[[x]]
  })
  val.vec <- unlist(val.vec)
  res.df <- cbind.data.frame(as.vector(names.vec),as.vector(val.vec)) #make vectors then df
}

#given a list of vectors this multiplies all of them
#vec.lst: list of vectors
#op=1, the product, 2 - if prod > 1, set it to 1
prodListVectors <-function(vec.lst,op=1){
  len <- length(vec.lst[[1]])
  res <- sapply(1:len, function(x){
    #cat('posn',x,'vect',length(vec.lst))
    vec <- sapply(vec.lst, function(y) y[x])
    #cat(vec)
    prod(vec)
  })
  switch(op,res,setAboveThreshOne(res))
}

#given a list of vectors this adds all of them
#vec.lst: list of vectors
#op=1, the sum, 2 - if sum > 1, set it to 1
sumListVectors <-function(vec.lst,op=1){
  len <- length(vec.lst[[1]])
  res <- sapply(1:len, function(x){
    #cat('posn',x,'vect',length(vec.lst))
    vec <- sapply(vec.lst, function(y) y[x])
    #cat(vec)
    sum(vec)
  })
  switch(op,res,setAboveThreshOne(res))
}

#this funcrion will take in either a vector or matrix and generate n number of copies
# or a vector or matrix of lenght n
#rows = T, duplicate y adding to rows, F - duplicate by adding to cols
#op=1, use n to generate n copies of the odor, 2, n is number of entries in total
genDataCopies <- function(dat,n=3,rows=T,op=1){
  #if you want to add cols or rows
  if (rows==F && (isDataType(data)>1)) data <- t(dat)
  else data <- dat 
  #first figure out whether it is a vector or matrix
  size <- ifelse(isDataType(data) == 1,length(data),nrow(data))
  #then if it is on the basis of length or copies, if length, you only need len
  copies <- switch(op,n,(n)/size) # n + 1 because we are using the floor function
  copy.part <- floor(copies) #the integer number of entries
  rest <- copies - copy.part #the remainder fraction of entries
  #cat(size,copies,copy.part,rest,'\n')
  if (isDataType(data) == 1){#its a vector
    res <- rep(data,copy.part)
    if(rest > 0 ) res <- c(res,data[1:round(rest/(1/size))]) #gen unit length # of entries rounded out
  }
  else {
    res <- do.call(rbind,replicate(copy.part,data,simplify = F))
    if(rest > 0 ) res <- rbind(res,data[1:round(rest/(1/size)),])
  }
  if(rows==F) res <- t(res) #transpose if we are supposed to add cols
  res
}

#given a vector will generate a matrix where the rows or columns are copies 
#of the vector
#vec: the vector
#n: no of copies
#rows= T add as rows, F - add as columns
genMatVecCopies<-function(vec,n=3,rows=T,op=1){
  #cat('genMAtVEcCopies',str(vec),'times:',n)
  if(rows)  dat <- matrix(rep(vec,n),byrow = T,ncol = length(vec))
  else dat <- matrix(rep(vec,n),byrow = F,nrow = length(vec))
  dat
}

#generates a vector of length n withs 1s in the posns specified and 0s elsewhere
#n: vel length, posns: posns where we should put a one
#values: the corresponding values to be assigned
createVecPosns <- function(n,posns,vals=c(),op=1){
  res <- rep(0,n)
  #get the frequency of the numbers in posns, and then assign them accordingly in res
  tmp <- table(posns) #the freq of the posns
  tmp1 <- as.integer(names(tmp)) #the posns
  res[tmp1] <- tmp
  if(op==2){#gotta fill in the values by replacing them appropriately for tmp
    #get the posns for each posn value in the vector posns, and assign values from vals
    values <- sapply(1:length(tmp), function(x){
        val_posns <- which(posns == tmp1[x])
        res_val <- sum(vals[val_posns])
    })
    res[tmp1] <- values
  }
  res
}

#this will create a list of n empty lists
#n - no of lists to create
#elem - to vbe duplicat4ed. If c(), creates an empty lsit
createLists <- function(n,elem=c(),op=1){
  tmp <- list(elem)
  res <- rep(tmp,n)
  res
}


#this 


#given two points in (x,y) form gives you the equation of the line in the form 
#of the slope and intercept

#Funtion to insert a row into a data frame at a particular position


#get the top n percentile of population: the faster function
#topn, the top n fraction, eg.g 10 is top 10 percentile
#op=1, set other entries to 0, 2 - delete all other entries, 3 - get their positions
#zero = 1, leave zeros in judging top N, 2 - take zeros out
getTopNPercentile.old <-function(pop,topn,zero=1,op=2){
  st.time <- Sys.time()
  #border condition, topn is 0, nothing to return
  if(topn==0) return(c())
  popv <- cleanNAVec(pop) #get rid of NAs and Infs, set them to 0
  t0.time <- Sys.time() - st.time
  #sortvec.lst <- getSortedVec(popv,op=4) 
  #sortvec.lst <- get(popv,op=4) 
  t1.time <- Sys.time() - st.time
  #condn to check if thresh calculation should include zeros or only non-zero nos
  if(zero==2) thresh <- ceiling(length(which(popv!=0))*topn/100) # take out all 0s
  else thresh <- ceiling(length(popv)*topn/100)
  t2.time <- Sys.time() - st.time
  #get the topn from sorted vector
  res <- getVecSortPosns(popv,decrease = T,op = 2) #sortvec.lst[[1]]
  names(res) <- names(popv)[getVecSortPosns(popv,decrease = T,op = 1)] #sortvec.lst[[3]] #re-assign original names in sorted order
  cat('\nget',t0.time,t1.time,t2.time)
  res[(thresh+1):length(res)] <- 0
  
  switch(op,res,res[1:thresh],sortvec.lst[[2]][1:thresh])
}


