#R functions to read, analyze, plot and detect patterns in data along with neural and computer algorithms to model complex data.    

#Copyright (C) 2023 Shyam Srinivasan 

#This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details.

#You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.


#function to calculate the euclidean distance between two vectors
# x and y, where each contains 2 columns of {x,y} coords

#this function takes two vectors of n dimensional points and returns a vector of the 
#distance between them
ComputeDistance <- function(x1,x2) {
  #the euclidean distance calculation
  # x3 <- sqrt((x1[,1]-x2[,1])^2+(x1[,2]-x2[,2])^2)
  # x3
  vec <- lpnorm(x1 - x2,2)
  vec
}

#this function takes 4 vectors of 2 dimensional points and returns a vector of the 
#distance between them
ComputeDistance2 <- function(x1,y1,x2,y2) {
  #the euclidean distance calculation
  x3 <- sqrt((x1-x2)^2+(y1-y2)^2)
  x3
}


#gets the slope of the line between two points
#ptx: of the form c(dim1,dim2,...)
#op: 1 - just x and y diemnsions
#op: 2 - x and y dimension slope angle in degrees
computeSlope <- function(pt1,pt2,op=2){
  #cat('\nSlope',pt2,':',pt1)
  slope <- (pt2[2]-pt1[2])/(pt2[1]-pt1[1])  
  angle <- atan(slope) * 180/pi
  
  return(switch(op,slope,angle))
}



#this function 2 vectors of x and y points and calculates the velocity
#x and y are vectors of x and y positions, and t is the no of frames/sec
# if op = 0, nothing, if op = 1, pad it out
ComputeSpeed <- function(x,y,t,op=1) {
  #leave out the first element
  x1 <- tail(x,-1)
  y1 <- tail(y,-1)
  #leave out the last element
  x2 <- head(x,-1)
  y2 <- head(y,-1)
  #the speed calculation
  x3 <- sqrt((x1-x2)^2+(y1-y2)^2)/(1/t)
  #cat('com',x1[1],x2[1],x3[1],y[1],y[2],t,'\n')
  if (op == 1) {x3 <- c(0,x3)} # add 0 velocity to the first position
                               # assume the fly starts from a stationary posn
  x3
}

#vairation of the speed function where data is the data set, and xc and yc
#are the column indices for the x and y positions
#this function takes 2 vectors of x and y points and calculates the velocity
#x and y are vectors of x and y positions, and t is the time between frames
# if op = 0, nothing, if op = 1, pad it out
ComputeSpeedData <- function(data,xc,yc,t,op=1) {
  x <- data[xc]
  y <- data[yc]
  #leave out the first element
  x1 <- tail(x,-1)
  y1 <- tail(y,-1)
  #leave out the last element
  x2 <- head(x,-1)
  y2 <- head(y,-1)
  #the speed calculation
  x3 <- sqrt((x1-x2)^2+(y1-y2)^2)*(1/t)
  if (op == 1) {x3 <- c(0,x3)} # add 0 velocity to the first position
  # assume the fly starts from a stationary posn
  x3
}


#this function to be used with lapply, gives you the greater of two numbers
GreaterThan <- function(x,y=0) {
  res = -1
  if ( x > y ) res = 1
  res
}

#this function calculates the angle of the two
CalculateAngle <- function(frame1,frame2,op=1) {
  slope <- (frame1[2]-frame2[2])/(frame1[1]-frame2[1])
  slope
  
}

#this is the old fuction which is wrong
#the function rounds no n to the nearest upper or lower multiple of r
#op =1, closest lower (<) multiple, = 2, higher (>) multiple
#op =3, closest lower (<=) multiple, = 4, higher (>=) multiple
getNearestMutiple1 <- function(n,r,op=1) {
  #div <- as.integer(n/r)
  if(r==0) { #if the divisor is 0, then have to return the number
    return(n*(-1)^op) #return -n or n
  }   
  div <- floor(n/r) #fixed error to take care of -ve nos b/w 0 and -1
  #cat('div',div,'\n')
  if (op > 2) { # if perfect multiple return it
    if ((n/r) == div) return(n)
    else return((div + (op-3))*r) #sub -3 isntead of -1
  }
  res <- (div + (op-1))*r
  res
}

#the function rounds no n to the nearest upper or lower multiple of r
#op =1, closest lower (<) multiple, = 2, higher (>) multiple
#op =3, closest lower (<=) multiple, = 4, higher (>=) multiple
getNearestMultiple <- function(n,r,op=1) {
  #div <- as.integer(n/r)
  if(r==0) { #if the divisor is 0, then have to return the number
    #return -n or n for 1 and 2 and 0 for op=3 and 4
    return(ifelse(op <= 2,n*(-1)^op,n) )
  }   
  if(n<0) {#if n is less than zero then we have to flip the op
    if(op <= 2) option <- 3 - op
    else option <- 7 - op
    #cat('\nNearestMultiple otions',abs(n),abs(r),option,'\n')
    return(-getNearestMultiple(abs(n),abs(r),op=option))#need the -ve sign
  }
  #if the number is less than the multiple factor then it is 0
  if(n < r && (op %% 2 == 1)) return(0) 
  #cat('options',op,n %% r,all.equal(n %% r,0),'\n')
  if((op < 3) && isTRUE(all.equal(n %% r,0)) ){ 
    div <- switch(op,n/r - 1,n/r + 1)  
    #fixed error and also used all.equal instead of == because of floating point errors
    #old stuff: error div <- switch(op,floor(n/r - log10(r)/r),ceiling(n/r + log10(r)/r))
  }
  else div <- switch(1 + op %% 2,ceiling(n/r),floor(n/r))
  res <- (div)*r
  res
}

#this function gets the nearest power of 10
getpow10 <- function(n,op=1) {
  #round(log10(n))
  if(n>1) return(floor(log10(n)) ) #ifno is greater than 1, just get the non-decimal
  if(n<1) return(-ceiling(log10(1/n)) ) #ifno is less than 1, just get the non-decimal of the reciprocal
  0 #if n==1
}

#specifies the number of digits to 
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

#get the number of decimal places in x
decimalplaces <- function(x) {
  #cat('\ndecimal places',x,':',abs(x - round(x)),':',.Machine$double.eps^0.5 )
  #print(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE))
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nodigits <- nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]])
    if(length(nodigits)==1) return(0)
    else return(nodigits[2])
  } else {
    return(0)
  }
}

#this function gets the next highest/lowest multiple in increwments of r when you start
#with st
#op=2, next highest multiple,
getRangeMultiple <- function(st,no,r,op=2){
  #get the remainder when we start with st, and go in multiples of r
  rem <- (no-st) %% r
  #cat('rem',rem,'\n')
  ifelse(rem>0,no-rem+r,no)
}

#gets the lowest number after 1 that divides this number
getLowestDivisor <-function(n,x=2,op=1){
  if(n %% x == 0 || n %% x == n) return(x) #if its a perfect divisor return it
  else return(getLowestDivisor(n,x+1)) #otherwise, go deeper into the rabbit hole
}

#if this is 0-length vector convert to 0
isvectorzero <-function(val,op=1){
  #print(length(val))
  switch(length(val)+1,0,val)
}

#function that generates sequences that are 
#op=1, alternatiing, e.g., 123123123, 2 = sequential, e.g., 111222333, 3 - random but evenly distributed, e.g., 122311231331
#seqnos: the numbers that should make up the sequence,
#size: the total length
genSequence <- function(seqnos,size,op=1){
  len <- ifelse(length(seqnos)>1,length(seqnos)>1,seqnos) #the size of the seqns list
  res <- switch(op,sapply(1:size, function(i) (i+len-1)%%len + 1),
                sapply(1:size, function(i)  ceiling(i/(size/len)) ),
                sample(sapply(1:size, function(i) (i+len-1)%%len + 1)) )
  res
}

#checks if all elements of set are part of superset, returns T if they are, F otherwise
isSubset <- function(set,superset,op=1){
  sum(as.numeric(set %in% superset)) == length(set)
}

#filters a vector to avoid Inf numbers Or just return a list of the posns of the Inf 
#causing numbers
#oper: 1 - filter so that log(vec) does not give inf
#op: 1 - posns, 2 - values
clearVecInf <- function(vec,oper=1,op=1){
  resvec <- vec
  if(oper==1){# apply log(vec)
    posns <- which(vec == 0)
    resvec[posns] <- 10^-10 #number close to 0
    res <- switch(op,posns,res)
    return(res)
  }
  F
}

#this gets the posns of NA and Inf in the vector
getVecNAInf <- function(vec,op=1){
  positions <- which(is.infinite(vec) | is.na(vec))
  positions
}

#given two sets, gets the overlap between them in terms of values. For instance if set A has 
#50 points and B 45 points, will get the number of points in A that are higher than the least number in A
#and vice versa, e.g.,
#|------------------------|    <- Set A
#         set B   |------------------|
#                 |-------|    <- OVERLAP(a,b)
#|----------------|            <- A - B
#op:1, overlap, 2 - set A - B, 3 - set B -a 
getSetOverlapNos <- function(setA,setB,op=1){
  if(mean(setA) > mean(setB)) lstsets <- swap(setA,setB)
  else lstsets <- list(setA,setB)
  assignVarVals(c('seta','setb'),lstsets)
  minb <- min(setb)
  maxa <- max(seta)
  mina <- min(seta)
  #cat('\nsetstats: setA: ',mean(seta),min(seta),max(seta),'\tsetB ',mean(setb),min(setb),max(setb))
  no.bina <- length(which(setb<maxa))
  no.ainb <- length(which(seta>minb))  
  no.anotinb <- length(which(seta < mina))
  no.bnotina <- length(which(setb > maxa))
  c(no.ainb,no.bina,no.anotinb,no.bnotina)  
}

#calculates the permutations of vector vec for the given number of combinations k, i.e., n choose k
#op=1, no repeats, 2 - allow repeats
perm <- function(vecn,k,op=1){
  #cat('\nperm',vecn,',',k)
  if(k <= 0) return(c())
  res <- c()
  for(i in seq_wrap(1,length(vecn)) ){
    others <- checkCond(op==1,setdiff(vecn,vecn[i]),vecn)
    combos <- perm(others,k-1)
    if(length(combos)>0) perms <- cbind(rep(vecn[i],nrow(combos)),combos)
    else perms <- vecn[i]
    res <- rbind(res,perms)
  }
  res
}

#permutes groups of vectors. If you have 5 groups or lists of vectors, generates a permutation
#of all possible combinations of a number from each group
#op=1: all possible combinations of numbners from every group, 2 - maintain order, first group always remains first, and so on, only change the numbers inside
permuteGroups <- function(lst,k,grpnos=1:length(lst),op=1){
  permgrps <- switch(op,perm(vecn=1:length(lst),k=k),t(combn(x=1:length(lst),m=k)) )
  #k=1 is a special case, just return all nos as a single matrix col
  if(k==1) return(matrix(unlist(lst),ncol = 1) )
  res <- c()
  for(i in seq_wrap(1,nrow(permgrps)) ){#iterate through all the groups
    itemperm <- permAllGroups(lst,grpnos = permgrps[i,])
    #print(itemperm)
    res <- rbind(res,itemperm)
    #cat('\n grpi',permgrps[i,],'res length',nrow(res),'\n')
  }
  if(k==1) res <- matrix(res,ncol = 1)
  res
}

#permutes groups of vectors. If you have 5 groups or lists of vectors, generates a permutation
#lst is of the form of a list of vectors: list(lstvec1,lstvec2...)
#of all possible combinations of a number from each group
#retop: 1 - matrix, 2 - df
permAllGroups <- function(lst,grpnos=1:length(lst),op=1,retop=1){
  #cat('\nPAG',grpnos,length(lst),',',1:length(lst))
  if(length(grpnos)<=1) return(switch(retop,lst[[grpnos]],as.data.frame(lst[[grpnos]])) ) 
  res <- c()
  i <- grpnos[[1]]
  #cat('\nPAG',grpnos,i)
  for(j in lst[[i]]){#iterarate through grp i
    others <- setdiff(grpnos,i)
    #cat('\n comb',j,':',others,'\n')
    perms <- cbind(j,permAllGroups(lst,grpnos = others,op=op))
    res <- rbind(res,perms)
  }
  switch(retop,res,as.data.frame(res) )
}

#perumtes numbers friom groups such that a number can appear in only one possible combination. i.e., if there is a group c(1,2,3), those three numbers 
#op=1: choose in order from each group, 2 - choose randomly from each group
permuteGroupsOnce <- function(lst,k,op=1){
  chooselst <- lst
  res <- c()
  #loop as long as there are k non-empty vectors
  #cat('\n',noEmptyVecs(chooselst))
  while(noEmptyVecs(chooselst)>=k){
    combos <- pickElemsLst(chooselst,no=k) #get k elements from k of the vecs
    chooselst <- combos[[2]] #the nnew list after the elements are taken out
    #cat('\npGO combos ',combos[[1]],noEmptyVecs(chooselst),'\n')
    #permute the elements you got
    res.perm <- perm(combos[[1]],k = length(combos[[1]])) #permute the k elements
    res <- rbind(res,res.perm)
  } 
  res
}

#function genNameComb that takes a vector of names lst.names as input and 
#generates combinations based on whether the names are numeric or strings:
genNameComb <- function(lst.names) {
  # Check if names are numeric or strings and generate combinations
  if (all(suppressWarnings(!is.na(as.numeric(lst.names))))) {
    combn(lst.names, 2, paste, collapse = ",")
  } else {
    combn(lst.names, 2, paste, collapse = "-")
  }
}

#pick elements from each of the list vectors, and purges them from the vectors after picking
#lst: list of vectors
#ind: the indices of tghe vectors from which elements will be picked
#no: the number of elements that you have to pick
pickElemsLst <- function(lst,no,op=1){
  ind <- sample(noEmptyVecs(lst,op=2),no) #pick no no of groups
  newlst <- lst
  #pick the first element from every list vector
  elems <- sapply(ind, function(i) newlst[[i]][1])
  #update the list by getting rid of the elements
  for(i in ind){
    newlst[[i]] <- newlst[[i]][-1] #remove the first element
  }
  list(elems,newlst) #return the   
}

#will give you the number of lists that are not empty
#op=1, no of nonempty vectors, 2 - the indices of theese vectors
noEmptyVecs <- function(lst,op=1){
  #cat('\n nEV')
  res <- sapply(lst, function(i) ifelse(length(i)>0,1,0))
  switch(op,sum(res),which(res==1))
}


#generates a sequence where there are no backward sequences. Returns NULL for backward sequences
seq_wrap <- function(from,to,by=1,op=1){
  if(to<from) return(c())
  seq(from=from,to = to,by = by)
}


#claculates the lpnorm of the vector
#p: the p part of the lpnorm, 2 for euclidean
lpnorm <- function(vec,p=2,op=1){
  res <- sapply(1:length(vec),function(x) vec[x]^p)
  res <- do.call(sum,as.list(res) )
  res^(1/p)
}

#x - the value that should be saturated
#ec50,: the ec 50,;n - the hill coefficient
#op=1 increasing function, 2 - decreasing function
satFn <-function(x,ec50,n=1,op=1){
  #if op =1, it is an increasing function, for 2 xn/(kn+xn) gets flipped and it is a decreasing function
  #cat('\n',(x^n)/(x^n + ec50^n))
  num <- switch(op,x^n,ec50^n)
  den <- x^n + ec50^n
  num/den
}

#x - the value that should be saturated
#ec50,: the ec 50,;n - the hill coefficient
#op=1 increasing function, 2 - decreasing function
hillFn <-function(x,ec50,n=1,op=1){
  satFn(x=x,ec50 = ec50,n = n,op = op)
}

#computes the cosine  similarity for 2 vectors
cosine <- function(x,y,op=1){
  if(length(dim(x))==2 || length(dim(y))==2){#matrix, get the cosine of all columns
    if(length(dim(x))==2) { #set it so that the matrix is mat1
      mat1 <- as.matrix(x)
      mat2 <- as.matrix(y)
    }
    else {
      mat1 <- as.matrix(y)
      mat2 <- as.matrix(x)
    }
    #cat('\n',str(mat1),'\n',ncol(mat1),ncol(mat2),sum(mat1[,1]*mat2[,1]),sqrt(sum(mat1[,1]*mat1[,1])))
    sim <- sapply(1:ncol(mat1),function(i){
      
      if(length(dim(mat2))==2) res <- sapply(1:ncol(mat2),function(j) {
        tmp <- sum(mat1[,i]*mat2[,j])/(sqrt(sum(mat1[,i]*mat1[,i])) * sqrt(sum(mat2[,j]*mat2[,j])) ) 
        # cat('\n',i,j,':',sum(mat1[,i]*mat2[,j]),sqrt(sum(mat1[,i]*mat1[,i])),sqrt(sum(mat2[,j]*mat2[,j])),
        #     (sqrt(sum(mat1[,i]*mat1[,i])) * sqrt(sum(mat2[,j]*mat2[,j])) ),tmp)
        tmp
      })
      else{
        res <- sum(mat1[,i]*mat2)/(sqrt(sum(mat1[,i]*mat[,i])) * sqrt(sum(mat2*mat2)) ) 
        cat('\nhere',sum(mat1[,i]*mat2),(sqrt(sum(mat1[,i]*mat[,i])) * sqrt(sum(mat2*mat2)) ) )
      }
      #cat('\n',i,res)
      res
    })
    return(t(sim))
  }
  sim <- sum(x*y)/(sqrt(sum(x*x)) * sqrt(sum(y*y)) )
  #cat('\n2 vevs',sum(x*y),(sqrt(sum(x*x)) * sqrt(sum(y*y)) ) )
  sim
}

#gets the mean and other moments of a matrix. Matirx has to be square
#diag=T, with diagonal, F - w/out diagonal
#op, 1 - mean
computeMatStats <- function(mat,diag=T,op=1){
  #gets the mean for now
  if(diag) mn <- sum(mat)/(ncol(mat)*nrow(mat))
  else mn <- (sum(mat) - sum(diag(mat)))/(ncol(mat)*(nrow(mat)-1))
  mn #the mean
}

#calculates the correlations for a list of vectors, will also do it for a data frame of columsn
#dat.lst: the data in as a list of vectors
#op: 1, correlation, 2 - cosine. 
computeVecListCor <- function(data.lst,op=1){
  novecs <- length(data.lst)
  if(isDataType(data.lst[[1]]) > 1) dat.lst <- lapply(data.lst,function(x) as.vector(x))
  else dat.lst <- data.lst
  res.mat <- sapply(1:novecs,function(i){
    res <- sapply(1:novecs,function(j){
      #cat('i j',i,j,'\t')
      cor(dat.lst[[i]],dat.lst[[j]])
    })
  }) 
  colnames(res.mat) <- names(dat.lst)
  rownames(res.mat) <- names(dat.lst)
  res.mat
}


#x - the value that should be saturated
#ec50,: the ec 50,;n - the hill coefficient
#op=1 increasing function, 2 - decreasing function
invSatFn <-function(y,ec50,n=1,op=1){
  num <- switch(op,y/(1-y),(1-y)/y)
  res <- (ec50^n * num)^(1/n)
  res
}

#given the height of a chord towards the center, i.e., from the circumfrence to the chord, will give the area 
#of the chord
#ht: height of the chord from the circumfrence to the chord
#r: radius
computeChordArea <- function(r,ht,op=1){
  x <- sqrt(ht*(2*r - ht))
  #theta <- 
  #cat('\n',x,pi*r^2,(asin(x/r)/(2*pi)),(r-ht)*x )
  r^2*(asin(x/r)) - ( (r-ht)*x )
}

#computes the overlapping, non-overlapping and total areas of both circles
#returns common area, noncommon area,total area, relative common area, relative noncommon area, circle1 nonoverlap, circle2 non-overlap
computeOverlappingCircles <-function(r1,r2,ht,op=1){
  chord1 <- computeChordArea(r=r1,ht = ht)
  chord2 <- computeChordArea(r=r2,ht = ht)
  chord.tot <- chord1 + chord2
  circ1 <- pi*r1^2
  circ2 <- pi*r2^2
  circ.tot <- circ1 + circ2
  c(chord.tot,circ.tot-(2*chord.tot),circ.tot-chord.tot,
    chord.tot/(circ.tot-chord.tot),(circ.tot-(2*chord.tot))/(circ.tot-chord.tot),
    circ1-chord.tot,circ2-chord.tot)
}

# Helper function to find the cross product magnitude in 2D
crossProductMagnitude <- function(a, b) {
  return(a[1] * b[2] - a[2] * b[1])
}

# Recursive function to find the convex hull
quickhull <- function(points, P1, P2, side) {
  if (nrow(points) == 0) return(matrix(nrow = 0, ncol = ncol(points)))
  
  # Calculate the cross products to find the point farthest from the line
  distances <- apply(points, 1, function(p) {
    crossProductMagnitude(P2 - P1, p - P1)
  })
  farthest.point <- points[which.max(distances), , drop = FALSE]
  
  # If no points are on the specified side, return an empty matrix
  if (max(distances) <= 0) {
    return(matrix(nrow = 0, ncol = ncol(points)))
  }
  
  # Points on the left side of the line P1-farthest.point
  left.of.P1.farthest <- points[apply(points, 1, function(p) {
    crossProductMagnitude(farthest.point - P1, p - P1) > 0
  }), , drop = FALSE]
  
  # Points on the left side of the line farthest.point-P2
  left.of.farthest.P2 <- points[apply(points, 1, function(p) {
    crossProductMagnitude(P2 - farthest.point, p - farthest.point) > 0
  }), , drop = FALSE]
  
  # Recursively find the convex hull on the two sides
  left.hull <- quickhull(left.of.P1.farthest, P1, farthest.point, side)
  right.hull <- quickhull(left.of.farthest.P2, farthest.point, P2, side)
  
  # Combine the hulls
  return(rbind(left.hull, farthest.point, right.hull))
}

# Function to compute the convex hull for n-dimensional points
# points: the points are in matrix form
computeConvexHull <- function(points) {
  # Check if points is a matrix and has at least two dimensions
  if (!is.matrix(points) || ncol(points) < 2) {
    stop("Input must be a matrix with at least two dimensions")
  }
  
  # Find the leftmost and rightmost points
  leftmost.point <- points[which.min(points[, 1]), , drop = FALSE]
  rightmost.point <- points[which.max(points[, 1]), , drop = FALSE]
  
  # Points on the left side of the line leftmost.point-rightmost.point
  left.of.line <- points[apply(points, 1, function(p) {
    crossProductMagnitude(rightmost.point - leftmost.point, p - leftmost.point) > 0
  }), , drop = FALSE]
  
  # Points on the right side of the line leftmost.point-rightmost.point
  right.of.line <- points[apply(points, 1, function(p) {
    crossProductMagnitude(rightmost.point - leftmost.point, p - leftmost.point) < 0
  }), , drop = FALSE]
  
  # Recursively find the convex hull on both sides
  left.hull <- quickhull(left.of.line, leftmost.point, rightmost.point, 1)
  right.hull <- quickhull(right.of.line, rightmost.point, leftmost.point, -1)
  
  # Combine the hulls
  convex.hull <- rbind(leftmost.point, left.hull, rightmost.point, right.hull)
  
  # Remove duplicates and ensure correct ordering
  convex.hull <- convex.hull[!duplicated(convex.hull), ]
  
  return(convex.hull)
}


#-----------new convex hull functions for 2d and 3d as of sep. 30, 2024 --------------

#gets the convex Hull for a matrix in SWC format 
swcGetConvexHull <- function(mat.swc,cols=2:4,op=1){
  # Extract coordinates (x, y, z)
  coords <- mat.swc[,cols]
  
  # Compute convex hull indices
  hull.indices <- chull(coords)
  
  # Subset mat.swc to include only convex hull points
  convex.hull.points <- mat.swc[hull.indices, ]
  
  return(convex.hull.points)
}

# Helper function to compute the cross product of two vectors of length 3
getCrossProduct3Vec <- function(u, v) {
  return(c(
    u[2] * v[3] - u[3] * v[2],
    u[3] * v[1] - u[1] * v[3],
    u[1] * v[2] - u[2] * v[1]
  ))
}

# Helper function to compute the volume of a tetrahedron formed by four points
getTetrahedronVolume <- function(a, b, c, d) {
  return(abs(sum((a - d) * getCrossProduct3Vec(b - d, c - d))) / 6)
}

# Helper function to find the furthest point from a plane formed by three points
findFurthestPtPlane <- function(points, a, b, c) {
  volumes <- sapply(1:nrow(points), function(i) {
    getTetrahedronVolume(a, b, c, points[i, ])
  })
  furthest.index <- which.max(volumes)
  return(furthest.index)
}

#the quickHll algo in 3d: gets all the points that form the hull
quickhull3d <- function(points) {
  # Base case: if there are fewer than 4 points, return them all
  if (nrow(points) <= 4) {
    return(1:nrow(points))
  }
  
  # Initialize the convex hull with the points on the outer edges
  hull.indices <- c()
  
  # Find the extreme points (min/max in each dimension)
  min.x.index <- which.min(points[, 1])
  max.x.index <- which.max(points[, 1])
  min.y.index <- which.min(points[, 2])
  max.y.index <- which.max(points[, 2])
  min.z.index <- which.min(points[, 3])
  max.z.index <- which.max(points[, 3])
  
  # Initial hull points are these extreme points
  hull.points <- unique(c(min.x.index, max.x.index, min.y.index, max.y.index, min.z.index, max.z.index))
  
  # Recursively find the furthest points and build the convex hull
  for (i in 1:length(hull.points)) {
    for (j in (i + 1):length(hull.points)) {
      for (k in (j + 1):length(hull.points)) {
        remaining.points <- points[-hull.points, ]
        furthest.index <- findFurthestPtPlane(remaining.points, points[hull.points[i], ], points[hull.points[j], ], 
                                              points[hull.points[k], ])
        if (length(furthest.index) > 0) {
          hull.indices <- unique(c(hull.indices, hull.points[i], hull.points[j], hull.points[k], furthest.index))
        }
      }
    }
  }
  
  return(hull.indices)
}

# Wrapper function for the 3D convex hull algorithm
getConvexHull3d <- function(mat.swc) {
  # Extract coordinates (x, y, z)
  coords <- mat.swc[, 2:4]
  
  # Compute convex hull indices using Quickhull in 3D
  hull.indices <- quickhull3d(coords)
  
  # Subset mat.swc to include only convex hull points
  convex.hull.points <- mat.swc[hull.indices, ]
  
  return(convex.hull.points)
}

# Helper function to calculate the area of a polygon in 2D
computeCHullArea2d <- function(coords) {
  n <- nrow(coords)
  x <- coords[, 1]
  y <- coords[, 2]
  # Shoelace formula to compute area of the polygon
  area <- 0.5 * abs(sum(x * c(y[-1], y[1]) - y * c(x[-1], x[1])))
  return(area)
}

# Helper function to compute volume of a tetrahedron
# tetrahedron.volume <- function(a, b, c, d) {
#   return(abs(sum((a - d) * cross.product(b - d, c - d))) / 6)
# }

# Compute volume for 3D convex hull points
computeCHullVolume3d <- function(points) {
  # Choose the first point as the reference point
  reference <- points[1, ]
  
  # Sum the volume of tetrahedra formed by the reference point and triangles of other points
  volume <- 0
  for (i in 2:(nrow(points) - 1)) {
    for (j in (i + 1):nrow(points)) {
      volume <- volume + getTetrahedronVolume(reference, points[i, ], points[j, ], points[nrow(points), ])
    }
  }
  
  return(volume)
}

# Main function to compute the convex hull area (2D) or volume (3D)
# op=1- area in 2d, 2 - volumne in 3d
computeConvexHullArea <- function(mat.swc,op=1) {
  # Extract the coordinates (x, y, z)
  coords <- mat.swc[, 2:4]
  
  # Determine whether the points are 2D or 3D based on z coordinates
  # if (all(coords[, 3] == 0)) {
  if(op==1){
    # 2D case: Compute the area
    return(computeCHullArea2d(coords[, 1:2]))  # Use only x and y coordinates
  } else {
    # 3D case: Compute the volume
    return(computeCHullVolume3d(coords))
  }
}


# # Example Usage
# points.2d <- matrix(c(1, 1, 3, 1, 2, 2, 8, 6, 3, 3, 4, 2, 5, 4, 1, 7), ncol = 2, byrow = TRUE)
# 
# # Compute the convex hull
# hull.2d <- computeConvexHull(points.2d)
# 
# # Print the result
# print(hull.2d)
# 
# # Plot the points and the convex hull
# plot(points.2d, main = "Convex Hull Example", xlab = "X", ylab = "Y", pch = 19)
# polygon(hull.2d, border = "red", lwd = 2)
# points(hull.2d, col = "red", pch = 19)




