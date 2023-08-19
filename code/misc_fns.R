#R functions to read, analyze, plot and detect patterns in data along with neural and computer algorithms to model complex data.    

#Copyright (C) 2023 Shyam Srinivasan 

#This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details.

#You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.


#this file contains misc, list, and string functions
#it contains three types of functions mainly. First are string functions, next are math and math manipulation functions
#and third are helper functions like findfn


':==' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) 
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}

testfn <- function(var1,val1,environ=environment()) {
  tmp <- deparse(substitute(var1))
  print(environ)
  cat(str(tmp))
  assign(tmp,val1,envir = parent.env(environ))
  #tmp <- deparse(substitute(var1[2])) 
  #assign(tmp,val1,envir = parent.env(environ))
  
}

testfn <- function(...) {
  assignVarVals(c('tst1','tst3'),list(1,2))
  cat(tst1,tst3,"testfn env ")
  print(environment())
  tst5 <- 1
}

#given a list of variables and variable names, it assigns the values to the variable names
assignVarVals <- function(lstvar,lstval) {
  #tmp <- deparse(substitute(var1))
  #print(lstval)
  environ.par <- parent.frame()
  res <- lapply(1:length(lstvar),function(x,var,val){
    #tmp <- deparse(substitute(var[x]))
    #assign(var[x],val[[x]],envir = environ.par)
    # cat('var',var[x],'val')
    # print(val[[x]])
    assign(var[x],val[[x]],envir = environ.par)
  },lstvar,lstval)
}

#some string functions now
#gets the the posns of the last occurance of pattern in string
getLastIndex <- function(string,pattern,op=1){
  perlpattern <- paste('(?=',pattern,')',sep = '') #prepare the pattern in PERL form
  #gets all the indices
  tmp <- gregexpr(perlpattern,string,perl=T,ignore.case=T)[[1]]
  tmp[length(tmp)] #gets the last index
}

#gets the indices of the last n elements
lastNIndex <- function(vec,n,op=1){
  c((1 + length(vec)-n):length(vec))
}

#gets the last n elements
lastN <- function(vec,n,op=1){
  vec[c((1 + length(vec)-n):length(vec))]
}


#gets the string left after the lasts occurance of pattern
getLastPattern <- function(string,pattern,op=1){
  #from http://stackoverflow.com/questions/26289681/r-regex-find-last-occurrence-of-delimiter
  preppattern <- paste('.*[',pattern,']',sep = '') #prepare the pattern
  sub(preppattern,'',string)
}

#insert a string or character at a particular posn
#x - the given string into which the pattern is to be inserted
#patt : the pattern to be inserted
#pos: position where it should be inserted
insertPatternStr <- function(x,patt,pos,op=1){
  lhs <- ifelse(pos<=1,'',substr(x,1,pos-1))
  rhs <- ifelse(pos>(nchar(x)+1),'',substr(x,pos,nchar(x)))
  #cat(pos,(nchar(x)),rhs)
  paste(lhs,patt,rhs,sep = '')
}

#cleans the list/vector of strings, of any unwanted characters
#op=1, character at the start, 2  pattern or character at end
cleanStrVec <- function(strvec,pat=',',op=1){
  #pattern at start or end
  res <- switch(op,sub(pattern = paste('^',pat,sep = ''),'',strvec),
                sub(pattern = paste(pat,'$',sep = ''),'',strvec))
  res
}

#given 2 vectors of strings, matches them, and returns the matches of the second string in the first
#i.e. the result is a vector 1.i.n which gives the position in the second string that matches i
#x1,x2 - the vectors of the two strings
matchStringVecs <- function(x1,x2,op=1){
  pos1 <- sapply(x1, function(x){#get the position number of second string
    pos <- which(x2==searchstring(x2,x))
    ifelse(length(pos)==0,0,pos)
  })
  pos1
}

#function that gets pattern when the string starts with it
#string: the string from which we expect to see pattern
#pattern: the pattern to be matched
#postpattern: will match the patterm and postpattern, e.g., if it is pattern_post, will also match that _post follows after
#default is .*, i.e., anything or nothing after pattern
getStartPattern <- function(string,pattern,postpattern='.*',op=1){
  #from http://stackoverflow.com/questions/26289681/r-regex-find-last-occurrence-of-delimiter
  preppattern <- paste('(',pattern,')+(?=',postpattern,')',sep = '')  #prepare the pattern
  #cat(preppattern)
  str_extract(string,pattern = preppattern)
}

#function that, when give a vector of strings, where each strring is a bunch of charracters and numbers, will strip the 
#starting character string of numbers and return the string portion: e.g., Air3232 will return Air
#op=1, return the starting string
getStartingString <- function(vecstring,op=1){
  getStartPattern(vecstring,pattern = '[^0-9]')
}

#function that gets pattern when the string starts with it
#string: the string from which we expect to see pattern
#pattern: the pattern to be matched
#prepattern: will match the patterm and prepattern, e.g., if it is pre_pattern, will also match that pre preceeds pattern
#default is '', i.e., there is nothing before the pattern. prepatterns are not the same as postpatterns, see first 2 lines expln
#postpattern: will match the patterm and postpattern, e.g., if it is pattern_post, will also match that _post follows after
#default is .*, i.e., anything or nothing after pattern
#op:
getPattern <- function(string,pattern,prepattern='',postpattern='.*',op=1){
  #do the prepattern first, unlike the postpattern, the pre cannot have infinite length so either the
  #string starts with pattern or it has a prepattern
  if(identical(prepattern,'')==F){ 
    #cat('some prepattern',prepattern,'\n')
    preppattern <- paste('(?<=',prepattern,')',pattern,sep = '')  #prepare the pattern
  }
  else preppattern <- pattern  #prepare the pattern
  #do the prepattern, if true, then do the postpattern, liam
  
  #now do the postpattern
  preppattern <- paste(preppattern,'+(?=',postpattern,')',sep = '')  #prepare the pattern
  #cat(preppattern)
  str_extract(string,pattern = preppattern)
}

#tells you if the pattern is a number or stringnumber like string1, string2 etc
isStringNo <- function(strings,op=1){
  #chieck if the strings ends with a number. Its ok if it starts with a character
  tmp <- grepl('^[A-Za-z]*[0-9]+$',strings)
  if(length(which(tmp==T)) > 0) return(T)
  F
}


#for plotting labels with superscripts
#gets the string in expression form where text after ^ is treated as a superscript
getStringExp <-function(string){
  tmp <- strsplit(string,'^',fixed = T)[[1]] #find the ^ and spliot string there
  if(length(tmp) <= 1) return(bquote(.(string))) #if no ^, retyrn the string
  #split the part after ^, and treat the part after the blank as normal text
  laststr <- strsplit(tmp[2],' ',fixed = T)[[1]]
  if(length(laststr) <= 1) return(bquote(.(tmp[1])^.(laststr[1])))
  bquote(.(tmp[1])^.(laststr[1])~.(laststr[2]))
}

#takes a string with seperator sep, and replaces it with new seperator newsep
replaceStringSep <-function(string,sep=' ',newsep="_",op=1){
  newstring <- strsplit(string,split = sep) #gets the individual words
  res <- newstring[[1]][1]
  #now concatenate the words with the new seperator
  for (i in 2:(length(newstring[[1]]) ) ) {
    res <- paste(res,newstring[[1]][i],sep = newsep)
  }
  res
}

#given a string like "1 2 3 4" or "cat,dog,mice" will separate out each of the individual elements 
#into a proper vector of numbers or strings
#str: the string that should be broken up
#sep: the separate in the string like blank or comma
#vectype: type of vector 1: vector of numbers, 2 - of strings
#op: 1- return the vector, 2 - return in usable format like c('1','2','3')
makeVectorFromString <- function(str,sep=' ',vectype=1,op=1){
  vec <- strsplit(str,split = sep)[[1]] #split the string based on sep
  if(vectype==1) vec <- as.numeric(vec) #if numeric output desired
  cat('vector: ',str(vec))
  vec <- switch(op,vec,genUsableVec(vec,vectype)) #if normal or usable vector desired
  vec
}

#given a vector, generates a vector in program usable format like c('1','2','3')
#of c(1,2,3)
#vectype: 1 = numbers, 2 - strings
genUsableVec <- function(vec,vectype=1,op=1){
  if(vectype==2) itemsep <- '\''
  else itemsep <- ''
  res <- paste('c(',itemsep,vec[1],itemsep,sep = '') #the first element
  #now add every subsequent element
  for (i in 2:length(vec)) {
    res <- paste(res,paste(itemsep,vec[i],itemsep,sep = ''),sep = ',')
  }    
  res <- paste(res,')',sep = '')
  res
}

#given a vector of strings that have numbers in them, such as string1,string2,string11,...
#it solves the problem wherein R sorts them as 1,11,2...
#if one of the strings has no numbers returns an empty string ""
#op=1, the sorted strings, 2 - the sorted posns
sortStringNos <- function(vec,decrease=F,op=1){
  if(is.numeric(vec)) {#it is a numeric vector
    return(switch(op,sort(vec,decreasing = decrease),order(vec,decreasing = decrease))) 
  }
  res <- gregexpr(pattern = '[0-9]+',vec) #get the matches for those posns with numbers
  res <- regmatches(vec,res) #get the text at those posns
  #if there are multiple matches, get the one that changes, not the the one that is constant
  no <- length(res[[1]]) #the no of matched elements
  #cat('\nsortstring',is.numeric(vec),str(res))
  elemno <- sapply(1:no, function(x){
    vals <- unlist(lapply(res,'[[',x)) #get all the elements in the no_th place
    #cat(vals)
    ifelse(areValsIdentical(vals),F,T) #return T if the values are changing
  })
  posns <- which(elemno==T) #get the posns that changed
  if (length(posns) == 1) res <- unlist(lapply(res,'[[',posns)) #if only 1 posn
  else res <- unlist(lapply(res,'[[',posns[1])) #return the first changing posns
  #sort along the changing posn element
  res <- sapply(res, function(x) ifelse(length(x)==0,'',x))
  res.sort <- vec[order(as.numeric(res),decreasing = decrease)]
  #cat('\n',sort(as.numeric(res)),'\n',res.sort,'\n')
  switch(op,res.sort,order(as.numeric(res),decreasing = decrease) )
}

#given a list of names, and the names to skip, will return the index of the skip name
#vec: vector of strings
#skip: ht eindex to skip if it contains these strings or substrings
#op=1, the index no, 2 - returns the vector without the skip element
getSkipStrIndex <- function(vec,skip,op=1){
  #gets the positions of skip elements in vec
  res <- sapply(skip, function(x) which(grepl(x,vec,fixed = T)))
  #cat('getskip',unlist(res),'end\n')
  pos <- unlist(res) #make itinto a vector
  if(length(pos) != 0 && op==2) res <- vec[-pos]
  else res <- vec
  switch(op,pos,res)
}

#given a comma separated string, tells you if any of the ensuing sub-strings are identical
#mainstr: the string whose substrings we are considering
#sep: the separator
areSubStringsSame <- function(mainstr,sep=',',op=1){
  tmp <- strsplit(x = mainstr,split = ',',fixed=F)
  #cat('splut',str(tmp))
  #got through and check all items against each other to make sure they are not identical
  res <- sapply(tmp[[1]], function(x) { 
    sapply(tmp[[1]], function(y){ 
      if(identical(x,y)) tmp <- 1
      else tmp <- 0
      tmp
    })
  })
  if(sum(unlist(res))>length(tmp[[1]])) return(T)
  F
}


#gets the index of strings that have identical substraings, returns a vector of these indices
#strvec: the vector od strings
#sep: the separator
#op=1, index of strings that have identical strings, 2 = index of strings whose substrings are not identical
getIndexSameSubStr <- function(strvec,sep=',',op=1){
  res <- sapply(strvec, function(x){
    #cat('\nx,areSubStringsSame(x,sep = sep),\t')
    areSubStringsSame(x,sep = sep)
  })
  switch(op,which(res == T),which(res == F))
}

#given a sttring separted by split, will get the first or nth substring substring
#sub: the substring no to get
getSubString <- function(string,split=',',sub=1,op=1){
  res <- strsplit(string,split = split)[[1]][sub]
}

#concantenates a vector of numbers into a string separated by sep
concatenateVecStr <- function(vec,sep=',',op=1){
  do.call(paste,c(as.list(vec),list(sep=',')) )  
}

#given a matrix of numbers, will return a vector of strings of the concatenated numbers from each row.
concatenateMatStr <- function(mat,sep=',',op=1){
  res <- sapply(1:nrow(mat),function(i) concatenateVecStr(mat[i,],sep = sep))
}

#check if the strings by search strings are part of the string
#searchstrs: the strings to search for
#not: the strings that should be taken out of the search
#op = 1, returns all sttrings that contain all search strings: intersect
#op = 2, returns all sttrings that contain aany search strings: union
#op= 3, returns all strings that contain both the searchstrs but exclude the "not" strings
#op 4, all strings that contain any of the search strings but exclude the not strings
searchString <-function(string,searchstrs,not=c(),op=1){
  tmp <- lapply(searchstrs, function(x,string){
    grep(x,string,ignore.case = T,value = T)
  },string)
  #now search for the smaller subset that includes search strings and "not" strings
  tmpnot <- lapply(c(not), function(x,string){
    grep(x,string,ignore.case = T,value = T)
  },string)
  notunion <- Reduce(union,tmpnot) #the list of all strings that contain a not string
  #print(tmp)
  #now get the intersection of these results
  switch(op,Reduce(intersect,tmp),Reduce(union,tmp),setdiff(Reduce(intersect,tmp),notunion),
         setdiff(Reduce(union,tmp),notunion))
  #grep(,filenames,ignore.case = T,value = T)
  
}

#strop=1, starts with open, contains cont, and does not contain not
#op =1 , return indices in vector, 2 return vector elements
searchStartString <- function(vecstr,openstr=c(),cont=c(),not=c(),strop=1,op=1){
  if(length(openstr) > 0 ){ #all the strings that start with one of the strings in open
    tmpopen <- lapply(openstr, function(x){
      tmp <- startsWith(vecstr,x) #get all strings that start with x 
      which(tmp == T)#convert from logical to numeric vector
    })
    #merge all the lists
    tmpopen <-  Reduce(union,tmpopen) #posns of all matches
  }
  else tmpopen <- 1:length(vecstr)
  #all the strings that contain one of the sttrings in cont
  if(length(cont) > 0 ){
    tmpcont <- lapply(cont, function(x){
      grep(x,vecstr,ignore.case = T,value = F)
    })
    tmpcont <- Reduce(union,tmpcont) #the list of all strings that contain a not string
  }
  else tmpcont <- 1:length(vecstr)
  #now search for the smaller subset that includes search strings and "not" strings
  tmpnot <- lapply(c(not), function(x){
    grep(x,vecstr,ignore.case = T,value = F)
  })
  notunion <- Reduce(union,tmpnot) #the list of all strings that contain a not string
  res <- setdiff(intersect(tmpopen,tmpcont),notunion)
  res <- switch(op,res,vecstr[res])
  res
}

#op same as ops for searchstring function
#not: the strings that should be taken out of the search
#op = 1, returns all sttrings that contain all search strings: intersect
#op = 2, returns all sttrings that contain aany search strings: union
#op= 3, returns all strings that contain both the searchstrs but exclude the "not" strings
#op 4, all strings that contain any of the search strings but exclude the not strings
#colno - that contains the strings to be searched
findDFSearchString <- function(mat.df,col=1,searchstrs,not=c(),op=1){
  searchres <- searchstring(mat.df[,col],searchstrs = searchstrs,not = not,op = op)
  posns <- which(mat.df[,col] == searchres)
  mat.df[posns,]
}


#given a two vector of strings, checks if the strings in the second vector start with any of the strings
#in the first vector
searchStringVecStart <- function(vec.str1,vec.str2,op=2){
  res <- lapply(vec.str2,function(x){
    present <- str_locate(vec.str1,x) #gets the posn where x starts in all the strings of vec.str1. A matrix
    present <- insertColDf(present,newcol = 1:nrow(present),posn = 3) #insert a col of row nos since we want posns
    posns <- which(present[,1]==1) #get all those posns where where the strings start with x
    present[posns,3]
  })
  res
}

#given a set of vectors of the forms "#1,#2,#3" arranges them so that they are in the following form where the rows
#are ordered by the first column, then the second and so on
#vecstrs: vector strings
#op:the sorted data frame, 2 = not sorted in anyway
orderVecStringNos <- function(vecstrs,vecval=c(),op=1){
  #break up each entry of the vector and output a matrix
  #cat('\n',vecstrs)
  entries <- sapply(1:length(vecstrs),function(i) {
    #cat('\t',vecstrs[i],str(strsplit(vecstrs[i],split = ',') ))
    as.numeric(unlist(strsplit(vecstrs[i],split = ',') ) ) 
  })
  entries <- as.data.frame(t(entries))
  #also include the original vectors and add an index
  if(length(vecval)==0) entries.full <- cbind.data.frame(entries,1:nrow(entries),vecstrs)
  else entries.full <- cbind.data.frame(entries,1:nrow(entries),vecstrs,vecval)
  res <- getMatSortCols(entries.full,cols=1:ncol(entries),op=3)
  switch(op,res,entries.full)
}

#takes a string with a blank between numbers and makes it into a numeric vector
#strpiece: the piece of string, with blanks separating numbers.
convertStringSentenceToNos <-function(strpiece,sep=' ',op=1){
  as.numeric(strsplit(strpiece,split = sep)[[1]])
}

#takes a vector of string with a blank between words and forms a new sentence with repsep between words
#strpiece: the piece of string, with blanks separating numbers.
convertStringBlanks2New <-function(strpiece,sep=' ',newsep='_',op=1){
  do.call(paste,as.list(c(strsplit(strpiece,split = sep)[[1]],sep = newsep) ) )
}


#takes a string of numbers separated by sep and gives you a matrix or df of those nos
#op=1, return the nos as a matrix, 2 - return a matrix, with the string in the first col and 
#the nos in the subsequent columns, 3 - op.2 + avg of the nos in the 4th column, 
#4: op.2,3 + min of the nos of the 4th column 
convertStringNos2Df <-function(strvec,sep=' ',op=1){
  res <- sapply(1:length(strvec), function(i)  
    as.numeric(strsplit(strvec[i],split = ',')[[1]]) )
  if(isDataType(res) == const.DataType$vector)  res <- t(matrix(res))
  res <- t(res) #bug fixed: so that the params are in the same row
  colnames(res) <- 1:ncol(res)
  if(op>=2 && op<=4) {#if you want a min and mean columnn
    res.df <- cbind.data.frame(strvec,res,stringsAsFactors = F)
    res.df <- ConvertDfCols(res.df,cols = 1:nrow(res)+1)
    if(op==3 || op==4) {
      res.df <- cbind.data.frame(res.df,apply(res.df[,1:nrow(res)+1],1,mean))
      colnames(res.df)[ncol(res.df)] <- 'mean'
      if(op==4) {
        res.df <- cbind.data.frame(res.df,apply(res.df[,1:nrow(res)+1],1,min))
        colnames(res.df)[ncol(res.df)] <- 'min'
      }
    }
  }
  else res.df <- cbind.data.frame(strvec,res)
  if(op==1) res.df <- cbind.data.frame(strvec,res)
  names(res.df)[1] <- 'id.string'
  res.df
}


#for a matrix of numbers and columns will sort it across the specified number of cols, i.e., the fist column,
# then for every item in the first coumn, the second column.
#mat: the matrix to be sorted
#cols: the columns to be sorted
getMatSortCols<-function(mat,cols=1:(ncol(mat)-2),op=1){
  mat.sort <- getMatSortPosns(mat,col = cols[1],op=3) #sort by the first col
  if(length(cols)==1) return(mat.sort) #only when 1 col needs to be sorted, return the sorted one
  #get the groups of different items, and sort them individually.
  nos <- unique(mat.sort[,cols[1]])
  noposns <- lapply(nos,function(x) which(mat.sort[,cols[1]]==x))
  for(i in seq_wrap(1,length(nos))){
    tmpmat <- mat.sort[noposns[[i]],] #sort the grp of each item
    tmp.sort <- getMatSortCols(tmpmat,cols = cols[-1],op=op)
    mat.sort[noposns[[i]],] <- tmp.sort
  }
  mat.sort
}

#given a data frame, will get all those rows that have one of the string patterns
#dat.df: the data frame
#patterns: the string patterns which have to be tested
#col: the column that is supposed to contain the patterns
getPatternRows <- function(dat.df,patterns,col=1,op=1){
  patmatch <- paste(patterns,collapse = '|') #put the patterns in an OR format for grep
  res.df <- dat.df[grep(pattern = patmatch,x = dat.df[,col]),]
  res.df
}

#another way to do the function on top
#this function, given a number of string criteria gets all the rows that satisfy them
#dat.df: the data frame
#cond: the string patterns which have to be tested
#notcond: exclude the stings that have these patterns
#col: the column that is supposed to contain the patterns
#factorop: op=1, only limit factors to the ones in this df
#op=1, intersection of patterns in condn i.e. all the patterns in condn, 
#2 - union of patterns i.e. at least one of the patterns in condn
getPatternDFRows <-function(dat.df,condn=c(),notcond=c(),col=1,factorop=1,op=1){
  #cat(dat.df[,col] %in% searchstring(dat.df[,col],condn,op=1))
  if(length(notcond)>0) opcond <- op+2
  else opcond <- op
  res.df <- dat.df[dat.df[,col] %in% searchstring(dat.df[,col],searchstrs = condn,not = notcond,op=opcond),]
  #adjust it so that the factors involved are only for this df
  if (factorop==1) res.df <- ConvertDfCols(res.df,cols = col,op=3) 
  res.df
}


#this function calculates the inverse function target of an eqn where we want
# to find x such that eq(x) == y
#this is done through a recursive binary search
#x1,x2, the two bounds for x within which the target y is present
#y: the target y for which we want to find the x
#prec: once y is within this precision we stop,default: 1/100th of y
#iter: ensures that we dont get stuck in an endless loop
#op=1, always getthe result lower than y, 2 - greater than y: eq is an increasing fn
#op=3, get the result greater than y, 4 - lower than y :eq is decreasing fn
binarySearchold <- function(x1,x2,y,eq,prec=.01,maxiter=20,op=1){
  #if within the precision limit this is our answer
  cat(maxiter,x1,x2,y,'diff',y-eq(x1),eq(x2)-y,prec*y,abs(x1 - x2),'\n')
  #stopping condn: within prec of the target
  if (op==1) if( abs(y-eq(x1))  < prec*y || abs(x1 - x2) <= prec*y) return(x1)
  if (op==3) if( abs(y-eq(x2))  < prec*y || abs(x1 - x2) <= prec*y) return(x2)
  if (op==2 || op==4) if( abs(eq(x2)-y)  < prec*y || abs(x1 - x2) <= prec*y) return(x2)
  #make sure we dont keep iterating
  if (maxiter == 1) return(switch(op,x1,x2,x2,x1))
  #increasing fn
  if(op==1){
    #first check if the value of x1 is higher than the target value of y
    #if so, the lowest value of x has to be x1
    if(eq(x1) > y) return(x1)
    newx <- getLowerVal(x1,x2,y,eq)
    return(binarySearch(x1=newx,x2,y,eq,prec=prec,maxiter=maxiter-1,op=op))
  }
  #decreasing fn
  if (op==3){
    #or if the value of x1 is lower than y for dec. fn
    #if so, the lowest value of x has to be x1
    if(eq(x1) < y) return(x1)
    newx <- getHigherVal(x1,x2,y,eq,op=2)
    #cat('newx',newx,'\n')
    return(binarySearch(x1,x2=newx,y,eq,prec=prec,maxiter=maxiter-1,op=op))
  }
  if (op==2){
    #first check if the value of x2 is loweer than the target value of y
    #if so, the highest value of x has to be x2
    if(eq(x2) < y) return(x2)
    newx <- getHigherVal(x1,x2,y,eq)
    return(binarySearch(x1,x2=newx,y,eq,prec=prec,maxiter=maxiter-1,op=op))
  }
  if(op==4){#eq is a decreasing fn
    #first check if the value of x2 is higher than the target value of y
    #if so, the highest value of x has to be x2
    if(eq(x2) > y) return(x2)
    newx <- getLowerVal(x1,x2,y,eq,op=2)
    #cat(maxiter,'\t',newx,':')
    return(binarySearch(x1,x2=newx,y,eq,prec=prec,maxiter=maxiter-1,op=op))
  }
  #this means y is not in between, in which case return the closer of the two
  #cat('return res')
  switch(op,x1,x2,newx,newx)    
  
}

#given x1, x2, and a target value y, finds the closest value of x that gives 
#a value for eq that is lower than y. here y is the target.
#eq: eqn of the form y = fnn(x)
#maxiter: max. no of iterations to stop inifinte recursion
#op=1: eq is increasing, 2: eq is decreasing
getLowerVal <- function(x1,x2,y,eq,maxiter=10,op=1){
  #first check if the value of x1 is higher than the target value of y for inc .fn
  #or if the value of x1 is lower than y for dec. fn
  #if so, the lowest value of x has to be x1
  if(eq(x1) > y && op == 1) return(x1)
  if(eq(x2) > y && op == 2) return(x2)
  z <- (x1+x2)/2
  #cat(maxiter,z,'\n')
  if (maxiter == 1) return (z) #maximum no of iterations
  #if the value is greater than y then we need a smaller value
  if ( eq(z) > y && op==1) z <- getLowerVal(x1,z,y,eq,maxiter = maxiter-1,op=op)
  if ( eq(z) > y && op==2) z <- getLowerVal(z,x2,y,eq,maxiter = maxiter-1,op=op)
  #cat('returnign',z)
  z
}

#given x1, x2, and a target value y, finds the closest value of x that gives 
#a value for eqthat is greater than y. here y is the target.
#eq: eqn of the form y = fnn(x)
#maxiter: max. no of iterations to stop inifinte recursion
#op=1: eq is increasing, 2: eq is decreasing
getHigherVal <- function(x1,x2,y,eq,maxiter=20,prec=.001,op=1){
  #first check if the value of x2 is loweer than the target value of y
  #if so, the highest value of x has to be x2
  if(eq(x2) < y && op == 1) return(x2)
  if(eq(x1) < y && op == 2) return(x1)
  if (abs(x1-x2) < prec*y) return(x2)
  z <- (x1+x2)/2
  cat(maxiter,z,eq(z),'\n')
  if (maxiter == 1) return (z) #maximum no of iterations
  #if the value is greater than y then we need a higher value
  if ( eq(z) < y && op==1) z <- getHigherVal(z,x2,y,eq=eq,maxiter = maxiter-1,op=op)
  cat('returning',maxiter,x1,x2,z,'\n')
  if ( eq(z) < y && op==2) z <- getHigherVal(x1,z,y,eq=eq,maxiter = maxiter-1,op=op)
  else z <- getHigherVal(z,x2,y,eq=eq,maxiter = maxiter-1,op=op)
  cat('returning2',maxiter,z,'\n')
  z
  
}



#this function calculates the inverse function target of an eqn where we want
# to find x such that eq(x) == y
#this is done through a recursive binary search
#x1,x2, the two bounds for x within which the target y is present
#y: the target y for which we want to find the x
#prec: once y is within this precision we stop,default: 1/100th of y
#iter: ensures that we dont get stuck in an endless loop
#op=1: get x1 closer to the origin, 2, get x2: determines which side of the x axis you want
binarySearch <- function(x1,x2,y,eq,prec=.01,maxiter=20,op=1){
  #if within the precision limit this is our answer
  #cat(maxiter,x1,x2,y,'diff',y-eq(x1),eq(x2)-y,prec*y,abs(x1 - x2),'\n')
  #determine dirn, for purely monotic inc. or dec function
  if (eq(x1) <= eq(x2)) dir <- 1
  else dir <- -1
  #cat('\nbinary',x1,x2,y,dir)
  #just get the closest val
  getClosestVal(x1,x2,y,eq,dir = dir,maxiter = 20,prec = prec,op=op)
}

#gets the highest index <= value in the vector
computeValueIndex <- function(x,vec,op=1){
  res <- length(which(vec>x))
  res
}

#get the next value that is not infininty, or, if there is a point in between that 
#is higher than the last, then get that point
#x1 and x2 are the first and last points, and eq is the equation which gives the point
#changed from earlier. x1: the whole vector, x2 - the last point
getNextValInf <- function(x1,x2,eq,prec=1,op=1){
  sep <- 10^(getpow10(x2-x1)-prec) * (x2-x1) #get the precision
  vec <- seq(x1,x2,sep)
  res <- sapply(vec,function(i){
    is.infinite(eq(i))
  })
  #cat('\ngetnextvalinf',x1,x2,sep,res)
  if(res[1]==T){
    first <- vec[which(res==F)[1]]
  }
  else first <- x1
  if(res[length(res)]==T){
    last <- vec[rev(which(res==F))[1]]
  }
  else last <- x2
  c(first,last)
}

#changed from getNextValInf, this is better function for graphs
#get the next value before max that is not infininty, or, if there is a point in between that 
#is higher than the last, then get that point
#modification of 
#x: the whole vector, x.max - the last point
getHighestValInf <- function(x,x.max,eq,prec=1,op=1){
  # sep <- 10^(getpow10(x2-x1)-prec) * (x2-x1) #get the precision
  vec <- x #seq(x1,x2,sep)
  res <- sapply(vec,function(i){
    is.infinite(eq(i))
  })
  #get the values of y
  res.y <- sapply(vec,function(i){
    eq(i)
  })
  res.y <- cleanNAVec(res.y) #take out all NA and Inf
  #get the higest and lowest values of y and corresponding x's
  x1 <- min(res.y);x2 <- max(res.y)
  #cat('\ngetnextvalinf',x1,x2,res,'\n',res.y,'\n')
  first <- which(res.y==x1);last <- which(res.y==x2)
  c(first,last)
}



#function takes in two arguments x1 and x2 and a function fn. If fn(x1 or x2) < zero, it increases or decreases x 
#until it is fn(x+/-a) >= zero
#step: takes a step in either direction and then moves back or forwards by prec steps.
#returns a changed x1 and x2
#zero: the lowest value possible, default 0
computeFnZero <- function(fn,x1,x2,zero=0,prec=.01,lgscale=10,op=1){
  #cat('\ncomputefnzero',x1,x2,zero,fn(x1))
  if(fn(x1) < zero) {#it is x1 that must be replaced
    #get the zero point, which is the log(pt,lgscale) add prec to make sure it is above 0
    xzero <- binarySearch(x1,x2,y=zero,prec = prec,eq = fn)
    #cat('\nhere',xzero,xzero+prec)
    return(c(xzero+prec,x2))
  }
  if(fn(x2) < zero) {#it is x2 that must be replaced
    #get the zero point, which is the log(pt,lgscale) add prec to make sure it is above 0
    xzero <- binarySearch(x1,x2,y=zero,prec = prec,eq = fn)
    return(c(x1,xzero-prec))
  }
  c(x1,x2) #they're both good so just return the originals
}

#gets the closest value to the target in the vector
#target - the target value, vec - tjhe vector of numbers
getClosestValVector <- function(vec,target,op=1){
  #algo get the diff between vec and target, and pick the minimum now
  vec.diff <- (vec - target)^2 #square to get rid of sign
  mat.diff <- cbind(1:length(vec.diff),vec.diff)
  mat.sort <- getMatSortPosns(mat.diff,col = 2)
  res <- vec[mat.sort[1]]
  res
}

#gets the closest values to the target in the vector within range
#target - the target value, vec - tjhe vector of numbers
#range - gets the closest values within the range specified as either 
#a percentage (op=1) or as absolute values (op=2)
#retop: 1 - get the values, 2 - get the posns
getClosestValRange <- function(vec,target,range=0.05,retop=1,op=1){
  #algo get the diff between vec and target, and pick the minimum now
  vec.diff <- abs(vec - target) #abs to get rid of sign
  if(op==1) valrange <- target*range
  else valrange <- range
  posns <- which(vec.diff <= valrange)
  #cat('\nvec: ',vec,'\tvalrange: ',valrange,'\tposns',posns)
  res <- switch(op,vec[posns],posns)
  res
}


#given a lst of numbers and a target number, will get the the number if it is present
#or the closest numbers lower and higher than it. 
#lstvec: vector of numbers 
#target: the target number 
#op: 1 - return the values, 2 - return the positions, 
getClosestCandidates <- function(lstvec,target,op=1){
  #got either way and get the closest guys
  lstpts <- unique(lstvec)
  #logic: for lowest. get the closet lowest element, and if none, thats fine too
  #as there is bound to be a number greater than target in that case
  #closest.small and high are positions in the unique vector: lstpts
  smaller <- -(lstpts - target)
  smaller <- lstpts[which(smaller>=0)]
  closest.small <- which(lstpts == sort(smaller,decreasing = T)[1] )
  higher <- (lstpts - target)
  higher <- lstpts[which(higher>=0)]
  closest.high <- which(lstpts == sort(higher,decreasing = F)[1] )
  # cat('\nclosest:',closest.small,lstpts[closest.small],'\t:',closest.high,lstpts[closest.high])
  # cat('\nclosest set',which(lstvec==closest.small),':',which(lstvec==lstpts[closest.high]))
  switch(op,c(lstpts[c(closest.small,closest.high)]),
         c(which(lstvec==lstpts[closest.small]),which(lstvec==lstpts[closest.high])))
}

#tells you if all the numbers in the vector are very close to each other, Basically, if they 
#if the extremes are within 10% of the mean
#range: the range within which the numbers should be to be onsideered close. 0.1 means 10%
#op: 1 -T/F if within range, 2 - the actual width fraction mnormalized to the mean
isVecInRange <- function(vec,range=0.1,op=1){
  avg <- mean(vec)
  res <- (max(vec)-min(vec))/avg
  switch(op,res<=range,res)
} 


#given x1, x2, and a target value y, finds the closest value of x on the line to y, depending on the
#options. i.e. closest value of inv eq(y) such that eq(x) == y
#eq: eqn of the form y = fnn(x)
#dir: 1, eq is increasing, -1-eqn is decreasing.
#maxiter: max. no of iterations to stop inifinte recursion
#op=1: get x1 closer to the origin, 2, get x2: determines which side of the axis you want
getClosestVal <- function(x1,x2,y,eq,dir=1,maxiter=.2/prec,prec=.01,op=1){
  #setup target for the precision calculation making sure that prec* is not 0
  if (op==1 && (eq(x1)-y==0)) return(x1) #x1 and y already match, so just return x1
  if(y==0) target <- 1
  else target <- y
  #cat('\ninitial',dir,x1,x2,eq(x1)-y,eq(x2)-y,'eq(x1,x2),y',eq(x1),eq(x2),y)
  #if y is below x1 or x2, then return the closet to y
  if( (eq(x1) - y)*dir > 0 && op == 1) return(x1)
  if( (eq(x2) - y)*dir < 0 && op == 2) return(x2)
  #also check if y is above x1 or x2,
  if( (y-eq(x2) )*dir > 0 && op == 1) cat('here',x2)
  if( (y-eq(x1) )*dir < 0 && op == 2) cat('erer',x1)

  #cat('\nstart',maxiter,x1,x2,'abs(x1-x2)',abs(x1-x2),'prec*target',prec*target,'target',target,'prec',prec)
  #stopping cond: return x1 if op==2 as that is the one above the x-axis and vice-versa
  #fixed error so that the precision is a percentage of difference of y scores and not x -scores
  if (abs(eq(x1)-eq(x2)) < prec*target && op == 1) return(x2)
  if (abs(eq(x1)-eq(x2)) < prec*target && op == 2) return(x1)
  
  z <- (x1+x2)/2
  #cat('\n',maxiter,z,eq(z))
  if (maxiter == 1) return (z) #maximum no of iterations: move this up
  #if the value is greater than y then we need a higher value
  #cat('\nreturning',maxiter,x1,x2,'z,eq(z),',z,eq(z),(eq(z) -y)*dir,'\n')
  #go at it from both sides by reducing x1 and x2
  if ( (eq(z) -y)*dir < 0 ) z <- getClosestVal(z,x2,y,eq=eq,dir=dir,maxiter = maxiter-1,op=op,prec = prec)
  else z <- getClosestVal(x1,z,y,eq=eq,dir=dir,maxiter = maxiter-1,op=op,prec = prec)
  #cat('\nreturning2',maxiter,z,'\n')
  z
}

#tells you if the elems of a vector are the same
#if there are no elements return F? Not sure.
elemsIdentical <- function(vec,op=1){
  len <- length(vec) #check all elements against each other, if not all are similar return F
  if(len==0) return(F) #empty: return F
  if(len==1) return(T)
  if(len>1) res <- sapply(1:(length(vec)-1), function(x) {
    #cat('\tEI_1',x,vec[x],vec[x+1],vec[x]==vec[x+1],identical(vec[x],vec[x+1],num.eq = F))
    vec[x]==vec[x+1] #identical seems to fail over here
  })
  #cat('\neI',str(vec))
  #cat(sum(res))
  if(sum(res)<(len-1)) return(F)
  T
}

#given n vectors given as lists, and their mix percentage will produce a vector that is a combination
#veclst: list of the vectors to be combined
#vecnames: the indices or identifiers of the vectors, needed for making the label/name
#op=1, choose mix based on elements, ie., 25 % from A, 75 % elements from B
#op=2, choose mix based on magnitude, ie., for any position,  25 % A + 75 % B
#mix: the mixtures of A and B, c(.25,.5,1) c(.25 of A, .75 of B,...); default 0.5, if n vectors, this should be n-1
combineVectors <- function(veclst,vecnames,mix=c(0.2),op=1){
  veclen <- length(veclst[[1]]) #length of vector
  #logic: put the odor with the greater proportion in front
  mixvec <- c(mix,1-sum(mix)) #proportions of each vector
  mixvecsize <- distOverVector(veclen,floor(mixvec*veclen)) #takes care of any stragglers because of rounding
  #df with indices of mixvec,the ratio mix,the vector names, and size of each mix component sortedby mix ratio
  mix.df <- cbind.data.frame(1:length(mixvec),mixvec,vecnames,mixvecsize)
  mix.df <- getMatSortPosns(mix.df,col=2,decrease = T,op=3) #sorted matrix
  mix.df <- insertColDf(mix.df,newcol = cumsum(mix.df[,4]),posn = 5) #gives position where you can insert
  zeroposns <- which(mix.df[,2]==0) #all those vectors whose mix component is 0
  if(length(zeroposns)>0) mix.df <- mix.df[-zeroposns,] #take out the components whose mix proportions are 0
  #cat('\nCV',mixvec,mixvecsize,'zero ',zeroposns)
  newvec <- rep(0,veclen)
  #cat('\ncV',mix,',',mixvecsize,',',veclen,',',mixvec,'\n')
  if(op==1){#substitute from either odor based on mix proportions
    mixposns <- sample(veclen)
    posns.lst <- lapply(1:nrow(mix.df),function(i) mixposns[((mix.df[i,5]-mix.df[i,4])+1):(mix.df[i,5])]  )
    newvec <- rep(0,veclen) #initilialize newvec
    newvec.lst <- lapply(1:length(posns.lst),function(i) {
      if(length(posns.lst[[i]])>0)  newvec[posns.lst[[i]]] <- veclst[[mix.df[i,1]]][posns.lst[[i]]] 
      newvec
    })
  }
  if(op==2){#weighted average
    newvec.lst <- lapply(1:length(mixvec),function(i) mixvec[i]*veclst[[i]]  )
  }
  newvec <- sumListVectors(newvec.lst)
  #make the combination label, first make a data frame of the labels and mixes
  # combolabel <- getMatSortPosns(cbind.data.frame(mixvec,vecnames),col=1,decrease = T,op=3 )
  # combonames <- concatenateVecStr(unlist(combolabel[,2]) )
  # combomix <- concatenateVecStr(unlist(combolabel[,1]) )
  res.label <- prepLabels(mixvec = mixvec,mixnames = mixnames,vecnames=vecnames)
  list(newvec,res.label)#paste(combonames,combomix,sep = ',') ) #return stimulus and mix proportion as name
  #mix.df
}

#prepare the labels to relfect the mix ratio and the components.
#If the first component is 0, i.e., noise, then flip that with the second highest component
prepLabels <- function(mixvec,mixnames,vecnames,op=1){
  combolabel <- getMatSortPosns(cbind.data.frame(mixvec,vecnames),col=1,decrease = T,op=3 )
  if(combolabel[1,2] == 0 ){#if the greatest concentration is noise, move it to the end
    combolabel <- rbind.data.frame(combolabel,combolabel[1,])
    combolabel <- combolabel[-1,]
  }
  combonames <- concatenateVecStr(unlist(combolabel[,2]) )
  combomix <- concatenateVecStr(unlist(combolabel[,1]) )
  res <- paste(combonames,combomix,sep = ',')  #return stimulus and mix proportion as name
}


#given a bunch of vectors and combination ratios will generate mixtures between all of them
#the output is a list in the form of the different combinations. and the names
#give the odor pair and proportion 
#combno - no of vectors to combine; if there is a fixed vector, then it is implicitly included as part of veclst
#mix - ratio in which to combine them, has to of length combno-1. Includes the fixed vector as the last component.
#fixedvec - a fixed vector to be mixed with vector combos of veclst, the ratio is determined by 1 - sum(mix)
#if combono =2, then mix should have 1 element.
#fixedmix: the ratio for the fixed vector. all ratios/proportions should sum to 1
#veclabels: the labels for eevery element on veclst, if c(), all elements have the same label,, 
#stimuli - veclst, stimlen - novecs
#op=1, all possible combinations of vectors from each type of label, 2 - combinations of vectors from each type of label only once, e.g., 
#if there are 4 vectors 1,2,3,4 if type 1,2,1,2, if 1,2 are pair then 1,4 cannot be a pair
genVectorCombos <- function(veclst,combno=2,mix=list(0.2),veclabels=c(),fixedvec=c(),mixop=1,op=1){
  novecs <- length(veclst)
  if(length(veclabels)==0) vlabels <- 1:novecs
  else vlabels <- veclabels
  #cat('\n',novecs,vlabels)
  if(length(fixedvec)==0)  permutations <- permuteGroupsFn(split(1:novecs,vlabels),k = combno,op=op) #no fixed vectors
  else permutations <- permuteGroupsFn(split(1:novecs,vlabels),k = combno-1,op=op) #fixed vector, so combno-1 changeable components
  #cat('\ngVC',combno,1:novecs,str(mix))
  #combine the odor with all the odors, skip itself
  res.combo <- lapply(1:nrow(permutations),function(i){
    #figure out how to use the various combindations of mix, for two odors if mix=(.2,.1) Have to do combinations of these
    #what does this look like for 3 odors if mix = list(c(.2,.2),c(.3,.2)). So, it has to be a list so list(.2,.1)
    res.mix <- lapply(1:length(mix),function(k){#all the mixture combinations for this pair
      #cat('\ngenVector',permutations[i,],mix[[k]],',',str(c(veclst[permutations[i,]],list(fixedvec))))
      if(length(fixedvec)>0) vecnames <- c(permutations[i,],0)
      else vecnames <- permutations[i,]
      #cat('\ngVC1',mix[[k]],str(vecnames) )
      combvec <- combineVectors(veclst = c(veclst[permutations[i,]],list(fixedvec)),vecnames=vecnames,mix = mix[[k]],op=mixop)
      combvec
    })
    mixlst <- lapply(res.mix,'[[',1) #the odors
    names(mixlst) <- lapply(res.mix,'[[',2) #name of the odor mixes
    mixlst
  })
  res.combo <- flattenLists(res.combo,level = 2) #flatten it out
  res.combo
}

#filters the vector combinatins so that if a combination is used once, it will not be used again. e.g., If there 
#are three vectors, 1, 2, and 3. If 1 and 2 are combined then 2 and 3 cannot be combined, but 2 and 1 is possible
filterVectorCombos <- function(veclst,sep=',',op=1){
  nameslst <- names(veclst)
  res <- lapply(nameslst,function(i){
    tmp <- strsplit(i,split = sep)[[1]]
    as.numeric(tmp[1:(length(tmp)/2)])
  })
  res
}

#given two intervals it will either tell you if they overlap, or how much they overlap
#result: 
#inputop: input, e.g., int1 is of the form 1 = c(x1,x2) or 2 = c(mn,sem) so it is c(mn-sem,mn+sem)
#inter1 is the target overlap used for normalization
#op: 1- give relative overlap, 2 - relative difference of the means mn1-mn2/(mn1)
#3: give the actual interval of the overlap
intervalOverlap<-function(interval1,interval2,inputop=1,op=1){
  #if only one element make it an interval, and take out NAs
  inter1 <- cleanInterval(interval1,op = inputop); inter2 <- cleanInterval(interval2,op = inputop)
  int1 <- convertMeanWidthInterval(inter1[1],inter1[2],op = inputop)
  if(op==3) int1m <- 1
  else int1m <- mean(int1) #mean needed later for normalization
  int2 <- convertMeanWidthInterval(inter2[1],inter2[2],op = inputop)

  if(int1[1] > int2[1]) assignVarVals(c('int1','int2'),swap(int1,int2)) #make sure the left of int1 is the smaller no 
  if(int1[2] < int2[1]) return(0) #if they are disjoint, return 0s
  #cat('\nres',int1m,';',abs(mean(int2) - mean(int1))/int1m,'1',int1,'2',int2)
  if(op==2) return(abs(mean(int2) - mean(int1))/int1m )   
  # the overlap is either the upper value of int1 or int2 - lower value of int2
  if(int1[2] < int2[2]) {
    if(op==3) res <- c(int2[1],int1[2])
    else res <- (int1[2]-int2[1])/int1m
    #cat('\nres1:',res)
  }
  else {
    if(op==3) res <- c(int2[1],int2[2])
    else res <- (int2[2]-int2[1])/int1m 
    #cat('\nres2:',res,int2[2],int2[1])
  }
  res
}

#function to make the intervals proper
#interval: the interval or aspiring interval
#op: what kind of output you want 1 = c(x1,x2) or 2 = c(mn,sem) 
cleanInterval <- function(interval,op=1){
  #if only one element make it an interval, and take out NAs
  inter <- cleanNAVec(interval)
  if(length(interval)==1) inter <- switch(op,c(inter,inter),c(inter,0)) 
  inter
}

#gets the point that is part of the most intervals.
#lstint: list of intervals
#op: 1 - get the point, 2 - get the interval
getIntervalsCommon <- function(lstinter,op=1){
  #get rid of infinity intervals if they are present
  lstinf <- sapply(lstinter,function(x) is.infinite(x[1])|is.infinite(x[2])) 
  lstint <- lstinter[which(lstinf==F)]
  #get the unique endpoints. Then, calcaulte the midpoint of all of these.
  lstpts <- sort(unique(unlist(lstint) ) )
  lst.mid <- sapply(1:(length(lstpts)-1), function(i) (lstpts[i]+lstpts[i+1])/2)
  #for each midpoijnt count how many intervals it falls under
  res <- sapply(lst.mid, function(x){
    no <- sapply(1:length(lstint), function(i){
      #to avoid the if loop. If a point is between a and b, -(x-a)(x-b)/|(x-a)(x-b)| 
      #should be -1 or 1 otherwise. subtract 1 and negate it so that you have 1 and 0  
      tst <- (x-lstint[[i]][1])*(x-lstint[[i]][2])/(abs((x-lstint[[i]][1])*(x-lstint[[i]][2]))  )
      -(tst - 1)/2
    })
    #cat('\n',x,':',sum(no))
    sum(no)
  })
  highest <- which(res==max(res))
  res <- cbind(lst.mid[highest],res[highest])
  colnames(res) <- c('gain','# intervals')
  #cat('\nres get inter',res[,1],'enmd')
  res
}


#converts c(mn,sem) to c(left extreme,right extreme)
#op=1, do nothhin, 2 - convert
convertMeanWidthInterval<-function(mn,width,op=2){
  switch(op,c(mn,width),c(mn-width,mn+width) )
}

#swaps two elems
swap<-function(elem1,elem2,op=1){
  list(elem2,elem1)
}


#given a vector and a presribed total, will pad out the vector randomly so that it sums to total
#basically, if there are some posns that are zero and vector doesnt sum to total, will assign ones to them
#tot: the total, vec = the vector
distOverVector <- function(tot,vec,op=1){
  #get the extra possn that havent been distributed
  extra <- tot - sum(vec)
  if(extra <= 0) return(vec)
  res.vec <- vec
  #cat('\nextra',extra,res.vec)
  #if there are empty spots in vec, assign extra to thos first
  zeropos <- which(res.vec==0)
  #cat('\nzero',extra,',zeropos',zeropos,res.vec)
  #have to make sure that the extra 1s accomodate all the zeropos, if not have to take from the highest
  if( length(zeropos) > 0){
    res.vec[zeropos] <- 1
    extra <- extra - length(zeropos)
    #we have to take away an equal number from other positions
    if(extra < 0){
      topposns <- which(res.vec == sort(res.vec,decreasing = T)[abs(extra)])
      res.vec[topposns] <- res.vec[topposns] - 1
      extra <- 0
    }
  }
  #cat('\ndist',extra,',zeropos',zeropos,res.vec)
  if(extra <= 0) return(res.vec) #if any spots are left
  #randomly assign them amongst the exisintg posns
  if(length(res.vec) >= extra) {
    posns <- sample(length(res.vec),extra)
  }
  else posns <- sample(length(res.vec),extra,replace = T) #more extras than positions
  for(i in seq_wrap(1,length(posns))){
    res.vec[posns[i]] <- res.vec[posns[i]] + 1
  }
  res.vec
}


#rounds all the numbers in the vector so that they sum to the total specified
roundVecToSum <- function(vec,total,op=1){
  if(round(sum(vec)) != total){
    cat('\n total does not add up',sum(vec),total)
    return(round(vec))
  }
  #logic: given a vector will sort it on the basis of the decimal digits
  #will round the top half to 1 and the bottom half to 0, for each number calculates
  #the number to add or subtract, and keeps track of a balance parameter, which is added 
  #to the middle element in the end
  len <- length(vec)
  fractions <- vec-floor(vec) #get the decimal part only 
  matsort <- cbind(fractions,1:len)
  posns.sort <- getMatSortPosns(matsort) #sort the decimals and get their index in inc. order
  addfrac <- rep(0,len)
  balance <- 0
  curpos <- len
  start <- 1
  while(start < curpos){
    #iterate through the sorted index, round top half up and bottom half down, use the sorted index
    #posns.sort to access the ith element
    #take the last posns, and bump it up to 1
    newposns <- findBalanceEnds(fractions[posns.sort[start:curpos]])
    newposns <- c(newposns[1]+start-1,newposns[2]+start-1)
    #cat('\nnewposns',newposns,':',addfrac[posns.sort[newposns[2]:curpos]],':',addfrac[posns.sort[start:newposns[1]]])
    addfrac[posns.sort[newposns[2]:curpos]] <- 1-fractions[posns.sort[newposns[2]:curpos]]
    #now get all posns starting from start that add upto being to at least this number 
    addfrac[posns.sort[start:newposns[1]]] <- -fractions[posns.sort[start:newposns[1]]]
    balance <- balance -sum(fractions[posns.sort[start:newposns[1]]]) + sum(1-fractions[posns.sort[newposns[2]:curpos]])
    start <- newposns[1] + 1
    curpos <- newposns[2] - 1 
    #cat('\nnewposns',newposns,':',addfrac[posns.sort[newposns[2]:curpos]],':',addfrac[posns.sort[start:newposns[1]]],';',start,curpos)
  }
  addfrac[posns.sort[start]] <- addfrac[posns.sort[start]] - balance #even out everything using balance
  res <- round(vec + addfrac)
  #cat('\nvec',vec,':',sum(vec),total,':',res,':',sum(res))
  res  
}

#finds the posns at the both ends so that they balance each other out 
#assumes vec is an increasing vector
findBalanceEnds <- function(vec,op=1){
  #cat('\nfindBalnce',vec)
  i <- 1
  startpos <- 1
  endpos <- length(vec)
  target <- vec[length(vec)]
  #do the starting pos
  while(i < length(vec)){
    if(sum(vec[1:i]) >= target ) {
      startpos <- i
      i <- length(vec)
    }
    i <- i + 1
  }
  i <- endpos
  target <- sum(vec[1:startpos])
  #cat('\nstartpos',startpos,target,i)
  #do the ending posn
  while(i > startpos){
    if(sum(vec[endpos:i]) >= target ) {
      endpos <- i
      i <- startpos
    }
    i <- i - 1
  }
  
  c(startpos,endpos)
}

#get all numbers from start that add up to at least tot
getVecSum <-function(vec,tot,start=1,op=1){
  len <- length(vec)
  cumsum <- 0
  for(i in seq_wrap(start,len)){
    cumsum <- cumsum + vec[i]
    if(cumsum >= tot) return(i)
  }
  len
}



#a wrapper function that gets the execution time of the function 
#f: function to call, or f(params) in which case ... is blank
#...: all the arguments of the function
timeWrap <-function(f,...){
  input <- list(...) #get the input as a slist
  #cat('input is',input,length(input),str(input))
  if(length(input) == 0) fn <- f #if argument and fn are presented as one
  else fn <- f(...) #if the are separate
  sttime <- Sys.time()
  res <- fn
  endtime <- Sys.time()
  cat('Execution time: ',endtime-sttime,'\n')
  res
}

#this installs a package in a prespecified directory
customInstall <- function(packname='',deflib=c(''),op=1){
  home <- ifelse(osinfo['sysname'] == "Windows",'C:/Users/shyam','/home/shyam')
  if (deflib == c('')) dir <- paste(home,'/extras/Rpackages',sep = '')
  else dir <- paste(home,deflib,sep = '')
  install.packages(packname,lib = dir)
  cat('\n Package ',packname,' installed in ',dir,'\n')
}


#finds the location of the fn 
findFnFile <- function(fn,op=1){
  srcfile <- attr(attr(fn,"srcref"),"srcfile") #gets the attributes then the location of the file
  fname <- srcfile$filename
  fname
}

#to get the source code for any function
getFnSource <- function(fn,op=1){
  getAnywhere(fn)
}

#this lists the variables currently residing in memory by size
getVarMem <- function(op=1){
  thing.vec <- c()
  thingsize.vec <- c()
  for (thing in ls(envir = .GlobalEnv)) {
    #message(thing) 
    #print(object.size(get(thing)), units='auto') 
    thing.vec <- c(thing.vec,thing)
    #cat(str(object.size(get(thing))))
    thingsize.vec <- c(thingsize.vec,object.size(get(thing)))
    #cat(str(object.size(get(thing))),str(thingsize.vec))
  }
  thing.df <- cbind.data.frame(thing.vec,as.numeric(thingsize.vec))
  getMatSortPosns(thing.df,col=2,decrease = T,op=2)
}

#this is just a placeholder on how to assign values to global vars inside functions
#var= the variable name to be assigned, should be a string
#val= the value to be assigned, can be anything list, nested data structure etc
#op=1, global environment, 2 - local function environment
assignGlobalVars <- function(var,val,op=1){
  target.env <- switch(op,.GlobalEnv,environment())
  assign(x=var, value = val, envir = target.env)
}


#move these two to statsfn.R

