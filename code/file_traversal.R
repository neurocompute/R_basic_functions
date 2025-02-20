#this file contains functions to traverse directories and process appropriate files within those directories and process them.

#the first 2 traversal files will not work for the campbell fly flies. They apply the search pattern test to both directory names and file names:
#and we might want the search to be applied to only one of those. So, recalibrate.

#this script traverses this and its subdirectories performing operations in every
#subdirectory. It's recursive
#op=1, add whatever dirname is to the path, 2 - get the canonical full path from root
traversedir <- function(foldnam=".",searchstr=".*",op=1){
  #list all the files in this dir that match the search string 
  files <- list.files(path=foldnam,pattern = searchstr)
  #filetypes <- file.info(paste(name,"/",files,sep=""))["isdir"] # get the file types
  filetypes <- file.info(files)["isdir"] # get the file types
  cat('\nfiles',files)
  if (length(files)==0) return(files)  
  #process dirs and files seperately, dirs first
  dirs <- files[which(filetypes == T)]
  filesonly <- files[which(filetypes != T)]

  name <- foldnam
  fpathfiles <- paste(name,"/",filesonly,sep = '')
  #go through the directories and get the files  
  if (length(dirs)>0) {
    dpaths <- paste(name,"/",dirs,sep = '')
    res <- sapply(dpaths, traversedir,searchstr,op+1) #get files in each dir
    res1 <- unlist(res) #make the lists into vectors
    dpathfiles <- paste(name,"/",res1,sep = '') #append this file path
    return (c(dpathfiles,fpathfiles))
  }
  c(fpathfiles)
}

#this function traverses this and its subdirectories and returns all the file. 
#It's recursive
#level keeps track of the level. Top level is 1
#searcstr specifies the files and directories to search, by default all
#dirname: name of current directory, set to 1
DirTraverse.old <- function(dirname='.',searchstr='.*',level=1,op=1){
  #list all the files in this dir that match the search string 
  files <- list.files(path=dirname,pattern = searchstr)
  cat("0.in",dirname,op,":",files,length(files),"\n")
  #pathfiles <- paste(dirname,"/",files,sep = "") #filename with full path
  #process dirs and files seperately, dirs first
  filetypes <- file.info(files)["isdir"] # get the file types
  print(filetypes)
  dirs <- files[which(filetypes == T)] #test if dir
  filesonly <- files[which(filetypes != T)]
  #go through the directories and get the files  
  cat('\n',dirs,filesonly)
  if (length(dirs)==0) { #if there are directories
    res <- sapply(dirs, DirTraverse,searchstr,level+1) #get files in each dir recursively
    res1 <- unlist(res) #make the lists into vectors
    names(res1) <- NULL #dont need the names
    return (c(res1,filesonly))
  }
  c(filesonly) #no dirs, just return the files
}

#this function traverses through the current and all sub-directories recursively. In two modes
#mode: 1- the search pattern is applied to the directory names and only lists out the directories that match it
#2: the search pattern applies to files only and lists out all the files that match the pattern
#3: separate search patterns for directories and files
#searchstr: if mode is 3, this is a 2 elements list (dirsearch,filesearch)
#op: 1 - list out from this directory, 2 - list full pathnames
DirTraverse <- function(foldname='.',searchstr=c('.*','.*'),level=0,mode=1,op=1){
  #cat('In dir ',foldname,'with mode ',mode,'and search',searchstr,'\n')
  dirsearch <- switch(mode,searchstr[1],'.*',searchstr[1]) #need the swtich for mode 2
  #searches for a string 4 characters long that are not alphabets or digits 
  filesearch <- switch(mode,'^[^\\d\\w]{4}$',searchstr[2],searchstr[2]) 
  
  #list all the files in this dir that match the search string 
  files <- list.files(path=foldname,pattern = '*')
  filetypes <- file.info(paste(foldname,'/',files,sep = ''))["isdir"] # get the file types
  if (dim(filetypes)[1]==0) return(c('NA')) #if no files, return NA

  #process dirs and files, and apply filters separately
  dirs <- files[which(filetypes == T)] #test if dir
  filesonly <- files[which(filetypes != T)]
  dirfilter <- grepl(pattern = dirsearch,x = dirs,ignore.case = T)
  filefilter <- grepl(pattern = filesearch,x = filesonly,ignore.case = T)
  filespath <- switch(op,paste(foldname,filesonly,sep = '/'),filesonly)
  filespath <- filespath[filefilter] #get rid of empty filenames

  if (length(dirs) > 0) { #if there are directories
    dirs <- paste(foldname,'/',dirs,sep = '') #add the directory name
    res <- sapply(dirs, DirTraverse,searchstr,level+1,mode,op) #get files in each dir recursively
    res1 <- unlist(res) #make the lists into vectors
    names(res1) <- NULL #dont need the names
    #the results are now dirs and files from this directory as well as its sub-directories
    res <- switch(mode,dirs[dirfilter],filespath,c(dirs[dirfilter],filespath)) 
    return(c(res,res1))#return (c(res1,filesonly[filefilter]))
  }
  switch(mode,dirs[dirfilter],filespath,c(dirs[dirfilter],filespath)) 
}


#this function traverses this and its subdirectories and returns all the file. 
#It's recursive
#level keeps track of the level. Top level is 1
#dirname: name of current directory, set to 1
#op specifies the operation to be performed 
#5 - any shell command you would like on the files specified 
#by searchstr, command- species the command
DirProcess <- function(dirname=".",searchstr=".*",command='',level=1,op=1){
  #list all the files in this dir that match the search string 
  files <- list.files(path=dirname)
  pathfiles <- paste(dirname,"/",files,sep = "") #filename with full path
  #process dirs and files seperately, dirs first
  filetypes <- file.info(pathfiles)["isdir"] # get the file types
  dirs <- pathfiles[which(filetypes == T)] #test if dir
  filesonly <- pathfiles[which(filetypes != T)]
  #go through the directories and get the files  
  if (length(dirs)>0) { #if there are directories
    #get files in each dir recursively
    if (level < 4){
      cat('\nlevel',level)
      res <- sapply(dirs, DirProcess,searchstr=searchstr,command=command,level=level+1,op=op) 
    }  
    cat('\nlevel',level)
  }
  #if any files are left, process those, and also add the dirname to the searchstr
  #for option 4
  if (length(filesonly) != 0) ProcessFile(filesonly,searchstr,command = command,oper=op)
  x=NULL
}

#does the specified operation on the set of files
ProcessFile <- function(filenames,searchstr,oper,command=''){
  #cat('filenames',filenames)
  if(oper == 1) { #ls -al on the file
    #prepare the system command. The filename should be in \"file\"
    lscmd <- paste("ls -al ","\"",filenames,"\"",sep = '')
    procfiles <- grep(searchstr,lscmd,ignore.case = T,value = T)
    #cat('jere',lscmd,searchstr)
    sapply(procfiles, system)
  }
  if(oper == 2) { #calling the trajectory making command
    procfiles <- grep(searchstr,filenames,ignore.case = T,value = T)
    subpat <- sub(searchstr[1],"\\1",procfiles) #gets the string between cam and traj
    #now, for both files check if there is bg file
    res <- sapply(subpat, function(x){ #check if subpat.*bg is present
      imgfile <- grep(paste(x,"-traj",sep = ""),filenames)
      bg <- grep(paste(x,".*bg",sep = ""),filenames)
      if(length(bg)) c(imgfile,bg) #if the background file exists
      })
    print(filenames[unlist(res)]) #
    #now, that you have both files.
  }
  if (oper==3) { #gets all the trajectory files and puts them in a different directory
    #seems empty for now, rewrite if needed
    #print(filenames)
    procfiles <- grep(searchstr[1],filenames,ignore.case = T,value = T)
    print(procfiles)
  }
  if (oper == 4){#combine all trajectory files into one file
    #cat('here\n')
    #this gets all the file names and their directory paths as a matrix
    #every column is the directtory path and filename
    filepaths <- sapply(c(1:length(filenames)), function(i,names){
      pathname <- getFilePath(names[i])
      fname <- tail(pathname,1)
      dname <- paste(head(pathname,-1),collapse = '/') #joins vectors into a string
      c(dname,fname)
    },filenames)
    #get the trajectory files and join them
    res <- jointrajfiles(filepaths[1,1],op=1)
    resname <- extractFlyExpType(filepaths[1,1])
    resname <- paste(filepaths[1,1],'/',resname,'-joinedtraj.txt',sep = '')
    #the things to do is exract the parent directory's name if it is titrate and add it
    #to the filename
    #now write this data frame to a file given by 
    WriteData(res,resname)
    #cat(resname,' written to file \n')
  }
  if(oper == 5) { #any shell command on the file, for now delete
    #prepare the system command. The filename should be in \"file\"
    lscmd <- paste(command,"\"",filenames,"\"",sep = '')
    procfiles <- grep(searchstr,lscmd,ignore.case = T,value = T)
    #cat('\nhere',str(procfiles),procfiles)
    sapply(procfiles, system)
  }
  
}

#processes the dirname to look for the following three phrases
#we need to look for three phrases in the dirname, 2.x, light, and or47 or wt
extractFlyExpType <-function(dirname,op=1){
  #first get the date, the 1st pattern with numbers and dots
  #pat2.x <- grep(".*2.([0-9]*).*",dirname,value = T)
  pat2.x <- sub(".*([0-9]+[.]+[0-9,.]*).*",'\\1',dirname)
  #now, to get the type of ly or47 or wt
  or47 <- grep("or47",dirname,ignore.case = T)
  wt <- grep("wt|berlin",dirname,ignore.case = T)
  titrate <- grep("titrat",dirname,ignore.case = T)
  #generate code for fly type, isvectorzero takes care of empty vectors
  flytype <- 2*isvectorzero(or47)+ isvectorzero(wt) + isvectorzero(titrate)
  #the things to do is exract the parent directory's name if it is titrate and add it
  #to the filename
  if (flytype==3) {
    titstr <- substr(dirname,(nchar(dirname)-4),nchar(dirname))
    titstr <- getLastPattern(dirname,'/')
    titstr <- paste('titrate_',titstr,sep = '')
  }
  fly <- switch(flytype,'wt','or47b',titstr)
  #see if it is light or without light
  light <- grep("light",dirname,ignore.case = T)
  light <- switch(isvectorzero(light)+1,"dark","light")
  filename <- paste(pat2.x,fly,light,sep = "")
}

#given a full length pathname, will give the name of the file and the directory path
getFilePath <-function(fullname,sep='/',op=1){
  #split by seperator and turn list into a vector
  splpath <- unlist(strsplit(fullname,sep))
  #cat(length(splpath),splpath)
  splpath
}

#this function traverses this and its subdirectories and returns all the file. 
#It's recursive
#level keeps track of the level. Top level is 1
#searcstr specifies the files and directories to search, by default all
#dirname: name of current directory, set to .
#params: can be used in any way
#op = 1, default, list files, 2 - copy all trajectory files specified 
#by searchstr to the directory params; process files by either 0r47light(op=3), 
#Or47dark(op=4),wtdark(op=5), wtlight(op=6),all(op=7)
DirectoryWideOps <- function(dirname=".",searchstr=".*",params=c(),level=1,op=1){
  #list all the files in this dir that match the search string 
  #cat('in dirwide',dirname,searchstr,params,level,'\n')
  files <- list.files(path=dirname)
  pathfiles <- paste(dirname,"/",files,sep = "") #filename with full path
  #process dirs and files seperately, dirs first
  filetypes <- file.info(pathfiles)["isdir"] # get the file types
  dirs <- pathfiles[which(filetypes == T)] #test if dir
  filesonly <- pathfiles[which(filetypes != T)]
  res <- NULL #initialize results
  #go through the directories and get the files  
  if (length(dirs)>0) { #if there are directories
    #get files in each dir recursively
    res <- lapply(dirs, DirectoryWideOps,searchstr=searchstr,params=params,
                  level=level+1,op=op) 
    #cat('results from subdirs ',str(res))
    res <- convertNestedListsDF(res) #convert to a data frame
  }
  #if any files are left, process those, and also add the dirname to the searchstr
  if (length(filesonly) != 0) res1 <- DirectoryOps(filesonly,searchstr,
                                                   params=params,oper=op)
  else res1 <- c() #else set to null, so that it is not added to results
  if (length(res1) > 0) {
    if ( length(res) == 0 ) res <- convertNestedListsDF(res1)
    else  res <- rbind(res,convertNestedListsDF(res1))
  }
  res
}

#does the specified operation on the set of files
#op = 1, default, list files, 2 - copy all trajectory files specified 
#by searchstr to the directory params; process files by either 0r47light(op=3), 
#Or47dark(op=4),wtdark(op=5), wtlight(op=6),all(op=7)
# for op=3 to 7, the operation is specified by params
DirectoryOps <- function(filenames,searchstr,params,oper){
  #cat('filenames',filenames)
  if(oper == 1) { #ls -al on the file
    #prepare the system command. The filename should be in \"file\"
    lscmd <- paste("ls -al ","\"",filenames,"\"",sep = '')
    procfiles <- grep(searchstr,lscmd,ignore.case = T,value = T)
    sapply(procfiles, shell)
  }
  #to fix: copying from sub-sub directories like the titrated ones is not done properly
  if (oper == 2) { #gets all the trajectory files and puts them in a different directory
    cat('option 2 ',searchstr,filenames,'\n')
    procfiles <- grep(searchstr,filenames,ignore.case = T,value = T)
    #if the params, i.e the taret dir is part of the filename then ignore this directory
    if (length(grep(params,filenames,ignore.case = T)) > 0) return()
    cpcmd <- paste("cp ","\"",procfiles,"\" ","\"",params,"\"",sep = '')
    cat('command',cpcmd,'\n')
    sapply(cpcmd,shell)
    T
  }
  #for operatiotns 3-7, searchstr[1] specifies fly type, searchstr[2] species condn
  #params specifies the operation to be performed and the parameters for the function
  if (oper == 3){#process all the or47bdark joined traj files, operation specified by params
    #first the type of file
    #cat('in option 3','\n')
    param <- unlist(params)
    procfiles <- searchstring(filenames,c('joinedtraj',searchstr))
    cat('in op 3',procfiles,'\n')
    #print(params[c(1:length(params))])
    if (length(procfiles) > 0 ) {
      res <- lapply(procfiles, DoOperOnFile,params)
      res <- do.call("rbind",res) #merges the data frames
    }
    else res <- c()
    return(c(res))
  }
}

#this function performs the required operation on the file filename and returns the 
#result. 
#filename: of the file containing the data.
#params is a list: the first element is the fucnction, and the rest of the list are 
#the function's argument list
DoOperOnFile <- function(filename,params){
  #cat('dooper',filename,'\n')
  fnparams <- params #the function argument list
  resdata <- ReadData(filename) #read in the data
  fnparams[[1]] <- resdata #the first agrgument is the data
  func <- params[[1]] # the function to be executed
  #cat(str(fnparams),'\n')
  res <- do.call(func,fnparams) #now, call the function with the arguments list
  data.frame(filename,res,stringsAsFactors = F) # return a dataframe of results
}

#given a list of files, returns a list which includes the dir name in the file
#input:listelem: an individual element of the list
listfullfile <- function(listelem,op=1){
  #adds the name of the directory to the filename using paste
  sapply(listelem,function(x,dir) {
    paste(dir,"/",x,sep = "")
  },names(listelem))
}

#will return a list, with each list containing all the files that are present in that directory
#filesvec: list of files with the full path
#op=1
getFilesSameDir <- function(filesvec,op=1){
  #go through the vctor of strings aand get the directory names
  dirs <- sapply(filesvec, function(x){
    res <- gregexpr('/',x) 
    index <- res[[1]][length(res[[1]])] - 1 #gets the string before the last /: that's the dir
    substr(x,1,index) #return this substring
  })
  #now group all those that belong to the same directory
  dirs <- unique(dirs) 
  dirs.lst <- createLists(length(dirs))
  names(dirs.lst) <- dirs
  dirs.lst <- lapply(dirs,function(x,dirs){
    #goes through the directories and gets all the files in this directory.
    tmp <- gregexpr(x,filesvec)
    posns <- which(tmp==1) #it is the right directory if it starts from posn 1
    filesvec[posns]
  })
  names(dirs.lst) <- dirs
  dirs.lst
}

# Function to get files from a directory with optional type filtering
# type: 1 - csv, 2 - txt, 3 - swc
# op: 1 - full name with parent directory, 2 - just the file name
getDirFiles <- function(directory, type = NULL,op=1) {
  # List all files in the directory
  all.files <- list.files(path = directory, full.names = TRUE)
  
  # Filter files based on the specified type if provided
  if (!is.null(type)) {
    # Define patterns based on type
    pattern <- switch(type,
                      `1` = "\\.csv$",  # Type 1 for .csv files
                      `2` = "\\.txt$",  # Type 2 for .txt files
                      `3` = "\\.swc$",  # Type 3 for .swc files
                      NULL)             # If type is not recognized, don't filter
    
    # If a pattern is specified, filter files using pattern matching
    if (!is.null(pattern)) {
      all.files <- all.files[grepl(pattern, all.files, ignore.case = TRUE)]
    }
  }
  all.files <- switch(op,all.files,basename(all.files))
  all.files
}


#generic improvement on the previous function
#built on top of DriTraverse, checks for a condition. if fulfilled does an operation in that directory. Should be run from root level of the data dir
#this particular directory traverse operation gets useful discrimination data from all directories
#if it is a file search, does the operation in that directory. 
#if it is a directory search, does the opertation inside the directory
#this function traverses through the current and all sub-directories recursively. In two modes
#mode: 1- the search pattern is applied to the directory names and only lists out the directories that match it
#2: the search pattern applies to files only and lists out all the files that match the pattern
#3: separate search patterns for directories and files
#searchstr: if mode is 3, this is a 2 elements list (dirsearch,filesearch)
#targetfn: name of the target function to call
#targetpars: the parameters for this function
#op: 1 - list out from this directory, 2 - list full pathnames
campAllDirDiscScoresGen <- function(foldname='.',searchstr=c('.*','.*'),level=0,mode=1,targetfn=c(),targetpars=list(),op=1){
  res <- NULL #initialize the return
  #cat('In dir ',foldname,'with mode ',mode,'and search',searchstr,'\n')
  dirsearch <- switch(mode,searchstr[1],'.*',searchstr[1]) #need the swtich for mode 2
  #searches for a string 4 characters long that are not alphabets or digits 
  filesearch <- switch(mode,'^[^\\d\\w]{4}$',searchstr[2],searchstr[2]) 
  
  #list all the files in this dir that match the search string 
  files <- list.files(path=foldname,pattern = '*')
  filetypes <- file.info(paste(foldname,'/',files,sep = ''))["isdir"] # get the file types
  if (dim(filetypes)[1]==0) return(c()) #if no files, return NA
  
  #process dirs and files, and apply filters separately
  dirs <- files[which(filetypes == T)] #test if dir
  filesonly <- files[which(filetypes != T)]
  dirfilter <- grepl(pattern = dirsearch,x = dirs,ignore.case = T)
  filefilter <- grepl(pattern = filesearch,x = filesonly,ignore.case = T)
  filespath <- switch(op,paste(foldname,filesonly,sep = '/'),filesonly)
  filespath <- filespath[filefilter] #get rid of empty filenames
  #cat('In',foldname,length(filespath),dirs,filespath,'\n')
  if (length(filespath)==0) cat('\nNo files that match the pattern in ',foldname)
  else {#setworking directory, execute and then set it back
    currentdir <- getwd()
    setwd(foldname)
    cat('\nIn foldname ',foldname,' and current dir ',currentdir,getwd(),names(targetpars),'\n')
    #print(campGetTrialDetails())
    #res <- c(campGetTrialDetails(),'elem')
    res <- c(do.call(targetfn,targetpars),'elem')
    #cat(str(res))
    setwd(currentdir)
  }
  #else do.call(targetfn,list(foldname=foldname))
  
  if (length(dirs) > 0) { #if there are directories
    res.empty <- c()
    cat('\n dirs are ',dirs)
    #condition 1: the subdirectory is empty and the return is empty
    if(identical(dirs,'NA') ) return(NULL)# nothing so just return null
    dirs <- paste(foldname,'/',dirs,sep = '') #add the directory name
    res <- lapply(dirs, campAllDirDiscScoresGen,searchstr,level+1,mode,targetfn,targetpars,op) #get files in each dir recursively
    names(res) <- dirs #preserive names
    #condition 2: valid and invalid subdirectories together. Take out the invalid ones(ones with 0 elements) 
    cat('\nfoldname: ',foldname)
    res <- removeListEmpty(res)
    #if(length(res)==1) {#if there is only one element, then we again need to bring the element up a level
    #  res <- res[[1]] 
    #  #cat('\nreturning one element ',length(res))
    #  return(res)
    #}
    #condition 3: if there are elements in this directory, and sub-directories with multiple elements
    #bring them all to the same level
    if(length(res) > 0){#multiple elements, check if they are elemental
      #cat('\nchecking elem list in ',foldname,'\n',str(res))
      elem.lst <- sapply(res, function(x) campIsElem(x))
      cat('\nelem lst is ',elem.lst)
      nonelem.lst <- which(elem.lst==F) #the non elements
      tmp.lst <- res[which(elem.lst==T)]
      if(length(nonelem.lst)>0) for (i in nonelem.lst) {
        tmp.lst <- c(tmp.lst,res[[i]])
      }
      #cat('\nnonelem.lst:',nonelem.lst,str(tmp.lst))
      res <- tmp.lst
    }
    #check here if any of these are empty, if they are delete them from the list
    cat('\nin ',foldname,' looking at ',dirs,' with dir no ',length(dirs),' and dir returning res length',length(res))
    return(c(res))#return (c(res1,filesonly[filefilter]))
    #str(c(tmp1[c(1,2)],tmp1[[3]][c(1,2)]))
  }
  #do we need to return this stuff?
  switch(mode,dirs[dirfilter],filespath,c(dirs[dirfilter],filespath)) 
  cat('\nElem returning',length(res),'\n')
  res
}


#given a list, checks if this an elemental element of the list. Basic check is that 
#the word 'elem' is contained within it. A bit kludgy. Fix later. Liam
dirIsElem <- function(elem,op=1){
  #if(is.list(elem)) return(F) #if it is a list, it ain't elemental
  #cat('\ndirIsElem')
  if(length(elem)>0) {
    res <- sapply(elem, function(x){
      identical(x,'elem')
    })
    #if(length(elem)==1) res <- identical(elem,)
    if (length(which(res==T))>0) return(T)
    else return(F)
  }
  F
}




#runs the target function in all the subdirectories that 1. either match a criterion or 2. just all subdirs
#built on top of DriTraverse, checks for a condition. if fulfilled does an operation in that directory. 
#Should be run from root level of the data dir
#if it is a file search, does the operation in that directory. 
#if it is a directory search, does the opertation inside the directory
#this function traverses through the current and all sub-directories recursively. In two modes
#mode: 1- the search pattern is applied to the directory names and only lists out the directories that match it
#2: the search pattern applies to files only and lists out all the files that match the pattern
#3: separate search patterns for directories and files
#searchstr: if mode is 3, this is a 2 elements list (dirsearch,filesearch)
#targetfn: name of the target function to call
#targetpars: the parameters for this function
#op: 1 - list out from this directory, 2 - list full pathnames
#runop: run options 1 - run the command on the files found, 2 - run a general command in the directory, nothing
#to return
DirRunFunction <- function(foldname='.',searchstr=c('.*','.*'),level=0,mode=1,targetfn=c(),
                           targetpars=list(),runop=1,op=1){
  res <- NULL #initialize the return
  #cat('In dir ',foldname,'with mode ',mode,'and search',searchstr,'\n')
  dirsearch <- switch(mode,searchstr[1],'.*',searchstr[1]) #need the swtich for mode 2
  #searches for a string 4 characters long that are not alphabets or digits 
  filesearch <- switch(mode,'^[^\\d\\w]{4}$',searchstr[2],searchstr[2]) 
  #list all the files in this dir that match the search string 
  files <- list.files(path=foldname,pattern = '*')
  filetypes <- file.info(paste(foldname,'/',files,sep = ''))["isdir"] # get the file types
  if (dim(filetypes)[1]==0) return(c()) #if no files, return NA

  cat('\nsearch patterns',dirsearch,'f',filesearch)
  #process dirs and files, and apply filters separately
  dirs <- files[which(filetypes == T)] #test if dir
  filesonly <- files[which(filetypes != T)]
  dirfilter <- grepl(pattern = dirsearch,x = dirs,ignore.case = T)
  filefilter <- grepl(pattern = filesearch,x = filesonly,ignore.case = T)
  filespath <- switch(op,paste(foldname,filesonly,sep = '/'),filesonly)
  filespath <- filespath[filefilter] #get rid of empty filenames
  cat('file search',filespath,'filefilter',filefilter,'files',filesonly)
  if (length(filespath)==0) cat('\nNo files that match the pattern in ',foldname)
  else {#setworking directory, execute and then set it back
    res <- runFunction(foldname,filespath,targetfn,targetpars,op=runop)
  }

  if (length(dirs) > 0) { #if there are directories
    res.empty <- c()
    #condition 1: the subdirectory is empty and the return is empty
    if(identical(dirs,'NA') ) return(NULL)# nothing so just return null
    dirs <- paste(foldname,'/',dirs,sep = '') #add the directory name
    res <- lapply(dirs, DirRunFunction,searchstr,level+1,mode,targetfn,targetpars,runop=runop,op=op) #get files in each dir recursively
    names(res) <- dirs #preserve names
    #condition 2: valid and invalid subdirectories together. Take out the invalid ones(ones with 0 elements) 
    res <- removeListEmpty(res)
    #condition 3: if there are elements in this directory, and sub-directories with multiple elements
    #bring them all to the same level
    if(length(res) > 0){#multiple elements, check if they are elemental
      elem.lst <- sapply(res, function(x) dirIsElem(x))
      #cat('\nelem lst is ',elem.lst)
      nonelem.lst <- which(elem.lst==F) #the non elements
      tmp.lst <- res[which(elem.lst==T)]
      if(length(nonelem.lst)>0) for (i in nonelem.lst) {
        tmp.lst <- c(tmp.lst,res[[i]])
      }
      #cat('\nnonelem.lst:',nonelem.lst,str(tmp.lst))
      res <- tmp.lst
    }
    #check here if any of these are empty, if they are delete them from the list
    return(c(res))#return (c(res1,filesonly[filefilter]))
  }
  #do we need to return this stuff?
  switch(mode,dirs[dirfilter],filespath,c(dirs[dirfilter],filespath)) 
  #cat('\nElem returning',length(res),'\n')
  res
}

#this runs the function target function with the target parameters and returns the result res
runFunction <- function(foldname,filespath,targetfn,targetpars,op=1){
  #logic get into the directory, set it as wd, then execture the function, get the results, 
  #set directory back and return the result
  currentdir <- getwd()
  setwd(foldname)
  cat('\nin dir',currentdir,'files:',filespath,'op=',op)
  res <- lapply(filespath, function(x){
    if (op==1) tarpars <- c(list(file=basename(x)),targetpars)
    else tarpars <- targetpars
    anim.sum <- c(list(do.call(targetfn,tarpars)))
    names(anim.sum) <- 'summary'
    anim.sum
  })
  res <- c(res,'elem')
  direct.name <- getFileAndDir(filespath)$directory_name
  names(res) <- c(direct.name,'elem')
  setwd(currentdir)
  res  
}



checkDir <- function(target,dirs,op=1){
  
}

#this function 
collapsedirfiles <-function(filelst,op=1){
  #print(names(filelst))
  flstnames <- names(filelst)
  res <- sapply(flstnames, function(x) {listfullfile(filelst[x])})
  unlist(res)
}

#this containts code to execute the main commands
maincommands <-function(op=1){
  #to execute any command on all files that satisfy the searchstr coomand, an eg.g below
  DirProcess(dirname = '.',searchstr = '-joinedtraj.tx',op = 5,command = "rm ")
  #create all the joined trajectory files
  DirProcess(dirname = '.',searchstr = '',op = 4) #from the root/parent dir
  #to copy the concatenated trajectory files from sub-sub-directories to ./traj
  DirectoryWideOps(dirname = '.',searchstr = 'joinedtraj',params='./traj',op = 2)
  #to execute the command to go into all the sub-directories and assemble the 
  #join-trajectory files
  DirProcess('.','.*cam-(.*)-traj.txt',op=4)
  #go through all the joined trajectory files and analyze those specified by the
  #the search strings in searchstr. Use the analysis function specified by params
  res <- DirectoryWideOps(dirname = '.',searchstr = c('or47b','dark'),
                          params=c(getThreshDistVec,10,3),op = 3)
  #call for getting the number of frames wherein they are within a distance of 5
  res <- DirectoryWideOps(dirname = '.',searchstr = c('or47','light'),
                          params=c(getThreshDistVec,5,op=3),op=3)
  #call for getting the correlation between the x posns when the flies are within a 
  #doistance 10
  res1 <- DirectoryWideOps(dirname = '.',searchstr = c('wt','dark'),
                           params=c(GetCorrPosnVec,10,xychoice='x',op=1),op=3)
  #get the distance between the flies m frames after wing extension
  res2 <- DirectoryWideOps(dirname = '.',searchstr = c('or47','dark'),
                           params=c(getDistanceBwFlies,threshsd=1.5,threshno=15,
                                    m=150,dirn=1,op=1),op=3)
  #get the correlation between flies m frames after wing extension
  res1 <- DirectoryWideOps(dirname = '.',searchstr = c('wt','dark'),
                           params=c(GetCorrAroundWingExt,threshsd=1.5,threshno=15,
                                    m=150,xychoice='y',dirn=1,op=1),op=3)
  #get the distane between flies m fframes after wing extension when the fly is 
  #stationary, chasing, and non-chasing and graphing after
  reschase47.1 <- DirectoryWideOps(dirname = '.',searchstr = c('or47','dark'),
                                   params=c(getDistanceEvents,threshsd=1.5,threshno=15,
                                            m=150,dirn=1,op=3),op=3)
  reschase47.2 <- DirectoryWideOps(dirname = '.',searchstr = c('or47','dark'),
                                   params=c(getDistanceEvents,threshsd=1.5,threshno=15,
                                            m=150,dirn=1,op=2),op=3)
  reschase47.3 <- DirectoryWideOps(dirname = '.',searchstr = c('or47','dark'),
                                   params=c(getDistanceEvents,threshsd=1.5,threshno=15,
                                            m=150,dirn=1,op=3),op=3)
  pl47 <- getDFColsAsLists(reschase47.1,reschase47.2,reschase47.3,cnames=c('stat','chase',
                                                                    'non-chase'))
  #plotting stripchart with all the data points
  fstripchartvecs(pl47,rndfact = 10,tickno = 10,markersize = .25)
  #plotting stripchart with just the means of the data points
  fchartvecs(pl47,rndfact = 10,tickno = 5,fixy = c(5,15),markersize = .5)
}
