#this file contains the startup scripts

#Sys.setenv(HOME="/home/shyam/program/R")
#Sys.setenv(R_LIBS_USER="/home/shyam/program/R/x86_64-pc-linux-gnu-library/3.2")

startupfns <- function(){
  currentdir <- getwd()
  osinfo <- Sys.info()
  #if (osinfo['sysname'] == "Windows") home <- 'C:/Users/shyam'
  #else home <- '/home/shyam'
  home <- ifelse(osinfo['sysname'] == "Windows",'C:/Users/shyam','/home/shyam')
  #putin a condition for flyball, feel bad hardcoding this
  home <- ifelse(osinfo['nodename'] == "FLYBALL",'C:/Users/labadmin/shyam',home)
  researchdir <- ifelse(osinfo['nodename'] == "FLYBALL",'/computational/R functions/code',
                        '/research/salk/computational/R functions/code')
  cat('\nos:',osinfo['sysname'],' home ',home)
  #setwd(paste(home,'/research/salk/computational/R functions',sep = ''))
  setwd(paste(home,researchdir,sep = ''))
  cat('home dir is ',home,' and research dir is ',researchdir,'\n')
  source("data_fns.R")
  source("data_fns.R")
  source("math_fns.R")
  #source("fly_anal1.R")
  source("graphs.R")
  source("data_distribution_fits.R")
  source("file_traversal.R")
  source("misc_fns.R")
  source("stats_fns.R")
  source("circuits.R")
  #current dir for now is measurements
  currentdir <- paste(home,'extras/sync/measurements',sep = '/')
  setwd(currentdir)
  cat('\n Current directory is ',currentdir)
}

campStartupFns<- function(){
  currentdir <- getwd()
  osinfo <- Sys.info()
  #if (osinfo['sysname'] == "Windows") home <- 'C:/Users/shyam'
  #else home <- '/home/shyam'
  home <- ifelse(osinfo['sysname'] == "Windows",'C:/Users/shyam','/home/shyam')
  setwd(paste(home,'/extras/sync/measurements/olfaction/campbell_analysis/',sep = ''))
  source('campbell_data_analysis.R')
  source('mb_analysis.R')
  
} 

pcxStartupFns <- function(){
  currentdir <- getwd()
  osinfo <- Sys.info()
  #if (osinfo['sysname'] == "Windows") home <- 'C:/Users/shyam'
  #else home <- '/home/shyam'
  home <- ifelse(osinfo['sysname'] == "Windows",'C:/Users/shyam','/home/shyam')
  setwd(paste(home,'/extras/sync/measurements/olfaction/mouse/',sep = ''))
  source('pcxmodel.R')
  source('pcx_correlations.R')
  source('pcxscale.R')
}

#some helpful hints snippets
helpfulhints <- function(){
  #1. Error in file(out, "wt") : cannot open the connection problem 
  #https://stackoverflow.com/questions/38086356/launching-r-help-error-in-fileout-wt-cannot-open-the-connection
  tempdir()
  # [1] "C:\Users\XYZ~1\AppData\Local\Temp\Rtmp86bEoJ\Rtxt32dcef24de2"
  dir.create(tempdir())
  tempdir();dir.create(tempdir())
  
  #2. 
  setwd('/media/shyam/common_space/research/salk/computational/R functions')
  setwd("/media/shyam/common_space/extras/sync/measurements/olfaction/variabilty learning/revisions")
  
}

#loading and saving workspace and history on infinity
save_workspace_revision_analysis_inifinity <-function(){
  save.image("C:/Users/shyam/extras/sync/measurements/olfaction/variabilty learning/revision_data_analysis/revision_analysis.RData")
  savehistory("C:/Users/shyam/extras/sync/measurements/olfaction/variabilty learning/revision_data_analysis/revision_anal_hist.Rhistory")
}

load_workspace_revision_analysis_inifinity <-function(){
  load("C:/Users/shyam/extras/sync/measurements/olfaction/variabilty learning/revision_data_analysis/revision_analysis.RData")
  loadhistory("C:/Users/shyam/extras/sync/measurements/olfaction/variabilty learning/revision_data_analysis/revision_anal_hist.Rhistory")
}

#loading and saving workspace and history on infinity
save_workspace_ika <-function(){
  save.image("~/temp/ika_workspace.RData")
  savehistory("~/temp/ika_history.Rhistory")
}

#loading and saving workspace and history on infinity
load_workspace_ika <-function(){
  load("~/temp/ika_workspace.RData")
  loadhistory("~/temp/ika_history.Rhistory")
}

#loading and saving workspace and history on infinity
load_workspace_glia_infinity <-function(){
  load("C:/Users/shyam/extras/sync/measurements/olfaction/glia/glia_density.RData")
  loadhistory("C:/Users/shyam/extras/sync/measurements/olfaction/glia/glia_history.Rhistory")
}

save_workspace_glia_infinity <- function() {
  save.image("C:/Users/shyam/extras/sync/measurements/olfaction/glia/glia_density.RData")
  savehistory("C:/Users/shyam/extras/sync/measurements/olfaction/glia/glia_history.Rhistory")
}


#loading and saving workspace and history on knuth pcx
save_workspace_knuth_pcx <-function(){
  save.image("~/temp/knuth_pcx_workspace.RData")
  savehistory("~/temp/knuth_pcx_history.Rhistory")
}

#loading and saving workspace and history on knuth pcx
load_workspace_knuth_pcx <-function(){
  load("~/temp/knuth_pcx_workspace.RData")
  loadhistory("~/temp/knuth_pcx_history.Rhistory")
}

#loading and saving workspace and history on knuth pcx
save_workspace_infinity_pcx <-function(){
  save.image("/Users/shyam/temp/infinity_pcx_workspace.RData")
  savehistory("/Users/shyam/temp/infinity_pcx_history.Rhistory")
}

#loading and saving workspace and history on knuth pcx
load_workspace_infinity_pcx <-function(){
  load("/Users/shyam/temp/infinity_pcx_workspace.RData")
  loadhistory("/Users/shyam/temp/infinity_pcx_history.Rhistory")
}

#saving the datafiles workspace file for tako/ika rstudioserver
save_workspace_ika_aug2024 <- function(){
  save.image("/data/shyam/rstudio/aug_30_ika_workspace.RData")
  savehistory("/data/shyam/rstudio/aug_30_ika_history.Rhistory")
}

#saving the datafiles workspace file for tako/ika rstudioserver
save_workspace_tako_dec2024 <- function(){
  save.image("/data/shyam/rstudio/dec2024_tako_workspace.RData")
  savehistory("/data/shyam/rstudio/dec2024_tako_history.Rhistory")
}

#clearing out unwanted data from memory commands
clearingDataCommands <- function(){
  data.vars <- ls()[!sapply(ls(), function(x) is.function(get(x)))]
  data.size.df <- data.frame(Variable = data.vars, Size = sapply(data.vars, function(x) object.size(get(x))),
                             readable = sapply(data.vars, function(x) humanReadableSize(object.size(get(x)))) )
  data.size.df <- data.size.df[order(data.size.df$Size, decreasing = TRUE), ]

  dat1 <- grep('tmp|temp|res|test|tst',data.size.df[,1])
  sum(data.size.df[dat1,2])/1024^3
  # [1] 45.73311
  sum(data.size.df[-dat1,2])/1024^3
  # [1] 0.08857115
  sum(data.size.df[,2])/1024^3
  # [1] 45.82168
  
  #save all the variables that I temporarily create such as tmp1, tmp2,....
  save(list = data.size.df[dat1,1],file = 'ika_temporary_variables_aug30_2024.RData')
  save(list = data.size.df[-dat1,1],file = 'ika_other_named_variables_aug30_2024.RData')
  #and, now you can remove them
  rm(list = data.size.df[dat1,1]) 
  
  
  #getting all data variables
  data.size.df <- getDataVarUsage()
  #clearing stuff on pascal
  transferValues('glia.res','gliarna',n=6)
  transferValues('temp','gliarna.tr',st=1,n=5)
  transferValues('tmp','gliarna.fen',st=1,n=9)
  transferValues('test','gliarna.chi',st=1,n=3)
  fish.cb <- test
  amanda.lit <- tmp
  #data to be kept for now
  dat2 <- grep('temp6|res7|res8|res9|test3|tst1|tst2|tst3|tst4|tst5|tst7|tst9',data.size.df[,1])
  #to take out
  dat1 <- grep('tmp|temp|res|test|tst',data.size.df[,1])
  rm(list = data.size.df[setdiff(dat1,dat2),1])
  
  #repeat to clean out more stuff:
  dat1 <- grep('vec|fit|circuit|wtd|or47|fashion|train|mat|relcum|osinfo|label|nosamples',data.size.df[,1])
  rm(list = data.size.df[dat1,1]) 
  
  rm(list = c('l','L','predictions','means'))
  
  
}


# Function to convert bytes to a human-readable format
# in terms of B, MB, GB ...
humanReadableSize <- function(size) {
  if (size < 1024) {
    return(paste(size, "B"))
  } else if (size < 1024^2) {
    return(paste(round(size / 1024, 2), "KB"))
  } else if (size < 1024^3) {
    return(paste(round(size / 1024^2, 2), "MB"))
  } else {
    return(paste(round(size / 1024^3, 2), "GB"))
  }
}

#takes as arguments the two prefixes of the variable names, as input 
#strings source and target, and moves the values from source1 to target1 and so on
#n: number of variable names source1, ... sourcen
#st: the variable number to start from
transferValues <- function(source, target,st=1, n) {
  for (i in st:n) {
    # Construct the old and new variable names
    old_var <- paste0(source, i)
    new_var <- paste0(target, i)
    
    # Check if the source variable exists
    if (exists(old_var)) {
      # Assign the value of the old variable to the new variable
      assign(new_var, get(old_var), envir = .GlobalEnv)
    } else {
      warning(paste("Variable", old_var, "does not exist."))
    }
  }
}

#function gets all the data variables and their usage
getDataVarUsage <- function(op=1){
  data.vars <- ls(envir = .GlobalEnv)[!sapply(ls(envir = .GlobalEnv), function(x) is.function(get(x)))]
  data.size.df <- data.frame(Variable = data.vars, Size = sapply(data.vars, function(x) object.size(get(x))),
                             readable = sapply(data.vars, function(x) humanReadableSize(object.size(get(x)))) )
  data.size.df <- data.size.df[order(data.size.df$Size, decreasing = TRUE), ]
  data.size.df
}


#fix for file(out,'wt') error issue
fixTempDir <- function(){
  dir.create(tempdir())
}

logisticsOtherCommand <- function(){
  
}