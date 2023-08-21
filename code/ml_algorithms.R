#R functions to read, analyze, plot and detect patterns in data along with neural and computer algorithms to model complex data.    
#Copyright (C) 2023 Shyam Srinivasan 

#This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details.

#You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.


#This file contains code for implementing machine learning algorithms (not neural networks) like SVM, LDA, kNN etc.
#Files or packages to include: e1071 for SVM, kNN is written by me.

#prrepare data for the learning algorithm
#dat.df: the data in terms of a data frame, where  stimulus is along the row, and the last column is the label
#train: the percentage of data that are to be training samples
#trainop: the option for how training data are chosen, 1 - train % of every odor, 2 - trials are chosen
#randomly
#filterop: which columns to filter: 0 dont filter, 1 - filter out columns that have the same value throughout
#op:1 - just the data in the form above with the rows sorted by the odors
#2: 2 : break them into training and test samples, based on train
#3: break them into training and test data based on loocv
#loocv : specifies the number of the fold that should be chosen for testing, so all other folds for training
#normop: 0 - do no normalization, 1 - standard z-score normalization, 2- do min-max normalization
prepDataAlgo <-function(dat.df,train=80,trainop=1,filterop=1,loocv=1,normop=0,op=1){
  resp.df <- getMatSortPosns(dat.df,col = ncol(dat.df),op=3) #sorts the df according to the label col
  if(normop>0) {#normalize everything except the last column, which contains the name
    norm.df <- normalizeMat(resp.df[,-ncol(resp.df)],normop = normop+1);
    #cat('\normalized',str(resp.df),str(norm.df))
    resp.df[,-ncol(resp.df)] <- norm.df
  }
  if(op==1) return(resp.df)
  if (filterop < 2) resp.df <- filterDataCols(resp.df,op=filterop)
  #op=2, choose train % as training trials, make 2 data dfs: train and test.dfs
  #fixed errorL commented out the loocv option: liam
  res <- splitData(dat.df = resp.df,splitno = train,op = trainop)#,loocv = loocv)
  #filter again, since there might be 0's in the training df now
  #cat('\n',str(res))
  if (filterop < 2) {
    cols <- filterDataCols(res[[1]],op=2)
    if(length(cols)>0) {
      newres <- list(res[[1]][,-cols],res[[2]][,-cols]);res <- newres
      #cat('\n',str(newres))
    }
  }
  res
}

#function estimates the training data centroid, calculated by simply taking the average of the trials
#for what they did in the CAmobell paper: Glenn
#for each stimulus set: Returns the averaged or centroids
estimateCentroids <- function(trdata,op=1){
  #delete later
}

#function that generates the formula for the classifier. lhs is the classification variable name, and rhs contains
#the predictor variable names. 
#dat.df: a data frame containing the data with the predictors in the columns, with the last col being the classification result
#op=1: the formula, 2: the formula as a string
genClassFormula <- function(dat.df,op=1){
  labelnames <- colnames(dat.df) #the names of preductors and class labels
  labelclass <- labelnames[ncol(dat.df)]
  #generate the rhs of predictor variables
  labelpred <- lapply(1:(ncol(dat.df)-2),function(i) paste(labelnames[i],'+',sep = '') )
  labelpred[[length(labelpred)+1]] <- labelnames[ncol(dat.df)-1] 
  #generate the formula
  form.string <- do.call(paste,c(labelclass,'~',labelpred,sep = '') )  
  form.res <- as.formula(form.string)  
  res <- switch(op,form.res,form.string)
  res
}


#use linear classifier algorithm to learn the data and then have a classifier
#data: list(train.dat,test.dat) in terms of a data frame, where  stimulus is along the row, and the last column is the label
#op: the type of result. 1 - % correct, 2 - list(pred,true), 3 - list(results,true)
ldaClassifier <- function(dat.ls,op=1){
  # labelname <- colnames(dat.ls[[1]])[ncol(dat.ls[[1]])]
  # form.eq <- as.formula(paste(labelname,'~','.',sep = ' ') )
  form.eq <- genClassFormula(dat.ls[[1]])
  #cat('\n',str(form.eq))
  model <- lda(form.eq,data = dat.ls[[1]])
  predictions <- predict(model,dat.ls[[2]])
  prob.mat <- predictions$posterior #the probability matrix
  rownames(prob.mat) <- dat.ls[[2]][,ncol(dat.ls[[2]])] #set the label names instead of trial numbers
  #res <- list(predictions,dat.ls[[2]][,ncol(dat.ls[[2]])]) 
  res <- switch(op,mean(predictions$class==dat.ls[[2]][,ncol(dat.ls[[2]])]),list(predictions$class,dat.ls[[2]][,ncol(dat.ls[[2]])]),
                list(prob.mat,dat.ls[[2]][,ncol(dat.ls[[2]])]))
  res
}

#given a training set, will do a nearest neighbor classification on them
#dat.ls: list(training set as df, test set as df) the stimuli/examples are in rows, with the last column containing the label
#op: 1- return the winning label, 2 - the probability of the different classes
kNNModel <- function(dat.ls,k=10,op=1){
  #get the training and test sets
  train.set <- dat.ls[[1]];test.set <- dat.ls[[2]]
  lenvec <- ncol(train.set)
  labs.train <- train.set[,ncol(train.set)]
  #prepare the label vector to populate for later
  labs <- unique(levels(labs.train))
  labs.vec <- rep(0,length(labs))
  names(labs.vec) <- labs
  #cat('\n',str(labs.vec),nrow(test.set))
  dist.ls <- lapply(1:nrow(test.set),function(i){
    #compute the distance of this vector from all the training vectors
    #and sort them by closest vector first
    distvec <- getDistVecMat(vec = test.set[i,-ncol(test.set)],mat = train.set[,-ncol(train.set)],op=1) 
    dist.df <- cbind.data.frame(distvec,labs.train)
    dist.df <- getMatSortPosns(dist.df,col=1,op=3)
    #cat('\ndist',str(distvec),str(dist.df))
    
    #take the k closest labels and then get the frequency of occurence of each label
    topk.vec <- computeKFreq(dist.df = dist.df,k = k,elem.template = labs.vec)
    #topk.vec <- sort(topk.vec,decreasing = T) #sort by frequency, and then based on op, return lab or probabilities
    switch(op,names(sort(topk.vec,decreasing = T))[1],topk.vec/k)
  })
  names(dist.ls) <- test.set[,ncol(test.set)]
  #put it in a nice format, vec for op =1 , and DF for op=2.
  #had a problem as you can't have duplicate rownames, so switched to transpose of matrix.
  #dist.ls <- switch(op,unlist(dist.ls),transposeDF(convertNestedListsDF(dist.ls))) 
  dist.ls <- switch(op,unlist(dist.ls),t(as.matrix(convertNestedListsDF(dist.ls))) ) 
  dist.ls
}

#function that takes the distance vector and the sorted order of the rankings. And, for those 
#classes that have a tie in their frequency, this breaks the tie, otherwise, just returns the frequency vector for the classes
#dist.df: the sorted distance matrix, that gives distance in the first col, and the second gives the label
#elem.temp: gives the element class template, where the values are 0, and the names are the classes
computeKFreq <- function(dist.df,k=nrow(dist.df),elem.template=c(),op=1){
  if(length(elem.template)==0){#if the template is empty, populate it
    elem.temp <- rep(0,length(levels(dist.df[,2])))
    names(elem.temp) <- as.character(levels(dist.df[,2]))
  } else elem.temp <- elem.template
  #take the topk elements and get their frequnecy
  topk.elems <- as.character(dist.df[1:k,2])
  elems.freq <- table(topk.elems)
  #unique freq. values 
  uniq.vals <- unique(elems.freq)
  dist.mat <- dist.df[1:k,] #the first k entries
  #go through all the unqiue fre. vals and resolve frequences that occur more than once
  for(i in uniq.vals){
    dup.nos <- which(elems.freq == i) #get the number of times this elem occurs
    no.dup <- length(dup.nos)
    if(no.dup > 1){#duplicates, so get to work
      #for each duplicate, get the overall distance for that class label
      dist.elems <- sapply(dup.nos,function(x) {#posns of the duplicated elems. Get the distance of each
        # cat('\n',x,':',names(elems.freq),':',dist.mat[,2],as.character(dist.mat[,2]))
        elem.name <- names(elems.freq)[x]
        elem.pos <- which(as.character(dist.mat[,2]) == elem.name)
        sum(dist.mat[elem.pos,1])
      })
      # sort the classes so that the furthest one is rank one
      dist.pos <- getVecSortPosns(dist.elems,decrease = T,sortop = 1,op = 1)#the sorted posns
      #get the smallest frequency
      delta <- min(elems.freq)*0.01 #the frequnecy to be used for adjusting tie-nreaks
      #add delta to the class frequencies such that the top half of the furthest ones freq are reduced, with the furthest being the most
      #and the bottom half of the furthest, i.e., the closest have delta added to them
      if(no.dup %% 2 == 0) delta.freq <- c(seq(-(no.dup/2) * delta,-delta,delta),seq(delta,(no.dup/2) * delta,delta) ) 
      else delta.freq <- c(seq(-(no.dup/2) * delta,-delta,delta),0,seq(delta,(no.dup/2) * delta,delta) ) 
      # cat('\npos',dup.nos,dist.elems,dist.pos,'dis pos sort',dup.nos[dist.pos],';',delta,':',delta.freq)
      elems.freq[dup.nos[dist.pos]] <- elems.freq[dup.nos[dist.pos]] + delta.freq
      dummy = no.dup
    }
  }
  #poulate the occurence vector with values from topk and if labels are not present, assign 0
  topk.vec <- elem.temp
  topk.vec[names(elems.freq)] <- elems.freq
  #print(elems.freq)
  # cat('\nres',topk.vec,':',names(topk.vec))
  # if(dummy %% 2 == 1) elem.tempds[2] <- 0
  #gets all the values with duplicates  
  topk.vec
}


#computes the distance between the vector and a matrix off vector, where the vectors are along the rows
#mat: the matrix of vectors, the vectors are along the row
#vec: the vector
#op: 1 - euclidean distance
getDistVecMat <- function(vec,mat,op=1){
  res.vec <- sapply(1:nrow(mat),function(i){
    tmp <- computeDistance(vec1 = vec,vec2 = mat[i,],op=1) #euclidean distance    
  })
  names(res.vec) <- 1:nrow(mat)
  res.vec
}

#use kNN classifier algorithm to learn the data and then have a classifier
#data: list(train.dat,test.dat) in terms of a data frame, where  stimulus is along the row, and the last column is the label
#op: the type of result. 1 - % correct, 2 - list(pred,true), 3 - list(results,true)
kNNClassifier <- function(dat.ls,kclass=0,op=1){
  if(kclass==0){#determine number of classes
    k <- length(unique(c(dat.ls[[1]][,ncol(dat.ls[[1]])],dat.ls[[2]][,ncol(dat.ls[[2]])]))) #of unqiue stimuli
  } else k <- kclass
  temp <- switch(op,kNNModel(dat.ls = dat.ls,k = k,op = 1),kNNModel(dat.ls = dat.ls,k = k,op = 1),
                 kNNModel(dat.ls = dat.ls,k = k,op = 2))
  # temp <- knn(train = dat.ls[[1]][,-ncol(dat.ls[[1]])],test = dat.ls[[2]][,-ncol(dat.ls[[2]])],
  #             cl = dat.ls[[1]][,ncol(dat.ls[[1]])],k=k,prob = T)
  res <- switch(op,mean((dat.ls[[2]][,ncol(dat.ls[[2]])]==temp)),list(temp,dat.ls[[2]][,ncol(dat.ls[[2]])]),
                list(temp,dat.ls[[2]][,ncol(dat.ls[[2]])])) #determine number of accurate odors
  res
}

#use a binary logistic classifier algorithm to learn the data and then have a classifier
#data: list(train.dat,test.dat) in terms of a data frame, where  stimulus is along the row, and the last column is the label
#thresh: the threshold for deciding yes or no. Above thresh is yes
#op: the type of result. 1 - % correct, 2 - list(pred,true), 3 - list(results,true)
logitRegClassifier <- function(dat.ls,params=list(),thresh=0.5,op=1){
  form.eq <- genClassFormula(dat.ls[[1]])
  glm.fits <- glm(form.eq,data = dat.ls[[1]],family = binomial)
  #turn off warnings, because it will complain if there is collinearity or if the number of predictors > no of classes, which is true
  var.options <- options()$warn #current warning setting
  options(warn=-1)
  glm.probs <- predict(glm.fits,newdata = dat.ls[[2]],type = 'response')
  options(warn=var.options) #reset old warning setting
  #glm.probs <- setVecThresh(glm.probs,thresh = thresh,op=2)
  test.classes <- dat.ls[[2]][,ncol(dat.ls[[2]])];levels(test.classes) <- c(levels(test.classes),'wrong')
  pred.classes <- test.classes
  #cat('\nglm.probs',glm.probs,';',which(glm.probs==0),';',length(test.classes))
  pred.classes[which(glm.probs==0)] <- 'wrong' 
  res <- switch(op,mean(setVecThresh(glm.probs,thresh = thresh,op=2)),list(setVecThresh(glm.probs,thresh = thresh,op=2),
                                                                           test.classes),list(glm.probs,test.classes) ) 
  res
}


#use a multinomial logistic classifier algorithm to learn the data and then have a classifier
#based on:https://datasciencebeginners.com/2018/12/20/multinomial-logistic-regression-using-r/
#data: list(train.dat,test.dat) in terms of a data frame, where  stimulus is along the row, and the last column is the label
#thresh: the threshold for deciding yes or no. Above thresh is yes
#op: the type of result. 1 - % correct, 2 - list(pred,true), 3 - list(results,true)
multiLogitRegClassifier <- function(dat.ls,params=list(),thresh=0.5,op=1){
  form.eq <- genClassFormula(dat.ls[[1]]);train <- dat.ls[[1]];test <- dat.ls[[2]];
  #turn off warnings, because it will complain if there is collinearity or if the number of predictors > no of classes, which is true
  train[,ncol(train)] <- relevel(train[,ncol(train)],ref = as.character(train[1,ncol(train)]) ) #the first label is the reference label
  var.options <- options()$warn #current warning setting
  options(warn=-1)
  multi.fit <- multinom(form.eq,data = train,trace=F,MaxNWts=10^6)
  options(warn=var.options) #reset old warning setting
  predictions <- predict(multi.fit, newdata = test, type = "probs")
  rownames(predictions) <- test[,ncol(test)] #assign the names of the odors instead of the rownames
  pred.labels <- getPredLabels(prob.mat = predictions)
  res <- switch(op,mean(pred.labels==test[,ncol(test)]),list(pred.labels,test[,ncol(test)]),
                list(predictions,test[,ncol(test)]) ) 
  res
}

#function that gets the predicted labels from the probability matrix
#prob.mat: the probability matrix, where the colnames are the names of the labels.
getPredLabels <- function(prob.mat,op=1){
  labelnames <- colnames(prob.mat)
  noentries <- nrow(prob.mat)
  #cat('\nnoyoPL',noentries,str(prob.mat))
  #gets the higest probability entry
  prob.max <- sapply(1:noentries,function(i) max(prob.mat[i,]) )
  #now get the label for this entry
  pred.labels <- sapply(1:length(prob.max),function(i) labelnames[which(prob.mat[i,]==prob.max[i])] )
  pred.labels
}


#use SVM classifier algorithm to learn the data and then have a classifier
#data: list(train.dat,test.dat) in terms of a data frame, where  stimulus is along the row, and the last column is the label
#op: the type of result. 1 - % correct, 2 - list(pred,true), 3 - list(results,true)
svmClassifier <- function(dat.ls,params=list(),op=1){
  #res6 <- campPrepDataSig(tst,celltype = 2,op=2)
  labelname <- colnames(dat.ls[[1]])[ncol(dat.ls[[1]])]
  form.eq <- as.formula(paste(labelname,'~','.',sep = ' ') )
  model_svm <- svm(form.eq, data = dat.ls[[1]], kernel = "linear", cost = 10, scale = FALSE,probability = T)
  pred_svm <- predict(model_svm,dat.ls[[2]][,-ncol(dat.ls[[2]])],probability = T)
  #cat('\npred',pred_svm,';',dat.ls[[2]][,ncol(dat.ls[[2]])])
  if(op==3){#get the prob. mat
    prob.mat <- attributes(pred_svm)$probabilities
    rownames(prob.mat) <- dat.ls[[2]][,ncol(dat.ls[[2]])] #the rownames are the names of the correct labels
  }
  res <- switch(op,mean(pred_svm==dat.ls[[2]][,ncol(dat.ls[[2]])]),list(pred_svm,dat.ls[[2]][,ncol(dat.ls[[2]])]),
                list(prob.mat,dat.ls[[2]][,ncol(dat.ls[[2]])]))
  res
}

#classify data according to whatever algorithm you want given by op
#data: the data in terms of a data frame, where  stimulus is along the row, and the last column is the label
#train: % of data that needs to be used for training
#trainop: the option for how training data are chosen, 1 - train % of every odor, 2 - trials are chosen
#randomly
#filterop: which columns to filter: 0 dont filter, 1 - filter out columns that have the same value throughout
#op: the type of learning algorithm to be used. 1-3 - linear: lda, qda, mda, 10 - knn, 20 : binary logistic regression, 
#21 -multinomial logistic regression, 30 - SVM
#validtype: the type of validation to be done, i.e., how should test data be selected.
#1 - leave one out cross validation, 2 - k-fold cross validation
#resop: the kind of results to be returned. 1: the accuracy of reporting, 2 - list(pred_labels,actual values), 3 - list(probability matrix,actual values)
#loocv: 0 - do not do leave one out cross validation, 1 - do it by choosing test stimulation sequentially like 1,2; 3,4; ...
# 2 - choose all possible combinations of test stimuli  
#normop: 0 - do no normalization, 1 - standard z-score normalization, 2- do min-max normalization
classifyData <- function(dat.df,train=80,trainop=1,filterop=1,resop=1,validtype=1,loocv=0,normop=0,op=10){
  #determine the number of samples of each kind, based on trainop
  alllabs <- dat.df[,ncol(dat.df)]
  labs <- table(table(alllabs)) # gives the frequency of number of labels of each stimulus
  notrials <- as.numeric(names(labs)[which(labs==max(labs))] ) #gets the no trials that occur most often
  foldsize <- notrials - floor((train/100) * notrials)
  #use this to determine how the test stimuli for loocv are chosen
  #if it does not fall perfectly, use randomization on last step
  #
  #include code here for the Leave one out Validation code in its various forms
  if(loocv>0){#we have to do loocv
    if(loocv==1){
      foldlst <- permuteGroupsFn(1:notrials,foldsize,permcomb = F,orderop = 1,op=2) 
    }
    if(loocv==2){
      foldlst <- permuteGroupsFn(1:notrials,foldsize,permcomb = F,orderop = 1,op=1) 
    }  
    #cat('\nfoldlst',str(nrow(foldlst)))
    res <- list()
    #we have to run the algorithm nrow(foldlst) no of times
    for(i in seq_wrap(1,nrow(foldlst))){#iterate through all the folds
      #cat('\nfold ',i,' params',nrow(foldlst[i,]))
      #split the data into training and test sets
      split.dat <- prepDataAlgo(dat.df = dat.df,train = train,trainop = trainop,filterop = filterop,op=2,loocv = list(),normop = normop)
      #run the classification algorithm
      item <- classAlgorithm(split.dat = split.dat,train = train,trainop = trainop,filterop = filterop,resop = resop,op=op)
      #cat('\n',i,str(item))
      res <- c(res,list(item))
    }
    #average out the results
    if(resop==1) res <- mean(unlist(res)) #average of all performances
    #average of probabilities, first instance of predicted labels, list of predicted labels
    if(resop==3) res <- list(meanListDFs(lapply(res,'[[',1)),res[[1]][[2]])# don't need this for now,lapply(res,'[[',2)) 
  } else {
    #cat('\nnot loocv')
    #split the data into training and test sets
    split.dat <- prepDataAlgo(dat.df = dat.df,train = train,trainop = trainop,filterop = filterop,normop = normop,op=2)
    #run the classification algorithm
    res <- classAlgorithm(split.dat = split.dat,train = train,trainop = trainop,filterop = filterop,resop = resop,op=op)
  }
  res
}

#chooses the algorithm that is to be run
#train: % of data that needs to be used for training
#trainop: the option for how training data are chosen, 1 - train % of every odor, 2 - trials are chosen
#randomly
#filterop: which columns to filter: 0 dont filter, 1 - filter out columns that have the same value throughout
#op: the type of learning algorithm to be used. 1-3 - linear: lda, qda, mda, 10 - knn, 20 : binary logistic regression, 
#21 -multinomial logistic regression, 30 - SVM
#validtype: the type of validation to be done, i.e., how should test data be selected.
#1 - leave one out cross validation, 2 - k-fold cross validation
#resop: the kind of results to be returned. 1: the % of correct, 2 - list(pred,true), 3 - list(results,true)
classAlgorithm <- function(split.dat,train=80,trainop=1,filterop=1,resop=1,op=10){
  if(op<10){#linear classification, lda, qda or one of the other ones
    res <- ldaClassifier(dat.ls = split.dat,op=resop)  
  }
  if(op==10){#kNN classification
    res <- kNNClassifier(dat.ls = split.dat,op=resop)  
  }
  if(op >= 20 || op < 30){#logistic regression classification
    if(op==20) res <- logitRegClassifier(dat.ls = split.dat,op=resop)  
    if(op==21) res <- multiLogitRegClassifier(dat.ls = split.dat,op=resop)  
  }
  if(op==30){#SVM classification
    res <- svmClassifier(dat.ls = split.dat,op=resop)  
  }
  res
}


#function to process the results of classified data or the function classifyData
#dat.ls: the results of classifyData, resop=3
#op: 1, accuracy, 2 - false positives, 3 - false negatives, 4 - all of them
processClassResults <- function(dat.ls,op=1){
  #get the number of classes
  if(is.factor(dat.ls[[2]])) allclasses <- levels(dat.ls[[2]])
  else allclasses <- unique(dat.ls[[2]])
  #cat('\nprClRes',str(allclasses))
  no.classes <- sapply(1:length(allclasses),function(i) countElem(vec = dat.ls[[2]],elem=allclasses[i]) )
  names(no.classes) <- allclasses
  #get the labels and do a confusion matrix to get the accuracy per class. 
  #so the pred labels are all the stimuli from test trials
  pred.labels <- getPredLabels(prob.mat = dat.ls[[1]])
  conf.mat <- table(pred.labels,dat.ls[[2]])
  #cat('\n',pred.labels,dat.ls[[2]],str(conf.mat))
  if(length(unique(pred.labels))<length(allclasses)){
    miss.labels <- setdiff(allclasses,pred.labels)
    miss.rows <- matrix(rep(0,length(allclasses)*length(miss.labels)),nrow = length(miss.labels) )
    conf.mat <- rbind(conf.mat,miss.rows)
    rownames(conf.mat)[(nrow(conf.mat)-length(miss.labels)+1):nrow(conf.mat)] <- miss.labels
    #cat('\n',str(conf.mat),miss.labels)
    #print(conf.mat)
  }
  conf.mat <- conf.mat[colnames(conf.mat),] #rearrange the matrix just in case the ith col and row are not the same element
  correct.perclass <- diag(conf.mat)
  accuracy.perclass <- sapply(names(no.classes),function(i) correct.perclass[i]/no.classes[i])
  #now, do a multi-class auc as per Hand and still 2001 and Matthias Döring's excellent article:https://www.datascienceblog.net/post/machine-learning/performance-measures-multi-class-problems/
  auc.mat <- computeAUC(prob.mat = dat.ls[[1]],test.labels = dat.ls[[2]])
  auc.measure <- sum(auc.mat)/(ncol(auc.mat)*(ncol(auc.mat)-1)*(1/2))
  res <- list(no.classes,accuracy.perclass,mean(accuracy.perclass),mean(pred.labels==dat.ls[[2]]),auc.measure,auc.mat)
  res
}



#function that computes the AUC for a set of results that has a prob. matrix and a vector of real test labels
#takes two classes and the test data and computes the pairwise AUC for them as per Hand and Till 2001 and 
#https://www.datascienceblog.net/post/machine-learning/performance-measures-multi-class-problems/
#prob.mat: the probability matrix of the test results, where the rows correspond to test.labels.
#test.labels: the test labels. They are the true labels ofthe prob.mat rows
#op=1, do pairwise AUC comparisons, 2 - do one to all AUC comparisons
computeAUC <- function(prob.mat,test.labels,op=1){
  if(is.factor(test.labels)) labs <- levels(test.labels)
  else labs <- unique(test.labels)
  nolabels <- length(labs)
  res.ls <- lapply(1:nolabels,function(i) {
    res <- sapply(1:nolabels,function(j) {
      auc.ij <- 0
      if(i<j) {#we only want to do a pair once
        #cat('\n',labs[i],labs[j])
        auc.ij <- (computePairAUC(prob.mat = prob.mat,test.labels = test.labels,test.pair = c(labs[i],labs[j]))
                   + computePairAUC(prob.mat = prob.mat,test.labels = test.labels,test.pair = c(labs[j],labs[i])) )/2
      }
      auc.ij
    })
    #cat('\nresults',res)
    names(res) <- labs  
    res
  })
  #cat('\nres',str(res.ls))
  names(res.ls) <- labs
  res <- do.call(cbind.data.frame,res.ls)
  res
}


#takes two classes and the test data and computes the pairwise AUC for them as per Hand and Till 2001 and 
#https://www.datascienceblog.net/post/machine-learning/performance-measures-multi-class-problems/
#prob.mat: the probability matrix of the test results, where the rows correspond to test.labels.
#test.labels: the test labels. They are the true labels ofthe prob.mat rows
#test.pair: the label pairs for which we need the AUC. in the form (i,j)
computePairAUC <- function(prob.mat,test.labels,test.pair,op=1){
  #cat('\npair',test.pair)
  #pair[1] species the f(x) and pair[2] specifies g(x)
  elem1.entries <- which(test.labels==test.pair[1]);elem2.entries <- which(test.labels==test.pair[2])
  noelem1 <- length(elem1.entries);noelem2 <- length(elem2.entries)
  fx <- prob.mat[elem1.entries,test.pair[1]]
  px <- prob.mat[elem2.entries,test.pair[1]] 
  allprobs <- c(fx,px)
  sortedprobs <- getVecSortPosns(allprobs,sortop = 1)
  #cat('\nsorted',allprobs,sortedprobs)
  si <- sum(sortedprobs[1:noelem1])
  aij <- (si - (noelem1*(noelem1+1)/2))/(noelem1*noelem2)
  aij
}



