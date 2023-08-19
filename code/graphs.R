#R functions to read, analyze, plot and detect patterns in data along with neural and computer algorithms to model complex data.    

#Copyright (C) 2023 Shyam Srinivasan 

#This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details.

#You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.



#this file contains functions to plot and to analyze data for plotting such as fitting to 
#distributions

#some misc information
#  #mgp is axis location labels, tick mark labels, and the third is the tick marks
#colorbrewer is an excellent site for figuring out the best sequence of colors for sequential, divergent series
#https://colorbrewer2.org/#type=sequential&scheme=PuBu&n=8
#rules on how to choose colors: 
#https://chartio.com/learn/charts/how-to-choose-colors-data-visualization/#colorbrewer
#colors on both sides of the zero axis:
#https://chartio.com/learn/charts/stacked-bar-chart-complete-guide/

#constants for colors and symbols that can be used across function
const.plsymbols <- c(19,15,c(1,0,5,2,6),c(19,15,18,17,25)) #set the symbols for plotting
#set the colors for plotting
const.plcolors <- c('black','red','blue','purple','brown','blueviolet','cadetblue','green','magenta','yellow',
              'orange','darkcyan','darkgoldenrod1','darkorchid4','violet','orchid',
              'palevioletred','plum','slateblue','gray') 
#set the grey scale colors
#const.plgreycolors <- c("#666666","#727272","#7F7F7F","#8C8C8C","#999999","#A5A5A5","#B2B2B2","#BFBFBF","#CCCCCC","#D8D8D8","#E5E5E5")
const.plgreycolors <- c("#333333","#666666","#999999","#BFBFBF","#D8D8D8","#EAEAEA")
const.plbluecols <- c('#fff7fb','#ece7f2','#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#045a8d','#023858')


#functions to darken the color. from jfortin1: https://gist.github.com/Jfortin1/72ef064469d1703c6b30, but modified
#so that shading applies to pure colors like red, green or black. Algorithm below in lighten
#color: in the form of a single number like "#501078" or 'purple'
#factor: by which the color should be darkened or lighteneddarken <- function(color, factor=1.4){
darken <- function(color, factor=1.4){
  colrange <- 255 #goes from 0 to 255
  col <- col2rgb(color) #gives a matrix 1*3 rows and 1*nocolors no of cols
  #algo: fix a scale of 0 to 1 from 0 to 255. Then add factor to lighten or subtract factor to darken. cutoff at 0 or 255 
  col <- sapply(1:nrow(col),function(i) {
    tmp <- col[i,] - (colrange*(factor/100))
    tmp[tmp<0] <- 0
    tmp
  })
  #col <- rgb(col, maxColorValue=255)
  #col <- do.call(rgb,c(as.list(col), maxColorValue=255) )
  col <- sapply(1:nrow(col),function(x) do.call(rgb,c(as.list(col[x,]), maxColorValue=255) ) ) 
  col
}

#functions to lighten the color
#color: in the form of a single number like "#501078" or 'purple'
#factor: by which the color should be darkened or lightened
lighten <- function(color, factor=40){
  colrange <- 255 #goes from 0 to 255
  col <- col2rgb(color) #returns a matrix., #gives a matrix 1*3 rows and 1*nocolors no of cols
  #algo: fix a scale of 0 to 1 from 0 to 255. Then add factor to lighten or subtract factor to darken. cutoff at 0 or 255 
  newcol <- sapply(1:nrow(col),function(i) {
    tmp <- col[i,] + (colrange*(factor/100))
    #cat('\n',col[i,],':',tmp)
    tmp[tmp>255] <- 255
    tmp
  })
  #cat('\nnoyo0',str(newcol))
  if(length(color) == 1) newcol <- rbind.data.frame(newcol)
  colnames(newcol) <- c('red','green','blue')
  res <- sapply(1:nrow(newcol),function(x) do.call(rgb,c(as.list(newcol[x,]), maxColorValue=255) ) ) 
  #col <- sapply(1:nrow(col),function(x) do.call(rgb,c(col[x,], maxColorValue=255) ) ) 
  res
}


#fn to plot a sequence or list of numbers
#type default "l" join points
plotfn <- function(data,typel="l"){
  #number of items in this list
  size <- length(data)
  timeseq <- seq(1,size,1)
  plot(timeseq,data,typel)
}

#this fn plots the two columns against each other
plotxy <- function(data,col1,col2) {
  size <- nrow(data) #get number of data elements
  xpos <- ReadCol(data,col1,size)
  ypos <- ReadCol(data,col2,size)
  #now, plot these
  plot(xpos,ypos,type='l')
}

#a fuction to get a nice fancy plot
#input: the x vector, the y vector, xlabel, ylabel, title
fplot <- function(x,y,xlabel='x',ylabel='y',title=''){
  norm <- 1
  par(pty='s') #specify a square plotting shape
  plot(x,y,frame.plot = F,main = title, xlab = xlabel,ylab = ylabel,xlim = c(min(x),
  ceiling(max(x) * 10)/10),ylim = c(min(y),max(y)),pch=16,cex=1,cex.lab=1.5,cex.axis=1.5)
  

  #if p is higher and line are higher they move the axis and label further away
  #axis(side=2,at=yticks,labels=rep("",length(yticks)),mgp=c(3,1,1),lwd=2)
  #mtext(text=as.character(yticks),side=2,at=yticks,las=1,cex=1.2,font=2,line=1.75)
  #do the x axis labels
  #axis(side=1,at=nox,labels=rep("",length(nox)),mgp=c(3,1,.5),lwd=2)
  #mtext(text=xlabels,side=1,at=nox,las=1,cex=1.2,font=2,line=1.75)
  
}

#dont use this function anymore fploteq is the bomb
#a fuction to get a nice fancy plot
#input: the x vector, the y vector, xlabel, ylabel, title
fplotlog <- function(x,y,cols=c(2),lgscale=10,xlabel='x',ylabel='y',title='',markersize=1.75,
                     rndfact=1,ticknox=2,ticknoy=2,fixy=1,fixx=1,logs=T,plottype=c()){
  #cat(str(y),'length y',length(y),'\n')
  plotx <- x
  ploty <- y[,cols]
  allx <- unlist(plotx) # get the x and y data points
  ally <- cleanNA(unlist(ploty))
  if(logs){
    allx <- log(allx,lgscale)
    ally <- log(ally,lgscale)
    plotx <- log(plotx,lgscale)
    ploty <- log(ploty,lgscale)
  }  
  #calculate all the plot variables
  tmp <- genPlotVars(allx,ally,rndfact,ticknox,ticknoy,fixy,fixx,logs = logs)
  assignVarVals(c('roundy','yrange','yticks','roundx','xrange','xticks'),tmp)
  cat(roundy,yrange,yticks,',',roundx,xrange,xticks,'\n')
  #par(mar=c(2,5,2,4)) #adjust the outer and inner marggines
  #par(oma=c(2,4,2,4))
  #par(mgp=c(4,1,.5))
  par(font.axis = 2) #make axes bold again, Haha!
  par(pin=c(2.5,2.5),xpd=NA) #adjust size of the display, cex=1.5
  
  if((length(ploty)==1) && (length(plottype)==0)) plottyp <- 1 #only 1 column to plot
  if(length(plottype)==0) {#if no plottype specified, choose default options for 1/2 cols
    plottyp <- ifelse(length(ploty)==1,1,3)
  } 
  else plottyp <- plottype
  fplottype(plotx,ploty,factors=y[,1],cols=cols,xrange=xrange,yrange=yrange,xlabel=xlabel,
            ylabel,markersize=markersize,
            title = title,plottype = plottyp)
  
  #needed: yticks, xlabels,nox
  #if p is higher and line are higher they move the axis and label further away
  plotaxes(xticks,yticks,xlabel=xlabel,ylabel=ylabel,logs=logs,lgscale = lgscale)
}


#function that generates round, range, and ticks
#xy : vecttor of either x or y values
#rndfact, tickno, and fix are the same as fplotlog
#rndfact: the factor to which the roundxy should be rounded, e.g., 5 would mean that they should be rounded to nearest power of 5
#tickno: tells you how big the divisions b/w ticks should be. higher tickno means more distance between ticks.
#logs: log scale or linear scale. If log scale will only produce integer ticks,
#sucha as 10^1 instead of 10^1.23
#forceop: T - force it so that tickwidth=rndfact*tickno. A bit kludgy in my opinion
#op: 1- default, 2 - count from 0.i.e. insert 0 as an entry in xy
genRangeTicks <- function(xy,rndfact,tickno,fix=1,fixop=1,logs=F,maxticks=10,forceop=F,op=1){
  #cat('gRT',rndfact,'\n') #might need to cleanup the lines until roundxy
  #adjust so that in the non-log case roundxy is > 0
  roundxy <- ifelse(logs,1,(10^getpow10(min(abs(xy)[abs(xy)>0])))) #get the lowest power of 10 that is an LCM
  if((abs(min(xy)-max(xy))/roundxy) > 10) roundxy <- (max(xy)-min(xy))/10 #we dont want too many ticks
  #cat('\nround2',roundxy,'min',min(xy),'max',max(xy))
  #what we want is that roundxy should be a multiple of rndfact in some fashion. So, first get the nearest multuple of roundfact closest to roundxy
  #and, then bring it back by addding the right number of decimal places to roundxy
  roundfact <- getNearestMultiple(round(roundxy*10^-getpow10(roundxy)),rndfact,op=3)
  if(roundfact==0) roundfact <- rndfact #if it is 0, make it either one or the default rndfact
  roundxy <- roundfact*10^getpow10(roundxy)
  #cat('\nround',roundxy,rndfact,roundfact)
  # #liam,take care of -ve nos: handled now
  starttick <- ifelse(op==2,0,getNearestMultiple(min(xy),roundxy,1)) #op=2, start from 0, otherwise, closest to min(xy)
  #if forceop, then the ticks are just what is specified by tickno and rndfact
  endtick <- ifelse(forceop,getRangeMultiple(starttick,max(xy),rndfact*tickno),getRangeMultiple(starttick,max(xy),roundxy*tickno) )
  rangexy <- c(starttick,endtick)
  #cat('\ngenrangeticks',starttick,endtick,roundxy,tickno,max(xy),forceop,getRangeMultiple(starttick,max(xy),roundxy*tickno))
  ticks <- seq(from=starttick,to=endtick,roundxy*tickno) # the no ofticks  
  ticks <- adjustTicks(ticks = ticks,fix = fix,roundxy = roundxy,tickno = tickno,fixop = fixop)
  #set range too to reflect the new fix
  rangexy <- c(ticks[1],tail(ticks,1))
  if(length(fix)>2) {#if the ticks are hardcoded, nothing to be done
    ticks <- fix
    rangexy <- c(fix[1],fix[length(fix)])
  }
  #make sure there are no trailing decimal places
  #xydec tells you whether you should have more decimal places than the number in roundxy
  xydec <- getAboveThresh(sapply(xy,decimalplaces),decimalplaces(roundxy))
  xydec <- ifelse(length(xydec)==0,0,max(xydec)) #we can allow upto 1 extra decimal place over roundxy if the data has more granularity
  nodec <- ifelse(xydec>decimalplaces(roundxy),decimalplaces(roundxy)+1,decimalplaces(roundxy))
  #cat('\n1ticks',ticks,',',nodec,'\nxy',xy,'\nxydec',sapply(xy,decimalplaces),'xydec',xydec,':',round(ticks,xydec))
  ticks <- unique(as.numeric(format(round(ticks,nodec),nsmall = nodec) ) )
  list(roundxy,rangexy,ticks)
}

#function that generates round, range, and ticks
#xy : vecttor of either x or y values
#rndfact, tickno, and fix are the same as fplotlog
#rndfact: the factor to which the roundxy should be rounded, e.g., 5 would mean that they should be rounded to nearest power of 5
#tickno: tells you how big the divisions b/w ticks should be. higher tickno means more distance between ticks.
#logs: log scale or linear scale. If log scale will only produce integer ticks,
#sucha as 10^1 instead of 10^1.23
genRangeTicks.new <- function(xy,rndfact,tickno,fix=1,fixop=1,logs=F,maxticks=10){
  cat('\ngRT',fixop,'\n')
  roundxy <- ((max(xy)-min(xy))/maxticks) 
  #adjust so that roundxy, start and end are on round numbers in terms of roundfact  
  cat('\nroundxy',roundxy,'min',min(xy),'max',max(xy))
  if(roundxy < 1) {
    pow10 <- getpow10(roundxy)
    roundxy <- floor(roundxy* 10^-pow10) * 10^(pow10) #e.g., changes -0.34 to 0.3
  }
  if(logs) roundxy <- 1
  roundxy <- roundxy*tickno
  starttick <- getNearestMultiple(min(xy),rndfact,op=3)
  cat('\nstarttick',starttick,min(xy))
  endtick <- getRangeMultiple(starttick,max(xy),roundxy)
  #rangexy <- c(getNearestMutiple(min(xy),roundxy,1),getNearestMutiple(max(xy),roundxy,2))
  rangexy <- c(starttick,endtick)
  cat('\nxy',xy,'rangexy',rangexy,'roundxy',roundxy,'tickno',tickno)
  #ticks <- seq(from=starttick,to=endtick,roundxy*tickno) # the no ofticks  
  ticks <- seq(from=starttick,to=endtick,roundxy) # the no ofticks  
  #adjust ticks if the axes are going to be fixed
  #cat('roundxy',roundxy,'st',starttick,'range and ticks',rangexy,', ticks,',ticks,'fix',fix,'\n')
  ticks <- adjustTicks(ticks = ticks,fix = fix,roundxy = roundxy,tickno = tickno,fixop = fixop)
  #set range too to reflect the new fix
  rangexy <- c(ticks[1],tail(ticks,1))
  list(roundxy,rangexy,ticks)
}


#given the fix option this will adjust the ticks:
#fixop: 1 - make the extreme ticks go roundxy*tickno above the extremes, 2 - stop exactly at fix
adjustTicks <- function(ticks,fix,roundxy,tickno,fixop=1,op=1){
  if(length(fix)>1) {#change according to fixop
    if(fixop==1){#start tickno/2 before fix1 and end tickno by 2 after fix2
      end <- getNearestMultiple(fix[2],roundxy*tickno/2,op=4)
      #cat('\nadjustticks',end,fix[2],roundxy,tickno/2)
      res <- seq(from=getNearestMultiple(fix[1],roundxy*tickno/2,op=3),
                 to=end,roundxy*tickno)
      if (tail(res,1)!=end) res<- c(res,end) #adjust the last tick
    }
    else { #the ticks starts and ends with specified range
      res <- seq(from=fix[1],to=fix[2],roundxy*tickno)
      if(tail(res,1)!=fix[2]) res <- c(res,fix[2]) #adjust the last tick
    }
    return(res)
  }
  else return(ticks) #nothing to be done return the ticks
}

#this function, given x and y, generates yticks,xlabels, and nox
#x and y: the data, rest of the var same as fplotlog
genPlotVars <- function(x,y,rndfact=1,ticknox=2,ticknoy=2,fixy=1,fixx=1,fixop=1,logs=F,maxticks=10){
  if(length(rndfact) > 1) {
    rndfactx = rndfact[1]
    rndfacty = rndfact[2]
  } 
  else {
    rndfactx = rndfact
    rndfacty = rndfact
  }
  options(scipen = 10)
  #find if logscale for both or just one
  if(length(logs)>1){#logs for only one axis
    logsx <- logs[1]
    logsy <- logs[2]
  }
  else{#log for both axes
    logsx <- logs
    logsy <- logs
  }
  #sets the range of min and max y values for plotting
  #cat('\n rndfact is',rndfact)
  rangestats <- genRangeTicks(y,rndfact = rndfacty,tickno = ticknoy,fix=fixy,fixop=fixop,
                              logs = logsy,maxticks = maxticks)
  assignVarVals(c('roundy','yrange','yticks'),rangestats)
  #cat('\nyticks',roundy,getNearestMutiple(min(y),roundy,1),getNearestMutiple(max(y),roundy,2)
  #    ,yticks,'\n')

  #cat('rndfactx',rndfactx)
  rangestats <- genRangeTicks(x,rndfact = rndfactx,tickno = ticknox,fix=fixx,fixop=fixop,
                              logs = logsx,maxticks = maxticks)
  assignVarVals(c('roundx','xrange','xticks'),rangestats)
  #cat('x',roundx,getNearestMutiple(min(x),roundx,1),getNearestMutiple(max(x),roundx*ticknox,4),
  #    xticks,'\n')
  #if fixx or fixy has more than 2 elements, those are the ticks
  list(roundy,yrange,yticks,roundx,xrange,xticks)
}

#given the plottype and plotx points, will drop a line from the points to the vertical 
#point given by hline
dropLine <- function(plotx,ploty,plottype,dropx,hline,plcolors,op=1){
  if(dropx==F) return(T) #nothing to do
  #cat('dropLine\n',dropx,hline,plottype)
  if(plottype==1){
    lapply(1:length(plotx),function(x){
      #just draw the line
      #cat('\n',plotx[x],str(ploty[x]),hline)  
      flines(c(plotx[x],plotx[x]),c(ploty[x],hline),color = plcolors[1],lwidth = .25)
    })
  }
  if(plottype==3){
    lapply(1:length(plotx),function(x){
      #ploty is a matrix, so we have afurther spliot
      lapply(1:ncol(ploty),function(j){
        flines(c(plotx[x],plotx[x]),c(ploty[x,j],hline),color = plcolors[j],lwidth = .25)
      })
    })
    
  }
}


#this function plots the type of graph you want
#here, when we say multiple columns, there is one x column but mulitiple y columns
#plottype: 1 - single column, all the same symbol, 2- single column different symbols
#based on factor column, 3 - multiple columns, each a different color: input either a DF or lists
#but they have to be the same length as x,
#4 - multiple columns each a different color and with different symbols for every factor
#5 - multiple columns each the same color and with different symbols for evert factor,
#6 - multiple columsn, same color and symbol
#7 - x and y are multiple columns, diff color for each x and y pair
#8 - null plot for those instances for when we just want to see the fits.
#9 - null plot. nothing to plot
#hline: draws a horizontal line at the y position given by hline
#vline: draws a vertical line at the x position(s) given by hline
#dropx: each point is connected to hline by a thin line
#pointset: the second set of points to be plotted. They are in the form of two columns pointset[,(vecx,vecy)], or c()
#pointsize: soize of the pointset 
fplottype <-function(plotx,ploty,factors=c(),cols,xrange,yrange,xlabel,ylabel,
                     markersize = 1.75,title='',plottype=1,dropx=F,hline=c(),vline=c(),pointset=c(),pointsize=2){
  #cat("\nplottype",plottype)
  plsymbols <- c(21,2:20) #set the symbols for plotting
  #set the colors for plotting
  plcolors <- c('black','red','blue','green','grey','magenta','yellow','brown','blueviolet','cadetblue',
                'orange','darkcyan','darkgoldenrod1','darkorchid4','lightpink','orchid',
                'purple','palevioletred','plum','slateblue') 
  #if markersize is at .15, we cant make the points smallewr without changing lwd, which we do proportionately decreasing it below 0.15
  if(markersize<0.15) linewidth <- 2 * (markersize/0.15)
  else linewidth <- 2
  #dropLine testing
  if(plottype==1){
    plot(plotx,ploty,frame.plot = F,main = title, xlim = xrange,
       ylim = yrange,pch=plsymbols[1],
       col=plcolors[1],yaxt="n",ylab='',cex=markersize,
       xaxt="n",xlab='',lwd=linewidth,las=1,cex.axis=1.5)
    dropLine(plotx = plotx,ploty = ploty,plottype = plottype,dropx = dropx,
             hline = hline,plcolors=plcolors)
    #return(T)
  }
  
  if(plottype==2){#2- single column different symbols, based on factor descriptions
    factor.symb <- factors
    #if factors is a string of characters it has to converted to a number
    if( class(factors) == class('char')) factor.symb <- (as.factor(factors))
    #cat('factior',factors,plsymbols[as.numeric(factor.symb)],as.numeric(factor.symb),'\n')
    plot(plotx,ploty,frame.plot = F,main = title, xlim = xrange,
         ylim = yrange,pch=plsymbols[as.numeric(factor.symb)],
         col=plcolors[1],yaxt="n",ylab='',cex=markersize,
         xaxt="n",xlab='',lwd=linewidth,las=1,cex.axis=1.5)
    #return(T)
  }
  if(plottype==3){#3 - multiple columns, each a different color
    #cat(ploty,plotx,' plottype 3\n')
    if(isDataType(ploty)==const.DataType$list)
      plty <- convertListToDF(ploty)
    else plty <- ploty
    plcolors <- c(plcolors,rep(plcolors,length(ploty))) #we need more colors with cols
    matplot(plotx,plty,frame.plot = F,main = title, xlim = xrange,
         ylim = yrange,pch=plsymbols[1],
         col=plcolors[1:length(ploty)],yaxt="n",ylab='',cex=markersize,
         xaxt="n",xlab='',lwd=linewidth,las=1,cex.axis=1.5,type = 'l')
    dropLine(plotx = plotx,ploty = plty,plottype = plottype,dropx = dropx,
             hline = hline,plcolors=plcolors)
    #return(T)
  }
  if(plottype==4){#4 - multiple columns each a diff color and diff symbols for every factor
    plcolors <- c(plcolors,rep(plcolors,length(ploty))) #upper limit on color
    plot(plotx,ploty[,1],frame.plot = F,main = title, xlim = xrange,
         ylim = yrange,pch=plsymbols[as.numeric(factors)],
         col=plcolors[1],yaxt="n",ylab='',cex=markersize,
         xaxt="n",xlab='',lwd=linewidth,las=1,cex.axis=1.5)
    lapply(2:length(cols),function(x) {
      points(plotx,ploty[,x],main = title, xlim = xrange,
             ylim = yrange,pch=plsymbols[as.numeric(factors)],col=plcolors[x],
             yaxt="n",ylab='',cex=markersize,
             xaxt="n",xlab='',lwd=linewidth,las=1,cex.axis=1.5)
    })
    #return(T)
  }
  if(plottype==5){#4 - multiple columns same color and diff symbols for every factor
    plot(plotx,ploty[,1],frame.plot = F,main = title, xlim = xrange,
         ylim = yrange,pch=plsymbols[as.numeric(factors)],
         col=plcolors[1],yaxt="n",ylab='',cex=markersize,
         xaxt="n",xlab='',lwd=linewidth,las=1,cex.axis=1.5)
    lapply(2:length(cols),function(x) {
      points(plotx,ploty[,x],main = title, xlim = xrange,
             ylim = yrange,pch=plsymbols[as.numeric(factors)],col=plcolors[1],
             yaxt="n",ylab='',cex=markersize,
             xaxt="n",xlab='',lwd=linewidth,las=1,cex.axis=1.5)
    })
    #return(T)
  }
  if(plottype==6){#6 - multiple columsn, same color and symbol
    cat(str(ploty))
    plot(plotx,ploty[,1],frame.plot = F,main = title, xlim = xrange,
         ylim = yrange,pch=plsymbols[1],
         col=plcolors[1],yaxt="n",ylab='',cex=markersize,
         xaxt="n",xlab='',lwd=linewidth,las=1,cex.axis=1.5)
    lapply(2:length(cols),function(x) {
      par(new=T)
      plot(plotx,ploty[,x],frame.plot = F,main = title, xlim = xrange,
             ylim = yrange,pch=plsymbols[1],col=plcolors[1],
             yaxt="n",ylab='',cex=markersize,
             xaxt="n",xlab='',lwd=linewidth,las=1,cex.axis=1.5)
    })
  }
  if(plottype==7 || plottype==8){#7 - x and y are multiple DFs, 
    #7 - diff color for each x and y pair, #8 - multiple columns, all the same color
    #print(plotx)
    if (plottype==8) plcolors <- rep(plcolors[5],length(ploty)) #same color grey
    else plcolors <- c(plcolors,rep(plcolors,length(ploty))) #making sure dont run out of colors
    #cat('\nlength plottype',length(plotx),plcolors)
    
    plot(plotx[[1]],ploty[[1]],frame.plot = F,main = title, xlim = xrange,
         ylim = yrange,pch=plsymbols[1],
         col=plcolors[1],yaxt="n",ylab='',cex=markersize,
         xaxt="n",xlab='',lwd=linewidth,las=1,cex.axis=1.5)
    #par(new=T)
  if(length(plotx)>1){ #only do this if more than one element in list 
      lapply(2:length(plotx),function(x) {
        par(new=T)
        plot(plotx[[x]],ploty[[x]],frame.plot = F,main = title, xlim = xrange,
             ylim = yrange,pch=plsymbols[1],col=plcolors[x],
             yaxt="n",ylab='',cex=markersize,
             xaxt="n",xlab='',lwd=linewidth,las=1,cex.axis=1.5)
      })
    }
    #return(T)
  }
  if(plottype==9){#null plot, no need to plot anything, but we still call plot for setting
    #up the plot, cex is set to 0 to get rid of points
    plot(xrange,yrange,frame.plot = F,main = title, xlim = xrange,
         ylim = yrange,pch=plsymbols[1],
         col=plcolors[1],yaxt="n",ylab='',cex=0,
         xaxt="n",xlab='',lwd=linewidth,las=1,cex.axis=1.5)
    #return(T)
  }
  
  #if there is a second set of points to be plotted
  if (length(pointset)>0) { #the markersize is bigger for these points
    #cat('pointsize',markersize*20,'\n')
    points(pointset[,1],pointset[,2],col=plcolors[1:length(ploty)],cex=pointsize,pch=plsymbols[15])
  }
  
  #if vertical or horizontal points are needed
  #cat('hline',hline,'\n')
  if (length(hline) > 0) sapply(1:length(hline), function(x) flines(xrange,c(hline,hline),color = plcolors[1],lwidth = .25))
  if (length(vline) > 0) sapply(1:length(vline), function(x) flines(c(vline[x],vline[x]),yrange,color = plcolors[1],lwidth = .25))
  return(T)
}

#this function plots the and y axes for the data
#xticks: the x axis ticks
#yticks: the y axis ticks
#logp : the low power that we should plot, default is 10
#logs: log scale
#tickunit: at least for display, gives the units in which the axes should be displayed
#if one number divides both axes. If two numbers then (xtickunit,ytickunit)
#op= 1, labels are normal, 2 - labels have superscipts
plotaxes <-function(xticks,yticks,xlabel='',ylabel='',logs=F,lgscale=10,tickunit=1,op=1){
  #do axis labels, line species the distance from axis, las the orientation
  #mtext(text=parse(text=ylabel),side=2,las=0,cex=1.2,font=2,line=4.2,lwd=2)#parse for superscripts
  mtext(text=getStringExp(ylabel),side=2,las=0,cex=1.2,font=2,line=5.2,lwd=2) #y-axus
  mtext(text=getStringExp(xlabel),side=1,las=0,cex=1.2,font=2,line=3.5,lwd=2) #x-axis
  #do the y-axis
  #cat('plot',xticks,yticks,as.character(yticks),"\n")
  #find if logscale for both or just one axis
  if(length(logs)>1){#logs for only one axis
    logsx <- logs[1]
    logsy <- logs[2]
  }
  else{#log for both axes
    logsx <- logs
    logsy <- logs
  }

  if(length(tickunit)>1){#logs for only one axis
    unitx <- tickunit[1]
    unity <- tickunit[2]
  }
  else{#log for both axes
    unitx <- tickunit
    unity <- tickunit
  }
  #cat('\npax: some')
    

  #y axis labels
  yticks.display <- yticks/unity
  axis(side=2,at=yticks,labels=rep("",length(yticks)),mgp=c(4,1,1),lwd=2)
  if (logsy) mtext(text=parse(text=paste(lgscale,"^",yticks,sep = '')),
        ,side=2,at=yticks,las=1,cex=1.2,font=2,line=1.75)
  else mtext(text=as.character(yticks.display),
             ,side=2,at=yticks,las=1,cex=1.2,font=2,line=1.75)
  #do the x axis labels
  xticks.display <- xticks/unitx
  axis(side=1,at=xticks,labels=rep("",length(xticks)),mgp=c(3,1,.75),lwd=2)
  #cat('ticks',xticks,yticks)
  if (logsx) mtext(text=parse(text=paste(lgscale,"^",xticks,sep = '')),
                  ,side=1,at=xticks,las=1,cex=1.2,font=2,line=1.75)
  else mtext(text=as.character(xticks.display),
             ,side=1,at=xticks,las=1,cex=1.2,font=2,line=1.75)
  T
}

#function to make the key for a heatmap
makeHeatMapKey <- function(delta=0.05){
  pal <- matrix(seq(0,1,delta),nrow = 3)
  image(t(pal), col =  hcl.colors(60, "YlOrRd", rev = TRUE),xaxt="n",yaxt="n")
  #plotaxes(xticks = c(0,.5,1),yticks = c())
  xticks <- c(0,0.25,0.5,0.75,1)
  xticks.display <- xticks/1
  #axis(side=1,at=xticks,labels=rep("",length(xticks)),mgp=c(3,1,.75),lwd=2)
  mtext(text=as.character(xticks.display),side=1,at=xticks,las=1,cex=1,font=1,line=0.25)
  
}

heatmapplot <- function(plmat,pal,op=1){
  # plot(plotx,ploty,frame.plot = F,main = title, xlim = xrange,
  #      ylim = yrange,pch=plsymbols[1],
  #      col=plcolors[1],yaxt="n",ylab='',cex=markersize,
  #      xaxt="n",xlab='',lwd=linewidth,las=1,cex.axis=1.5)
  # dropLine(plotx = plotx,ploty = ploty,plottype = plottype,dropx = dropx,
  #          hline = hline,plcolors=plcolors)
  size=1
  xysize <- 2.5 * size

  # layout.matrix <- matrix(c(2,1),nrow = 2,ncol = 1)
  # layout(mat = layout.matrix,heights = c(1,2),widths = c(4))
  
    
  #par("fin"=c(5,5))
  #par("pin"=c(8,8),xpd=NA)
  #par(mar = c(5, 4, 0, 0))
  #par(fig=c(0,.8,0,.8), new=TRUE)
  heatmap(plmat,scale='none',Rowv = NA,Colv = NA)
  
  heatmap(plmat,scale='none',Rowv = NA,Colv = NA)
  # par("fin"=c(5,5))
  #par(mar = c(0, 4, 0, 0))
  # par(fig=c(0,0.8,0.55,1), new=TRUE)
  # par("pin"=c(2,.2),xpd=NA) #adjust size of the display parametrized by aspect, cex=1.5
  # image(t(pal), col =  hcl.colors(60, "YlOrRd", rev = TRUE),xaxt="n",yaxt="n")
  # #plotaxes(xticks = c(0,.5,1),yticks = c())
  # xticks <- c(0,0.25,0.5,0.75,1)
  # xticks.display <- xticks/1
  # #axis(side=1,at=xticks,labels=rep("",length(xticks)),mgp=c(3,1,.75),lwd=2)
  # mtext(text=as.character(xticks.display),side=1,at=xticks,las=1,cex=1,font=1,line=0.25)
}

#this function checks whether x and y are vectors or lists of vectors, and returns an
#appropriate data structure
#
processPlotXY <-function(x,y,logs=F,lgscale=10,rndfact=1,ticknox=2,ticknoy=2,fixy=1,
                             fixx=1){
  #cat(str(x))
  #write function to figure out default vector or data frame
  #if x is a list it means the data is a list of x and y vectors
  if (typeof(x) == typeof(list())){
    #added code so that if x is a list it can be a list of data frames of x and y vectors or list of x and y vectors
    if(isDataType(x)==const.DataType$dataframe){#df
      plotx <- lapply(x,function(item) item[,1]);ploty <- lapply(x,function(item) item[,2])  
    }
    if(isDataType(x)==const.DataType$list){#list
      #test if each itme is a list or DF and act appropriately
      if(isDataType(x[[1]])==const.DataType$list){
        plotx <- lapply(x,function(item) item[[1]]);ploty <- lapply(x,function(item) item[[2]])  
      }
      else {#matrix or DF
        #cat('\n',str(x))
        plotx <- lapply(x,function(item) item[,1]);ploty <- lapply(x,function(item) item[,2])  
      }
    }
  }
  else {
    plotx <- x
    ploty <- y
  }
  allx <- unlist(plotx) # get the x and y data points
  ally <- cleanNA(unlist(ploty))
  #logs is split into (logsx,logsy)
  if (length(logs)>1){
    logsx <- logs[1]
    logsy <- logs[2]
  }
  else{#if not split then x and y axes are the same
    logsx <- logs
    logsy <- logs
  }
  if(logsx || logsy){#if x or y axis should be logscale
    #if x is a list it means the data is a list of x and y vectors
    if (typeof(x) == typeof(list())){
      if(logsx){
        allx <- log(allx,lgscale)
        if(isDataType(x)==3) plotx <- lapply(x,function(x) log(x[,1],lgscale))    
        if(isDataType(x)==4) plotx <- lapply(x,function(x) log(x[[1]],lgscale))    
      }
      if(logsy){
        ally <- log(ally,lgscale)
        ploty <- lapply(x,function(x) log(x[,2],lgscale))  
        if(isDataType(x)==3) ploty <- lapply(x,function(x) log(x[,2],lgscale))    
        if(isDataType(x)==4) plotx <- lapply(x,function(x) log(x[[2]],lgscale))    
      }
    }
    else {#x and y specify x and y values
      if(logsx){
        allx <- log(allx,lgscale)
        plotx <- log(plotx,lgscale)
      }
      if(logsy){
        ally <- log(ally,lgscale)
        ploty <- log(ploty,lgscale)
      }
    }
  }  
  list(allx,ally,plotx,ploty)
}


#a fuction to get a nice fancy plot 
#the options are present in op = 1, regular, 2= only curves for a range of x values, and 
#the list of curves are given by eq
#input: the x vector, the y vector, xlabel, ylabel, title
#x : an x vector or a list of plotting data. If list each element contains x and y columns
#y : could be a y vector or a data frame of columns
#fixx,y : fixes the range of the axes, if there are more than two elements, those are the ticks, e.g., c(1,3,5) means the xy-ticks
#fixop: 1 - make the extreme ticks go roundxy*tickno above the extremes, 2 - stop exactly at fix
#tickno: tells you how big the divisions should be. The fewer ticks you want, the higher thisno
#rndfact: the factor to which the roundxy should be rounded, e.g., 5 would mean that they should be rounded to nearest power of 5
#eq: one equation or a list of equations for multiple data
#full: whether you wnt to plot across the whole length of the axis (full) or just from smallest
#data point to highest data point
#stddev: the standard deviation or error to be plotted for each point. It g=has to follow 
#whatever format the data is in, which is x and y
#stdorient: orientation of the std error bars
#stdthick: thickness of the std deveiation bars
#stdepsilon: width of the horizontal std dev bars
#plottype: whether you want
#aspect: the y to x aspect of the plot, 1 - equal x and y, .5 would be y axis is half of x
#size: size of the panel for the graph. 1 is normal, andd 2 is double the size, 2.2 could be max
#tickunit: at least for display, gives the units in which the axes should be displayed
#if one number divides both axes. If two numbers then (xtickunit,ytickunit)#hline: draws a horizontal line at the y position given by hline
#vline: draws a vertical line at the x position given by vline
#dropx: each point is connected to hline by a thin line
#pointset: the second set of points to be plotted. They are (vecx,vecy) format, or c()
#pointsize: soize of the pointset 
#logs: logscale or not, F - not logscle, or (T,F) - log for x axis, not for Y. and the 3 other combinations.
#op=1, normal data x and y, =2 plot equations for a range of x. No Y specified,
#op=3, plot all the normal data but plot just one of the equations
fploteq <- function(x,y=c(),eq=F,xlabel='x',ylabel='y',title='',
                    markersize=1.2,logs=F,lgscale=10,factors=c(),rndfact=1,ticknox=1,ticknoy=1,
                    fixy=1,fixx=1,fixop=2,plottype=c(),digits=3,full=T,stddev=c(),stdorient=1,
                    stdthick=3,stdepsilon=0.4,aspect=1,size=1,tickunit=1,hline=c(),vline=c(),dropx=F,
                    pointset=c(),pointsize=2,op=1){
  #write function to figure out default vector or data frame
  #process the X and y data to get the plot and allx and ally values
  #cat(str(x),y,op,str(eq),'\n')
  vecx <- x
  if(op==2){#only want curves need y points to figure axis, get all the y points
    vecy <- getEqnPoints(x=x,eq = eq)
    #cat('\n the y points:',vecy)
  }
  else vecy <- y
  tmp <- processPlotXY(vecx,vecy,logs = logs,lgscale = lgscale,ticknox = ticknox,
                       ticknoy = ticknoy,fixy = fixy,fixx = fixx)
  assignVarVals(c('allx','ally','plotx','ploty'),tmp)
  #calculate all the plot variables
  #cat('\nfplot all',allx,ally,'plot',str(plotx),str(ploty),'rnd',rndfact)
  tmp <- genPlotVars(allx,ally,rndfact,ticknox,ticknoy,fixy,fixx,fixop=fixop,logs = logs)
  assignVarVals(c('roundy','yrange','yticks','roundx','xrange','xticks'),tmp)
  
  cat('range x:',xrange,',',xticks,',y:',yrange,',',yticks,'\n')
  #par(mar=c(2,5,2,4)) #adjust the outer and inner marggines
  #if (op==2) plot(xrange,yrange)
  par(oma=c(4,4,2,4))
  par(font.axis = 2) #make axes bold again, Haha!
  xysize <- 2.5 * size
  par(pin=c(xysize,xysize*aspect),xpd=NA) #adjust size of the display parametrized by aspect, cex=1.5
  
  #redundant:if((length(ploty)==1) && (length(plottype)==0)) plottyp <- 1 #only 1 column to plot
  cols <- 0
  if(length(plottype)==0) {#if no plottype specified, choose default options for 1/2 cols
    if(typeof(ploty)==typeof(list())) {
      if(typeof(plotx)==typeof(list())) plottyp <- 7 #both x and y are lists
      else plottyp <- 3 #one x, multiple y
      #cat('type',typeof())
    }
    else {#there is no y list specified so either one x and y or just plotting eqns
      if (op==2) plottyp <- 9 #plotting only eqns so dont need points
      else plottyp <- 1 #one x and y
    }
  } 
  else plottyp <- plottype
  cat('plottype',plottyp,'\t')
  fplottype(plotx,ploty,factors=factors,cols=cols,xrange=xrange,yrange=yrange,xlabel=xlabel,
            ylabel,markersize=markersize,
            title = title,plottype = plottyp,dropx = dropx,hline = hline,vline=vline,pointset = pointset)
  #do the stddev
  if(length(stddev) > 0)  plotmeansd(noxlabels=x,means=y,std=stddev,epsilon = stdepsilon,linethick = stdthick*markersize/1.75,
                                     markersize = 0.2*markersize,orient = stdorient,logs = logs,lgscale=lgscale,op=2)
  #plotmeansd(noxlabels=x,means=y,std=stddev,markersize = markersize,op=2)
  #curve and plot are equivalent here
  #plot(eq,xlim = c(min(allx),max(allx)),add=T)
  
  #if (logs) pleq <- function(x) log(eq(lgscale^x),lgscale) #if it is logscale, change fn
  #else pleq <- function(x) eq(x)
  #curve(pleq,from =min(allx),to=max(allx), add = T)

  #check if a function or list of functions has been specified, if not skip equation fitting
  if(plottyp==8) eqplotcolor <- 2 #the plot color is black for all curves on grey point background
  else eqplotcolor <- 1
  if (typeof(eq) == typeof(function(x) x) || typeof(eq) == typeof(list(1))) 
    plotcurves(eq,minx = min(allx),maxx=max(allx),logs = logs,lgscale = lgscale,
               xrange = xrange,yrange = yrange,full = full,plotcolor = eqplotcolor)
  #extra stuff to be drwan like joining the point to an imaginary line
  drawMiscOptions()
  #needed: yticks, xlabels,nox
  #do the x and y axis labels
  plotaxes(xticks,yticks,xlabel=xlabel,ylabel=ylabel,logs=logs,lgscale = lgscale,tickunit = tickunit)
}

#empty function for now
#extra stuff to be drwan like joining the point to an imaginary line
drawMiscOptions <-function(){
  #
}


#if there are multiple equaations to be plotted, this plots all of them
#pleq is either one equation or a list of equations
#min and max are the limits to be plotted
#full=T specifcies you want the fitted line to extend over the maximum xrange and yrange
#=F speccifies that it goes from minx to maxx
#plotcolor: 1, just go with default in fplottype, 2 - all grey, 3 - black 
plotcurves <-function(eq,minx,maxx,logs=F,lgscale=10,xrange=c(),yrange=c(),full=T,
                      plotcolor=1,op=1){
  #cat('\nplotcurves',minx,maxx)
  #logs is split into (logsx,logsy)
  if (length(logs)>1){
    logsx <- logs[1]
    logsy <- logs[2]
  }
  else{#if not split then x and y axes are the same
    logsx <- logs
    logsy <- logs
  }
  #if multiple curves plot them one after the other
  plcolors <- c('black','red','blue','green','grey','magenta','yellow','brown') #set the colors for plotting
  if (plotcolor==2) plcolors <- rep(plcolors[1],length(plcolors)) #all curves in black
  if(typeof(eq) == typeof(list())) {#list of eqns, plot each sepearately
    #make sure you have enough colors for the lines
    plcolors <- c(plcolors,rep(plcolors,length(eq)))
    #cat('\n list len',length(eq),'\t')
    lapply(1:length(eq),function(i){
      eqni <- eq[[i]]
      #first logscale the x axis then the y axis
      if(logsx) pleqx <- function(x) eqni(lgscale^x) #lgscale the x coords
      else pleqx <- function(x) eqni(x)
      if(logsy) pleq <- function(x) log(pleqx(x),lgscale) #lgscale the evaluated fn i.e. y
      else pleq <- function(x) pleqx(x)
      #make sure that with logs=T, we do not plot past the point where y =0, avoid negative y values
      #this function takes the normal range and returns a modified x1 or x2 above 0
      #if the y values go below 0, then choose those values of x
      if(xrange[1] < xrange[2]) zeroval <- yrange[1]
      else zeroval <- yrange[2]
      mod.xrange <- computeFnZero(fn=pleqx,x1=xrange[1],x2=xrange[2],zero = zeroval,lgscale = lgscale,prec = .01)
      #cat('\nmod.xrange',mod.xrange,pleqx(mod.xrange[1]),pleqx(mod.xrange[2]),pleq(mod.xrange[1]),
      #     pleq(mod.xrange[2]))
      if(full==T){
        if(pleq(mod.xrange[2]) >= pleq(mod.xrange[1])) yind <- 0 #if inc. fn ytarget is the lower y
        else yind <- 1 #when dec fn, yind is 1 to reverse the yrange indices
        #cat('\nfull',yind,xrange,yrange,'\n')
        xmin <- binarySearch(x1=mod.xrange[1],x2=mod.xrange[2],y=yrange[1+yind],eq=pleq,op=1)
        xmax <- binarySearch(x1=mod.xrange[1],x2=mod.xrange[2],y=yrange[2-yind],eq=pleq,op=2)
        #cat('\n xmax',xrange,xmin,xmax,yrange,pleq(xrange[2]))
      }
      else {
        xmin <- minx
        xmax <- maxx
      }
      #cat('\nplotcurve',xmin,xmax)
      curve(pleq,from =xmin,to=xmax,add = T,col=plcolors[i])
    })
  }
  else {
    #first logscale the x axis then the y axis
    if(logsx) pleqx <- function(x) eq(lgscale^x) #lgscale the x coords
    else pleqx <- function(x) eq(x)
    if(logsy) pleq <- function(x) log(pleqx(x),lgscale) #lgscale the evaluated fn i.e. y
    else pleq <- function(x) pleqx(x)
    #pleq <- function(x) log(eq(x),lgscale)
    #pleq <- function(x) eq(lgscale^x)
    #get the modified xrange values, get the lowest value of x that is above 0
    #if the y values go below 0, then choose those values of x
    if(xrange[1] < xrange[2]) zeroval <- yrange[1]
    else zeroval <- yrange[2]
    mod.xrange <- computeFnZero(fn=pleqx,x1=xrange[1],x2=xrange[2],zero = zeroval,lgscale = lgscale,prec = .01)
    if(full==T){
      if(pleq(mod.xrange[2]) >= pleq(mod.xrange[1])) yind <- 0 #if inc. fn ytarget is the lower y
      else yind <- 1 #when dec fn, yind is 1 to reverse the yrange indices
      #cat('\nmodxrange',mod.xrange,yrange[1+yind],yrange[2-yind])
      xmin <- binarySearch(x1=mod.xrange[1],x2=mod.xrange[2],y=yrange[1+yind],eq=pleq,op=1)
      xmax <- binarySearch(x1=mod.xrange[1],x2=mod.xrange[2],y=yrange[2-yind],eq=pleq,op=2)
      #cat('\n',xmin,pleq(xmin),xmax,pleq(xmax))
    }
    else {
      xmin <- minx
      xmax <- maxx
    }
    #cat('\nplotcurve',xmin,xmax,'\txrange',xrange,', yrange',yrange)
    curve(pleq,from =xmin,to=xmax,add = T)
  }
}


#this function fneq plots a function from xmin to xmax
#the rest of the parameters are the same as fploteq
fplotFn <- function(x=c(0,1),eq,xlabel='x',ylabel='y',title='',
                   markersize=1.75,logs=F,lgscale=10,factors=c(),rndfact=1,ticknox=1,ticknoy=1,
                   fixy=1,fixx=1,fixop=1,plottype=c(),digits=3,
                   full=T,stddev=c(),stdorient=1,aspect=1,size=1,tickunit=1,hline=c(),vline=c(),dropx=F,
                   pointset=c(),pointsize=2,op=1){
  #get all the arguments and pass them on
  arguments <- formals('fplotFn') #the formal args of the function, the way its stored before the call
  arguments.used <- match.call() #these are the arguments that are actually used in the call
  #cat(str(as.list(arguments.used)) )
  #extract all arguments except fneq,mix,maxx,op
  fplot.args <- arguments[setdiff(names(arguments),c('op','y',names(arguments.used)) )]
  #print(c(fplot.args,as.list(arguments.used)[-1],list(op=2)) )
  #cat('\nfn',str(eq) )
  #now call the fploteq function.
  do.call(fploteq,c(fplot.args,as.list(arguments.used)[-1],list(op=2) ) )
}

#function to draw a fancy bar plot
fbplot <-function(vec,xlabel,ylims,colorvec) {
  barplot(vec,main="",xlab=xlabel,col=colorvec,beside = TRUE,cex.axis=1.5,ylim = ylims)
}

#this function draws or adds a line plot to an existing plot
#input: the x vector, the y vector, color, and line width
flines <- function(x,y,color="black",lwidth=2,op=1){
  lines(x,y,col = color, lwd = lwidth)
}

#this function plots either the x axis or the yaxis for fstripchartXXX
#ticks: the x axis ticks
#logp : the low power that we should plot, default is 10
#logs: log scale
#xcat: T- x labels are cateogry labels, F - x axis labels are nnumeric 
#angle: F - horizontal loabels, T - 45 % labels
#flipaxis: 1- default, 2 - x becomes y and vice vera
#op= 1, labels are normal, 2 - labels have superscipts
plotStripAxes <-function(xticks,yticks,xcat=T,xlabel='',ylabel='',logs=F,lgscale=10,
                         angle=F,flipaxis=1,op=1){
  #for flipaxis
  if(flipaxis == 2) assignVarVals(c('xticksnew','yticksnew','xlab','ylab','sidex','sidey'),list(yticks,xticks,ylabel,xlabel,1,2))
  else  assignVarVals(c('xticksnew','yticksnew','xlab','ylab','sidex','sidey'),list(xticks,yticks,xlabel,ylabel,1,2))
  #find if logscale for both or just one axis, For now log only affectts Y-axis
  if(length(logs)>1){#logs for only one axis  
    logsx <- logs[1]
    logsy <- logs[2]
  }
  else{#log for both axes
    logsx <- logs
    logsy <- logs
  }
  #do axis labels, line species the distance from axis, las the orientation
  #mtext(text=parse(text=ylabel),side=2,las=0,cex=1.2,font=2,line=4.2,lwd=2)#parse for superscripts
  
  #do the y-axis
  #midvaly = placement of the ylabel in the center
  if(is.character(yticksnew)==T) midvaly <- getMidVal(1:length(yticksnew))
  else midvaly <- getMidVal(yticksnew)
  mtext(text=getStringExp(ylab),side=sidey,las=0,cex=1.2,font=2,line=4.5,lwd=2,at=midvaly) #ylabel
  #make the axis, and then place axis labels
  axis(side=sidey,at=yticksnew,labels=rep("",length(yticksnew)),mgp=c(4,1,1),lwd=2)
  if (logsy) mtext(text=parse(text=paste(lgscale,"^",yticksnew,sep = '')),
                   ,side=sidey,at=yticksnew,las=1,cex=1.2,font=2,line=1.75)
  else mtext(text=as.character(yticksnew),
             ,side=sidey,at=yticksnew,las=1,cex=1.2,font=2,line=1.75)

  #do the x axis labels
  if(is.character(xticksnew)) nox <- c(1:length(xticksnew)) #check if labels are strings or numbers
  else nox <- xticksnew
  #check if we want numeric or categgorical labels
  if(xcat)  xlabs <- as.character(xticksnew) #its category labels
  else xlabs <- as.numeric(xticksnew) #numeric labels, not sure why you might need it
  #get the middle value of nox vector, and place the xlabel there
  axis(side=sidex,at=nox,labels=rep("",length(xticksnew)),mgp=c(3,1,.5),lwd=2)
  mtext(text=getStringExp(xlab),side=sidex,las=0,cex=1.2,font=2,line=3.5,lwd=2,at=getMidVal(nox))
  #sets the ypos for the labels needed in case you have angled labels, if lowest value 
  #the ypos has to be determined in relation to the lowest ytick. e.g., if it is -1 and the the
  #y-axis goes from -1 to 9. Then it should be a fifth of the distance below or at -(1+2). empirically determined
  if(is.character(yticksnew)==T) valy <- 1:length(yticksnew)
  else valy <- yticksnew
  ypos <- valy[1] - (valy[length(valy)]-valy[1])/5 #ypos <- par("usr")[3] #original val 
  #the x-axis labels
  if (angle) text(x=nox,ypos,srt=45,adj=1,labels = xlabs,xpd=T,
                  las=1,cex=1.2,font=2)#,pos=1,offset = 2)
  else mtext(text=xticksnew,side=sidex,at=nox,las=1,cex=1.2,font=2,line=1.25)
  T
}

#gets the middle element of a list of sorted numbers. 
getMidVal <- function(vec,op=1){
  len <- length(vec)
  half <- ceiling(len/2)
  mean(c(vec[half],vec[len+1-(half)] ))
}

#thsi function takes a regular data frame and puts in a form good for stripchart function
#op=1: the data frame contains the data in columns and the names are the colnames
makeStripchartDF <- function(data.df,op=1){
  nocol <- length(data.df) #gets the no of cols in the data frame
  res.ls <- lapply(1:nocol,function(x){ #for each col generate name,col data frame pair
    size <- length(data.df[,x])
    tmp <- rep(names(data.df)[x],size)
    data.frame(tmp,data.df[,x])
  })
  convertNestedListsDF(res.ls) #concantenate the list of data frames
}

#this function converts a vector into a data frame where the xlabel becomes the first col
convertVec2StripChart <-function(vec,xlabel='',op=1){
  n <- length(vec)
  cbind.data.frame(rep(xlabel,n),vec)
}

#I think this is the latest function. get rid of the others later liam
#this function draws a stripchart but with a more minimalist axes
#input: a data frame, with x axis as the 1st column, and y axis as the 2md column
#the 1st column contains a number of factors, and all factors of the same type are grouped together
#methodstr: for ex: jitter
#rndfact: the nearest power of 10 mulitiplied by rndfact, for example 5 * 1000
#tickno: divvides the number of ticks by tickno
#xcat: T- x labels are cateogry labels, F - x axis labels are nnumeric
#markersizemean: size of the mean marker
#width specifies how wide the x axis is going to be. width is xlim=10/width  
#xlab = 0, default, the 1st column is the x axis, 1 - the rownames are the x axis
#veclab: the label if data.df is a vector
#sem: 1 do sem, 2 do sd, 0 - do nothing, 3 - do CI
#op =1 default,
fstripchart <-function(data.df,methodstr='jitter',rndfact=1,ticknoy=1,fixy=1,lgscale=10,
                       logs=F,xlabel='',ylabel='',xcat=T,width=2,xlab=0,veclab='',jittersize=.2,
                       markersize=1.75,markersizemean=markersize,sem=1,angle=F,op=1){
  if (is.null(dim(data.df))) dat.df <- convertVec2StripChart(data.df,xlabel = veclab)
  else dat.df <- data.df
  if (xlab == 1) dat.df <- makeStripchartDF(data.df) 
  x <- dat.df[,1] # get the x and y data points
  y <- dat.df[,2]
  #sets the range of min and max y values for plotting
  rangestats <- genRangeTicks(y,rndfact = rndfact,tickno = ticknoy,fix=fixy,logs = logs)
  assignVarVals(c('roundy','yrange','yticks'),rangestats)

  options(scipen = 10)
  #split the y vector by cateogories based on x
  data.lst <- split(as.vector(y),x) # this automatically sorts tst
  #set the xlabels numeric or character
  if(typeof(x)==typeof('')) xlabels <- as.numeric(names(data.lst))
  else xlabels <- names(data.lst)
  #set no of xlabels
  nox <- c(1:length(xlabels))
  #par(mar=c(2,0,2,4)) #adjust the outer and inner marggines
  par(oma=c(3,2,2,4))
  par(font.axis = 2) #make axes bold again, Haha!
  #print(par()$pin)
  par(pin=c(2,2.5),xpd=NA) #adjust size of the display, cex=1.5
  #par(pin=c(1,2.25),xpd=NA) #adjust size of the display
  #this is old formula for specifying the data
  #stripchart(reformulate(names(data.df)[1],response = names(data.df)[2]),data=data.df,
  stripchart(data.lst,vertical=TRUE,pch=19,col="gray",yaxt="n",ylab="",cex=markersize,
             frame.plot = FALSE,method=methodstr,xlim=c(1,10/width),ylim=yrange,jitter = jittersize,
             xaxt="n",lwd=2,las=1,cex.axis=1.5)
  #do the y axis labels,mgp, p controls the axis posn, line controls label posn
  plotStripAxes(xticks = xlabels,yticks=yticks,xlabel = xlabel,ylabel = ylabel,
            lgscale = lgscale,logs = logs,xcat = xcat,angle=angle)
  #calculate the mean and sd of the data, and the xlabels
  layermean <- sapply(data.lst,mean)   
  std <- sapply(data.lst,function(x) switch(sem,sd(x,na.rm = T)/(length(x)^.5),
                                            sd(x,na.rm = T),1.96*sd(x,na.rm = T)/(length(x)^.5),
                                            0))
  #default and op=2, plot se, op=3 plot std
  #std <- switch(op,std/(length(data.lst[[1]]))^.5,std/(length(data.lst[[1]]))^.5,std)
  plotmeansd(layermean,std,nox,epsilon = markersizemean*.1,markersize = markersizemean)
}

#this function draws a stripchart with a more minimalist axes
#data.lst: the data input is a list of data frames rather than a single one
#data.lst is a data frame where the columsn are vectors of the different elements to be plotted. 
#methodstr: for ex: jitter, overplot, or stack
#rndfact: the nearest power of 10 mulitiplied by rndfact, for example 5 * 1000
#tickno: divvides the number of ticks by tickno
#markersizemean: size of the mean marker
#angle: specifies the angle for the x axis, F = 0 deg, T - 45 deg
#sem: 1 do sem, 2 do sd, 0 - do nothing, 3 - do CI
#semthick: controls the thickness of the sem lines
#orient: orientation of sem, vertical=1 and horizaontal=2
#pairplot: 0 default no paired plotting, 1 - do paired plots, 2 - paired plots except if more than 2 points, link 1 to 2 to 3
#flipaxis: 1 - as it is, 2 - the y-axis becomes the x-axis
#stretchx: the amount by which to stretch the x-axis. the default is 5x. To nullify, do 0.2
#col: adds color to the various points of the plot. Mainly matters when we have a list.
#ticklabels: specifies the strings for the xticks, if not specified, take the cue from data.lst
#op =1 default, 2 - print se and means
fstripchartvecs <-function(data.lst,methodstr='jitter',markersize=1.2,rndfact=1,lgscale=10,
                           logs=F,tickno=2,fixy=1,angle=T,sem=1,semthick=2,xlabel='',ylabel='',ticklabs=c(),markersizemean=markersize,
                           xcat=T,orient=1,pairplot=0,flipaxis=1,stretchx=1,col=F,op=1){
  x <- getListNames(data.lst) #names(data.lst) # get the x and y data points
  #cat('\nfstrip',getListNames(data.lst),'x:',length(x))
  y <- cleanNA(unlist(data.lst))
  #y value that is rndfact times the highest pow of 10 lower than min(y) 
  #fix: so that y is not 0
  options(scipen = 10)
  #sets the range of min and max y values for plotting
  rangestats <- genRangeTicks(y,rndfact = rndfact,tickno = tickno,fix=fixy,logs = logs)
  assignVarVals(c('roundy','yrange','yticks'),rangestats)
  
  #calculate the mean and sd of the data, and the xlabels
  layermnstd <- getMeanSds(data.lst = data.lst,sem = sem)
  #pick names for the tick labels on the x-axis. first check if they are specified, if not next stop: data.lst, and default: numbers
  if(length(ticklabs)>0) xlabels <- ticklabs
  else {#check if there are no names specified for the lists, then assign numbers as the list names
    if(length(x)>0) 
        xlabels <- x
    else  xlabels <- as.character(1:length(x))
  }
  nox <- c(1:length(x))
  #cat('\nnox',nox,',',c(1:length(x)))
  par(mai=c(2,0,2,0)) #adjust the outer and inner marggines
  par(oma=c(2,0,2,0))
  par(font.axis = 2) #make axes bold again, Haha!
  #par(oma=c(1,0,1,1))
  par(pin=c(.5,2.25),xpd=NA) #adjust size of the display
  plotMultipleStrips(data.lst = data.lst,markersize = markersize,yrange = yrange,methodstr = methodstr,plcolors = const.plcolors,flipaxis = flipaxis,
                     nox=nox,stretchx=stretchx,plsymbols = const.plsymbols,col=col)
  #do the y axis labels,mgp, p controls the axis posn, line controls label posn
  #if p is higher and line are higher they move the axis and label further away
  plotStripAxes(xticks = xlabels,yticks=yticks,xlabel = xlabel,ylabel = ylabel,
                lgscale = lgscale,logs = logs,xcat = xcat,flipaxis = flipaxis,angle=angle)
  plotMultipleMeanSds(data.lst = layermnstd,std = std,nox = nox,markersizemean = markersizemean,semthick = semthick,
                      orient = orient,plcolors = const.plcolors,flipaxis = flipaxis,plsymbols = const.plsymbols,col=col)
  if(pairplot>0) drawLinesList(data.lst = data.lst,pairplot=pairplot,plcolors = const.plcolors,col=col) #draws lines between points
  #if(pairplot>0) drawLines(xpts = nox,ypts = data.lst,op=pairplot)
  layermnstd
}

#plots multiple stripchats as needed
#data.lst: is the data.lst argument to fstripchartvecs, rest definition see fstripchartvecs 
#stretchx: the amount by which to stretch the x-axis. the default is 5x. To nullify, do 0.2
#col: adds color to the various points of the plot. Mainly matters when we have a list.
#flipaxis: 1 - as it is, 2 - the y-axis becomes the x-axis
plotMultipleStrips<-function(data.lst,markersize,yrange,methodstr,plsymbols=c(),plcolors=c(),flipaxis=1,nox=nox,stretchx=stretchx,col=F,op=1){
  flipval <- switch(flipaxis,T,F)
  #cat('\n',plcolors)
  lightcolors <- lighten(plcolors,factor = 80) #lightened colors for the data
  #cat('\n',plcolors,':',lightcolors)
  # the defauilt stretch is 5 fold over the default assigned by R. On top that you can use the stretchfator to adjust accordingly
  # the way it currently works: specifying a limit like c(1,2) assigns this interval to the current default length, and then other nos are assigned in relation to this interval
  #stretchfactor: controls the spacing between the x ticks,for 2 ticks it is 1, and for 8 it is 0.2. the lower the #, the less the space: eqn y = -.9/7x + 8.6/7
  stretchfactor <- (0.5/6)*length(nox) + (2.5/3)
  #xlimit <- c(min(nox),min(nox)+(stretchfactor*(max(nox)-min(nox))/(stretchx)) )
  xlimit <- c(min(nox),min(nox)+(stretchfactor/stretchx) )
  ylimit <- yrange
  if(flipaxis==2) {
    stretchfactor <- (-0.9/7)*length(nox) + (8.6/7)
    xlimit <- c(yrange[1],yrange[1]+(stretchfactor*(yrange[2]-yrange[1])/(stretchx)) )
    ylimit <- c(min(nox),max(nox))
  }
  cat('\nlimits',xlimit,';',ylimit,';nox ',nox,' stretch:',stretchfactor)
  #xlimit <- c(1,2)
  if(isDataType(data.lst[[1]]) == 4 ){#nested list
    for(i in seq_wrap(1,length(data.lst))){
      if(col==T) colop <- i #if you dont want color, just plot grey points
      else colop <- 1 #1 - black
      #plotMultipleStrips(data.lst = x,markersize = markersize,yrange = yrange,methodstr = methodstr)
      if(i==1)  stripchart(data.lst[[i]],
                           vertical=flipval,pch=plsymbols[i],col=lightcolors[colop],xlim=xlimit,yaxt="n",ylab="",cex=markersize,
                           ylim=ylimit,frame.plot = FALSE,method=methodstr,
                           xaxt="n",lwd=2,las=1,cex.axis=1.5)
      else stripchart(data.lst[[i]],
                      vertical=flipval,pch=plsymbols[i],col=lightcolors[colop],xlim=xlimit,yaxt="n",ylab="",cex=markersize,
                      ylim=ylimit,frame.plot = FALSE,method=methodstr,
                      xaxt="n",lwd=2,las=1,cex.axis=1.5,add = T)
    }
  }
  else {#not a nested list
    if(col==T) colop <- 2 #if you dont want color, just plot grey points
    else colop <- 1
    stripchart(data.lst,
               vertical=flipval,pch=plsymbols[1],col=lightcolors[colop],xlim=xlimit,yaxt="n",ylab="",cex=markersize,
               ylim=ylimit,frame.plot = FALSE,method=methodstr,
               xaxt="n",lwd=2,las=1,cex.axis=1.5)
  }
}


#draws the lines between the different lists of points. For better definition look at pairplots defn in fstripchartvecs
#pairplot: 0 default no paired plotting, 1 - do paired plots, 2 - paired plots except if more than 2 points, link 1 to 2 to 3
#data.lst: is the data.lst argument to fstripchartvecs
drawLinesList <- function(data.lst,pairplot,plcolors=c(),col=F,op=1){
  colop <- 1
  if(isDataType(data.lst[[1]]) == 4 ){#nested list
    for(i in seq_wrap(1,length(data.lst))){
      if(col==T) colop <- i
      drawLines(xpts = c(1:length(data.lst[[i]])),ypts = data.lst[[i]],col = plcolors[colop],op=pairplot)
    }
  }
  else {#not a nested list
    nox <- c(1:length(data.lst))
    drawLines(xpts = c(1:length(data.lst)),ypts = data.lst,op=pairplot,col = plcolors[colop])
  }
}

#draws the means and Sds for the different lists
#data.lst: is the data.lst argument to fstripchartvecs, rest definition see fstripchartvecs 
plotMultipleMeanSds <- function(data.lst,layermean,std,nox,markersizemean,semthick,orient,plcolors=c(),plsymbols=c(),flipaxis=1,col=F,op=1){
  colop <- 1
  if(isDataType(data.lst[[1]]) == 4 ){#nested list
    #cat('\n',plcolors)
    for(i in seq_wrap(1,length(data.lst))){
      if(col==T) colop <- i
      plotmeansd(means =data.lst[[i]][[1]],std=data.lst[[i]][[2]],nox=nox,epsilon = .1,markersize = markersizemean,
                 linethick = semthick,orient = orient,colop = colop,plsymbol = plsymbols[i],plcols = plcolors,flipaxis = flipaxis)
    }
  }
  else {#not a nested list
    if(col==T) colop <- 2
    #cat('\nplotmeans',str(data.lst))
    #cat('\n',str(data.lst),'nox ',nox)
    plotmeansd(means = data.lst[[1]],std=data.lst[[2]],noxlabels = nox,epsilon = .1,markersize = markersizemean,
              linethick = semthick,orient = orient,colop = colop,plsymbol = plsymbols[1],plcols = plcolors,flipaxis=flipaxis)
  }
}

#gets the mean and sd or sem for the list, and if it is a nested list,
#gets a list of c(mean,sd)
#op = 1 get mean/sd of all the leaf lists
#op = 2 gets the mean/sd of the all the leaf list elements
getMeanSds <- function(data.lst,sem,op=2){
  if(isDataType(data.lst[[1]]) == 4 ){#nested list
    res <- lapply(data.lst,function(x) getMeanSds(x,sem = sem,op=1))
  }
  else {#not a nested list
    #calculate the mean and sd of the data, and the xlabels
    layermean <- sapply(data.lst,function(x) mean(x,na.rm = T))   
    std <- sapply(data.lst,function(x) switch(sem,sd(x,na.rm = T)/(length(x)^.5),
                                              sd(x,na.rm = T),1.96*sd(x,na.rm = T)/(length(x)^.5),
                                              0))
    res <- list(layermean,std)
  }
  res
}

#gets the names of the list, if a nested list, 
#op = 1 gets the names of all the leaf lists
#op = 2 gets the names of the all the leaf list elements
getListNames<-function(lst.names,op=2){
  if(isDataType(lst.names[[1]]) == 4 && op==2){#nested list
    allnames <- lapply(lst.names,function(x) names(x))
    res <- unique(unlist(allnames))
    if(length(res)==0) res <- unique(unlist(lapply(lst.names,function(x) 1:length(x)) ) )
    #cat('\nlistnames',str(res),':',length(allnames))
  }
  else {#not a nested list
    res <- names(lst.names)  
    if(length(res)==0) res <- 1:length(lst.names)
  }
  res
}

#draws a line between two pairs of points specified by their x and y positions, In this case it will draw
#lines between sets 1 and 2, 3 aand 4 and so on
#xpts: the x points/coordinates, a vector of the points of each set on the x -axis
#ypts: the list of the point sets that are to be plotted
#op=1,lines between sets 1 and 2, 3 aand 4 and so on
#op=2, lines from 1 to 2 to 3 and so on
drawLines <- function(xpts,ypts,col='black',op=1){
  nogrps <- length(xpts)
  xpts.mat <- matrix(rep(xpts,length(ypts[[1]])),ncol = nogrps,byrow = T) #multiple copies of first row, i.e. xpts
  #cat('\ndL ',str(ypts),str(xpts.mat))
  switch(op,
  sapply(0:(nogrps/2 - 1), function(x){ #now draw the lines for each pair of sets
    #cat('\nDL',x,2*x + 1,2*x + 2)
    segments(x0 = xpts.mat[,2*x + 1],y0 = ypts[[2*x + 1]],x1 = xpts.mat[,2*x + 2],y1 = ypts[[2*x + 2]],col = col) 
  }),
  sapply(1:(nogrps-1), function(x){ #now draw the lines for each pair of sets
    #cat('\nDL',x,2*x + 1,2*x + 2)
    segments(x0 = xpts.mat[,x],y0 = ypts[[x]],x1 = xpts.mat[,x+1],y1 = ypts[[x+1]],col = col) 
  }) )
  
  T
}

#given 2 vectors of means and std, and the x axis label vectors,
#this will draw a mean and std, where epsilon is the width of the bar
#but this does it for multiple lists
#the plot is along the vertical axis
#epsilon: specifies the width of the std bar. calculated as a percentage of markerwidth
#noxlabels: is every x axis point where you need to put the sd bar, can be a vector or list of vectors/df
#linethick: the line thickness of the error bars
#orient: orientation of the bars: 1 - vertical, 2 - horizontal
#plsymbol: specifies the symbol type
#logs: logscale or not, F - not logscle, or (T,F) - log for x axis, not for Y. and the 3 other combinations.
#lgscale: scale for the logs, default 10
#op: not sure what it means, but assumption is that 1 - draw the means point, 2 - do not draw the means point
plotmeansd <- function(means,std,noxlabels,colop=1,plcols=c(),plsymbol=19,epsilon=8,linethick=8,markersize=1.75,orient=2,flipaxis=1,logs=c(),lgscale=10,op=1){
  #if the std dev is a list, then it plots each list one by one with a recursive call,
  #otherwise, it just plots them as x and y vectors
  #cat('\nplotmeans',noxlabels,':',std)
  if(length(std)!=0){#there is a std so plot the stdev
    #cat('\n',typeof(noxlabels),typeof(std))
    if (typeof(noxlabels) == typeof(list()) || typeof(std) == typeof(list()) ){#the inputs are in the form of lists, so std should be too
      #case where there is only one x values for all the y's
      if (typeof(noxlabels) != typeof(list())  ) {
        #since noxlabels is a single vector, have to generate std # of lists 
        noxlabs <- rep(list(noxlabels),length(std)) #no of times you have to plot
        nomeans <- as.list(means)
      }
      else {#fdefault, x contains a list of dfs
        noxlabs <- lapply(noxlabels,function(x) x[,1]) #the x values in each list
        nomeans <- lapply(noxlabels,function(x) x[,2]) #the y values of each list
      }
      sapply(1:length(noxlabs),function(item) {
        #cat('\nitem',item)
        plotmeansd(means = nomeans[[item]],noxlabels = noxlabs[[item]],
                   std = std[[item]],colop=item,markersize = markersize,op=op,
                   orient = orient,epsilon = epsilon,linethick = linethick,logs = logs,lgscale = lgscale)
        # plotmeansd(means = noxlabels[[item]][,2],noxlabels = noxlabels[[item]][,1],
        #            std = std[[item]],colop=item,markersize = markersize,op=op,
        #            orient = orient,epsilon = epsilon,linethick = linethick)
      })  
    }    
    else{#the input is in the form of x and y  vectors
      #cat('\ncop',colop)
      #if multiple stddev plot them one after the other
      if(length(plcols)==0) plcolors <- c('black','red','blue','green','grey','magenta') #set the colors for plotting
      else plcolors <- plcols
      #calculate the std dev bar widths, but onyl if x labels are numeric, as a percentage
      #of markersize. for a markerssize of 1.75, epsilon is .04
      if(is.numeric(noxlabels)) epsilon <- epsilon * (markersize/1.75)#earlier: epsilon*(max(noxlabels)-min(noxlabels))#episolon*x-axis width 
      else epsilon <- epsilon*25 # default width is .1 from earlier 
      #liam fixed error where x and y axes are backwards
      #Also introduced options for flip axis, when you change the x and y axes. Then the locations of the means and the orientations of SEM bars has to be flipped, too
      if (op==1) {#op=1 means draw the means point, too, guess
        #cat('\nmeans',means,':',noxlabels)
        if(flipaxis==1) points(noxlabels,means,pch=plsymbol,cex=markersize*1,col=plcolors[colop])
        else points(means,noxlabels,pch=plsymbol,cex=markersize*1,col=plcolors[colop])
      }
      #the std
      linewd <- linethick * (markersize/1) #new fix #epsilon*5 * (markersize/1) #setting the line width based on epsilon&markerrsize
      if(flipaxis==1) assignVarVals(c('noxlabs','centers','stde'),list(noxlabels,means,std))
      else assignVarVals(c('noxlabs','centers','stde'),list(means,noxlabels,std))
      #cat('\norient flip',orient,flipaxis,' mean:',centers,':',noxlabels)
      if(length(logs)>1){#have to do logscale
        if(logs[1]==T) noxlabs <- log(noxlabs,lgscale)
        if(logs[2]==T) {
          stde <- (1/log(lgscale))*(stde/means) #propogation of errors. With log it is std(x)/x * log(10)
          centers <- log(centers,lgscale)
        }
      }
      if(orient==flipaxis) {
        #cat('\norient here',centers-stde,';',centers+stde,';\nstd',std,':',stde,'\nmeans',(std/means)*.434,'\nnolabels',noxlabels,':',1/log(lgscale))
        segments(noxlabs,centers-stde,noxlabs,centers+stde,lwd=linewd,col = plcolors[colop]) #the vertical bar
        segments(noxlabs-epsilon,centers-stde,noxlabs+epsilon,centers-stde,lwd=linewd,col = plcolors[colop])
        segments(noxlabs-epsilon,centers+stde,noxlabs+epsilon,centers+stde,lwd=linewd,col = plcolors[colop])
      }
      else{
        #cat('\norient here',noxlabels-std,';',noxlabs+std,';',std,'\n',means,'\nnolabels',noxlabels)
        segments(noxlabs-std,centers,noxlabs+std,centers,lwd=linewd,col = plcolors[colop])
        segments(noxlabs-std,centers-epsilon,noxlabs-std,centers+epsilon,lwd=linewd,col = plcolors[colop])
        segments(noxlabs+std,centers-epsilon,noxlabs+std,centers+epsilon,lwd=linewd,col = plcolors[colop])
      }
      #plotmeansd(means = y,noxlabels = x,std = stddev,op=2)
    }  
  }
  else return() #nothing to plot, so return
}

#given 2 vectors of means and std, and the x axis label vectors,
#this will draw a mean and std, where epsilon is the width of the bar
#if op =1, plot mean,op=2, dont plot the mean
#the plot is along the horizontal axis
#noxlabels: is every x axis point where you need to put the sd bar
#orient: whether the means and std should be along the x=1 or y=2 axis
#linewidth: width of the std lines
plotmeansdx <- function(means,std,noxlabels,epsilon=.02,op=1,linewidth=2,orient=1){
  cat('\nplotmeansdx',means-std,'\n',noxlabels,'\n',means+std,'\n',noxlabels)
  if(orient==1){
    # the mean
    if (op == 1) points(means,noxlabels,pch=19,cex=1.5)
    #the std
    segments(means-std,noxlabels,means+std,noxlabels,lwd=linewidth)
    segments(means-std,noxlabels-epsilon,means-std,noxlabels+epsilon,lwd=linewidth)
    segments(means+std,noxlabels-epsilon,means+std,noxlabels+epsilon,lwd=linewidth)
  } else {
    # the mean
    if (op == 1) points(noxlabels,means,pch=19,cex=1.5)
    #the std
    segments(noxlabels,means-std,noxlabels,means+std,lwd=linewidth)
    segments(noxlabels-epsilon,means-std,noxlabels+epsilon,means-std,lwd=linewidth)
    segments(noxlabels-epsilon,means+std,noxlabels+epsilon,means+std,lwd=linewidth)
    
  }
}


#this function plots a horizontal bar plot
#change it so that all parameters are a list of arguments
#input: a data frame with the data in columns. Will average and do the sds. Could also potentially be a list
#if fixaxis is 1, automatically choose highest and lowest, otherwise choose the fixaxis
#vector
#color: specified the color palette, g - grey scale, rgb - basically color, b - blue color scale, 's' -same color all scales
#colorsel: spcifies the level of grey, between 1 and 6 with 1 being the darkest  
#sepwidth: denotes the tick width
#spaces: space between bars. default: 0.05
#bordcol: NA no border, or whatever color is specified in border
#angle: the angle for the x,y-axis labels
#outputop: 0 - do nothing; 1- if you want to print se and means
#grpspace: the space between groups. actual value is 2 *grpspaces
#linewidth: thickness of the std lines
#sem: T plot sems, F do not plot sems
#op =1 default, 2 - only one row/vector to print, 3 - multiple rows, with each row being one group. No of groups are the number of rows
#4  - if you want to stack 'em
#op= 3 and 4 specify the condition where there are groups and sub-groups, where each sub-group is 
#repeared across the groups, like knn, LDA results for dissimilar and similar cells.
HorzBarPlot <- function(dat.df, color = 'g',colorsel=1,rndfact=1,fixx=1,ticknox=1,lastx=0,sepwidth=1,spaces=0.05,grpspace=2.5,angle=c(),
                        font=c(2,2),barwidth=1,linewidth=1,logs=F,horz=T,bordcol=NA,grplabel=F,outputop=0,sem=T,op=1) 
{
  if(grplabel) grplabs <- rownames(dat.df) #if specified, group labels
  else grplabs <- c()
  grp <- T # = T for op=1-3, F for 4 
  #assign bar colors
  barcolors <- switch(color,
                      'g' = const.plgreycolors[1:length(means)], #greyscale 
                      'c' = const.plcolors[1:length(means)], #color scale
                      'b' = const.plbluecols[1:length(means)], #blue color scale
                      's' = rep(const.plgreycolors[colorsel],length(means)), # same color all scales
  )
  if(op==3 || op==4){#we are grouping by rows or stacking by rows
    data.lst <- unlist(dat.df[1,])
    barspaces <- c(spaces,grpspace*spaces) #specifies spaces between sub-groups and groups
    means <- t(as.matrix(dat.df)); sd <- rep(0,length(means));ranmeans <- means
    # if(color=='g') barcolors <- const.plgreycolors[1:ncol(dat.df)] #greyscale or color scale
    # else barcolors <- const.plcolors[1:ncol(dat.df)]  
    if(op==3){
      catlabs <- rep(colnames(dat.df),nrow(dat.df)) #nnames of categorical labels
    } else {
      catlabs <- rep(rownames(dat.df)) #nnames of categorical labels
      ranmeans <- apply(dat.df, 1, sum)
      grp <- F
    }
    #cat('\n',data.lst)  
  } else {#op=1 and 2
    #calculate the mean and sd of the data, and the xlabels
    data.lst <- dat.df
    means <- sapply(dat.df,mean);std <- sapply(data.lst,sd)/(length(data.lst[[1]]))^.5;ranmeans <- means
    barspaces <- spaces #specifies spaces between groups; no sub-groups here
    catlabs <- colnames(dat.df) #nnames of categorical labels
  }
  #common setup stuff
  #set the range of min and max y values for plotting
  #old: rangestats <- genRangeTicks(ranmeans,rndfact = rndfact,tickno = ticknox,fix=fixx,logs = logs,op=2)#op=2, force ticks from 0
  rangestats <- genRangeTicks(unlist(dat.df),rndfact = rndfact,tickno = ticknox,fix=fixx,logs = logs,op=2)#op=2, force ticks from 0
  assignVarVals(c('roundx','xrange','xticks'),rangestats)
  # value plus/minus a little round-off to accomodate the circle
  highestx <- max(dat.df)+roundx; lowestx <- 0 - roundx 
  #cat('\nxticks',xticks,', roundx:',roundx,'lowest,highestx:',lowestx,highestx,'means',means)
  par(pin=c(2,2),xpd=F)
  if ( outputop == 1) cat(means,", ",std)
  if(horz==T){
    bp <- barplot(means,width=barwidth*0.1, space = barspaces,beside = grp,horiz = horz)
    bp <- barplot(means, col = barcolors, border = bordcol,horiz = horz,xlim = c(lowestx,highestx),
                width=0.1*barwidth, space = barspaces, axes = F,yaxt="n",xaxt="n",beside = grp)
    plotTicksAxes(xticks = xticks,yticks = bp,ylabel = catlabs,xlabel = xticks,ypos = abs(bp[1]-bp[2])/2,font=font,
                  xpos = (highestx-lowestx)/15, las = 1,cex = 1.2,angle = c(0,0),tickwidth = sepwidth,horz = horz,grouplabels=grplabs)
  }
  else {#the bars are vertical
    #have to do bp first to get the xlim for getting the length of the x axis with the categorical variables
    bp <- barplot(means,width=barwidth*0.1, space = barspaces,beside = grp)
    bp <- barplot(means, col = barcolors, border = bordcol,horiz = horz,ylim = c(lowestx,highestx),xlim = c(0,bp[length(bp)]+bp[1]),
                width=0.1*barwidth, space = barspaces, axes = F,yaxt="n",xaxt="n",beside=grp)
    #cat('\nxpos',abs(bp[1]-bp[2]),'bp:',bp,':',0 - abs(bp[1]-bp[2]),'colmeans')
    plotTicksAxes(xticks = bp,yticks = xticks,xlabel = catlabs,ylabel = xticks,xpos = abs(bp[1]-bp[2])/2,font = font,
                  ypos = (highestx-lowestx)/15, las = 1,cex = 1.2,angle = c(90,0),tickwidth = sepwidth,horz = horz,grouplabels=grplabs)
  }
  #adding the points, means, SD
  #points(as.matrix(data.lst),rep(bp,each=3),pch=21,col="black",cex=2)
  #make the points into a DF of x and y points, and plot means and SD
  if(op==1){
    if (horz) tmp <- makeXYDf(data.lst=data.lst,means = bp)
    else tmp <- makeXYDf(data.lst=data.lst,means = bp,op=2)
    #print(tmp);print(data.lst)
    points(tmp,pch=const.plsymbols[3],col="black",cex=2) #symbol three is a circle
  }
  if(sem) plotmeansdx(means,std,bp,epsilon=.02,op=2,orient = ifelse(horz,1,2),linewidth = linewidth)

}

#plots the ticks for the x and y axes, and also the axes as required
#xticks,yticks: gives the positional values of the xticks and yticks
#xlabel,ylabel: the string values for the ticks
#xpos,ypos: the placement of the labels beyong the axes. the distance from the axes
#axeop: F - no axes, T - plot axes
#angle: orientation angle c(x orinetation, y orientation)
#horz: T - the plot or y valuse are along the x axis, F - vice-versa
#font: font size (x axis,y axis)
plotTicksAxes <- function(xticks,yticks,xlabel=c(),ylabel=c(),xpos=0,ypos=0,angle=c(45,0),font=c(2,2),axeop=F,cex=1.2,las=1,
                          tickcol='white',tickwidth=1,horz=F,grouplabels=c(),op=1){
  #x axis
  yposn <- 0 - ypos #(highestx-lowestx)/15
  #cat('\nx',ypos,';',horz)
  adjv <- ifelse(horz,c(0.5,0.5),c(1,0.5))
  text(x=xticks,y=yposn,srt=angle[1],adj = adjv,labels = xlabel,xpd=T,las=las,cex=cex,font=font[1])#adj centers the label
  
  #y-axis
  xposn <- 0 - xpos #abs(bp[1]-bp[2])/2
  #cat('\ny',ypos,';',xticks,',x ',xposn)
  text(x=xposn,y = yticks,adj = 1,srt=angle[2],las=las,cex=cex,font=font[2],labels=ylabel,xpd=T)
    
  #group labels if called for.
  nolabs <- length(grouplabels)
  if(nolabs>0) {#group labels
    if(!horz) text(x=xticks[seq(ceiling(nolabs/2),length(xticks),nolabs)],y=yposn*8,srt=0,adj = c(0.5,0.5),labels = grouplabels,xpd=T,las=las,cex=cex,font=font[1]*0.5)  
    else text(y=yticks[seq(ceiling(nolabs/2),length(yticks),nolabs)],x=xposn*8,srt=0,adj = c(0.5,0.5),labels = grouplabels,xpd=T,las=las,cex=cex,font=font[1]*0.5)  
    #cat('\ngrplabsl',xticks[seq(ceiling(nolabs/2),length(xticks),nolabs)],';',grouplabels,':',horz)
  } 
  
  #specify the axesless ticks on the data
  if(horz==F) abline(h=yticks, col=tickcol, lwd=tickwidth)
  else abline(v=xticks, col=tickcol, lwd=tickwidth)
  T  
}

#this function plots a horizontal bar plot
#change it so that all parameters are a list of arguments
#input: a data frame with the data in columns. Will average and do the sds
#if fixaxis is 1, automatically choose highest and lowest, otherwise choose the fixaxis
#vector
#data.df: the data is taken along the rows, i.e., each bar contains data from the same row.
#std: the standard deviation for the data. THe size is 
#color: the color of the bars. default: 'g': grey, 'c': normal colors, 'b': the blue shade paletter
#fontsize: controls the size of the y-axis font
#sepwidth: the width of the xticks white line that runs through
#sepcolor: the color pf the separator
#spaces: space between bars. default: 0.05
#angle: the angle for the x axis label
#linewidth: specifies the width of the line for the stdev
#borderop: to border or not T: borderr, F - no border
#width: specifies the widths of the bars: 1 - specifies the same width for all
#c(1,2,1,1,...): assigns bar widths according to their proportions of the total
#op =1 default, 2 - print se and means
stackedHorzBarPlot <- function(data.df, std=c(),color = 'g',rndfact=1,fixx=1,ticknox=1,lastx=0,spaces=0.05,
                               borderop=F,logs=F,horz=T,fontsize=1,sepwidth=0.5,sepcolor="white",legend=F,
                               angle=0,linewidth=1,width=1,op=1) {
  #calculate the mean and sd of the data, and the xlabels
  xlabels <- rownames(data.df)
  dat <- c(unlist(data.df),apply(data.df,1,sum))
  #sets the range of min and max y values for plotting
  rangestats <- genRangeTicks(dat,rndfact = rndfact,tickno = ticknox,fix=fixx,logs = logs)
  assignVarVals(c('roundx','xrange','xticks'),rangestats)
  cat('\nround',roundx,'xrange',xrange,'xticks',xticks)
  highestx <- xrange[2]+roundx;lowestx <- 0
  #cat('\nticks',xticks,',',roundx,lowestx,highestx,'\n')
  #par(oma=c(1,0,1,2))
  par(pin=c(2.5,2),xpd=F)
  #par(xpd=F)# makes sure that the lines are drawn within the frame plot region
  #set the color array
  if(length(color)==1){
    colarr <- c(1:3);names(colarr) <- c('g','c','b') 
    colour <- switch(colarr[color],const.plgreycolors,const.plcolors,const.plbluecols)
    cat('\ncolors ',colour,' ',const.plcolors)
  }
  else colour <- color
  #bp denotes the y axis midpoints of the barplot data, col = colors()[c(23,89,12,10,20)],
  if(horz==T) bp <- barplot(t(as.matrix(data.df)),col=colour,border = borderop,space = spaces,horiz = horz,
                            xlim = c(0,highestx),width=width,  axes = T,yaxt="n",xaxt="n")
  else bp <- barplot(t(as.matrix(data.df)),col=colour,border = borderop,space = spaces,horiz = horz,
                     ylim = c(0,highestx),width=width,  axes = T,yaxt="n",xaxt="n")

  #barplot(t(as.matrix(data.df)),col=colors()[c(23,89,12,10,20)] ,border="white",space=0.04,font.axis=2,xlab="group",horiz = T)
  #cat('\nbp',bp,':',xticks,highestx)
  if(horz)  {#if you want a horizontal bar plot
    cat('labels',row.names(data.df),'bp',bp)
    axis(side=2, at=bp,labels=rownames(data.df),las=1,tick=F,cex.axis=1.2*fontsize,mgp=c(1,.25,.1))
    abline(v=xticks,col=sepcolor,lwd=sepwidth)
    if(xticks[length(xticks)] > max(dat)) xticks <- xticks[-length(xticks)]
    #mgp is axis location labels, tick mark labels, and the third is the tick marks
    axis(side=1, at=xticks,tick=F,cex.axis=1.4*fontsize,mgp=c(0,0.01,0))
  }
  else {#vertical bar plot
    axis(side=1, at=bp,labels=rownames(data.df),las=1,tick=F,cex.axis=1.2*fontsize,mgp=c(1,.25,.1))
    abline(h=xticks,col=sepcolor,lwd=sepwidth)
    if(xticks[length(xticks)] > max(dat)) xticks <- xticks[-length(xticks)]
    #mgp is axis location labels, tick mark labels, and the third is the tick marks
    axis(side=2, at=xticks,tick=F,cex.axis=1.4*fontsize,mgp=c(0,0.01,0),las=1)
  }
  if(length(std)!=0){
    means <- apply(data.df,1,sum) #get the sum of variables: sum up the rows
    cat('\nmeans',means,'std',std,'bp',bp)
    plotmeansdx(means,std,bp,epsilon=.0005,op=2,orient = ifelse(horz,1,2),linewidth = linewidth)
  }
  #legend. legend=legend,las=0.1,args.legend = list(bty = 'n', x = 'topright')
  if(legend){#if we need a legend.
    opar = par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
    #need to source legendxx in the extra directory in order to use box.cex
    legend(x = "right", legend = colnames(data.df), fill = colour, bty = "n", ncol = 1, y.intersp = 2,box.cex = c(2,2))
    par(opar) # Reset par
  }  
}

#https://www.r-graph-gallery.com/211-basic-grouped-or-stacked-barplot.html

#given a vector, draws a a grid plot which divides the vector into  n cols or n rows
#vec: vector to be plotted
#splitdim: how it should be split, into rows of size splitdim
#byrow: T if split by rows, i.e. each row has splitdim elements, F if split by cols
#if you say byrow then the vector is spread byrow, and vice_versa
drawHeatMapPlot <-function(vec,splitdim=10,byrow=T,op=1){
  if (length(dim(vec)) == 2) {
    if(length(splitdim)==2) mat <- vec[1:splitdim[1],1:splitdim[2]]
    else mat <- vec[1:splitdim,1:splitdim]
  } else {
    nr <- length(vec)/splitdim
    if(byrow) mat <- matrix(vec,nrow = nr,byrow = T)
    else mat <- matrix(vec,ncol = nr,byrow = F)
  }
  res <- mat
  names(res) <- NULL
  #print(res)
  #heat map without clustering or labelled rows and columsn
  heatmap(res,scale='none',Rowv = NA,Colv = NA,labRow = '',labCol = '',
          col=paste("gray",1:99,sep=""))
  #heatmap(mat,Rowv = NA,Colv = NA,col=paste("gray",1:99,sep=""))
}


#this takes a plot area of 100 by 100 and draws either a vector or matrix of grid plots.
#vec: the vector that you want to draw
#col: specifies the color to be used, by default gray
#grayscale: if T uses grayscale at a 100 levels
drawVecGridPlot <-function(vec,col='gray',grayscale=T,op=1){
  #first make it square
  par(pin=c(2.5,2.5))
  #now define the plot area
  plot(c(0, 100),c(0,100), type = "n", xlab = "", ylab = "",frame.plot = F,xaxt='n',yaxt='n')
  #now define the wd and ht
  no <- length(vec)
  wd <- 100/no
  ht <- 100/no
  #now define the positions of the boxes
  xleft <- c(0:(no-1))*wd 
  xright <- xleft + wd
  yleft <- rep(0,no)
  yright <- yleft + ht
  #cat(xleft,yleft,xright,yright)
  if(grayscale==T) {
    high <- max(vec)
    low <- min(vec)
    #reverse since gray scale has 0 as black while we want 100 to be black
    val = 100 - floor((low-vec)*100/(low-high))#normalizes val to be between 0 and 100
    color=paste(col,val,sep="")
  }
  else color = vec*100
  #cat(color)
  rect(xleft,yleft,xright,yright,col=color)
}


#this takes a plot area of 100 by 100 and draws either a vector or matrix of grid plots.
#vec: the matrix that you want to draw
#col: specifies the color to be used, by default gray
#grayscale: if T uses grayscale at a 100 levels
drawMatGridPlot.old <-function(vec,col='gray',grayscale=T,op=1){
  #first make it square
  par(pin=c(2.5,2.5))
  #now define the plot area
  plot(c(0, 100),c(0,100), type = "n", xlab = "", ylab = "",frame.plot = F,xaxt='n',yaxt='n')
  #first calculations for conversions from 1 dim to 2 dim
  #total size 
  size <- length(vec)
  nr <- dim(vec)[1]
  nc <- dim(vec)[2]
  cat('\n',size,nr,nc)
  #now define the wd and ht
  no <- length(vec)
  wd <- 100/nr
  ht <- 100/nc
  xleft <- (c(0:(size-1)) %% nr) * wd
  yleft <- floor(c(0:(size-1)) / nr) * wd
  #now define the positions of the boxes
  xright <- xleft + wd
  yright <- yleft + ht
  if(grayscale==T) {
    high <- max(vec)
    low <- min(vec)
    #reverse since gray scale has 0 as black while we want 100 to be black
    val = 100 - floor((low-vec)*100/(low-high))#normalizes val to be between 0 and 100
    #cat(val[,1],vec[,1],xleft[c(1:10)],yleft[c(1:10)])
    val <- t(val) #have to transpose it if you want the first row of vec to be the bottom row
    color=paste(col,val,sep="")
  }
  else color = vec*100
  #cat(color)
  rect(xleft,yleft,xright,yright,col=color)
}

#takes a matrix as input, and then draws rectangles for each entry, and the color of the entry is weighted by the value
#of that element. 
#vec: the matrix that you want to draw
#col: specifies the color to be used, by default gray
#grayscale: if T uses grayscale at a 100 levels
#levels: no of levels of the color, distributed linearly from 0 to 255. Most of the visible variation lies
#in the range of 0 to 100. levels controls this upper number.
#xisize and ysize give the size of the boxes, the higher the score the bigget the range
#coloffset: to get some control of the upper level of the color spectrum. Most likely not needed
drawMatGridPlot <-function(vec,col='gray',grayscale=T,levels=1,xsize=1,ysize=1,coloffset=0,op=1){
  #first make it square
  par(pin=c(2.5,2.5))
  #par(mai=c(1,0,1,2))
  #now define the plot area: the larger the 50/size the smaller the individual box sizes will be:
  #idea is to draw a plot and then not plot the frame or anything
  plot(c(0, 50/xsize),c(0,50/ysize), type = "n", xlab = "", ylab = "",frame.plot = F,xaxt='n',yaxt='n')
  #determine boxsize: boxsize specifies the size of the both
  nr <- ncol(vec)
  nc <- nrow(vec)
  if(nr>nc) boxside <- 100/nr
  else boxside <- 100/nc
  #cat('\nboxsize',boxside)
  xleft <- sapply(1:nc, function(i) (i-1)*boxside )
  xright <- xleft + boxside
  ybottom <- sapply(1:nr, function(i) (i-1)*boxside )
  ytop <- ybottom + boxside
  #this is all very confusing, just use lighten and elimitnate the res
  deltacol <- (levels*90)/(max(vec)-min(vec)) #specifies the scale multiplier to get the desired color range effect
  lapply(1:nr,function(i){
    lapply(1:nc,function(j){
      #cat('\t',j,val,vec[j,i],valcol,';')
      valcol <- lighten(col,factor = vec[j,i] *(deltacol) + coloffset )
      #cat('\t',vec[j,i] *(deltacol))
      rect(xleft = (j-1)*boxside,ybottom = (i-1)*boxside,xright = (j)*boxside,ytop = (i)*boxside,col=valcol)
    })  
  })
  T
}

#similar to drawMatGridPlot except here each column gets its own color
#vec: the matrix that you want to draw
#col: specifies the color to be used, by default gray
#grayscale: if T uses grayscale at a 100 levels
#levels: no of levels of the color, distributed linearly from 0 to 255. Most of the visible variation lies
#in the range of 0 to 100. levels controls this upper number.
#xisize and ysize give the size of the boxes, the higher the score the bigget the range
#coloffset: to get some control of the upper level of the color spectrum. Most likely not needed
drawMatGridPlotCol <-function(vec,cols=c('gray'),grayscale=T,levels=1,xsize=1,ysize=1,coloffset=0,op=1){
  if(ncol(vec) > length(cols) ) ccols <- const.plcolors[1:ncol(vec)]
  else ccols <- cols
  #first make it square
  par(pin=c(2.5,2.5))
  #par(mai=c(1,0,1,2))
  #now define the plot area: the larger the 50/size the smaller the individual box sizes will be:
  #idea is to draw a plot and then not plot the frame or anything
  plot(c(0, 50/xsize),c(0,50/ysize), type = "n", xlab = "", ylab = "",frame.plot = F,xaxt='n',yaxt='n')
  #determine boxsize: boxsize specifies the size of the both
  nr <- ncol(vec)
  nc <- nrow(vec)
  if(nr>nc) boxside <- 100/nr
  else boxside <- 100/nc
  #cat('\nboxsize',boxside)
  xleft <- sapply(1:nc, function(i) (i-1)*boxside )
  xright <- xleft + boxside
  ybottom <- sapply(1:nr, function(i) (i-1)*boxside )
  ytop <- ybottom + boxside
  #this is all very confusing, just use lighten and elimitnate the res
  deltacol <- sapply(1:ncol(vec),function(i) levels*90/mean(vec[,i])   )#specifies the scale multiplier to get the desired color range effect
  #cat('\ndMGP',str(deltacol),vec[,2],vec[,3],vec[,4],';',nc)
  lapply(1:nr,function(i){
    lapply(1:nc,function(j){
      #cat('\n',j,';',str(ccols))
      valcol <- lighten(ccols[i],factor = vec[j,i] *(deltacol[i]) + coloffset )
      #if(i==1) cat('\t ',ccols[i],i,valcol,vec[j,i] *(deltacol[i]) + coloffset, deltacol[i] )
      #cat('\t',vec[j,i] *(deltacol))
      rect(xleft = (j-1)*boxside,ybottom = (i-1)*boxside,xright = (j)*boxside,ytop = (i)*boxside,col=valcol)
    })  
  })
  T
}



#generate the appropritate pie-chart
#dat: data as a vector
#select: only draw the labels for these, if empty draw all labels
#radius: of the pie chart; determines its size
drawPieChart<-function(dat,select=c(),radius=2,op=1){
  if(is.null(select)) labs <- dat
  else{
    labs <- rep('',length(dat))
    labs[select] <- select
  }
  pie(dat,labs,radius = radius)
}

#ok, given a color palette, draws all the boxes with a gap between them
#the logic is to just draw a stacked barplot, but make the separation width big, and making sure the width
#of the bar and height are about equal
#colpal: the color palette
drawColorPalette <- function(colpal,op=1){
  pal <- const.plbluecols
  
  dat <- seq(1,length(colpal),1)
  dat.df <- rbind.data.frame(rep(1,length(colpal)))
  cat(str(dat.df))
  #stackedHorzBarPlot(transposeDF(dat.df),color = 'b')
  stackedHorzBarPlot(dat.df,color = colpal)
}


#graphical pttarams that have worked
graphparams.saved <- function(){
  #omi params, the way things were before
  par(omi=c(0,1.2,1.5,2))
  #in order for the points to be not cutoff on the right, specifify xpd
  
}


#given a vector of values, prepares it in a form to be plotted by fpotew
#gap: if you want a gap between the points, gap = 1 consecutive, gap = 2 alternate
makeVecForFplot <- function(vec,gap=1,op=1){
  len <- length(vec)
  res <- data.frame((1:len)*gap,vec)
  res
}


#given a list of vectors, prepares it in a ofmr to be plotted by fploteq
#wherein 
#gap: if you want a gap between the points, gap = 1 consecutive, gap = 2 alternate
makeVecForFplotList <- function(veclst,gap=1,op=1){
  res <- lapply(veclst,function(x) makeVecForFplot(x,gap = gap,op=op))
  res
}

#these are the params from the fploteq function, gets passed on with the ... params
#x,y=c(),eq=F,xlabel='x',ylabel='y',title='',
#markersize=1.75,logs=F,lgscale=10,factors=c(),rndfact=1,ticknox=1,ticknoy=1,
#fixy=1,fixx=1,fixop=1,plottype=c(),digits=3,
#full=T,stddev=c(),aspect=1,op=1)
#given data in a data frame format, will plot column 1 by default against all other columns
fploteq.DF <-function(dat.df,...,col=1){
  nocols <- length(dat.df)
  #fploteq(x=dat.df[,col],y=dat.df[,-col])
  fn.params <- c(list(x=dat.df[,col],y=dat.df[,-col]),...)
  do.call(fploteq,fn.params)
}

#-----------------------------------------

#some helper functions
#given a set of x points, and equation or equations, gets the correspoding y points
#x : set of x points for which you want to calcualte the Y points
#eq: an equation or list of equations
getEqnPoints <- function(x,eq,op=1){
  #get the first and last points of x; if it is monotonic same as getting endpoints.
  #changed old function of getNextValInf to getHighestValInf
  if (typeof(eq) == typeof(list(1))){ #a list of functions getNextValInf
    vecy <- sapply(eq,function(xfn) {
      xfn(getHighestValInf(x,max(x),eq = xfn) )
    })
  }
  else  vecy <- eq(getHighestValInf(x,max(x),eq = eq) )
  vecy
}


#this function draws a list polot similar to the mathematica plots. highlifhgts include
#dropping a line to an imaginary horizontal line, absenting the x a-axis, making the line
#visible, keeping tiocks but not the axis
#most of the params are the same as the fploteq
fplotlist <- function(dat,...,op=1){
  
}

#this function implements the sparklines advocated by Edward Tufte
#type: of sparkline, 1 - line, 2 - binary lines, 3 bar plots, 4
sparkline <- function(dat,type=1,op=1){
  
}

#old stuff, might not need anymore - superannuated 
#--------------------------------------------------------

#this function draws a strip chart with just the means and sd without any of the
#inidividual data points
#data.lst: the data input is a list of data frames rather than a single one
#data.lst is a list of vectors of the different elements to be plotted
#methodstr: for ex: jitter
#rndfact: the nearest power of 10 mulitiplied by rndfact, for example 5 * 1000
#tickno: divvides the number of ticks by tickno
#op =1 default, 2 - print se and means
fchartvecs <-function(data.lst,methodstr='jitter',markersize=1.75,rndfact=1,
                      tickno=2,fixy=1,op=1){
  x <- names(data.lst) # get the x and y data points
  y <- cleanNA(unlist(data.lst))
  #y value that is rndfact times the highest pow of 10 lower than min(y)
  #fix: so that y is not 0
  roundy <- (10^getpow10(min(y[y>0])))*rndfact*.1
  #cat('fchart',y,min(y),roundy,'\n')
  #sets the range of min and max y values for plotting
  yrange <- c(getNearestMutiple(min(y),roundy,1),getNearestMutiple(max(y),roundy,2))
  yticks <- seq(getNearestMutiple(min(y),roundy,1),getNearestMutiple(max(y),roundy,2),
                roundy*tickno) # the no of y ticks  
  options(scipen = 10)
  cat(roundy,getNearestMutiple(min(y),roundy,1),getNearestMutiple(max(y),roundy,2),
      roundy*tickno,'\n')
  if ( length(fixy) > 1 ) { #pre-specified y axis labels
    yrange <- fixy
    yticks <- seq(from = fixy[1],to=fixy[2],roundy*tickno) # the no of y ticks
    #cat('\n',yticks,", ",roundy,', ',tickno) #another way to print
  }
  #calculate the mean and sd of the data, and the xlabels
  layermean <- sapply(data.lst,function(x) mean(x,na.rm = T))   
  std <- sapply(data.lst,function(x) sd(x,na.rm = T)/(length(x)^.5))
  if ( op == 2) cat(layermean,", ",std)
  xlabels <- names(data.lst)
  nox <- c(1:length(xlabels))
  #par(mar=c(2,5,2,4)) #adjust the outer and inner marggines
  #par(oma=c(2,4,2,4))
  par(font.axis = 2) #make axes bold again, Haha!
  par(pin=c(1,2.5),xpd=NA) #adjust size of the display, cex=1.5
  #par(pin=c(1,2.25),xpd=NA) #adjust size of the display
  cat('lmean',layermean)
  stripchart(as.list(layermean),
             vertical=TRUE,pch=19,col="gray",xlim=c(1,2),yaxt="n",ylab="",cex=markersize,
             ylim=yrange,frame.plot = FALSE,method=methodstr,
             xaxt="n",lwd=2,las=1,cex.axis=1.5)
  #do the y axis labels,mgp, p controls the axis posn, line controls label posn
  #if p is higher and line are higher they move the axis and label further away
  axis(side=2,at=yticks,labels=rep("",length(yticks)),mgp=c(3,1,1),lwd=2)
  mtext(text=as.character(yticks),side=2,at=yticks,las=1,cex=1.2,font=2,line=1.75)
  #do the x axis labels
  axis(side=1,at=nox,labels=rep("",length(nox)),mgp=c(3,1,.5),lwd=2)
  mtext(text=xlabels,side=1,at=nox,las=1,cex=1.2,font=2,line=1.75)
  plotmeansd(layermean,std,nox,epsilon = .1)
}



fstripchartvecs.old <-function(data.lst,methodstr='jitter',markersize=1.75,rndfact=1,lgscale=10,
                           logs=F,tickno=2,fixy=1,angle=T,sem=1,semthick=5,xlabel='',ylabel='',markersizemean=markersize,
                           xcat=T,orient=1,pairplot=0,op=1){
  x <- getListNames(data.lst) #names(data.lst) # get the x and y data points
  y <- cleanNA(unlist(data.lst))
  #y value that is rndfact times the highest pow of 10 lower than min(y) 
  #fix: so that y is not 0
  options(scipen = 10)
  #sets the range of min and max y values for plotting
  rangestats <- genRangeTicks(y,rndfact = rndfact,tickno = tickno,fix=fixy,logs = logs)
  assignVarVals(c('roundy','yrange','yticks'),rangestats)
  
  #calculate the mean and sd of the data, and the xlabels
  layermean <- sapply(data.lst,function(x) mean(x,na.rm = T))   
  std <- sapply(data.lst,function(x) switch(sem,sd(x,na.rm = T)/(length(x)^.5),
                                            sd(x,na.rm = T),1.96*sd(x,na.rm = T)/(length(x)^.5),
                                            0))
  #cat('\nmean',layermean,'stdev',std)
  #if there are no names specified for the lists, then assign numbers as the list names
  if(length(names(data.lst))>0)
    xlabels <- names(data.lst)
  else  xlabels <- as.character(1:length(data.lst))
  nox <- c(1:length(data.lst))
  par(mai=c(2,0,2,0)) #adjust the outer and inner marggines
  par(oma=c(2,0,2,0))
  par(font.axis = 2) #make axes bold again, Haha!
  #par(oma=c(1,0,1,1))
  par(pin=c(.5,2.25),xpd=NA) #adjust size of the display
  stripchart(data.lst,
             vertical=TRUE,pch=19,col="gray",xlim=c(1,2),yaxt="n",ylab="",cex=markersize,
             ylim=yrange,frame.plot = FALSE,method=methodstr,
             xaxt="n",lwd=2,las=1,cex.axis=1.5)
  #do the y axis labels,mgp, p controls the axis posn, line controls label posn
  #if p is higher and line are higher they move the axis and label further away
  #axis(side=2,at=yticks,labels=rep("",length(yticks)),mgp=c(3,1,1),lwd=2)
  #mtext(text=as.character(yticks),side=2,at=yticks,las=1,cex=1.2,font=2,line=1.75)
  #do the x axis labels
  #cat(nox,xlabels,length(data.lst))
  #nox <- c(1,2)
  #axis(side=1,at=nox,labels=rep("",length(nox)),mgp=c(3,1,.5),lwd=2)
  #if (angle) text(x=nox,par("usr")[3]-9.5,srt=45,adj=1,labels = xlabels,xpd=T,las=1,cex=1.2,font=2)
  #else mtext(text=xlabels,side=1,at=nox,las=1,cex=1.2,font=2,line=1.75)
  plotStripAxes(xticks = xlabels,yticks=yticks,xlabel = xlabel,ylabel = ylabel,
                lgscale = lgscale,logs = logs,xcat = xcat,angle=angle)
  plotmeansd(layermean,std,nox,epsilon = .1,markersize = markersizemean,linethick = semthick,orient = orient)
  if(pairplot>0) drawLines(xpts = nox,ypts = data.lst,op=pairplot)
}


fstripchartvecs1.old <-function(data.lst,methodstr='jitter',markersize=1.2,rndfact=1,lgscale=10,
                           logs=F,tickno=2,fixy=1,angle=T,sem=1,semthick=2,xlabel='',ylabel='',markersizemean=markersize,
                           xcat=T,orient=1,pairplot=0,op=1){
  x <- getListNames(data.lst) #names(data.lst) # get the x and y data points
  #cat('\nfstrip',getListNames(data.lst),'x:',length(x))
  y <- cleanNA(unlist(data.lst))
  #y value that is rndfact times the highest pow of 10 lower than min(y) 
  #fix: so that y is not 0
  options(scipen = 10)
  #sets the range of min and max y values for plotting
  rangestats <- genRangeTicks(y,rndfact = rndfact,tickno = tickno,fix=fixy,logs = logs)
  assignVarVals(c('roundy','yrange','yticks'),rangestats)
  
  #calculate the mean and sd of the data, and the xlabels
  layermnstd <- getMeanSds(data.lst = data.lst,sem = sem)
  #if there are no names specified for the lists, then assign numbers as the list names
  if(length(x)>0)
    xlabels <- x
  else  xlabels <- as.character(1:length(x))
  nox <- c(1:length(x))
  #cat('\nnox',nox,',',c(1:length(x)))
  par(mai=c(2,0,2,0)) #adjust the outer and inner marggines
  par(oma=c(2,0,2,0))
  par(font.axis = 2) #make axes bold again, Haha!
  #par(oma=c(1,0,1,1))
  par(pin=c(.5,2.25),xpd=NA) #adjust size of the display
  plotMultipleStrips(data.lst = data.lst,markersize = markersize,yrange = yrange,methodstr = methodstr,plcolors = const.plcolors,plsymbols = const.plsymbols)
  #do the y axis labels,mgp, p controls the axis posn, line controls label posn
  #if p is higher and line are higher they move the axis and label further away
  plotStripAxes(xticks = xlabels,yticks=yticks,xlabel = xlabel,ylabel = ylabel,
                lgscale = lgscale,logs = logs,xcat = xcat,angle=angle)
  plotMultipleMeanSds(data.lst = layermnstd,std = std,nox = nox,markersizemean = markersizemean,semthick = semthick,
                      orient = orient,plcolors = const.plcolors,plsymbols = const.plsymbols)
  if(pairplot>0) drawLinesList(data.lst = data.lst,pairplot=pairplot,plcolors = const.plcolors) #draws lines between points
  #if(pairplot>0) drawLines(xpts = nox,ypts = data.lst,op=pairplot)
}


genRangeTicks.old.jul25_21 <- function(xy,rndfact,tickno,fix=1,fixop=1,logs=F,maxticks=10){
  cat('gRT',rndfact,'\n')
  #adjust so that in the non-log case roundxy is > 0
  roundxy <- ifelse(logs,1,(10^getpow10(min(abs(xy)[abs(xy)>0]))))
  if((abs(min(xy)-max(xy))/roundxy) > 10) roundxy <- (max(xy)-min(xy))/10 #we dont want too many ticks
  roundfact <- getNearestMultiple(roundxy,rndfact*10^getpow10(roundxy),op=3) #get the closet multiple of roundfactor
  if(roundfact == 0) roundxy <- rndfact*10^getpow10(roundxy) #and adjust roundxy according to whether it is less or greater than roundfact
  else roundxy <- roundfact
  cat('\nround',roundxy,';',(10^getpow10(min(abs(xy)[abs(xy)>0]))),',',roundfact,min(xy),abs(xy)>0)
  #if the numbner of ticks is too mych adjust rounding factor to have no more than maxticks, doesnt seem to help
  #if((max(xy)-min(xy))/roundxy > maxticks) roundxy<-10^getpow10((max(xy)-min(xy))/maxticks) #not sure you need this now
  #sets the range of min and max y values for plotting
  # #liam,take care of -ve nos: handled now
  starttick <- getNearestMultiple(min(xy),roundxy,1)
  endtick <- getRangeMultiple(starttick,max(xy),roundxy*tickno)
  rangexy <- c(starttick,endtick)
  ticks <- seq(from=starttick,to=endtick,roundxy*tickno) # the no ofticks  
  ticks <- adjustTicks(ticks = ticks,fix = fix,roundxy = roundxy,tickno = tickno,fixop = fixop)
  #set range too to reflect the new fix
  rangexy <- c(ticks[1],tail(ticks,1))
  if(length(fix)>2) {#if the ticks are hardcoded, nothing to be done
    ticks <- fix
    rangexy <- c(fix[1],fix[length(fix)])
  }
  cat('\n',ticks,'round',roundxy)
  
  #make sure there are no trailing decimal places
  ticks <- unique(as.numeric(format(round(ticks,decimalplaces(roundxy)),nsmall = decimalplaces(roundxy)) ) )
  list(roundxy,rangexy,ticks)
}

#a fuction to get a nice fancy plot
#input: the x vector, the y vector, xlabel, ylabel, title
#x : an x vector or a list of plotting data. If list each element contains x and y columns
#y : could be a y vector or a data frame of columns
#fixx,y : fixes the range of the axes
#fixop: 1 - make the extreme ticks go roundxy*tickno above the extremes, 2 - stop exactly at fix
#tickno: tells you how big the divisions should be. The fewer ticks you want, the higher thisno
#eq: one equation or a list of equations for multiple data
#full: whether you wnt to plot across the whole length of the axis (full) or just from smallest
#data point to highest data point
#stddev: the standard deviation or error to be plotted for each point. It g=has to follow 
#whatever format the data is in, which is x and y
#plottype: whether you want
#op=1, normal data x and y, =2 plot equations for a range of x. No Y specified,
fploteq.old <- function(x,y=c(),eq=F,xlabel='x',ylabel='y',title='',
                        markersize=1.75,logs=F,lgscale=10,factors=c(),rndfact=1,ticknox=1,ticknoy=1,
                        fixy=1,fixx=1,fixop=1,plottype=c(),digits=3,
                        full=T,stddev=c(),op=1){
  #write function to figure out default vector or data frame
  #process the X and y data to get the plot and allx and ally values
  #cat(str(x))
  tmp <- processPlotXY(x,y,logs = logs,lgscale = lgscale,ticknox = ticknox,
                       ticknoy = ticknoy,fixy = fixy,fixx = fixx)
  assignVarVals(c('allx','ally','plotx','ploty'),tmp)
  #calculate all the plot variables
  tmp <- genPlotVars(allx,ally,rndfact,ticknox,ticknoy,fixy,fixx,fixop=fixop,logs = logs)
  assignVarVals(c('roundy','yrange','yticks','roundx','xrange','xticks'),tmp)
  
  cat('range',xrange,',',xticks,',',yrange,',',yticks,'\n')
  #par(mar=c(2,5,2,4)) #adjust the outer and inner marggines
  par(oma=c(4,4,2,4))
  par(font.axis = 2) #make axes bold again, Haha!
  par(pin=c(2.5,2.5),xpd=NA) #adjust size of the display, cex=1.5
  
  #redundant:if((length(ploty)==1) && (length(plottype)==0)) plottyp <- 1 #only 1 column to plot
  cols <- 0
  if(length(plottype)==0) {#if no plottype specified, choose default options for 1/2 cols
    if(typeof(ploty)==typeof(list())) {
      if(typeof(plotx)==typeof(list())) plottyp <- 7 #both x and y are lists
      else plottyp <- 3 #one x, multiple y
      #cat('type',typeof())
    }
    else  plottyp <- 1 #one x and y
  } 
  else plottyp <- plottype
  cat('plottype',plottyp,'\t')
  fplottype(plotx,ploty,factors=factors,cols=cols,xrange=xrange,yrange=yrange,xlabel=xlabel,
            ylabel,markersize=markersize,
            title = title,plottype = plottyp)
  #do the stddev
  plotmeansd(noxlabels=x,means=y,std=stddev,markersize = markersize,op=2)
  
  #curve and plot are equivalent here
  #plot(eq,xlim = c(min(allx),max(allx)),add=T)
  
  #if (logs) pleq <- function(x) log(eq(lgscale^x),lgscale) #if it is logscale, change fn
  #else pleq <- function(x) eq(x)
  #curve(pleq,from =min(allx),to=max(allx), add = T)
  #check if a function or list of functions has been specified, if not skip equation fitting
  if (typeof(eq) == typeof(function(x) x) || typeof(eq) == typeof(list(1))) 
    plotcurves(eq,minx = min(allx),maxx=max(allx),logs = logs,lgscale = lgscale,
               xrange = xrange,yrange = yrange,full = full)
  #needed: yticks, xlabels,nox
  #do the x and y axis labels
  plotaxes(xticks,yticks,xlabel=xlabel,ylabel=ylabel,logs=logs,lgscale = lgscale)
}


