## Pending step is to generate inner functions to be called with different layouts



##' Function for visual inspection of dives (Both high resolution and summarized profiles should be provided)
##'
##' This functions generate a multiframe plot with information about the dive. I both accept single or multiple dives (class 'data.frame'). If multiple dives are to be visual inspected, to pass from one to the next, [enter] must be pressed. If capture is true, each time, before passing to the next dive, it will need a number, followed by two [enter] (e.g. [1][enter][enter]). numeric values for classification used in our project are the following. \cr
##' \itemize{
##' \item 1: Non drift dive
##' \item 2: true negative drift dive
##' \item 3: uncertain negative drift dive
##' \item 4: true positive drift dive
##' \item 5: uncertain positive drift dive
##' }
##' It can be expanded to include other kind of dives.
##' @title Visual dive examination
##' @param cand Summarized dive profile/s (class 'data.frame')
##' @param sel High resolution dive profile/s (class 'data.frame')
##' @param capture Logical. If TRUE, a numeric argument must be imputed for classification (e.g. 1 = non drift, 2, negative drift and so on). If this process is not followed until the end, it will generate an error, thus it is strongly recommended to classify dives in small bacthes of as much as 500. 
##' @param speed Logical. If TRUE, the High resolution data frame (sel) must include a column with speeds
##' @param lower Character string. It determines what to be plotted in the lower window: \cr
##' \itemize{
##' \item bout: it will print all the Hig Resolution Time-Depth information for a period of three hours, centered at the current dive (thus plotting a bout)
##' \item diff1: It will plot the first difference of the current dive
##' \item diff2: It will plot the second difference of the current dive
##' }
##' @param sr The sampling interval of the High resolution time-depth (in seconds). Tipically 4 or 30 secs
##' @param rv value for the rolling means and variances
##' @param format Character string. It sets the desired layout depending on what to be plotted
##' @return It will open a graphical window and plot the requested dive/s. If capture is set to TRUE, it will return a vector with the dive classification. The vector is linked to the dive number, and for non checked dives, it will return a 0 (e.g., if checked dives 5 and 7 as class 2, it will return the vector [0,0,0,0,2,0,2] )
##' @author Fernando Arce
diveExam <- function(cand,sel,capture=FALSE,speed=FALSE,lower='bout', sr= 30,rv=1,format='comp',ide='emacs',proc=FALSE){
    if (ide == 'emacs'){
        cat('\n','You are a cool dude!!\n','\n','\n','\n')
    } else if (ide == 'RStudio'){
        cat('\n', 'RStudio? Why?? :\'(\n','\n', 'You should give a try to emacs\n','\n','\n','\nSeriously!!\n')
    } else {
        cat('\n','WTF is', ide,'?','\n',  'You should give a try to emacs\n')
    }
    
    if (capture==TRUE) type <- numeric(max(cand$dive)) # vector for storing dive type

    ## shortcuts and function remaping
    rr <- round
    ff <- floor
    sel$cd <- sel$cordep
    fs <- as.POSIXlt(min(sel$Date))# first time at sea


    for (i in cand$dive){
        ## cleaning data and generating variables
        ## variables are generated to reduce posterior
        ## dimensions of lines and the script in general
        number = which(cand$dive==i)
        td <- sel[sel$dive==i,] ## old tmp.dive
        td <- Differences(td)
        td$time <- td$Date-min(td$Date)
        tp <- cand[cand$dive==i,] ## old tmp.proc
        tmp.start <- as.POSIXlt(min(td$Date))
        tit <-c(tp$T1,tp$T2,tp$T3,tp$T4) ## old name tmp.inflT
        RD <- td[td$cd > 10, ] # real diving action
        SD <- td[td$cd <=10, ] # surface period
        ma <- max(RD$time) - min(RD$time)
        
       

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## First plot
        ##
        FirstPlot <- function() {
        par(mar=c(0,4,3,0))
        plot(RD$time,RD$cd,type='l',col='dodgerblue',
             ylim=c(max(td$cd),0),
             xlim=c(0,max(td$Date - min(td$Date))),
             main=paste('Dive id =',i,'\n','Source=',name,'\n',
                 'Checked=',number,sep=' '),
             xlab='Time (sec)',ylab='Depth (meters)',bty=']',
             col.axis='gray30',col.lab='gray20',col.main='cornflowerblue',
             cex.main=.8,axes=FALSE,lwd=2)
        axis(2,at = c(0, rr(max(RD$cd),0)))
        segments(max(RD$time)/2,max(RD$cd)+max(RD$cd)/25,
                 max(RD$time)/2,max(RD$cd)-max(RD$cd)/20,
                 col='firebrick',lty=3,lwd=3)
        temp0 <- td[td$cd < 10, ]
        points(temp0$time,temp0$cd,col='grey',type ='l')
        axis(3,at = c(min(temp0$time),max(temp0$time)),
             labels=c(0,rr((max(temp0$time)/60-min(temp0$time)/60),1) ))
        if (lower == 'bout') axis(1,at=seq(0,ma,length.out=5),
                labels = rr(seq(0,ma/60,length.out=5),1))
        if (speed==TRUE) {
            tmp.drift <- td[td$Speed==0, ]
            points(tmp.drift$time,tmp.drift$cd, col='darkgoldenrod4',pch=19)
        }

        abline(v=c(min(RD$time),max(RD$time)/2,max(RD$time)),col='firebrick')
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        
        points(tit-4,c(tp$D1,tp$D2,tp$D3,tp$D4),
               pch=19,type='b',cex=2,col='black')
        points(tit,c(tp$D1,tp$D2,tp$D3,tp$D4),pch=19,cex=.5)
        

        ## temporary removed
        ## if (tp$ds==1){
        ##     points(c(tp$T1,tp$T2),c(tp$D1,tp$D2),pch=19,type='b',lwd=1.2,col='lightcoral')
        ## } else if (tp$ds==2){
        ##     points(c(tp$T2,tp$T3),c(tp$D2,tp$D3),pch=19,type='b',lwd=1.2,col='lightcoral')
        ## } else if (tp$ds==3){
        ##     points(c(tp$T3,tp$T4),c(tp$D3,tp$D4),pch=19,type='b',lwd=1.2,col='lightcoral')
        ## }
        ## points(tp$max.time,tp$max.depth,col='deeppink',cex=.8)

        ##abline(h=c(200,800),cex=.2,col='bisque1',lty=4)
        ##abline(v=seq(0,5000,1200),col='bisque1',cex=.2)
        ##tmp.mod <- lm(c(tp$D1,tp$D2,tp$D3,tp$D4)~tit)
        ##clip(min(tit),max(tit),-10000,10000)
        ##abline(tmp.mod,col='lightblue')

    }
        ## End of the first plot
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## Start of the second plot
        ##
        SecondPlot <- function(lower=lower) {
        par(mar=c(4,4,0,0))
        if (lower == 'bout'){
            start <- as.POSIXlt(min(td$Date) - 5400)
            end <- as.POSIXlt(max(td$Date) + 5400)
            bout <-sel[(sel$Date > start) & (sel$Date < end),]
            bout$t <- as.POSIXlt(bout$Date)
            plot(bout$Date,bout$cordep,type = 'l',ylim=c(max(bout$cordep),-40),
                 ylab='Depth (meters)',xlab='Day',axes = FALSE,col='darkgrey')
            axis(1,at=seq(min(bout$Date),max(bout$Date),length.out=3),
                 labels = c(paste(start$hour,':',start$min,sep=''),
                     paste (end$mday,'-',end$mon + 1,sep=''),paste(end$hour,':',end$min,sep='')))
            axis(2)
            ## para el eje de abajo necesito tres puntos, con labels, al principio la hora y en medio el dia, al final la hora
            points(td$Date,td$cd,type='l',col='black')
        } else {
            if (lower == 'diff1'){
                plot(RD$time[-1],RD$diff1[-1],type = 'l',xlab='',
                     ylab = 'diff1',xlim=c(0,max(td$time) - min(td$time)),
                     axes = FALSE,lwd=.5, ylim = c(1.2*min(RD$diff1[-1]),2*max(RD$diff1[-1])))
                mtext('Time (mins)',side=1,line= 2.5, at = ff(ma/2))
                axis(1,at=seq(0,ma,length.out=5),
                     labels = rr(seq(0,ma,length.out=5)/60,1))
                axis(2,at=c(rr(min(RD$diff1[-1]),2),0,rr(max(RD$diff1[-1]),2)))
                points(c(0,ma),(rep(mean(RD$diff1[-1]),2)),
                       type ='l',col='chartreuse4',lwd=.5,cex=.5)
                par(new = T)
                plot(RD$time[-c((1:(rv+1)),((nrow(RD)-rv):nrow(RD)))],
                     RD$diff1v[-c((1:(rv + 1)),((nrow(RD)-rv):nrow(RD)))],
                     type='l',xlab='',ylab='diff1',
                     xlim=c(0,max(td$time)-min(td$time)),axes=FALSE,lwd=.5,
                     col='red',ylim=c(-max(RD$diff1v[(rv+2):(nrow(RD)-(rv+1))]),
                                          max(RD$diff1v[(rv+2):(nrow(RD)-(rv+1))])))
                point <- max(RD$diff1v[(rv+3):(nrow(RD)-(rv+2))])
                axis(4,col='red',pos=max(RD$time)+20,at=c(0,rr(point/2,1),rr(point,1)))
                ## that's new
                ## abilne(v=c())
            } else {
                if (lower=='diff2'){
                     plot(RD$time[-c(1,2)],RD$diff2[-c(1,2)],type = 'l',xlab='',
                     ylab = 'diff2',xlim=c(0,max(td$time) - min(td$time)),
                     axes = FALSE,lwd=.5, ylim = c(1.2*min(RD$diff2[-c(1,2)]),2*max(RD$diff2[-c(1,2)])))
                mtext('Time (mins)',side=1,line= 2.5, at = ff(ma/2))
                axis(1,at=seq(0,ma,length.out=5),
                     labels = rr(seq(0,ma,length.out=5)/60,1))
                axis(2,at=c(rr(min(RD$diff2[-c(1,2)]),2),0,rr(max(RD$diff2[-c(1,2)]),2)))
                points(c(0,ma),(rep(mean(RD$diff2,na.rm=TRUE),2)),
                       type ='l',col='chartreuse4',lwd=.5,cex=.5)
                par(new = T)
                plot(RD$time[-c((1:(rv+2)),((nrow(RD)-rv):nrow(RD)))],
                     RD$diff2v[-c((1:(rv + 2)),((nrow(RD)-rv):nrow(RD)))],
                     type='l',xlab='',ylab='diff1',
                     xlim=c(0,max(td$time)-min(td$time)),axes=FALSE,lwd=.5,
                     col='dodgerblue',ylim=c(-max(RD$diff2v[(rv+3):(nrow(RD)-(rv+2))]),
                                          max(RD$diff2v[(rv+3):(nrow(RD)-(rv+2))])))
                point <- max(RD$diff2v[(rv+3):(nrow(RD))])
                axis(4,col='dodgerblue',pos=max(RD$time)+20,at=c(0,rr(point/2,2),rr(point,2)))
                    

                }
            }
        }


    }

        
        ## End of the second plot
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~
        
    ##     ##~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##     ## Start of the third plot
    ##     ##
    ##     ThirdPlot <- function(){
    ##     plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='',xlim=c(0,10),ylim=c(5,10))
    ##     text(3,10,paste('Seal ID =',name,sep=' ') ,pos=4)
    ##     td$Date <-  as.POSIXlt(td$Date)
    ##     text(3,9.7,paste('Year =',min(td$Date$year + 1900),sep=' ') ,pos=4)
    ##     text(3,9.4, paste('Date =',min(td$Date$mday),'-',months(min(td$Date)),sep=' '), pos=4)
    ##     text(3,9.1,paste('Days at sea =',rr(min(td$Date) - fs,1),sep=' ') ,pos=4)
    ##     text(3,8.8,paste('Dive duration =', rr(tp$DIVE_DUR/60,1),
    ##                      'mins',sep= ' '),pos = 4)
    ##     text(3,8.5,paste('order =', tp$order,sep= ' '),pos=4)
    ##     ##text(3,8.2,paste('DriftEst =',rr(tp$NDE,2),sep=' '),pos=4)
    ##     ##text(3,7.9,paste('nds =',rr(tp$NDE,2),sep=' '),pos=4)
    ##     text(3,7.6,paste('avratio =',rr(tp$avratio,2),sep=' '),pos=4)
    ##     text(3,7.3,paste('pseg1 =',rr(tp$propseg1,2),sep=' '),pos=4)
    ##     text(3,7,paste('pseg2 =',rr(tp$propseg2,2),sep=' '),pos=4)
    ##     text(3,6.7,paste('pseg3 =',rr(tp$propseg3,2),sep=' '),pos=4)
    ##     text(3,6.4,paste('mdepthb =',rr(tp$mdepthbias,2),sep=' '),pos=4)
    ##     text(3,6.1,paste('DivId = ', tp$DivId), pos=4)
    ##     text(3,5.8,paste('hp1 =', rr(tp$hp1,2), sep=' '),pos=4)
    ##     text(3,5.5,paste('hp2 =', rr(tp$hp2,2), sep=' '),pos=4)
    ##     text(3,5.2,paste('hp3 =', rr(tp$hp3,2), sep=' '),pos=4)
    ## }
        
            ## layout definitions
    Mat1 <- matrix(c(1,1,1,3,1,1,1,3,2,2,2,3),nrow=3,ncol=4,byrow=TRUE)
    Mat2 <- matrix(c(1,1,1,3,1,1,1,4,2,2,2,5),nrow=3,ncol=4,byrow=TRUE)

    ## layout selection
    if (format == 'predef') {
        layout(Mat1)
        FirstPlot()
        SecondPlot(lower='bout')
        ##ThirdPlot()
    } else if (format == 'comp') {
        layout(Mat2)
        FirstPlot()
        SecondPlot(lower='bout')
        SecondPlot(lower='diff1')
        SecondPlot(lower='diff2')
        ##ThirdPlot()
    }


        
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##
        ## Returning values (if dive selection is desired)
        
        if (capture==FALSE){
            cat ("Press [enter] to continue")
            line <- readline()
        } else {
            if (capture==TRUE) {
                type[i]=scan()          # I can change it by scan(what='') for scanning also lettersX
            }
        }
    }
    
    ##
    
    if (capture==FALSE){
        return(cat('Visualization process ended'))
    } else {
        if (capture==TRUE){
            return(type)
        }
    }
    
}

