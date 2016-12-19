library(shiny)

function(input, output){
  
  readData <- reactive({
    
    inputFile <- input$data
    if (is.null(inputFile)) 
      return(read.csv(file.path("./Data/ClotLysisDoses.csv"))) 
    else(read.table(inputFile$datapath, sep = input$sep, header = input$header))
    
    
  })
  
  output$which <- renderUI({
    
    inputFile <- input$data
    if (is.null(inputFile)) 
      return(file.path("./Data/ClotLysisDoses.csv")) 
    else(file.path(inputFile))
    
    
  })
  
  var<- reactive({
    plate0<-readData()
    plate1<-plate0[,-1]
    mycols<-colnames(plate1)
  })
  
  output$which <- renderUI({
    
    inputFile <- input$data
    if (is.null(inputFile)) 
      return(file.path("./Data/ClotLysisDoses.csv")) 
    else(file.path(inputFile))
    
    
  })
  
  output$what<-renderUI({
    selectInput("colmnames",
                label= h5("Select a column of absorbance data"),
                choices = var())
  }) 
  
  output$nowwhat<-renderUI({
    selectInput("colmnames",
                label= h5("Select a column of absorbance data"),
                choices = var())
  }) 
          ###MULTIPLE PLOTS##
  output$myplotAll<-renderPlot({
    if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    
    plate0<-readData()
    Time<-plate0[,1]
    plate1<-plate0[,-1]
    
    dataCols=length(plate1[1,]) #tests the width of the data, usually 96 columns
    RowNum<-input$numrows 
    
    vect1<-1:dataCols #make dummy vectors for later data
    vect2<-1:dataCols
    vect3<-1:dataCols
    vect4<-1:dataCols
    vect5<-1:dataCols
    vect6<-1:dataCols
    vect7<-1:dataCols
    vect8<-1:dataCols
    vect9<-1:dataCols
    vect10<-1:dataCols
    vect11<-1:dataCols
    vectz<-1:dataCols
    
    pointsnum<-length(plate1[,1])-1
    
    par(mfrow=c(RowNum,dataCols/RowNum))
    par(mar=c(0.2,0.2,0.2,0.2)) # dimensions for figure
   
    for(i in 1:dataCols){ 
      yi<- plate1[,i] # go through data cols (wells) 1:96
      
      lysPoint<-input$num
    
      switch(input$abini,
             "global zero"=minAbs<-input$minabs,
             "nth absorbance"=minAbs<-plate1[input$first,i],
             "min abs + offset below"=minAbs<-min(yi)+input$offset)
      
      
      yimax<-max(yi)    #max absorbance
      pointmax<-which(yi==yimax)[1] #reading for max absorbances
      clotTime<-Time[c(1:pointmax)] #vector of time to max
      clotAbs<-yi[c(1:pointmax)]  #vector of absorbances to max
      lysTimeall<-Time[-c(1:pointmax)] #vector of all lysis Time
      lysAbsall<-yi[-c(1:pointmax)] #vector of all lysis absorbances
      if(minAbs>=min(lysAbsall)){pointmin<-which(lysAbsall<=minAbs)[1]}  else{(pointmin<-length(lysAbsall) )}
      #if(minAbs>=min(yi)){pointmin<-which(yi<=minAbs)[1]}  else{(pointmin<-which(yi>=minAbs)[1] )}
      lysTime<-lysTimeall[1:pointmin]#vector of lysis times to set minimum
      lysAbs<-lysAbsall[1:pointmin] #vector of lysis absorbances to set minimum
      MaxTime<-Time[pointmax]#The time for the max absorbance
      MaxAbs<-yi[pointmax]#The maximum absorbance
      lysTimeend<-max(lysTime)#The time at set minimum lysis absorbance
      pc50Lys<-lysPoint*(MaxAbs-minAbs)+minAbs#The absorbance that will give set percentage of lysis and clotting
      lysPointTime1<-which(abs(lysAbs-pc50Lys)==min(abs(lysAbs-pc50Lys)))[1]
      clotPointTime1<-which(abs(clotAbs-pc50Lys)==min(abs(clotAbs-pc50Lys)))[1]
      lysTimePC<-lysTime[1:lysPointTime1]#vector of time from maximum to chosen lysis
      lysAbsPC<-lysAbs[1:lysPointTime1]#vector of abs from maximum to chosen lysis
      clotTimePC<-clotTime[1:clotPointTime1]#vector of time to percent clotting
      clotAbsPC<-clotAbs[1:clotPointTime1]#vector of absorbances to chosen clotting
      AUC<-round(sum(yi[1:(pointmax+pointmin)]), 2)#area under the curve to full lysis time
  
      yiD1<-diff(yi)
      TimeD1<-Time[-1]
      
      timeMaxD1<-TimeD1[which(yiD1==max(yiD1))][1]
      timeMinD1<-TimeD1[which(yiD1==min(yiD1))][1]
      
      vect1[i]<-MaxAbs         		#max Abs
      vect2[i]<-MaxTime         #time to max Abs
      vect3[i]<-max(clotTimePC)   #time to % clotting     
      vect4[i]<-max(lysTimePC)				#time to selected % lysis
      vect5[i]<-max(lysTimePC)-max(clotTimePC)  #time between clotting and lysis
      vect6[i]<-max(lysTimePC)-MaxTime	  	#time to selected %lysis from peak
      vect7[i]<-lysTimeend		#time to complete lysis
      vect8[i]<-AUC           #sum of abs, like area under curve to full lysis
      vect9[i]<-minAbs        #chosen minimum absorbance
      vect10[i]<-timeMaxD1
      vect11[i]<-timeMinD1
      
      plot2<-plot(Time, yi, type = "l", col= "red", lwd = 3, xlim= c(input$xrange[1], input$xrange[2]), 
                  ylim=c(input$yrange[1], input$yrange[2]),xaxt="n", yaxt="n")
     
      lines(lysTime, lysAbs, col="slategrey", lwd=3)
      lines(clotTimePC, clotAbsPC, col="blue", lwd=3)
      lines(lysTimePC, lysAbsPC, col = "green", lwd=3)
      
      switch(input$tabRes,
             "Time to max"=abline("v"= MaxTime, lty=2),
             "Time to clotting"=abline("v"= max(clotTimePC), lty=2),
             "Time to chosen lysis"=abline("v"= max(lysTimePC), lty=2),
             "Time between clotting and lysis"=arrows(max(clotTimePC), pc50Lys, max(lysTimePC), pc50Lys, length = 0.05, angle = 10),
             "Time to lysis from peak"=arrows(MaxTime, pc50Lys, max(lysTimePC), pc50Lys, length = 0.05, angle = 10),
             "Time to full lysis"=abline("v"=lysTimeend, lty=2),
             "Time at max rate increase"=abline("v"=timeMaxD1, lty=2),
             "Time at max rate decrease"=abline("v"=timeMinD1, lty=2))
      
      plot2
      
      
    }
    
    
    
    
  })
  
      ###RESULTS TABLE FOR MULTIPLE PLOTS###
  output$resultsTable<-renderTable({
  if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
  
  plate0<-readData()
  Time<-plate0[,1]
  plate1<-plate0[,-1]
  
  
  dataCols=length(plate1[1,]) #tests the width of the data, usually 96 columns
  RowNum<-input$numrows 
  
  vect1<-1:dataCols #make dummy vectors for later data
  vect2<-1:dataCols
  vect3<-1:dataCols
  vect4<-1:dataCols
  vect5<-1:dataCols
  vect6<-1:dataCols
  vect7<-1:dataCols
  vect8<-1:dataCols
  vect9<-1:dataCols
  vect10<-1:dataCols
  vect11<-1:dataCols
  vectz<-1:dataCols
  
  pointsnum<-length(plate1[,1])-1
  
  par(mfrow=c(RowNum,dataCols/RowNum))
  par(mar=c(0.2,0.2,0.2,0.2)) # dimensions for figure
  
  for(i in 1:dataCols){ 
    yi<- plate1[,i] # go through data cols (wells) 1:96
    
    lysPoint<-input$num
    
    switch(input$abini,
           "global zero"=minAbs<-input$minabs,
           "nth absorbance"=minAbs<-plate1[input$first,i],
           "min abs + offset below"=minAbs<-min(yi)+input$offset)
    
    yimax<-max(yi)    #max absorbance
    pointmax<-which(yi==yimax)[1] #reading for max absorbances
    clotTime<-Time[c(1:pointmax)] #vector of time to max
    clotAbs<-yi[c(1:pointmax)]  #vector of absorbances to max
    lysTimeall<-Time[-c(1:pointmax)] #vector of all lysis Time
    lysAbsall<-yi[-c(1:pointmax)] #vector of all lysis absorbances
    if(minAbs>=min(lysAbsall)){pointmin<-which(lysAbsall<=minAbs)[1]}  else{(pointmin<-length(lysAbsall) )}
    lysTime<-lysTimeall[1:pointmin]#vector of lysis times to set minimum
    lysAbs<-lysAbsall[1:pointmin] #vector of lysis absorbances to set minimum
    MaxTime<-Time[pointmax]#The time for the max absorbance
    MaxAbs<-yi[pointmax]#The maximum absorbance
    lysTimeend<-max(lysTime)#The time at set minimum lysis absorbance
    pc50Lys<-lysPoint*(MaxAbs-minAbs)+minAbs#The absorbance that will give set percentage of lysis and clotting
    lysPointTime1<-which(abs(lysAbs-pc50Lys)==min(abs(lysAbs-pc50Lys)))[1]
    clotPointTime1<-which(abs(clotAbs-pc50Lys)==min(abs(clotAbs-pc50Lys)))[1]
    lysTimePC<-lysTime[1:lysPointTime1]#vector of time from maximum to chosen lysis
    lysAbsPC<-lysAbs[1:lysPointTime1]#vector of abs from maximum to chosen lysis
    clotTimePC<-clotTime[1:clotPointTime1]#vector of time to percent clotting
    clotAbsPC<-clotAbs[1:clotPointTime1]#vector of absorbances to chosen clotting
    AUC<-round(sum(yi[1:(pointmax+pointmin)]), 2)#area under the curve to full lysis time
    
    yiD1<-diff(yi)
    TimeD1<-Time[-1]
    
    timeMaxD1<-TimeD1[which(yiD1==max(yiD1))][1]
    timeMinD1<-TimeD1[which(yiD1==min(yiD1))][1]
    
    vect1[i]<-MaxAbs        		#max Abs
    vect2[i]<-MaxTime         #time to max Abs
    vect3[i]<-max(clotTimePC)   #time to % clotting     
    vect4[i]<-max(lysTimePC)				#time to selected % lysis
    vect5[i]<-max(lysTimePC)-max(clotTimePC)  #time between clotting and lysis
    vect6[i]<-max(lysTimePC)-MaxTime	  	#time to selected %lysis from peak
    vect7[i]<-lysTimeend		#time to complete lysis
    vect8[i]<-AUC           #sum of abs, like area under curve to full lysis
    vect9[i]<-round(minAbs, 3)        #chosen minimum absorbance
    vect10[i]<-timeMaxD1
    vect11[i]<-timeMinD1
    }
    
    mat3<- matrix(vect3, byrow=TRUE, nrow=RowNum)
    mat1<- matrix(vect1, byrow=TRUE, nrow=RowNum)
    mat1a<- matrix(vect1-vect9, byrow=TRUE, nrow=RowNum)
    mat2<- matrix(vect2, byrow=TRUE, nrow=RowNum)
    mat4<- matrix(vect4, byrow=TRUE, nrow=RowNum)
    mat5<- matrix(vect5, byrow=TRUE, nrow=RowNum)
    mat6<- matrix(vect6, byrow=TRUE, nrow=RowNum)
    mat7<- matrix(vect7, byrow=TRUE, nrow=RowNum)
    mat8<- matrix(vect8, byrow=TRUE, nrow=RowNum)
    mat9<- matrix(vect9, byrow=TRUE, nrow=RowNum)
    mat10<-matrix(vect10, byrow=TRUE, nrow=RowNum)
    mat11<-matrix(vect11, byrow=TRUE, nrow=RowNum)
    matz<- matrix(colnames(plate1), byrow=TRUE, nrow=RowNum)
    data<-switch(input$tabRes, 
                 "Column names"=matz,
                 "Max abs"=mat1,
                 "Max abs - zero"=mat1a,
                 "Time to max"=mat2,
                 "Time to clotting"=mat3, 
                 "Time to chosen lysis"=mat4,
                 "Time between clotting and lysis" =mat5, 
                 "Time to lysis from peak"=mat6,
                 "Time to full lysis"=mat7,
                 "AUC"=mat8,
                 "Time at max rate increase"=mat10,
                 "Time at max rate decrease"=mat11,
                 "Chosen zero"=mat9
    )
    
    #write.table(data, "clipboard", sep="\t", col.names=F, row.names=F) #OPTIONAL- live if local
    
    data
    
  })
    ###SINGLE CURVE##
  output$myplot<-renderPlot({
    if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    
    plate0<-readData()
    Time<-plate0[,1]
    plate1<-plate0[,-1]
   
    #Smoothing
    yii<-plate1[,input$colmnames] # Rather than get and attach, this just reads in the column chosen earlier
    p<-numeric(length(yii)-2)
    p[1]<-yii[1]
    for(a in 2:(length(yii)-1)){
      p[a]<-(yii[a-1]+yii[a]+yii[a+1])/3 # this function calculates moving averages of absorbances
    }
    yia<-p
    if (input$smoothing) yi<-yia
    else(yi<-yii)
    if (input$smoothing) Time<-Time[-length(Time)]
    else(Time<-Time)
    
    yiD1<-diff(yi)
    yiD1ch<-sign(yiD1)==sign(yiD1[+1])
    TimeD1<-Time[-1]
    
    timeMaxD1<-TimeD1[which(yiD1==max(yiD1))][1]
    timeMinD1<-TimeD1[which(yiD1==min(yiD1))][1]
    time0D1<-TimeD1[which(yiD1==FALSE)][1]
    
    lysPoint<-input$num
     
    switch(input$abini,
           "global zero"=minAbs<-input$minabs,
           "nth absorbance"=minAbs<-yi[input$first],
           "min abs + offset below"=minAbs<-min(yi)+input$offset)
    
    yimax<-max(yi)    #max absorbance
    pointmax<-which(yi==yimax)[1] #reading for max absorbances
    clotTime<-Time[c(1:pointmax)] #vector of time to max
    clotAbs<-yi[c(1:pointmax)]  #vector of absorbances to max
    lysTimeall<-Time[-c(1:pointmax)] #vector of all lysis Time
    lysAbsall<-yi[-c(1:pointmax)] #vector of all lysis absorbances
    if(minAbs>=min(lysAbsall)){pointmin<-which(lysAbsall<=minAbs)[1]}  else{(pointmin<-length(lysAbsall) )}
    lysTime<-lysTimeall[1:pointmin]#vector of lysis times to set minimum
    lysAbs<-lysAbsall[1:pointmin] #vector of lysis absorbances to set minimum
    MaxTime<-Time[pointmax]#The time for the max absorbance
    MaxAbs<-yi[pointmax]#The maximum absorbance
    lysTimeend<-max(lysTime)#The time at set minimum lysis absorbance
    pc50Lys<-lysPoint*(MaxAbs-minAbs)+minAbs#The absorbance that will give set percentage of lysis and clotting
    lysPointTime1<-which(abs(lysAbs-pc50Lys)==min(abs(lysAbs-pc50Lys)))[1]
    clotPointTime1<-which(abs(clotAbs-pc50Lys)==min(abs(clotAbs-pc50Lys)))[1]
    lysTimePC<-lysTime[1:lysPointTime1]#vector of time from maximum to chosen lysis
    lysAbsPC<-lysAbs[1:lysPointTime1]#vector of abs from maximum to chosen lysis
    clotTimePC<-clotTime[1:clotPointTime1]#vector of time to percent clotting
    clotAbsPC<-clotAbs[1:clotPointTime1]#vector of absorbances to chosen clotting
    AUC<-round(sum(yi[1:(pointmax+pointmin)]), 2)#area under the curve to full lysis time
    
    Plot1<-plot(Time, yi, type = "l", col= "red", lwd = 3, xlim= c(input$xrange[1], input$xrange[2]), 
                ylim=switch(input$curveRes, 
                             "All"=c(input$yrange[1], input$yrange[2]),
                             "Clotting"=c(input$yrange[1], input$yrange[2]), 
                             "Lysis"=c(input$yrange[1], input$yrange[2]),
                             "1st Derivative (o -magenta)"=c(min(yiD1), input$yrange[2])),
                            main=paste("clotting and lysis for",input$colmnames), xlab = "Time", ylab = "Absorbance")
    abline("h"= MaxAbs, lty=2)
    abline("v"= MaxTime, lty=2, lwd=2)
    abline("h"= pc50Lys, lty=2)
    abline("v"= max(lysTimePC), lty=2, lwd=2)
    abline("v"= max(clotTimePC), lty=2, lwd=2)
    abline("h"= minAbs, lty=2)
    abline("v"=lysTimeend, lty=2, lwd=2)
    lines(lysTime, lysAbs, col="slategrey", lwd=3)
    lines(clotTimePC, clotAbsPC, col="blue", lwd=3)
    lines(lysTimePC, lysAbsPC, col = "green", lwd=3)
    
    
    abline("v"= timeMaxD1, lty=1, col="magenta")
    abline("v"= timeMinD1, lty=1, col="magenta")
    abline("v"= time0D1, lty=1, col="magenta")
    abline("h"= 0, lty=2, col="magenta")
    
    Plot<-switch(input$curveRes, 
                 "All"=Plot1,
                 "Clotting"=Plot1, 
                 "Lysis"=Plot1,
                 "1st Derivative (o -magenta)"=points(TimeD1, yiD1))
    

  })
  
  ###CURVE RESULTS TABLE###
  
  output$curveTable<-renderTable({
    if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    
    plate0<-readData()
    Time<-plate0[,1]
    plate1<-plate0[,-1]
    
    #Smoothing
    yii<-plate1[,input$colmnames] # Rather than get and attach, this just reads in the column chosen earlier
    p<-numeric(length(yii)-2)
    p[1]<-yii[1]
    for(a in 2:(length(yii)-1)){
      p[a]<-(yii[a-1]+yii[a]+yii[a+1])/3 # this function calculates moving averages of absorbances
    }
    yia<-p
    if (input$smoothing) yi<-yia
    else(yi<-yii)
    if (input$smoothing) Time<-Time[-length(Time)]
    else(Time<-Time)
    
    
    lysPoint<-input$num
   
    switch(input$abini,
           "global zero"=minAbs<-input$minabs,
           "nth absorbance"=minAbs<-yi[input$first],
           "min abs + offset below"=minAbs<-min(yi)+input$offset)
    
    yiD1<-diff(yi)
    yiD1ch<-sign(yiD1)==sign(yiD1[+1])
    TimeD1<-Time[-1]
    
    timeMaxD1<-TimeD1[which(yiD1==max(yiD1))][1]
    timeMinD1<-TimeD1[which(yiD1==min(yiD1))][1]
    time0D1<-TimeD1[which(yiD1==FALSE)][1]
    
    yimax<-max(yi)    #max absorbance
    pointmax<-which(yi==yimax)[1] #reading for max absorbances
    clotTime<-Time[c(1:pointmax)] #vector of time to max
    clotAbs<-yi[c(1:pointmax)]  #vector of absorbances to max
    lysTimeall<-Time[-c(1:pointmax)] #vector of all lysis Time
    lysAbsall<-yi[-c(1:pointmax)] #vector of all lysis absorbances
    if(minAbs>=min(lysAbsall)){pointmin<-which(lysAbsall<=minAbs)[1]}  else{(pointmin<-length(lysAbsall) )}
    lysTime<-lysTimeall[1:pointmin]#vector of lysis times to set minimum
    lysAbs<-lysAbsall[1:pointmin] #vector of lysis absorbances to set minimum
    MaxTime<-Time[pointmax]#The time for the max absorbance
    MaxAbs<-round(yi[pointmax], 3)#The maximum absorbance
    MaxAbs_zero<-MaxAbs-minAbs
    lysTimeend<-max(lysTime)#The time at set minimum lysis absorbance
    pc50Lys<-lysPoint*(MaxAbs-minAbs)+minAbs#The absorbance that will give set percentage of lysis and clotting
    lysPointTime1<-which(abs(lysAbs-pc50Lys)==min(abs(lysAbs-pc50Lys)))[1]
    clotPointTime1<-which(abs(clotAbs-pc50Lys)==min(abs(clotAbs-pc50Lys)))[1]
    lysTimePC<-lysTime[1:lysPointTime1]#vector of time from maximum to chosen lysis
    lysAbsPC<-lysAbs[1:lysPointTime1]#vector of abs from maximum to chosen lysis
    clotTimePC<-clotTime[1:clotPointTime1]#vector of time to percent clotting
    clotAbsPC<-clotAbs[1:clotPointTime1]#vector of absorbances to chosen clotting
    AUC<-round(sum(yi[1:(pointmax+pointmin)]), 2)#area under the curve to full lysis time
  
    Na<-paste("Results for",input$colmnames, "at", input$num*100, "% clotlysis")
    Nc<-paste("Results for",input$colmnames, "at", input$num*100, "% clotting")
    Nl<-paste("Results for",input$colmnames, "at", input$num*100, "% lysis")
    Nd<-paste("Results for",input$colmnames, "1st derivative max positive, negative and zero")
    U<-("The maximum absorbance is")
    Uz<-("The maximum absorbance minus chosen zero is")
    V<-("Time at maximum absorbance is")
    W<-("The time to % clot formation from time zero is")
    X<-("The time to % clot lysis from time zero is")
    Y<-("The time between chosen % clotting and lysis is")
    Z<-("The time from peak clotting to chosen % lysis is")
    Zz<-("The time for complete lysis from time zero is")
    Mn<-("The selected minimum absorbance is")
    Uc<-("The area under the curve is")
    D1A<-("The time at maximum absorbance increase")
    D1B<-("The time at maximum absorbance decrease")
    D10<-("The time at plateau")
    
    A<-matrix(c(
      U, MaxAbs,
      Uz, MaxAbs_zero,
      V, MaxTime,
      W, max(clotTimePC),
      X, max(lysTimePC),
      Y, max(lysTimePC)-max(clotTimePC),
      Z, lysTimeend-MaxTime,
      Zz, lysTimeend,
      Mn, round(minAbs, 3),
      Uc, AUC        ),
      byrow=TRUE, nrow=10)
    
    colnames(A)<-c(Na, "Result")
    
    B<-matrix(c(
      U, MaxAbs,
      Uz, MaxAbs_zero,
      V, MaxTime, 
      W, max(clotTimePC)),
      byrow=TRUE, nrow=4)
    colnames(B)<-c(Nc, "Result") 
    
    C<-matrix(c(
      X, max(lysTimePC),
      Y, max(lysTimePC)-max(clotTimePC),
      Z, max(lysTimePC)-MaxTime,
      Zz, lysTimeend,
      Mn, round(minAbs, 3),
      Uc, AUC        ),
      byrow=TRUE, nrow=6)
    
    colnames(C)<-c(Nl, "Result")
    
    D<-matrix(c(
      D1A, timeMaxD1, 
      D1B, timeMinD1,
      D10, time0D1), 
      byrow=TRUE, nrow=3)
      
    colnames(D)<-c(Nd, "Result") 
    
    data<-switch(input$curveRes, 
                 "All"=A,
                 "Clotting"=B, 
                 "Lysis"=C,
                 "1st Derivative (o -magenta)"=D
                 
    )
    
  })
  
 
  
  output$contents<-renderDataTable({
    readData()
    
  })
  
  output$text3<-renderText({
    paste(input$tabRes, "for",input$num*100, "%", "clot formation or clot remaining")
    
  })
  
  output$text4<-renderText({
    paste("Results for",input$tabRes)
    
  })
}