
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyServer(function(input, output) {
   
  output$my.plot <- renderPlot({
     
    ## proportion protected
    prop.prot <- (input$N1*input$VE1 + input$N2*input$VE2)/input$N
    prop.need.prot <- c(1-1/input$R.opt,1-1/input$R.mod,1-1/input$R.pes)
    ## 
    sd.prop.need.prot <- prop.need.prot*input$pct.uncer
    #sd.prop.need.prot <- sqrt(prop.need.prot*(1-prop.need.prot)/100)
    
    par(oma=c(0,0,3,0))
    plot(-100,-100,xlim=c(0,1),ylim=c(-.5,3.25),axes=F,xlab="proportion protected",ylab="scenarios")
    axis(1)
    
    red.col = rgb(red=1,green=0,blue=0,alpha=.4)
    yellow.col = "yellow"#rgb(red=255,green=255,blue=204,maxColorValue=255,alpha=.4)
    green.col = rgb(red=0,green=1,blue=0,alpha=.4)
    
    ## bounds for opt scen
    upper1.opt <- max(prop.need.prot[1],0)
    lower1.opt<- 0

    upper2.opt <- min(prop.need.prot[1]+sd.prop.need.prot[1],1)
    lower2.opt<- upper1.opt
    
    upper3.opt <- 1
    lower3.opt<- upper2.opt
    
    polygon(x=c(lower1.opt,upper1.opt,upper1.opt,lower1.opt),
            y=c(-.5,-.5,0.5,0.5),col=red.col,border=FALSE)
    polygon(x=c(lower2.opt,upper2.opt,upper2.opt,lower2.opt),
            y=c(-.5,-.5,0.5,0.5),col=yellow.col,border=FALSE)
    polygon(x=c(lower3.opt,upper3.opt,upper3.opt,lower3.opt),
            y=c(-.5,-.5,0.5,0.5),col=green.col,border=FALSE)
    
    upper1.mod <- max(prop.need.prot[2],0)
    lower1.mod<- 0
    
    upper2.mod <- min(prop.need.prot[2]+sd.prop.need.prot[2],1)
    lower2.mod<- upper1.mod
    
    upper3.mod <- 1
    lower3.mod<- upper2.mod
    
    polygon(x=c(lower1.mod,upper1.mod,upper1.mod,lower1.mod),
            y=c(.75,.75,1.75,1.75),col=red.col,border=FALSE)
    polygon(x=c(lower2.mod,upper2.mod,upper2.mod,lower2.mod),
            y=c(.75,.75,1.75,1.75),col=yellow.col,border=FALSE)
    polygon(x=c(lower3.mod,upper3.mod,upper3.mod,lower3.mod),
            y=c(.75,.75,1.75,1.75),col=green.col,border=FALSE)
    
    upper1.pes <- max(prop.need.prot[3],0)
    lower1.pes<- 0
    
    upper2.pes <- min(prop.need.prot[3]+sd.prop.need.prot[3],1)
    lower2.pes<- upper1.pes
    
    upper3.pes <- 1
    lower3.pes<- upper2.pes
    
    polygon(x=c(lower1.pes,upper1.pes,upper1.pes,lower1.pes),
            y=c(2,2,3,3),col=red.col,border=FALSE)
    polygon(x=c(lower2.pes,upper2.pes,upper2.pes,lower2.pes),
            y=c(2,2,3,3),col=yellow.col,border=FALSE)
    polygon(x=c(lower3.pes,upper3.pes,upper3.pes,lower3.pes),
            y=c(2,2,3,3),col=green.col,border=FALSE)
  
    mtext(text="estimated \n proportion \n protected",at=prop.prot,side=3)
    mtext(text="optimisitic",side=2,at=0,cex=.9)
    mtext(text="moderate",side=2,at=1.25,cex=.9)
    mtext(text="pessimistic",side=2,at=2.5,cex=.9)
    abline(v=prop.prot,lty=2,lwd=2,col=1)    
    par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    
    legend("topright",c("spread highly likely","spread possible","spread unlikley"),col=c(red.col,yellow.col,green.col),lty=1,
           lwd=4,bty="n")
           
    
  })
  
  output$ind.dir.plot <- renderPlot({
    
    ## proportion protected
    prop.prot <- (input$N1*input$VE1 + input$N2*input$VE2)/input$N
    prop.need.prot <- c(1-1/input$R.opt,1-1/input$R.mod,1-1/input$R.pes)
    ## 
    sd.prop.need.prot <- prop.need.prot*input$pct.uncer
    #sd.prop.need.prot <- sqrt(prop.need.prot*(1-prop.need.prot)/100)
    
    par(oma=c(0,0,3,0))
    plot(-100,-100,xlim=c(0,1),ylim=c(-.5,3.25),axes=F,xlab="proportion protected",ylab="scenarios")
    axis(1)
    
    red.col = rgb(red=1,green=0,blue=0,alpha=.4)
    yellow.col = "yellow"#rgb(red=255,green=255,blue=204,maxColorValue=255,alpha=.4)
    green.col = rgb(red=0,green=1,blue=0,alpha=.4)
    
    ## bounds for opt scen
    upper1.opt <- max(prop.need.prot[1],0)
    lower1.opt<- 0
    
    upper2.opt <- min(prop.need.prot[1]+sd.prop.need.prot[1],1)
    lower2.opt<- upper1.opt
    
    upper3.opt <- 1
    lower3.opt<- upper2.opt
    
    polygon(x=c(lower1.opt,upper1.opt,upper1.opt,lower1.opt),
            y=c(-.5,-.5,0.5,0.5),col=red.col,border=FALSE)
    polygon(x=c(lower2.opt,upper2.opt,upper2.opt,lower2.opt),
            y=c(-.5,-.5,0.5,0.5),col=yellow.col,border=FALSE)
    polygon(x=c(lower3.opt,upper3.opt,upper3.opt,lower3.opt),
            y=c(-.5,-.5,0.5,0.5),col=green.col,border=FALSE)
    
    upper1.mod <- max(prop.need.prot[2],0)
    lower1.mod<- 0
    
    upper2.mod <- min(prop.need.prot[2]+sd.prop.need.prot[2],1)
    lower2.mod<- upper1.mod
    
    upper3.mod <- 1
    lower3.mod<- upper2.mod
    
    polygon(x=c(lower1.mod,upper1.mod,upper1.mod,lower1.mod),
            y=c(.75,.75,1.75,1.75),col=red.col,border=FALSE)
    polygon(x=c(lower2.mod,upper2.mod,upper2.mod,lower2.mod),
            y=c(.75,.75,1.75,1.75),col=yellow.col,border=FALSE)
    polygon(x=c(lower3.mod,upper3.mod,upper3.mod,lower3.mod),
            y=c(.75,.75,1.75,1.75),col=green.col,border=FALSE)
    
    upper1.pes <- max(prop.need.prot[3],0)
    lower1.pes<- 0
    
    upper2.pes <- min(prop.need.prot[3]+sd.prop.need.prot[3],1)
    lower2.pes<- upper1.pes
    
    upper3.pes <- 1
    lower3.pes<- upper2.pes
    
    polygon(x=c(lower1.pes,upper1.pes,upper1.pes,lower1.pes),
            y=c(2,2,3,3),col=red.col,border=FALSE)
    polygon(x=c(lower2.pes,upper2.pes,upper2.pes,lower2.pes),
            y=c(2,2,3,3),col=yellow.col,border=FALSE)
    polygon(x=c(lower3.pes,upper3.pes,upper3.pes,lower3.pes),
            y=c(2,2,3,3),col=green.col,border=FALSE)
    
    mtext(text="estimated \n proportion \n protected",at=prop.prot,side=3)
    mtext(text="optimisitic",side=2,at=0,cex=.9)
    mtext(text="moderate",side=2,at=1.25,cex=.9)
    mtext(text="pessimistic",side=2,at=2.5,cex=.9)
    abline(v=prop.prot,lty=2,lwd=2,col=1)    
    par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    
    legend("topright",c("spread highly likely","spread possible","spread unlikley"),col=c(red.col,yellow.col,green.col),lty=1,
           lwd=4,bty="n")
    
    
  })
  
  output$table = renderTable({
    prop.prot <- (input$N1*input$VE1 + input$N2*input$VE2)/input$N
    prop.need.prot <- c(1-1/input$R.opt,1-1/input$R.mod,1-1/input$R.pes)
    ## 
    #sd.prop.need.prot <- prop.need.prot*input$pct.uncer
    rc <- as.data.frame(rev(prop.need.prot))
    rownames(rc) <- c("pessimistic","moderate","optimistic")
    colnames(rc) <- c("proportion needed to be protected")
    rc
    })
    
  
})
