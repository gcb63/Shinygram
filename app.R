library(shiny)
library(plot3D)
library(sp)
library(gstat)

ui <- fluidPage(
  
  titlePanel("Semivariogram Analysis"),
  
  fluidRow(
    column(3, 
        h4("Input Data"),
        fileInput('file', 'CSV file to upload',
            accept = c('text/csv','text/comma-separated-values','.csv')
        ),
        selectInput("xvar", "X coordinate: ", choices =NULL), 
        selectInput("yvar", "Y coordinate: ", choices =NULL),
        selectInput("pvar", "Property: ", choices=NULL)
    ),
    column(6,
        plotOutput("postmap"),
        plotOutput("svg")
    ),
    column(3,
        h4("Empirical Semivariogram"),
        numericInput("lagsp","Lag spacing: ", value=NULL),
        numericInput("maxlag","Maximum lag: ", value=NULL),
        selectInput("trend","Remove trend of order: ",choices=c(0,1,2)),
        hr(),
        h4("Semivariogram Model"),
        selectInput("modform","Model Form: ",
                    choices=c("[NONE]","Exponential","Spherical","Gaussian")),
        numericInput("nugget","Nugget: ",value=0),
        numericInput("range","Range: ",value=0),
        numericInput("sill","Sill: ",value=0)
    )
  )
)
 

server <- function(input, output, session) { # added session for updateSelectInput
  
  nndist <- function(xy) {
    nd <- nrow(xy)
    nnd <- rep(0,nd)
    for (i in 1:nd) {
      dst <- sqrt((xy[,1]-xy[i,1])^2+(xy[,2]-xy[i,2])^2)
      nnd[i] <- min(dst[dst>0])
    }
    return(nnd)
  }
  
  info <- eventReactive(input$file, {
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)
    
    # Changes in read.table 
    f <- read.csv(inFile$datapath, header = T)
    vars <- names(f)
    # Update select input immediately after clicking on the action button. 
    updateSelectInput(session, "xvar","X coordinate: ", choices = c("[NONE]",vars))
    updateSelectInput(session, "yvar","Y coordinate: ", choices = c("[NONE]",vars))
    updateSelectInput(session, "pvar","Property: ", choices = c("[NONE]",vars))
    
    f
  })
  
  observeEvent( c(input$xvar,input$yvar), {
    if (!is.null(input$xvar) & input$xvar != "[NONE]" &
        !is.null(input$yvar) & input$yvar != "[NONE]" ) {
      f <- info()
      f <- subset(f, select = c(input$xvar,input$yvar))
      f <- na.omit(f)
      dlag <- signif(median(nndist(f)),2)
      mlag <- signif(0.5*max(as.vector(dist(f))),2)
      updateNumericInput(session,"lagsp","Lag spacing: ",value=dlag)
      updateNumericInput(session,"maxlag","Maximum lag: ",value=mlag)
    }
  })

  output$postmap <- renderPlot({
    if (!is.null(input$xvar) & input$xvar != "[NONE]" &
        !is.null(input$yvar) & input$yvar != "[NONE]" &
        !is.null(input$pvar) & input$pvar != "[NONE]" ) {
      f <- info()
      f <- subset(f, select = c(input$xvar,input$yvar,input$pvar))
      scatter2D(f[,1],f[,2],colvar=f[,3],pch=16,asp=1,cex=1.3,
                xlab=input$xvar,ylab=input$yvar,cex.lab=1.3,
                main=paste("Posting of",input$pvar),cex.main=1.3)
    }
  })
  
  output$svg <- renderPlot({
    if (!is.null(input$xvar) & input$xvar != "[NONE]" &
        !is.null(input$yvar) & input$yvar != "[NONE]" &
        !is.null(input$pvar) & input$pvar != "[NONE]" &
        !is.null(input$lagsp) & !is.null(input$maxlag) ) {
      f <- info()
      f <- subset(f, select = c(input$xvar,input$yvar,input$pvar))
      f <- na.omit(f)
      coordinates(f) <- c(input$xvar,input$yvar)
      if (input$trend=="0") {
        form <- formula(paste(input$pvar,"~1",sep=""))
      } else if (input$trend=="1") {
        form <- formula(paste(input$pvar,"~",input$xvar,"+",input$yvar,sep=""))
      } else {
        form <- formula(paste(input$pvar, "~",
                              input$xvar, "+", input$yvar, "+",
                              "I(", input$xvar, "^2) + I(",input$yvar,"^2) +",
                              "I(", input$xvar, "*", input$yvar,")",
                              sep=""))
      }
      prop.vg <- variogram(form,f,width=input$lagsp,cutoff=input$maxlag)
      with(prop.vg,
           plot(gamma~dist,pch=16,col="blue",cex=1.3,ylim=c(0,1.2*max(prop.vg$gamma)),
                xlim=c(0,1.02*input$maxlag),xlab="Lag Distance",ylab="Semivariance",cex.lab=1.3,
                main=paste("Omnidirectional Semivariogram of",input$pvar),cex.main=1.3))
      if (input$trend=="0") {
        legtext <- "No trend removed"
      } else if (input$trend=="1") {
        legtext <- "First-order trend removed"
      } else {
        legtext <- "Second-order trend removed"
      }
      legend("bottomright",legtext)
      if ( input$modform!="[NONE]" &
           !is.na(input$nugget) & input$nugget >= 0 &
           !is.na(input$range) & input$range >= 0 &
           !is.na(input$sill) & input$sill >= 0 &
           (input$nugget + input$range + input$sill) > 0 ) {
        if (input$modform=="Exponential") {
          vgmod <- variogramLine(vgm(input$sill,"Exp",input$range/3,input$nugget),maxdist=1.02*input$maxlag)
        } else if (input$modform=="Spherical") {
          vgmod <- variogramLine(vgm(input$sill,"Sph",input$range,input$nugget),maxdist=1.02*input$maxlag)
        } else if (input$modform=="Gaussian") {
          vgmod <- variogramLine(vgm(input$sill,"Gau",input$range,input$nugget),maxdist=1.02*input$maxlag)
        }
        lines(gamma~dist,vgmod,col="red",lwd=2)
      }
    }
  })
  
}

shinyApp(ui, server)