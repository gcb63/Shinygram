library(shiny)
library(plot3D)
library(sp)
library(gstat)

ui <- fluidPage(
  
  titlePanel("Semivariogram Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'CSV file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )
      ),
      selectInput("xvar", "X coordinate: ", choices = NULL), 
      selectInput("yvar", "Y coordinate: ", choices = NULL),
      selectInput("pvar", "Property: ", choices=NULL),
      numericInput("lagsp","Lag spacing: ", value=NULL),
      numericInput("maxlag","Maximum lag: ", value=NULL)
    ),
    
    mainPanel(
      plotOutput("postmap"),
      plotOutput("svg")
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
  
  observeEvent( c(input$xvar,input$yvar,input$pvar), {
    if (!is.null(input$xvar) & input$xvar != "[NONE]" &
        !is.null(input$yvar) & input$yvar != "[NONE]" &
        !is.null(input$pvar) & input$pvar != "[NONE]") {
      f <- info()
      f <- subset(f, select = c(input$xvar,input$yvar,input$pvar))
      f <- na.omit(f)
      dlag <- median(nndist(f))
      mlag <- 0.5*max(as.vector(dist(f)))
      updateNumericInput(session,"lagsp","Lag spacing: ",value=dlag)
      updateNumericInput(session,"maxlag","Maximum lag: ",value=mlag)
    }
  })

  output$postmap <- renderPlot({
    if (!is.null(input$xvar) & input$xvar != "[NONE]" & !is.null(input$yvar) & input$yvar != "[NONE]" & !is.null(input$pvar) & input$pvar != "[NONE]" ) {
      f <- info()
      f <- subset(f, select = c(input$xvar,input$yvar,input$pvar))
      scatter2D(f[,1],f[,2],colvar=f[,3],pch=16,asp=1)
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
      form <- formula(paste(input$pvar,"~1",sep=""))
      plot(variogram(form,f,width=input$lagsp,cutoff=input$maxlag))
    }
  })
  
}

shinyApp(ui, server)