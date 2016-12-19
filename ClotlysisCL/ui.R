library(shiny)

fluidPage(
  
  titlePanel (h2("Analysis of clotting and lysis data",  align = "right")),
 
 
                
  sidebarLayout(
    
    sidebarPanel(
      conditionalPanel(condition="input.tabselected==1",

      fluidRow(
      column(6, fileInput("data", label = "Select dataset:")),
     
      column(2,radioButtons(inputId = "sep", label = "File type", choices = c(csv = ",", txt ="\t"), selected = ",")),
      
      column(4,checkboxInput(inputId= "header", label = "Columns have header text", value = TRUE))
      ),
      
      fluidRow(
      
     
      column(4, numericInput("numrows",
                             label = h5("How many rows?"), value = 3)),
      
      column(8,  offset= 0, numericInput("num", 
                              label = h5("% Clot formed or remaining (0.5 is 50%)"),step = 0.1,
                              value = 0.5))
       ),
      

        
      sliderInput("xrange", 
                  label =h5( "x axis range"),
                  min = 0, max = 25000, value = c(0, 2500)),
      
      sliderInput("yrange", 
                  label =h5( "y axis range"),
                  min = -1.50, max = 2.50, value = c(0, 0.9)),
      
      
      #helpText(h5("Zeroing options", align="center")),
      
      fluidRow(
        
        column(5, radioButtons(inputId="abini", label = h5("Baseline Options", align = "center"), 
                               choices = c("global zero", "nth absorbance", 
                                           "min abs + offset below"))),
        column(4, numericInput("minabs",
                               label = h5("global zero"), step = 0.01, value = 0.048)),
        
        column(3, numericInput("first",
                               label = h5("nth point"), value = 1))
        
        ),  
      
      
      
      sliderInput("offset", 
                  label =h6( ""),
                  min = -.15, max = .15, value = 0),
      
      
   
      
     
                  
      radioButtons(inputId="tabRes", label = h5("Results Table", align = "center"), choices = c("Column names", "Max abs","Max abs - zero", "Time to max",
                                                                          "Time to clotting","Time to chosen lysis",
                                                                          "Time between clotting and lysis","Time to lysis from peak",
                                                                          "Time to full lysis", "AUC", "Time at max rate increase", "Time at max rate decrease",
                                                                          "Chosen zero"
                                                                          ), selected = "Max abs") 
      
      ),
      
      #helpText(h4("Please cite this page if you find it useful, Longstaff C, 2016, Shiny App for calculating clot lysis times, version 0.9
               #URL address, last accessed", Sys.Date())),
      
      conditionalPanel(condition="input.tabselected==2",
                       
                       uiOutput("what"),
                        
                        radioButtons(inputId="curveRes", label = "Select Results", 
                                             choices = c("All", "Clotting","Lysis", "1st Derivative (o -magenta)"), selected = "All"),
                        
                         
                        checkboxInput(inputId= "smoothing", label = "Smooth data", value = FALSE)
                       
                       
      ),
      
      
      
      conditionalPanel(condition="input.tabselected==3", 
                       helpText(h4("Raw data file:")),
                       uiOutput("which")
      ),
      
      conditionalPanel(condition="input.tabselected==4",
                       helpText(h4("Please cite this page if you find it useful, Longstaff C, 2016, Shiny App for calculating clot lysis times, version 0.9
               URL address, last accessed", Sys.Date()))
                       
      )
     
   
    
    
  ),
  mainPanel( 
    tabsetPanel(#type="tab",
                tabPanel("Plots", value = 1,
                         
                         
                         plotOutput(outputId = "myplotAll"),
                         
                         h5(textOutput("text3")),
                         
                         h5("Results Table"), tableOutput("resultsTable"), align = "center"),
                
                 
                
                   
                
                tabPanel("Curve", value = 2,
                         
                          
                         
                         plotOutput(outputId = "myplot"),
                         
                       
                         
                         fluidRow(
                          column(7,
                                 
                                 h4("Results Table"), tableOutput("curveTable"), align = "centre")
                         
                         )),
                
                
                
              
                tabPanel("Raw data", value = 3, dataTableOutput("contents")),
                       
                
                tabPanel("Help", value = 4,
                         
                         tags$blockquote(h5("►Load a data file in csv or txt fomat (tab separator). An example set of data is provided",
                                            tags$br(),
                                            "►Data should be supplied as one column of time and any number of columns of absorbances",
                                            tags$br(),
                                            "►Note: Data files should not contain any gaps or spaces between characters, avoid unusual characters",
                                            tags$br(),
                                            "►If the columns have headers, check the box",
                                            tags$br(),
                                            "►The graphs respond to selections made and the axis can be adjusted with the sliders",
                                            tags$br(),
                                            "►Type in a value for percentage clotting and lysis",
                                            tags$br(),
                                            "►Adjust the zero absorbance value to set the point for complete lysis",
                                            tags$br(),
                                            "►There are options to set a global zero absorbance or nth reading or minimum absorbance with offset",
                                            tags$br(),
                                            "►All the plots are shown or individual curves can be scrutinised on the next tab",
                                            tags$br(), 
                                            "►Results from data analysis are shown in the tables",
                                            tags$br(),
                                            "►The arrangement of graphs and tables can be controlled by changing the number of rows in the dialog box",
                                            tags$br(),
                                            "►Highlight, copy and paste the tables for further analysis and copy and save graphs by right clicking",
                                            tags$br(),
                                            "►Raw data loaded for analysis can be inspected in the 'Raw data' tab",
                                            tags$br(),
                                            "►Code files and help notes are available in a github repository https://github.com/drclongstaff/",
                                            tags$br()
                                           
                                            
                         )
  
  ),
  
                
                  
                  
                  
                  tags$img(src="screenCapS6.png", width=600, height=700)
                  
                ),
                
               id = "tabselected"
                
    )
  )
  
)   
)
