'
Script    : app
Created   : September 05, 2018
'
source("PackageLoad.R")
source("Utils.R")
source("DataFetch.R")
library(stringr)
fileToReco <- fread("MsisdnFile.csv")
ui <- dashboardPagePlus(
  skin = "blue",
  dashboardHeaderPlus(title = "CRBT Recommender"),
  dashboardSidebar(sidebarMenu(
    menuItem("About CRBT",
             tabName = "crbt"),
    menuItem(
      "Tone Details",
      tabName = "toneDetails",
      startExpanded = FALSE,
      menuItem(
        "Tone Categories",
        tabName = "toneCategories",
        icon = icon("table")
      ),
      menuItem(
        "Tone Description",
        tabName = "toneDescriptions",
        icon = icon("table")
      ),
      menuItem(
        "Tone Chart",
        tabName = "toneSunburst",
        icon = icon("bar-chart-o")
      )
    ),
    menuItem(
      "Recommendations",
      tabName = "recommendations",
      startExpanded = FALSE,
      menuItem(
        "Recommendations",
        tabName = "topReco",
        icon = icon("table")
      ),
      menuItem(
        "Top Recommendations",
        tabName = "barplot",
        icon = icon("table")
      ),
      menuItem(
        "Recommendation Chart",
        tabName = "toneRBurst",
        icon = icon("bar-chart-o")
      )
    )
    
  )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "crbt",
        br(),
        br(),
        box(
          width = NULL,
          title = "Caller Ring Back Tone",
          status = "primary",
          solidHeader = TRUE,
          HTML(
            "A <strong>caller ringback tone (RBT)</strong> is the sound a caller hears while waiting for a phone to be answered."
          ),
          br(),
          br(),
          br(),
          HTML(
            "In North America, a standard caller <strong>RBT</strong> is repeated as a two-second tone with a four-second pause between tones. In other countries, like the UK, Ireland, Australia and New Zealand, it is a double ring. As mobile technology has advanced, the caller <strong>RBT</strong> term has become more synonymous with a customized <strong>RBT</strong> that replaces a standard caller <strong>RBT</strong>."
          ),
          br(),
          br(),
          br(),
          HTML(
            "A caller <strong>RBT</strong> is also known as an answer tone, ringback tone, audible ring, callertune, call tone or connecting tone."
          )
        )
      ),
      tabItem(
        tabName = "toneCategories",
        br(),
        br(),
        box(
          width = NULL,
          title = "Tone Categories",
          status = "primary",
          solidHeader = TRUE,
          
          shinyjs::useShinyjs(),
          actionButton("refresh", "Refresh", icon = icon("refresh")),
          br(),
          br(),
          DT::dataTableOutput("categories")
        )
        
      ),
      tabItem(
        tabName = "toneDescriptions",
        br(),
        br(),
        box(
          width = NULL,
          title = "Tone Descriptions",
          status = "primary",
          solidHeader = TRUE,
          shinyjs::useShinyjs(),
          actionButton("refresh2", "Refresh", icon = icon("refresh")),
          br(),
          br(),
          DT::dataTableOutput("descriptions")
        )
      ),
      tabItem(
        tabName = "toneSunburst",
        br(),
        br(),
        box(
          width = NULL,
          title = "Tone Sunburst Diagram",
          status = "primary",
          solidHeader = TRUE,
          sunburstOutput("tSburst")
        )
      ),
      tabItem(
        tabName = "topReco",
        br(),
        br(),
        box(
          width = NULL,
          title = "Recommendations",
          status = "primary",
          solidHeader = TRUE,
          DT::dataTableOutput("toprecos")
        )
      ),
      
      tabItem(tabName = "barplot", fluidPage(fluidRow(
        column(
          width = 12,
          box(
            width = NULL,
            title = "Top 10 Recommendations",
            status = "primary",
            solidHeader = TRUE,
            column(4, wellPanel(list(
              selectInput(
                "select",
                label = h5("Select MSISDN ")
                ,
                choices = as.list(getMsisdn()),
                selectize = FALSE
              )
              
            ))),
            
            column(4, wellPanel(
              dateInput(
                "preddate",
                label = h5('Prediction Date'),
                value = Sys.Date()
              )
            )),
            
            mainPanel(DT::dataTableOutput("desc")),
            column(3, offset = 0,  actionButton(inputId = "update", label = "SUBMIT"))
            
          )
        )
      )))
      ,
      tabItem(
        tabName = "toneRBurst",
        br(),
        br(),
        box(
          width = NULL,
          title = "Recommendation Sunburst Diagram",
          status = "primary",
          solidHeader = TRUE,
          sunburstOutput("tRburst")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  mydata <- reactive({
    toneCategories <- getToneCategories()
    toneCategories <- toneCategories[, -c(1)]
    names(toneCategories) <- c("Parent ID", "Category ID", "Name")
    DT::datatable(
      toneCategories,
      filter = 'top',
      extensions = c('Buttons', 'AutoFill', 'ColReorder'),
      options = list(
        autoFill = TRUE,
        colReorder = TRUE,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          'print',
          list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )
        )
      )
    )
  })
  
  output$categories <- DT::renderDataTable({
    mydata()
  })
  
  observeEvent(input$refresh, {
    shinyjs::reset("form")
    output$categories <- DT::renderDataTable({
      mydata()
    })
    
    
  })
  
  mydata2 <- reactive({
    toneDescriptions <- getToneDescription()
    names(toneDescriptions) <-
      c(
        "Parent ID",
        "Tone ID",
        "Category ID",
        "Tone Name",
        "Download Count",
        "Like Count",
        "Rank",
        "Name"
      )
    DT::datatable(
      toneDescriptions,
      filter = 'top',
      extensions = c('Buttons', 'AutoFill', 'ColReorder'),
      options = list(
        autoFill = TRUE,
        colReorder = TRUE,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          'print',
          list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )
        )
      )
    )
  })
  
  output$descriptions <- DT::renderDataTable({
    mydata2()
  })
  
  observeEvent(input$refresh2, {
    shinyjs::reset("form")
    output$descriptions <- DT::renderDataTable({
      mydata2()
    })
  })
  
  observeEvent(input$update, {
    output$desc <- DT::renderDataTable({
      top <-  formatQuery(input$select, input$preddate)
      names(top) <-
        c("MSISDN",
          "TONE ID",
          "PREDICTION_DATE",
          "TYPE_ID")
      DT::datatable(
        top,
        filter = 'top',
        extensions = c('Buttons', 'AutoFill', 'ColReorder'),
        options = list(
          autoFill = TRUE,
          colReorder = TRUE,
          dom = 'Blfrtip',
          buttons = list(
            'copy',
            'print',
            list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            )
          )
        )
      )
    })
  })
  
  output$toprecos <- DT::renderDataTable({
    topRECOS <- getRecos(fileToReco)
    names(topRECOS) <-
      c(
        "Subscriber Number",
        "Category ID",
        "Parent ID",
        "Tone ID",
        "Tone Name",
        "Download Count",
        "Like Count",
        "Rank",
        "Name"
      )
    DT::datatable(
      topRECOS,
      filter = 'top',
      extensions = c('Buttons', 'AutoFill', 'ColReorder'),
      options = list(
        autoFill = TRUE,
        colReorder = TRUE,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          'print',
          list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )
        )
      )
    )
  })
  
  
  
  output$tSburst <- renderSunburst({
    sunData <- getSunburstCategories()
    sunburst(sunData, legend = FALSE)
  })
  
  output$tRburst <- renderSunburst({
    topREC <- getRecoCategories(fileToReco)
    sunburst(topREC, legend = list(r = 8, w = 150))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
