# Claudio Zanettini

source("other_f.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "MD crashes"),
    dashboardSidebar(
      
      actionButton("license", label = "about", style = "text-transform: lowercase; font-style: italic;"),
      
        selectInput(
            inputId = "imp_county",
            label = "Filter: Select a County", 
            choices = as.character(counties),
            selected = "Howard"
        ),
        
        selectInput(
            inputId = "imp_var",
            label = "Color: Select a Variable",
            choices = other_var,
            selected = "none"
            
        ),

        selectInput(
            inputId = "imp_hov",
            label = "Hover and Histogram: Select a Variable",
            choices = hover_var,
            selected = "year"
        ),
        
        conditionalPanel( condition = "output.show_p",
          downloadBttn("download",
                       label = "Download Selection", 
                       style = "material-flat", 
                       size = "sm",
                       color = "primary")
          )
        ),
        
       
    dashboardBody(
        plotlyOutput("map", height = "auto"),
        plotOutput("ist"),
        DTOutput("brush",  width = "100%")
        
    )
        
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    # Reactive Data ----
    dat_fr <- reactiveVal()
    dat_fr <- reactive({
      
      all %>%
        filter(county_desc == input$imp_county)
        
    })
    
    # Default zoom. This is used when event_data("plotly_relayout") is null
    
    m_lay <- reactiveVal()
    m_lay_last <- reactiveVal()
    
    m_center = list(lon = -76, lat = 39)
    m_zoom = 5
    
    # Reactive map layout -----
    
    m_lay <- reactive({
        
      x <- event_data("plotly_relayout")  
      
               
      # Default values
      if (is.null(x)){
        m_lay <- list(mapbox.center = list(lon = m_center$lon, lat = m_center$lat), mapbox.zoom = m_zoom)
      } 
      
      if(length(x) == 5){
        m_lay <- event_data("plotly_relayout")
      } 
      
      if(length(x) != 5 && !is.null(x)){
        m_lay <- isolate(m_lay_last())
      }

      m_lay_last(m_lay)

      m_lay
      

    })

    
    # license --------
    observeEvent(
      ignoreNULL = TRUE,
      eventExpr = {
        input$license
      },
      handlerExpr = {
        sendSweetAlert(
          session = session,
          title = NULL,
          html = TRUE,
          text =
            HTML("
             <a href=\"https://github.com/c1au6i0/maryland_crashes\"> code of the app is on my github </a> <br>
             Data provided by Maryland State Police <br>
             <a href=\"https://opendata.maryland.gov/Public-Safety/Maryland-Statewide-Vehicle-Crashes/65du-s3qu\"> link to data </a>"),
          width = "400px"
        )
      }
    )
    
    # render Map -----
    output$map <- renderPlotly({
        p <- mapbox(dat = dat_fr(),
                    hover = input$imp_hov, 
                    variab = input$imp_var,
                    c_lon = isolate(m_lay()$mapbox.center$lon),
                    c_lat = isolate(m_lay()$mapbox.center$lat),
                    c_zoom = isolate(m_lay()$mapbox.zoom)
                    )
    })
    
    
    # Reactives to show download button -----
    show_p <- reactiveVal(value = FALSE)
    output$show_p <- reactive({show_p()})
    outputOptions(output, "show_p", suspendWhenHidden = FALSE)

    # get data selected with lasso in plotly -----
    selected_dat <- reactive({
      d <- event_data("plotly_selected")
      
      if (!is.null(d)){
        d_keys  <- as.numeric(d$pointNumber + 1)
        brush <- dat_fr()[d_keys,]
        show_p(TRUE) 
        brush
        }
      })
    
    # Reactive Lasso ------
    output$brush <- renderDataTable(selected_dat(), options = list(scrollX = TRUE))
    
    
    # Render Inst -----
    output$ist <- renderPlot({
        if(!is.null(input$imp_hov)) {
         inst <- ist_plot(req(selected_dat()), variab = input$imp_hov)
         
         inst
         # ggplotly(inst)
        }
      })
    
    # Download -----
    output$download <- downloadHandler(
      filename = function() {
        paste0("selection", ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(selected_dat(), file)
      }
    )
     
    
}

# Run the application 
shinyApp(ui = ui, server = server)


