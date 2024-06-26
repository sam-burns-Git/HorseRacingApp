library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(flexdashboard)
library(readxl)
library(xml2)

dat = read_excel("SportingLifeFull2.xlsx")
model = glm(Incomplete ~ factor(Track)+factor(Season)+factor(TypeofFence)+
              factor(DistanceCat)+factor(GroundConditions)+factor(Runners)+factor(Age)+factor(Weight), data= dat)
# Define UI for application that draws a plot
ui = (
  dashboardPage(skin = 'green',
    dashboardHeader(title = " FYP 118425036",
                    dropdownMenu(type = 'notifications',
                                 notificationItem(
                                   text = 'Enter values in "Widgets" tab',
                                   icon = icon("question")
                                 ),
                                 notificationItem(
                                   text = 'See percentage chance in "Dashboard"',
                                   icon = icon("question")))),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard",tabName = 'dashboard',icon = icon('tachometer-alt')),
        menuItem('Widgets', icon = icon('th'), tabName = 'widgets'))
      ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'dashboard',
                box(tableOutput("values"), background  = 'green', width =7),
                box(gaugeOutput("prediction"),width=7,title="Chance of Completing the Race",background ="green")),
        tabItem(tabName = 'widgets',
                radioButtons("Fence","Type of Fence:",
                             c("Hurdle","Chase"), inline = TRUE),
                selectInput("Track","Track:",
                            c("Bellewstown","Clonmel",'Cork','Down Royal','Downpatrick',
                              'Fairyhouse','Galway','Gowran Park','Kilbeggan','Killarney',
                              'Limerick','Listowel','Naas','Navan','Punchestown','Roscommon',
                              'Sligo','Thurles','Tipperary','Tramore','Wexford')),
                sliderTextInput("Distance","Distance Category:",
                                choices = c("<2m3f","2m3f - 2m5f","2m6f - 3m",">3m"),grid = TRUE, force_edges = TRUE),
                sliderTextInput("Age",
                                "Age of the Horse:",
                                choices = c('3','4','5','6','7','8','9','10','11','12+'),
                                selected = '3',
                                grid = TRUE, force_edges = TRUE),
                sliderTextInput("Weight","Weight Carried:",
                                choices = c("<11","11 ->  11-6","11-7 -> 11-13","12+"),grid = TRUE, force_edges = TRUE),
                sliderTextInput("GroundC","Ground Conditions:",
                                choices = c("Heavy","Soft","Yielding","Good"),grid = TRUE, force_edges = TRUE),
                selectInput("Runners","No.of Runners:",
                            c("<10","10-12","13-15","16-18",">18")),
                sliderTextInput("Season","Season:",
                                choices = c ('Winter','Spring',"Summer",'Autumn'),grid = TRUE, force_edges = TRUE)))
      )
    )
  )
# Define server logic required to produce probability
server = (function(input,output){
  Values <- reactive({
    data.frame(
      Variable = c("Track",
               "Age",
               "No. of Runners",
               "Ground Conditions",
               "Weight",
               "Season",
               "Distance",
               "Type of Fence"),
      Value = as.character(c(input$Track,
                             input$Age,
                             input$Runners,
                             input$GroundC,
                             input$Weight,
                             input$Season,
                             input$Distance,
                             input$Fence)),
      stringsAsFactors = TRUE)
    })
  # Show the variables in a table
  output$values <- renderTable({
    Values()
  })
  TestSet = reactive({
    data.frame(
      Track = input$Track,
      Season = input$Season,
      TypeofFence = input$Fence,
      DistanceCat = input$Distance,
      GroundConditions = input$GroundC,
      Runners = input$Runners,
      Age = input$Age,
      Weight = input$Weight)
  })
  Prediction = reactive({
    predict(model, newdata = TestSet(),type = 'response')
    })
  output$prediction<- renderGauge({
    gauge(as.integer(100*(1 - round(Prediction(),2))), min = 0, max = 100, symbol = '%', 
          label = paste("Prediction"),
          sectors = gaugeSectors(success = c(90, 100), 
                                 warning = c(75, 89),
                                 danger = c(0, 74)))
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

