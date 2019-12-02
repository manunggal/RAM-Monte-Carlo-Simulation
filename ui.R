

# UI

header = dashboardHeader(title = "RAM Simulation",
                         titleWidth = 300)

sidebar = dashboardSidebar(
  disable = TRUE,
  collapsed = TRUE,
  sidebarMenu()
)

body = dashboardBody(
  tabsetPanel(
    tabPanel("simulation",
             fluidRow(
               box(
                 width = 4,
                 title = "Inputs", status = "primary", solidHeader = TRUE,
                 rHandsontableOutput("hot"),
                 br(),
                 actionButton("gen_rbd", "Generate RBD", icon("project-diagram"),
                              style="color: #fff; background-color: #A52A2A; border-color: #A52A2A"),
                 br(),
                 splitLayout(
                   numericInput("mission_time", "Mission Time Duration (Years)", 5, min = 1, max = 10),
                   numericInput("sim_number", "Simulations Number:", 1000, min = 10, max = 5000)
                 ),
                 actionButton("simulate", "Start Simulation", icon("paper-plane"),
                              style="color: #fff; background-color: #A52A2A; border-color: #A52A2A")
                 ),
               column(
                 width = 8,
                 fluidRow(
                   box(width = 12, height = 300, status = "primary", solidHeader = TRUE,
                       title = "Reliability Block Diagram (RBD)", DiagrammeROutput("rbd")
                     )
                 ),
                 fluidRow(
                   valueBoxOutput("reliability"),
                   valueBoxOutput("availability"),
                   valueBoxOutput("failure_numbers")
                 )
               )
             ),
             fluidRow(
               box(withSpinner(plotlyOutput("plot_reliability")), width = 4,
                   title = "Reliability Timeseries",
                   status = "primary", solidHeader = TRUE),
               box(withSpinner(plotlyOutput("plot_maintenance")), width = 4,
                   title = "Numbers of Failure Probability",
                   status = "primary", solidHeader = TRUE),
               box(withSpinner(plotlyOutput("plot_failure_cause")), width = 4,
                   title = "System Failure Causes",
                   status = "primary", solidHeader = TRUE)
             )
             # fluidRow(
             #   # debugging
             #   ########
             #   verbatimTextOutput("text_output"),
             #   dataTableOutput('data_1'),
             #   dataTableOutput("data_2"),
             #   dataTableOutput("data_3")
             #   ########
             # )
             
    ),
    tabPanel("readme", uiOutput("readme"), style = "background-color: #ffffff;")
      
  )
  
  
)

ui = dashboardPage(header, sidebar, body) 






