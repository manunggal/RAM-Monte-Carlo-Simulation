

# UI

header = dashboardHeader(title = "RAM Simulation",
                         titleWidth = 300)

sidebar = dashboardSidebar(
  disable = TRUE,
  collapsed = TRUE,
  sidebarMenu()
)

body = dashboardBody(
  fluidRow(
    valueBoxOutput("rel_boxout"),
    valueBoxOutput("avail_boxout"),
    valueBoxOutput("failure_boxout")
  ),
  fluidRow(
    box(
      title = "Inputs", status = "primary", solidHeader = TRUE,
      width = 4,
      rHandsontableOutput("hot"),
      br(),
      actionButton("gen_rbd", "Generate RBD", icon("project-diagram"), 
                   style="color: #fff; background-color: #A52A2A; border-color: #A52A2A"),
      br(),
      splitLayout(
        numericInput("mission_time", "Mission Time Duration (Years)", 5, min = 1, max = 20),
        numericInput("sim_number", "Simulations Number:", 1000, min = 10, max = 10000)
      ),
      actionButton("simulate", "Start Simulation", icon("paper-plane"), 
                   style="color: #fff; background-color: #A52A2A; border-color: #A52A2A")
    ), 
    box(
      title = "Reliability Block Diagram (RBD)", status = "primary", solidHeader = TRUE,
      width = 8,
      DiagrammeROutput("rbd")
    ) 
  ),
  fluidRow(
    box(plotlyOutput("plot_reliability"), width = 4,
        title = "Reliability Timeseries",
        status = "primary", solidHeader = TRUE),
    box(plotlyOutput("plot_maintenance"), width = 4,
        title = "Expected Failure Numbers",
        status = "primary", solidHeader = TRUE),
    box(plotlyOutput("plot_failure_cause"), width = 4,
        title = "Failure Numbers Probability",
        status = "primary", solidHeader = TRUE)
  ),
  fluidRow(
    # debugging
    ########
    verbatimTextOutput("text_output"),
    dataTableOutput('data_1'),
    dataTableOutput("data_2"),
    dataTableOutput("data_3")
    ########
  )
  
)

ui = dashboardPage(header, sidebar, body) 







