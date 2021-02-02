#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("AMRS Firmware Obsolescence Testing"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 2,
            selectInput("s3_select_1",
                        "S3 Location of Reference Data",
                        choices = c("None", "data/reference.sensor.RDS", "data/test.sensor.1.RDS", "data/test.sensor.2.RDS"),
                        selected = "None"
            ),
            selectInput("s3_select_2",
                        "S3 Location of Test Unit 1's Data",
                        choices = c("None", "data/reference.sensor.RDS", "data/test.sensor.1.RDS", "data/test.sensor.2.RDS"),
                        selected = "None"
            ),
            selectInput("s3_select_3",
                        "S3 Location of Test Unit 2's Data",
                        choices = c("None", "data/reference.sensor.RDS", "data/test.sensor.1.RDS", "data/test.sensor.2.RDS"),
                        selected = "None"
            ),
            conditionalPanel(condition = "output.refe_cond_test == 'True' & output.test1_cond_test == 'True' & output.test2_cond_test == 'True'",  
                             actionButton("run_analysis", "Run Analysis?"))
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 10,
                  shiny::fluidRow(
                      shiny::textOutput("duplicate_check")
                  ),
                  shiny::fluidRow(
                    shinydashboard::valueBoxOutput("refe_load_box", width = 4),
                    shinydashboard::valueBoxOutput("test1_load_box", width = 4),
                    shinydashboard::valueBoxOutput("test2_load_box", width = 4)
                    ),
                  shiny::fluidRow(
                      shiny::plotOutput(
                          "plot"
                      )
                  ),
                  shiny::fluidRow(
                    shiny::h2("Head of Reference's Data"),
                    shiny::tableOutput("s3_location_1"),
                    shiny::h2("Head of Test Unit 1's Data"),
                    shiny::tableOutput("s3_location_2"),
                    shiny::h2("Head of Test Unit 2's Data"),
                    shiny::tableOutput("s3_location_3")
                  )
        )
    )
))
