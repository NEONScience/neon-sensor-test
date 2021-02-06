#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(tidyr)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    default.app.dir = "C:/1_GitHub/neon-sensor-test/neon-sensor-test_shiny-app/"
    
    
    output.refe.data = shiny::reactive({
        if(input$s3_select_1 != "None"){
            readRDS(file = paste0(default.app.dir, input$s3_select_1)) %>%
                tidyr::unite(col = "timestamp", c(DATE, TIME), remove = FALSE,sep = "T") %>%
                dplyr::mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
        } else {
            data.table::data.table()
        }
    })
    
    output.test1.data = shiny::reactive({
        if(input$s3_select_2 != "None"){
            readRDS(file = paste0(default.app.dir, input$s3_select_2)) %>%
                tidyr::unite(col = "timestamp", c(DATE, TIME), remove = FALSE,sep = "T") %>%
                dplyr::mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
        } else {
            data.table::data.table()
        }
    })
    output.test2.data = shiny::reactive({
        if(input$s3_select_3 != "None"){
            readRDS(file = paste0(default.app.dir, input$s3_select_3)) %>%
                tidyr::unite(col = "timestamp", c(DATE, TIME), remove = FALSE,sep = "T") %>%
                dplyr::mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
        } else {
            data.table::data.table()
        }
    })
    
    # Test that all inputs are properly loaded
    # Verify REFERENCE DATA IS LOADED
    output$refe_load_box <- shinydashboard::renderValueBox({
        if(nrow(output.refe.data()>0)){
            data.load = "True"
        } else {
            data.load = "False"
        }
        shinydashboard::valueBox(
            value = paste0("Refe Data Loaded Successfully: ", data.load),
            subtitle = "",
            width = 4,
            color = "navy"
        )
    })
    output$refe_cond_test = shiny::renderText(
        if(nrow(output.refe.data()>0)){
            "True"
        } else {
            "False"
        }
    )
    outputOptions(output, "refe_cond_test", suspendWhenHidden = FALSE)
    # Verify REFERENCE DATA IS LOADED
    
    # Verify TEST1 DATA IS LOADED
    output$test1_load_box <- shinydashboard::renderValueBox({
        if(nrow(output.test1.data()>0)){
            data.load = "True"
        } else {
            data.load = "False"
        }
        shinydashboard::valueBox(
            value = paste0("Test Data 1 Loaded Successfully: ", data.load),
            subtitle = "",
            width = 4,
            color = "navy"
        )
    })
    output$test1_cond_test = shiny::renderText(
        if(nrow(output.test1.data()>0)){
            "True"
        } else {
            "False"
        }
    )
    outputOptions(output, "test1_cond_test", suspendWhenHidden = FALSE)
    # Verify TEST1 DATA IS LOADED
    
    # Verify TEST2 DATA IS LOADED
    output$test2_load_box <- shinydashboard::renderValueBox({
        if(nrow(output.test2.data()>0)){
            data.load = "True"
        } else {
            data.load = "False"
        }
        shinydashboard::valueBox(
            value = paste0("Test Data 2 Loaded Successfully: ", data.load),
            subtitle = "",
            width = 4,
            color = "navy"
        )
    })
    output$test2_cond_test = shiny::renderText(
        if(nrow(output.test2.data()>0)){
            "True"
        } else {
            "False"
        }
    )
    outputOptions(output, "test2_cond_test", suspendWhenHidden = FALSE)
    # Verify TEST2 DATA IS LOADED
    
    
    output$s3_location_1 = shiny::renderTable({
        head(output.refe.data(),10)
    }) 
    
    output$s3_location_2 = shiny::renderTable({
        head(output.test1.data(),10)
    }) 
    
    output$s3_location_3 = shiny::renderTable({
        head(output.test2.data(),10)
    }) 
    
    
    
    # When Run Analysis Button is pressed, start the magic!
    
    duplicate_check = shiny::eventReactive(input$run_analysis,{
        
        # First compare that all three inputs are not duplicated
        one_two = sum(output.refe.data()$`thetXaxs [026]` == output.test1.data()$`thetXaxs [026]`)
        one_three = sum(output.refe.data()$`thetXaxs [026]` == output.test2.data()$`thetXaxs [026]`)
        two_three = sum(output.test1.data()$`thetXaxs [026]` == output.refe.data()$`thetXaxs [026]`)
        
        
        dupe_check = one_two + one_three + two_three
        
        if(dupe_check == 0){
            dupe_check_test = "All three data sets are unique and not duplicated!"
            message("no dupes")
        } else {
            dupe_check_test = paste0("There are ", dupe_check, " duplicated rows...")
            message("dupes!")
        }
        message("App dev, things are going smoothyl")
        dupe_check_test
    })
    
    output$duplicate_check = shiny::renderText(
        duplicate_check()
    )
    
    
    output$plot = shiny::renderPlot({
        if(duplicate_check() == "All three data sets are unique and not duplicated!"){
            
            ggplot2::diamonds
            
            ggplot() +
                geom_point(data = output.refe.data(),  aes(x = timestamp, y = `thetXaxs [026]`, color = "Reference"))+
                geom_point(data = output.test1.data(), aes(x = timestamp, y = `thetXaxs [026]`, color = "Test 1"))+
                geom_point(data = output.test2.data(), aes(x = timestamp, y = `thetXaxs [026]`, color = "Test 2"))+
                scale_x_datetime()
                
            
        } else {
            
            ggplot()+
                geom_text(label = "text")+
                annotate("text", label = paste0("No data found"), x = 0, y = 0, color = "black")+
                theme_minimal()
        }        
        
        
    })

})
