#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rcarbon)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(HTML(paste0(tags$sup("14"), "C calibration using rcarbon"))),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("bp",
                         "uncalibrated BP Value:",
                         value = 4000),
            numericInput("std",
                         "Standard Deviation uncalibrated BP Value:",
                         value = 25)
            
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            h2(textOutput("calibRange")),
            
            plotOutput("calibPlot"),
            HTML(
                "<p>This small online tool is based on the R Package ´rcarbon´ (Crema/Bevan 2020)
                       and can be used for quick calibration of <sup>14</sup>C dates online.</p>
                       
                       <p>If you consider more elaborated tasks, why not using the original package inside R?</p>
                       
                       <p>You can find the package at <a href='https://github.com/ahb108/rcarbon'>https://github.com/ahb108/rcarbon!</a> </p>
                       
                       <h3>References:</h3>
                       
                       <p>Crema, E.R., Bevan, A. 2020
                       Inference from Large Sets of Radiocarbon Dates:
                       Software and Methods Radiocarbon,
                       doi:10.1017/RDC.2020.95</p>")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$calibPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        result <- calibrate(x = input$bp,errors = input$std)
        
        # draw the histogram with the specified number of bins
        plot(result, HPD = T)
    })
    
    output$calibRange <- renderText({
        # calibrate
        result <- calibrate(x = input$bp,errors = input$std)
        this_hpdi <- rcarbon::hpdi(result)[[1]]
        # get cal result
        cal_range <- rev(range(this_hpdi))
        
        out_cal_range <- paste0(cal_range, collapse = " – ")
        
        cal_range_bcad <- rcarbon::BPtoBCAD(cal_range)
        
        if (cal_range_bcad[1]>0 & cal_range_bcad[2]>0) {        
            ext1 <- ""; ext2 = "cal AD"
        } else if (cal_range_bcad[1]<=0 & cal_range_bcad[2]<=0) {
            ext1 <- ""; ext2 = "cal BC"
        } else {
            ext1 <- ifelse(cal_range_bcad[1]>0, "cal AD", "cal BC")
            ext2 <- ifelse(cal_range_bcad[2]>0, "cal AD", "cal BC")
        }
        ext <- c(ext1,ext2)
        
        text_cal_range_bcad <- paste(abs(cal_range_bcad), ext)
        out_cal_range_bcad <- paste0(text_cal_range_bcad, collapse = " – ")
        
        # get sigma ranges
        #paste(matrix(apply(this_hpdi, 1, paste, collapse=' - '), ncol=1), collapse="<br>")
        
        out <- paste0(out_cal_range, " cal BP (", out_cal_range_bcad,")")
        out
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
