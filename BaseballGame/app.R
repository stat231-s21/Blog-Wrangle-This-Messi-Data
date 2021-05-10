library(shiny)
library(tidyverse)
library(plotly)
library(shinyjs)
library(stringr)

# Pitch Choices
load("data/game_sim.Rdata")

pitch_choices <- game_sim_data$pitch_name %>% unique()


pitches_thrown <- data.frame()

strikezone <- data.frame(x=c(-0.85, 0.85, 0.85, -0.85), 
                         y=c(1.6, 1.6, 3.5, 3.5), 
                         xend = c(0.85, 0.85, -0.85, -0.85), 
                         yend = c(1.6, 3.5, 3.5, 1.6))

ui <- fluidPage(
    useShinyjs(),

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("pitcherThrows",
                         label = "Pick the Pitchers Throwing Arm",
                         choices = c("Right", "Left"),
                         selected = c("Right"),
                         inline = TRUE),
            radioButtons("batterStands",
                         label = "Pick the Batter's Side",
                         choices = c("Right", "Left"),
                         selected = c("Right"),
                         inline = TRUE),
            radioButtons("pitchThrown",
                          label = "Pick the Pitch to Throw",
                          choices = pitch_choices,
                          selected = c("4-Seam Fastball"),
                          inline = FALSE),
            actionButton("throwPitch",
                         label = "Throw Pitch!"),
            actionButton("newAtBat",
                         label = "Restart At-Bat")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textOutput("countName")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    rv <- reactiveValues(plot = NULL,
                         pitches_thrown = data.frame(), 
                         num_strikes = 0,
                         num_balls = 0,
                         num_outs = 0,
                         atBatDescription = "")
    
    
        
    observeEvent(input$throwPitch, {
        
        if(rv$num_strikes + rv$num_balls == 0) {
            disable("pitcherThrows") 
            disable("batterStands")
        }
    
    
        
    
        data <- game_sim_data %>%
            filter(strikes == rv$num_strikes, balls == rv$num_balls, 
                   outs_when_up == rv$num_outs, pitch_name == input$pitchThrown,
                   p_throws == substr(input$pitcherThrows, 1, 1),
                   stand == substr(input$batterStands, 1, 1))
        pitch <- data[sample(1:nrow(data), 1),]
        rv$pitches_thrown <- rbind(rv$pitches_thrown, pitch)
        if (pitch$type == "B") {
            rv$num_balls <- rv$num_balls + 1
            if(rv$num_balls == 4){
                disable("throwPitch")
                rv$atBatDescription = sub(",", ".", str_extract(pitch$des, ".*?[a-z0-9][,.]"))
            }
        } else if (pitch$type == "S") {
            rv$num_strikes <- rv$num_strikes + 1
            #TODO: fouling off pitches with 2 strikes, ask about color changes
            if (rv$num_strikes == 3){
                disable("throwPitch")
                rv$atBatDescription = sub(",", ".", str_extract(pitch$des, ".*?[a-z0-9][,.]"))
            }
        } else {
            rv$atBatDescription = sub(",", ".", str_extract(pitch$des, ".*?[a-z0-9][,.]"))
        }
        
        rv$plot <- ggplot(rv$pitches_thrown) +
            geom_segment(data = strikezone, aes(x=x, y=y, xend=xend, yend=yend)) +
            geom_curve(aes(x=release_pos_x, y=release_pos_z, 
                           xend=plate_x, yend=plate_z, color=pitch_type),
                       curvature = -.05) +
            geom_point(aes(x=plate_x, y=plate_z, color=pitch_type)) +
            coord_fixed() +
            theme_void() 
    })
    
    observeEvent(input$newAtBat, {
        enable("pitcherThrows")
        enable("batterStands")
        enable("throwPitch")
        rv$num_strikes = 0
        rv$num_balls = 0
        rv$atBatDescription = ""
        rv$pitches_thrown = data.frame()
        rv$plot = NULL
        })
    
    output$distPlot <- renderPlot({
        if (is.null(rv$plot)) return()
        rv$plot
    })
    
    output$countName <- renderText({
        if (rv$atBatDescription == ""){
            paste(rv$num_balls, "-", rv$num_strikes)
        }
        else(
            rv$atBatDescription
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
