
library(shiny)
library(tidyverse)

# Pitch Choices
load("data/game_sim.Rdata")
pitch_choices <- game_sim_data$pitch_name %>% unique()

num_strikes <- 0
num_balls <- 0
num_outs <- 0 

pitches_thrown <- data.frame()

strikezone <- data.frame(x=c(-0.85, 0.85, 0.85, -0.85), 
                         y=c(1.6, 1.6, 3.5, 3.5), 
                         xend = c(0.85, 0.85, -0.85, -0.85), 
                         yend = c(1.6, 3.5, 3.5, 1.6))

ui <- fluidPage(

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
                         label = "Throw Pitch!")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    rv <- reactiveValues(plot = NULL,
                         pitches_thrown = NULL)
    
    observeEvent(input$pitchThrown, {
        data <- game_sim_data %>%
            filter(strikes == num_strikes, balls == num_balls, 
                   outs_when_up == num_outs, pitch_name == input$pitchThrown)
        pitch <- data[sample(1:nrow(data), 1),]
        pitches_thrown <- rbind(pitches_thrown, pitch)
        
        if (pitch$type == "B") {
            num_balls <- num_balls + 1
        } else if (pitch$type == "S") {
            num_strikes <- num_strikes + 1
            #TODO: num_strikes == 3
        } else {
            #TODO: Handle other event
        }
        
        rv$plot <- ggplot(pitches_thrown) +
            geom_segment(data = strikezone, aes(x=x, y=y, xend=xend, yend=yend)) +
            geom_curve(aes(x=release_pos_x, y=release_pos_z, 
                           xend=plate_x, yend=plate_z, color=pitch_type),
                       curvature = -.05) +
            geom_point(aes(x=plate_x, y=plate_z, color=pitch_type)) +
            coord_fixed() +
            theme_void() 
    })
    
    output$distPlot <- renderPlot({
        if (is.null(rv$plot)) return()
        rv$plot
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
