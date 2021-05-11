library(shiny)
library(tidyverse)
library(plotly)
library(shinyjs)
library(stringr)
library(GeomMLBStadiums)

# Load dataset
load("data/game_sim.Rdata")

# List of pitch choices
pitch_choices <- game_sim_data$pitch_name %>% unique()

# Coordinates for drawing bases
bases <- data.frame(x = c(155, 160, 155, 150, 125, 120, 125, 130,
                          95, 90, 95, 100, 125, 120, 125, 130),
                    y = c(173, 168, 163, 168, 133, 138, 143, 138, 
                          173, 168, 163, 168, 208, 203, 198, 203),
                    base = c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4))
# Coordinates for drawing strikezone
strikezone <- data.frame(x=c(-0.85, 0.85, 0.85, -0.85), 
                         y=c(1.6, 1.6, 3.5, 3.5), 
                         xend = c(0.85, 0.85, -0.85, -0.85), 
                         yend = c(1.6, 3.5, 3.5, 1.6))

#####################################
#   UI
#####################################

ui <- fluidPage(
    useShinyjs(),

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with radio buttons, buttons, and text
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
                         label = "Restart At-Bat"),
            textOutput("countName")
        ),

        # Main panel with graphs
        mainPanel(
            plotOutput("sprayChart"),
            plotOutput("strikezone")
        )
    )
)

#####################################
#   SERVER
#####################################

server <- function(input, output) {
    
    # List of reactive Values
    rv <- reactiveValues(plot = NULL,
                         ball_in_play = data.frame(),
                         pitches_thrown = data.frame(), 
                         num_strikes = 0,
                         num_balls = 0,
                         num_outs = 1,
                         atBatDescription = "",
                         bases = c(FALSE, FALSE, FALSE, TRUE))
    
    #####################################
    #   Function: Update bases
    #####################################
    
    updateBases <- function(outcome) {
        event <- case_when(outcome == "single" ~ 1,
                           outcome == "field_error" ~ 1,
                           outcome == "double" ~ 2,
                           outcome == "triple" ~ 3,
                           outcome == "home_run" ~ 4,
                           TRUE ~ 0)
        
        newBases <-c(event, ifelse(rv$bases[1], 1 + event, 0), 
                     ifelse(rv$bases[2], 2 + event, 0), ifelse(rv$bases[3], 3 +event, 0))
        
        runs <- 0
        rv$bases <- c(F,F,F,T)
        for(i in newBases) {
            if (i >= 4) runs <- runs + 1
            else if (i>0) rv$bases[i] = T
        }
        
    }
    
    #####################################
    #   Throw Pitch: When button clicked    
    #####################################
        
    observeEvent(input$throwPitch, {
        rv$atBatDescription = ""
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
            if (rv$num_strikes == 2 && pitch$description == "foul") {
                rv$atBatDescription = paste0(pitch$player_name, " hit a foul ball. The count is ", 
                                            rv$num_balls, " - ", rv$num_strikes, ".")
            } else {
                rv$num_strikes <- rv$num_strikes + 1
                #TODO: ask about color changes
                if (rv$num_strikes == 3){
                    disable("throwPitch")
                    rv$atBatDescription = sub(",", ".", str_extract(pitch$des, ".*?[a-z0-9][,.]"))
                }
            }
            
        } else {
            rv$atBatDescription = sub(",", ".", str_extract(pitch$des, ".*?[a-z0-9][,.]"))
            disable("throwPitch")
            updateBases(pitch$events)
        }
        
        if (!is.na(pitch$hc_y)) {
            rv$ball_in_play <- pitch %>%
                mutate(xend = 125, yend = 208)
        }
        
        rv$plot <- ggplot(rv$pitches_thrown) +
            geom_segment(data = strikezone, aes(x=x, y=y, xend=xend, yend=yend)) +
            geom_curve(aes(x=release_pos_x, y=release_pos_z, 
                           xend=plate_x, yend=plate_z, color=pitch_type),
                       curvature = -.05) +
            geom_point(aes(x=plate_x, y=plate_z, color=pitch_type)) +
            coord_fixed() +
            theme_void() +
            labs(color = "Pitch Type")
    })
    
    #####################################
    #   Restart At Bat: When button clicked    
    #####################################
    
    observeEvent(input$newAtBat, {
        enable("pitcherThrows")
        enable("batterStands")
        enable("throwPitch")
        rv$num_strikes = 0
        rv$num_balls = 0
        rv$atBatDescription = ""
        rv$pitches_thrown = data.frame()
        rv$plot = NULL
        rv$ball_in_play = data.frame()
    })
    
    #####################################
    #   Output: Spray Chart and Bases  
    #####################################
    
    output$sprayChart <- renderPlot({
        bases_df <- data.frame(base = c(1:4), runner_on = rv$bases)
        bases <- bases %>%
            inner_join(bases_df, by = "base") %>%
            mutate(runner_on = ifelse(runner_on, "on", "off"))
        cols <- c("on" = "yellow", "off" = "white")
        
        p <- ggplot() + 
            geom_mlb_stadium(stadium_ids = "generic",
                             stadium_segments = "all") +
            geom_polygon(data = bases, aes(x=x, y=y, group=base, 
                                           fill = runner_on),
                         color = "black") +
            scale_fill_manual(values = cols) 
        
        if (nrow(rv$ball_in_play > 0)) {
            p <- p + 
                geom_point(data = rv$ball_in_play, aes(x= hc_x, y=hc_y), size = 6) +
                geom_segment(data = rv$ball_in_play, aes(x= hc_x, y=hc_y, xend=xend, yend=yend), size=2)
        }
        
        p <- p +
            annotate("text", x = 35, y = 160, label = "Outs") +
            annotate("text", x = 35, y = 170, label = "Balls") +
            annotate("text", x = 35, y = 180, label = "Strikes")
        
        # Add dots for outs
        outs_df <- data.frame()
        for (i in 1:2) {
            outs_df <- rbind(outs_df, data.frame(x=45+i*6, y=160, 
                                                 out = ifelse(rv$num_outs >= i, "yes", "no")))
        }
        # Add dots for strikes
        strikes_df <- data.frame()
        for (i in 1:2) {
            strikes_df <- rbind(strikes_df, data.frame(x=45+i*6, y=180, 
                                                 out = ifelse(rv$num_strikes >= i, "yes", "no")))
        }
        # Add dots for balls
        balls_df <- data.frame()
        for (i in 1:3) {
            balls_df <- rbind(balls_df, data.frame(x=45+i*6, y=170, 
                                                 out = ifelse(rv$num_balls >= i, "yes", "no")))
        }
        
        cols <- c("yes" = "black", "no" = "grey")
        p <- p +
            geom_point(data = outs_df, aes(x=x, y=y, color = out)) +
            geom_point(data = balls_df, aes(x=x, y=y, color = out)) +
            geom_point(data = strikes_df, aes(x=x, y=y, color = out)) +
            scale_color_manual(values = cols)
        
        p +
            scale_y_reverse() +
            coord_fixed() +
            theme_void() +
            theme(legend.position = "none")
    })
    
    #####################################
    #   Output: Strikezone and Pitches 
    #####################################
    
    output$strikezone <- renderPlot({
        if (is.null(rv$plot)) {
            ggplot(rv$pitches_thrown) +
                geom_segment(data = strikezone, aes(x=x, y=y, xend=xend, yend=yend)) +
                coord_fixed() +
                theme_void()
        } else {
            rv$plot
        }
        
    })
    
    #####################################
    #   Output: Text for count and description
    #####################################
    
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

# TODO:
#   Outs
#   HBP, Walks
#   Score (the score is x-y, can you save the 9th)
#   Difficulty? Different scores?
#   plotly
#   restart batter next -> better
#   restart inning
