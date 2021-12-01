# Deploy ----------------------------------------------------
# Run these in console, comment out, CHECK THE FILE PATH in the rsconnect::deployApp section!)
# library(rsconnect)
# rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>") $... check shinyapps.io -> tokens
# rsconnect::deployApp('C:\\Users\\keene\\OneDrive\\Documents\\R Projects\\GaTech\\CSE 6242 Analytics and Data Viz\\Project\\team123_app')



# Libs --------------------------------------------------------------------
library(dplyr)
library(shiny)
library(shiny)
library(bslib) #library(shinythemes)
# library(readxl)
library(ggplot2)
# library(ggimage)
library(glue)
library(png)
# library(gsheet)
library(reactable)
# library(stringr)
# library(htmltools)
# library(ggrepel)
library(tidyr)
library(showtext)

# Data --------------------------------------------------------------------

# load local data
df_punts <- read.csv('data/demo_punts.csv')
df_returners <- read.csv('data/demo_returners.csv') %>% mutate(Avg_Yards_Over_Expected = round(Avg_Yards_Over_Expected,2))
df_tracking <- read.csv('data/demo_tracking_polar.csv')
evaluate2 <- read.csv('data/evaluate2.csv')
evaluate3 <- read.csv('data/evaluate3.csv')


# create demo play list
play_list = c('2018090900_485','2018102810_351','2019090800_595','2019120110_3456')



# Themes ------------------------------------------------------------

font_add_google("Play") #from system fonts, download font... only do once?

## Automatically use showtext to render text
showtext_auto()
# thematic_shiny(font = "auto") #no longer used, just need showtext for fonts as images already match theme.


# PRIMARY
#000000 Black            --> background
#ff6600 orange           --> primary
#303030 v. dark grey     --> secondary
#00bfc4 blue             --> accent 
#F5F5F5 HTML White smoke --> text

# REF
#FFFFFF white
#F5F5F5 HTML White smoke
#D3D3D3 HTML light grey
#BEBEBE HTML x11 grey, for black
#808080 HTML Grey
#303030 v. dark grey
#000000 Black


# reactable table theme
options(reactable.theme = reactableTheme(
    color = "#F5F5F5",
    headerStyle = list(background = "#303030", color = "#ff6600"),
    backgroundColor = "#000000",
    borderColor = "#303030",
    stripedColor = "#181818",
    highlightColor = "#282828",
    inputStyle = list(backgroundColor = "#2C2D35"),
    selectStyle = list(backgroundColor = "#2C2D35"),
    pageButtonHoverStyle = list(backgroundColor = "#2C2D35"),
    pageButtonActiveStyle = list(backgroundColor = "#808080"),
    searchInputStyle = list(width = "100%",backgroundColor = "#f5f5f5",color = "#000000")
))


# Shiny theme
theme_v1 <- bs_theme(
    bg = "#000000", fg = "#F5F5F5", 
    primary = "#ff6600", secondary = "#303030", base_font = font_google("Play"), 
    heading_font = font_google("Play"), bootswatch = "darkly"
)

# To use customizer...
# thematic::thematic_shiny(font = "auto") # here
# theme = bs_theme(), # set in UI
# bs_themer() # set in server


# Functions ---------------------------------------------------------------

# color pal
red_yellow_green_pal <- function(x) rgb(colorRamp(c("#A50000", "#FEE700","#00CD00"))(x), maxColorValue = 255)

# update width of title images based on screen size
update_width <- function(screen_width,max_width) {
    
    if (screen_width - 50 > max_width) {
        new_width <- max_width
    } else {
        new_width <- screen_width - 50
    }
    
    return(new_width)
}




# returns html code to display the corresponding play gif
play_gif <- function(play,height){
    #c('<img src="',grade_box_src,'" height="150" width="150">')
    as.character(img(src = glue("{play}.gif"), height = height))
}


# UI ----------------------------------------------------------------------



ui <- navbarPage(
    theme = theme_v1,
    title = "Team 123 - Fall '21",
    position = 'fixed-top',
    collapsible = TRUE,
    tabPanel("Play Evaluation",
             fluidPage(
                 tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
                 br(),
                 br(),
                 br(),
                 br(),
                 uiOutput("title_ui_1"),
                 fluidRow(
                     column(
                         12, align = "center", offset = 0.0,
                         selectInput("play_id", h4("Select Play:"), choices = play_list, selected = play_list %>% head(1))
                     )
                 ),
                 hr(style = "border-top: 1px solid #303030;"),
                 fluidRow(
                     column(
                         12, align = "center",
                         img(src = "text_model.png", width = 280),
                         p(""),
                     )
                     
                 ),
                 hr(style = "border-top: 3px solid #ff6600;"),
                 fluidRow(
                     column(6, align = "center", offset = 3,
                            p("
                            This light-weight demo has 4 stored plays to explore (A single play can have up to five-thousand data points).
                            Select a game and play id and an animated plot will show the the play as described by the player trackign data. 
                            Two predictions are show: (1) A pre-snap prediction created using non-player tracking data in RED, and (2) 
                            A post-snap model which predicts the final location between the time of the punt and the next event in BLUE. 
                            The actual result is shown in BLACK.
                              ")
                     )
                 ),
                 fluidRow(
                     column(
                         12, align = "center",
                         htmlOutput("insert_gif"),
                     )
                 ),
                 hr(style = "border-top: 1px solid #303030;"),
                 #uiOutput("slider_ui"),
                 fluidRow(
                     column(6, align = "center", offset = 3,
                            p("
                            By levearging tracking data, significantly more accurate predictions can be made.
                            Additionally, the predicted resulting x-position and probability of different punt outcomes can be modeled during the play. 
                            These models allow for additional insight into player performance and strategy.
                              ")
                     )
                 ),
                 fluidRow(
                     column(
                         5, align = "center", offset = 1,
                         
                         plotOutput('plot_reg',width = 'auto')
                     ),
                     column(
                         5, align = "center", offset = 0,
                         
                         plotOutput('plot_class',width = 'auto')
                     )
                 ),
                 hr(style = "border-top: 1px solid #303030;"),
                 fluidRow(
                     column(
                         12,align = "center",
                         img(src = "text_team123.png", width = 280)
                     )
                 )
             )
    ),
    tabPanel("Returners",
             br(),
             br(),
             br(),
             br(),
             fluidPage(
                 uiOutput("title_ui_2"),
                 hr(style = "border-top: 1px solid #303030;"),
                 fluidRow(
                     column(
                         12, align = "center",
                         img(src = "text_returners.png", width = 280),
                         p(""),
                     )
                     
                 ),
                 hr(style = "border-top: 3px solid #ff6600;"),
                 fluidRow(
                     column(10, align = "center", offset = 1,
                            p("
                            This approach allow for additional insight into player performance and strategy. 
                            For example, the average yards over expectation can be calculated based on the predicted starting field position at the time of the punt. 
                            In this table, we explore which returners advanced the ball further than the prediction.
                            This is an example output, and while a postive average over expection may be attributed to good decision making or skill, 
                            this analysis is not intended to be a comprehensive analysis of player performance - it does demonstrate the ability to be 
                            building block for more advanced and accurate player and strategy evalution. 
                            (Minimium 50 punt returns, Seasons 2017-2020).
                              ")
                     )
                 ),
                 hr(style = "border-top: 1px solid #303030;"),
                 fluidRow(
                     column(10, align = "center", offset = 1,
                            reactableOutput("ranking_table")
                     )
                 ),
                 hr(style = "border-top: 1px solid #303030;"),
                 fluidRow(
                     column(
                         12,align = "center",
                         img(src = "text_team123.png", width = 280)
                     )
                 )
             )
    )
    
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
    
    # title page 1
    output$title_ui_1 <- renderUI({
        fluidRow(
            column(
                12, align = "center", offset = 0, br(),
                if (length(input$dimension[1])!=0) {
                    img(src =  'title_team123.png',width = update_width(input$dimension[1],650))
                }
            )
        )
    })
    
    # title page 2
    output$title_ui_2 <- renderUI({
        fluidRow(
            column(
                12, align = "center", offset = 0, br(),
                if (length(input$dimension[1])!=0) {
                    img(src =  'title_team123.png',width = update_width(input$dimension[1],650))
                }
            )
        )
    })
    
    # insert gif
    output$insert_gif <- renderText({
        temp_play <- input$play_id
        play_gif(temp_play,height = 600)
    })
    
    
    # output$slider_ui <- renderUI({
    #     fluidRow(
    #         column(
    #             12, align = "center", offset = 0, br(),
    #             sliderInput("frame_id", h4("Frame-by-Frame Animation:"),
    #                min = min(evaluate3 %>% filter(game_id_play_id == input$play_id) %>% pull(frame_id)), 
    #                max = max(evaluate3 %>% filter(game_id_play_id == input$play_id) %>% pull(frame_id)), 
    #                value = min(evaluate3 %>% filter(game_id_play_id == input$play_id) %>% pull(frame_id)), 
    #                step = 1,
    #                animate = animationOptions(interval = 1000, loop = TRUE)
    #                )
    #         )
    #     )
    # })
        
        
    
    
    
    
    ### Plots
    
    # Plots
    output$plot_reg <- renderPlot({
        
        play_result <- evaluate3 %>% filter(game_id_play_id == input$play_id) %>% pull(x_end) %>% unique()
        #slider_frame_id <- input$frame_Id
        #frame_id_pred <- evaluate3 %>% filter(game_id_play_id == input$play_id, frame_id == input$frame_id) %>% pull(pred)
        min_frame_id = evaluate3$frame_id %>% min()
        
        evaluate3 %>% filter(game_id_play_id == input$play_id) %>% 
            ggplot() + 
            geom_point(aes(y = pred, x = frame_id),size = 2, color = '#F5F5F5', alpha = 0.5) +
            geom_smooth(aes(y = pred, x = frame_id),color = '#ff6600') +
            geom_hline(yintercept = play_result, color = '#00bfc4') + 
            annotate(geom = "label", x = min_frame_id, y = play_result, label = "Actual Result", hjust = "left",color = '#00bfc4',fill = "black") +
            labs(
                title = "Post-Snap Regression Model",
                subtitle = 'Predictions are updated during the play',
                x = 'Frame ID',
                y = 'x: Predicted Result'
            ) +
            theme(legend.position = "none",
                  plot.background = element_rect(fill = "black",color = 'black'),
                  panel.background = element_rect(fill = "black",color = 'black'),
                  panel.border = element_blank(), 
                  panel.grid = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_line(colour = "#F5F5F5"),
                  text = element_text(colour = "#F5F5F5", size=16,family = 'Play'),
                  axis.text = element_text(colour = "#F5F5F5", size = 14, family = 'Play')
            )
    })
    
    # Plots
    output$plot_class <- renderPlot({
        evaluate2 %>% filter(game_id_play_id == input$play_id) %>% 
            pivot_longer(cols = c('pred_downed', 'pred_fair_catch', 'pred_muffed', 'pred_out_of_bounds', 'pred_return', 'pred_touchback')) %>% 
            ggplot(aes(y = value, x = frame_id, color = name)) + 
            geom_smooth() + 
            geom_point(size = 2, alpha = 0.5) +
            labs(
                title = "Post-Snap Classification Model",
                subtitle = 'Outcome probabilities are updated during the play',
                x = 'Frame ID',
                y = 'Probability of Play Outcome'
            ) +
            theme(plot.background = element_rect(fill = "black",color = 'black'),
                  panel.background = element_rect(fill = "black",color = 'black'),
                  panel.border = element_blank(), 
                  panel.grid = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_line(colour = "#F5F5F5"),
                  text = element_text(colour = "#F5F5F5", size=16,family = 'Play'),
                  axis.text = element_text(colour = "#F5F5F5", size = 14, family = 'Play'),
                  legend.position="right",
                  legend.title = element_blank(),
                  legend.key = element_rect(colour = NA, fill = NA),
                  legend.text = element_text(colour="#F5F5F5"),
                  legend.background=element_blank()
            )
    })
    
    
    ### RANKING PAGE
    output$ranking_table <- renderReactable({

        # table
        reactable(
            df_returners,
            filterable = FALSE, searchable = TRUE,
            showPageSizeOptions = TRUE, pageSizeOptions = c(5,10,25), defaultPageSize = 10,
            highlight = TRUE,
            outlined = TRUE,
            defaultSorted = c("Avg_Yards_Over_Expected"), defaultSortOrder = "desc",
            defaultColDef = colDef(
                header = function(value) gsub("_", " ", value, fixed = TRUE),
                #cell = function(value) format(value, nsmall = 1),
                align = "left",
                minWidth = 70
                #headerStyle = list(background = "#f7f7f8")
            ),
            columns = list(
                Name = colDef(minWidth = 150),
                Avg_Yards_Over_Expected = colDef(cell = function(value) format(value, nsmall = 1),
                                       style = function(value){
                                           value <- ifelse(is.na(value),0,value)
                                           normalized <- (value - (-2)) / (2-(-2)) #(value - min) / (max-min)
                                           normalized <- ifelse(normalized>1.0,1.0,ifelse(normalized<0,0,normalized))
                                           color <- red_yellow_green_pal(normalized)
                                           list(color = color, fontWeight = "bold")
                                       })
            )
        )
    })

    
    
}


# Run ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)