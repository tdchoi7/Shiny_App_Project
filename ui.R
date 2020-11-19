



shinyUI(
    fluidPage(
        
        useShinyjs(),
        
    # Application title
        titlePanel("Factors Affecting Longevity of Heart Failure Patients"),
    

        sidebarLayout(
            sidebarPanel(
                
# Widgets ---------------------------------------------------------------------------------------
                
                # slider widget to adjust bin width
                sliderInput(
                    inputId = "bins",
                    label = "Bin Width Adjustment",
                    min = 2,
                    max = 20,
                    value = 10,
                    step = 2,
                    ticks = F
                    ),
            
                # slider widget to adjust age range in graphs
                sliderInput(
                    inputId = "age",
                    label = "Age Adjustment",
                    min = min(hearts$age),
                    max = max(hearts$age),
                    value = c(60, 75),
                    dragRange = T,
                    round = T,
                    ticks = F
                    ),
                
                # checkbox to allow Tab 4's graph to be adjusted by the age slider above
                checkboxInput(
                    inputId = "adjust_age",
                    label = HTML("<b>", "Check Box to Allow Age Adjustment", "</b>"),
                    value = FALSE
                    ),
                
                # checkbox to filter out for DEATH_EVENT == 1
                checkboxInput(
                    inputId = "yes_death",
                    label = HTML("<b>", "Check Box to Filter for Culmination in Death Event", "</b>"),
                    value = FALSE
                    )
                ),
                
                
        mainPanel(
            
            # allows for tabular input in server.R using the values to designate tabs
            tabsetPanel(id = "my_tabs",

# Tabs ------------------------------------------------------------------------------------------
                           
                    tabPanel("1) # Pts", value = "A",
                             plotOutput(outputId = "count_age")
                             ),
                    
                    tabPanel("2) Pre-Ex Cnd F vs M", value = "B",
                             plotOutput(outputId = "count_factors"),
                             fluidRow(
                                 htmlOutput(outputId = "count_factors_stats"))
                             ),
                    
                    tabPanel("3) Survival F vs M", value = "C",
                             fluidRow(
                                 column(6, plotOutput(outputId = "post_time_sex")),
                                 column(6, plotOutput(outputId = "post_time_sex2"))),
                             fluidRow(
                                 column(6, htmlOutput(outputId = "post_time_sex_stats")),
                                 column(6, htmlOutput(outputId = "post_time_sex2_stats")))
                             ),
                    
                    tabPanel("4) SerNa vs SerCr", value = "D",
                             fluidRow(
                                 column(6, plotOutput(outputId = "sna_scr_filtered")),
                                 column(6, plotOutput(outputId = "sna_scr_unfiltered"))),
                             fluidRow(
                                 column(6, htmlOutput(outputId = "sna_scr_filtered_stats")),
                                 column(6, htmlOutput(outputId = "sna_scr_unfiltered_stats")))
                             ),
                    
                    tabPanel("5) CrPhK vs SerCr", value = "E",
                             fluidRow(
                                 column(6, plotOutput(outputId = "crpk_scr_filtered")),
                                 column(6, plotOutput(outputId = "crpk_scr_unfiltered"))),
                             fluidRow(
                                 column(6, htmlOutput(outputId = "crphk_scr_filtered_stats")),
                                 column(6, htmlOutput(outputId = "crphk_scr_unfiltered_stats")))
                             ),
                    
                    tabPanel("6) No Evdnt Corr", value = "F",
                             fluidRow(
                                 column(6, plotOutput(outputId = "ef_scr")),
                                 column(6, plotOutput(outputId = "plt_scr"))),
                             fluidRow(
                                 column(6, htmlOutput(outputId = "ef_scr_unfiltered_stats")),
                                 column(6, htmlOutput(outputId = "plt_scr_unfiltered_stats")))
                    )
                )
            )
        )
    )
)















