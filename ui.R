


# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        
        useShinyjs(),
        
    # Application title
        titlePanel("Longevity in Heart Failure Patients by Factors"),
    

        sidebarLayout(
            sidebarPanel(
                
                # slider widget to adjust bin width
                sliderInput(
                    inputId = "bins",
                    label = "Bin Width Adjustment Range",
                    min = 2,
                    max = 20,
                    value = 10,
                    step = 2,
                    ticks = F),
            
                # slider widget to adjust age range in graphs
                sliderInput(
                    inputId = "age",
                    label = "Age Adjustment Range",
                    min = min(hearts$age),
                    max = max(hearts$age),
                    value = c(60, 75),
                    dragRange = T,
                    round = T,
                    ticks = F),
                
                # checkbox to allow Tab 4's graph to be adjusted by the age slider above
                checkboxInput(
                    inputId = "adjust_age",
                    label = "Allow Age Adjustment",
                    value = FALSE
                      ),
                
                # checkbox to filter out for DEATH_EVENT == 1
                checkboxInput(
                    inputId = "yes_death",
                    label = "Filter for Culmination in Death Event",
                    value = FALSE
                      )),
                
                
        mainPanel(
            
            textOutput("tabtext"),
                tabsetPanel(id = "my_tabs",
    
                    tabPanel("1) # Pts", value = "A",
                             plotOutput(outputId = "count_age")),
                    tabPanel("2) Survival F vs M", value = "B",
                             fluidRow(
                                 column(6, plotOutput(outputId = "post_time_sex")),
                                 column(6, plotOutput(outputId = "post_time_sex2")))),
                    tabPanel("3) Pre-Ex Cnd F vs M", value = "C",
                             plotOutput(outputId = "count_factors")),
                    tabPanel("4) SerNa vs SerCr", value = "D",
                             fluidRow(
                                 column(6, plotOutput(outputId = "sna_scr_filtered")),
                                 column(6, plotOutput(outputId = "sna_scr_unfiltered")))),
                    tabPanel("5) CrPhK vs SerCr", value = "E",
                             fluidRow(
                                 column(6, plotOutput(outputId = "crpk_scr_filtered")),
                                 column(6, plotOutput(outputId = "crpk_scr_unfiltered")))),
                    tabPanel("6) No Evdnt Corr", value = "F",
                             fluidRow(
                                 column(6, plotOutput(outputId = "ef_scr")),
                                 column(6, plotOutput(outputId = "plt_scr"))))
                )
            )
        )
    )
)






























