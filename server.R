


shinyServer(

    function(input, output, session) {
        
        observe({
            input$adjust_age})
        
        # hides the age adjustment slider based on checkbox input
        observeEvent(input$adjust_age, {
            if (input$adjust_age == TRUE) {
                showElement("age")
            } else {
                hideElement("age")
            }
        })
        
        
        observe({
            input$my_tabs})
        
        # shows different widgets based on the tabs selected
        observeEvent(input$my_tabs, {
            ifelse(input$my_tabs == "A",
                   showElement("bins") & hideElement("age") & hideElement("adjust_age") & hideElement("yes_death"),
                   hideElement("bins") & showElement("age"))
            
            if (input$my_tabs == "C" |
                input$my_tabs == "D" |
                input$my_tabs == "E" |
                input$my_tabs == "F") {
                    input$adjust_age == FALSE &
                    hideElement("age") &
                    showElement("adjust_age") &
                    showElement("yes_death")
            } else if (input$my_tabs == "B") {
                    hideElement("adjust_age") &
                    hideElement("yes_death") &
                    showElement("age")
            }
        })
        
        ########################################################################################
        
        # allows for reactive input from slider to adjust graph accordingly
        hearts_slider = reactive({
            hearts %>% 
                filter(., age >= input$age[1] & age <= input$age[2]) %>%
                group_by(., sex) 
        })
        
        # allows user to use the age slider once the adjust_age checkbox is checked
        hearts_slider_2 = reactive({
            if (input$adjust_age == TRUE) {
                return (hearts %>%
                            filter(., age >= input$age[1] &
                                       age <= input$age[2]) %>%
                            group_by(., sex))
            } else {
                return (hearts %>%
                            group_by(., sex))
            }
            
        })
        
        # allows user to filter the data (and hence, the graphs) for positive death events once yes_death checkbox is checked
        death_filter = reactive({
            if (input$yes_death == TRUE) {
                return (hearts_slider_2() %>% filter(., DEATH_EVENT == 1))
            } else {
                return (hearts_slider_2())
            }
            
        })
        
        ########################################################################################

        # "No. Pts Pre-Ex Cnd"
        output$count_age = renderPlot(
            hearts %>% 
                ggplot(aes(x = age)) + 
                labs(title = "Number of Patients by Age Group (Adjustable Bin Width)", 
                     x = "Age", y = "Number of Patients") +
                geom_histogram(binwidth = input$bins, 
                               fill = "purple", 
                               color = "green", 
                               alpha = 0.3) +
                scale_x_continuous(breaks = round(seq(min(hearts$age), 
                                                      max(hearts$age), by = 5), 1)) +
                theme_bw()
        )
        
        ########################################################################################

        # "Survival F vs M"
        output$post_time_sex = renderPlot(
            hearts_slider() %>% 
                filter(., DEATH_EVENT == 1) %>% 
                ggplot(mapping = aes(x = sex, y = time, fill = sex))+
                coord_cartesian(ylim = c(0, 300)) +
                geom_boxplot(alpha = 0.25) +
                stat_summary(fun = quantile, 
                             geom = "text",
                             aes(
                                 label = sprintf("%1.f", ..y..),
                                 color = sex),
                             position = position_nudge(x = 0.45),
                             size = 4) +
                labs(title = "Days Until Death (Adjustable Age - Youngest Age: 42)", 
                     x = "Sex", y = "Number of Days") +
                scale_fill_brewer(palette = 'Set1') +
                theme_bw())
        
        output$post_time_sex2 = renderPlot(
            hearts_slider() %>% 
                ggplot(mapping = aes(x = sex, y = time, fill = sex)) +
                coord_cartesian(ylim = c(0, 300)) +
                geom_boxplot(alpha = 0.25) +
                stat_summary(fun = quantile, 
                             geom = "text",
                             aes(
                                 label = sprintf("%1.f", ..y..),
                                 color = sex),
                             position = position_nudge(x = 0.45),
                             size = 4) +
                labs(title = "Days Until Death or Loss of Contact (Adjustable Age - Youngest Age: 40)", 
                     x = "Sex", y = "Number of Days") +
                scale_fill_brewer(palette = 'Set1') +
                theme_bw()
        )
        
        ########################################################################################
        
        # "Pre-Ex Cnd F vs M"
        output$count_factors = renderPlot(
            death_filter() %>% 
                summarise(., 
                          Anaemia = sum(anaemia),
                          Diabetes = sum(diabetes),
                          `High Blood Pressure` = sum(high_blood_pressure),
                          Smoker = sum(smoking)) %>% 
                gather(., key = Conditions, value = `Number of Patients`, 2:5) %>%
                ggplot(aes(x = Conditions, y = `Number of Patients`)) +
                geom_bar(aes(fill = sex), 
                         stat = "identity", 
                         position = "dodge", 
                         alpha = 0.3) +
                labs(title = "Number of Patients with Pre-Existing Conditions (Adjustable Age)", 
                     x = "Pre-Existing Conditions", y = "Number of Patients") +
                scale_fill_brewer(palette = 'Set1') +
                theme_bw()
        )
        
        ########################################################################################
        
        # "Sodium vs Creatinine"
        output$sna_scr_filtered = renderPlot(
            
            death_filter() %>% 
                filter(., 
                       ifelse(sex == "M", serum_creatinine >=1.5, serum_creatinine >=1.2)) %>% 
                ggplot(aes(x = serum_creatinine, y = serum_sodium)) +
                geom_point(aes(color = sex), alpha = 0.5) +
                geom_line(stat = "smooth", aes(color = sex), se = F, alpha = 0.25, size = 2) +
                labs(title = "Serum Creatinine vs Serum Sodium Filtered if High Serum Creatinine (Adjusted by Sex)", 
                     x = "Serum Creatinine mg/dL", y = "Serum Sodium mEq/L") +
                scale_color_brewer(palette = 'Set1') +
                theme_bw()
        )
        
        output$sna_scr_unfiltered = renderPlot(
            
            death_filter() %>% 
                ggplot(aes(x = serum_creatinine, y = serum_sodium)) +
                geom_point(aes(color = sex), alpha = 0.5) +
                geom_line(stat = "smooth", aes(color = sex), se = F, alpha = 0.25, size = 2) +
                labs(title = "Serum Creatinine vs Serum Sodium Unfiltered", 
                     x = "Serum Creatinine mg/dL", y = "Serum Sodium mEq/L") +
                scale_color_brewer(palette = 'Set1') +
                theme_bw()
        )
        
        ########################################################################################
        
        # "CrPK vs Creatinine"
        output$crpk_scr_filtered = renderPlot(
            
            death_filter() %>% 
                filter(., 
                       ifelse(sex == "M", serum_creatinine >=1.5, serum_creatinine >=1.2)) %>% 
                ggplot(aes(x = creatinine_phosphokinase, y = serum_creatinine)) +
                geom_point(aes(color = sex), alpha = 0.5) +
                geom_line(stat = "smooth", aes(color = sex), se = F, alpha = 0.25, size = 2) +
                labs(title = "Creatinine Phosphokinase vs Serum Creatinine Filtered if High Serum Creatinine (Adjusted by Sex)", 
                     y = "Serum Creatinine mg/dL", x = "Creatinine Phosphokinase mcg/L") +
                scale_color_brewer(palette = 'Set1') +
                theme_bw()
        )
        
        output$crpk_scr_unfiltered = renderPlot(
            
            death_filter() %>% 
                ggplot(aes(x = creatinine_phosphokinase, y = serum_creatinine)) +
                geom_point(aes(color = sex), alpha = 0.5) +
                geom_line(stat = "smooth", aes(color = sex), se = F, alpha = 0.25, size = 2) +
                labs(title = "Creatinine Phosphokinase vs Serum Creatinine Unfiltered", 
                     y = "Serum Creatinine mg/dL", x = "Creatinine Phosphokinase mcg/L") +
                scale_color_brewer(palette = 'Set1') +
                theme_bw()
        )

        ########################################################################################
        
        # Platelets vs Creatinine
        output$ef_scr = renderPlot(
            
            death_filter() %>% 
                ggplot(aes(y = ejection_fraction, x = serum_creatinine)) +
                geom_point(aes(color = sex), alpha = 0.5) +
                geom_line(stat = "smooth", aes(color = sex), se = F, alpha = 0.25, size = 2) +
                labs(title = "Serum Creatinine vs Ejection Fraction", 
                     x = "Serum Creatinine mg/dL", y = "Ejection Fraction %") +
                scale_color_brewer(palette = 'Set1') +
                theme_bw()
        )
        
        ########################################################################################
        
        # Platelets vs Creatinine
        output$plt_scr = renderPlot(
            
            death_filter() %>% 
                ggplot(aes(y = platelets, x = serum_creatinine)) +
                geom_point(aes(color = sex), alpha = 0.5) +
                geom_line(stat = "smooth", aes(color = sex), se = F, alpha = 0.25, size = 2) +
                labs(title = "Serum Creatinine vs Platelets", 
                     x = "Serum Creatinine mg/dL", y = "Platelets /mcL") +
                scale_color_brewer(palette = 'Set1') +
                theme_bw()
        )

})

        
        


        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        