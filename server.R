



shinyServer(

    function(input, output, session) {

     
           
# Widget Adjuster ------------------------------------------------------------------------------------------------

        # observeEvents that allow widgets to filter the data being used
        
        # hides the age adjustment slider based on checkbox input
        observeEvent(input$adjust_age, {
            if (input$adjust_age == TRUE) {
                showElement("age")
            } else {
                hideElement("age")
            }
        })
        
        
        # resets the parameters set by the widgets upon selection of new tab
            # except for tab C where reset is explicitly called in the block below
        observeEvent(input$my_tabs, {
            if (input$my_tabs == "A" |
                input$my_tabs == "B" |
                input$my_tabs == "D" |
                input$my_tabs == "E" |
                input$my_tabs == "F") {
                reset("bins")
                reset("age")
                reset("adjust_age")
                reset("yes_death")
            }
        })
        
        # shows different widgets based on the tabs selected
        observeEvent(input$my_tabs, {
            ifelse(input$my_tabs == "A",
                   showElement("bins") & hideElement("age") & hideElement("adjust_age") & hideElement("yes_death"),
                   hideElement("bins") & showElement("age"))

            if (input$my_tabs == "B" |
                input$my_tabs == "D" |
                input$my_tabs == "E" |
                input$my_tabs == "F") {
                    hideElement("age") &
                    showElement("adjust_age") &
                    showElement("yes_death")
            } else if (input$my_tabs == "C") {
                    reset("age") &
                    showElement("age") &    
                    hideElement("adjust_age") &
                    hideElement("yes_death")
            }
        })
        
    #* Filters Associated with Widgets ========================================================================

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
        
        # allows user to filter the data (and hence, the graphs) for positive death events
        # once yes_death checkbox is checked
        death_filter = reactive({
            if (input$yes_death == TRUE) {
                return (hearts_slider_2() %>% filter(., DEATH_EVENT == 1))
            } else {
                return (hearts_slider_2())
            }
            
        })

        
        
# Plot for Tab 1 (# Pts) ----------------------------------------------------------------------------------------

        # Plot for tab "# Pts"
        output$count_age = renderPlot({
            hearts %>% 
                ggplot(aes(x = age)) + 
                labs(title = "Number of Patients by Age Group", 
                     x = "Age", y = "Number of Patients") +
                geom_histogram(binwidth = input$bins, 
                               fill = "purple", 
                               color = "green", 
                               alpha = 0.3) +
                scale_x_continuous(breaks = round(seq(min(hearts$age), 
                                                      max(hearts$age), by = 5), 1)) +
                theme_bw()
        })
    

        
# Plot for Tab 2 (Pre-Ex Cnd F vs M) ------------------------------------------------------------------------------

        # Plot for tab "Pre-Ex Cnd F vs M"
        output$count_factors = renderPlot({
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
                labs(title = "Number of Patients with Pre-Existing Conditions", 
                     x = "Pre-Existing Conditions", y = "Number of Patients") +
                scale_fill_brewer(palette = 'Set1') +
                theme_bw()
        })
        
    #* Tab 2 Filters, Statistical Calculations, and Outputs =======================================================
        
        # Filter for total number of females represented on the graph
        num_female = reactive({
            death_filter() %>% filter(., sex == "F")
        })
        
        # Filter for total number of males represented on the graph
        num_male = reactive({
            death_filter() %>% filter(., sex == "M")
        })
        
        # Calculate and output the count for females and males
        output$count_factors_stats = renderText({
            paste(
                "The total number of ",
                "<i>", "females ", "</i>",
                "is: ",
                "<b>", NROW(num_female()), "</b>",
                "<br/>",
                "The total number of ",
                "<i>", "males ", "</i>",
                "is: ",
                "<b>", NROW(num_male()), "</b>"
            )
        })
        
        
        

# Plot for Tab 3 (Survival F vs M) ----------------------------------------------------------------------------------
        
        # Plot for tab "Survival F vs M"
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
                labs(title = "Days Until Death (Youngest Age: 42)", 
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
                labs(title = "Days Until Death or Loss of Contact (Youngest Age: 40)", 
                     x = "Sex", y = "Number of Days") +
                scale_fill_brewer(palette = 'Set1') +
                theme_bw()
        )
        
    #* Tab 3 Filters, Statistical Calculations, and Outputs--------------------------------------------------------------
        
        # preparation for statistical calculation and output by setting a reactive filter to 
        # work in concert with the hearts slider input used for the graph above
        hearts_slider_t_test_male_death = reactive({
            hearts_slider() %>% 
                filter(., sex=="M", DEATH_EVENT == 1)
            })
        
        hearts_slider_t_test_female_death = reactive({
             hearts_slider() %>% 
                filter(., sex=="F", DEATH_EVENT == 1)
            })
        
        hearts_slider_t_test_male = reactive({
            hearts_slider() %>% 
                filter(., sex=="M")
            })
        
        hearts_slider_t_test_female = reactive({
             hearts_slider() %>% 
                filter(., sex=="F") 
            })
        
        #** statistical calculation and outputs ############################################################################
        hearts_slider_t_test_death = reactive({
            t.test(hearts_slider_t_test_female_death()$time,
                   hearts_slider_t_test_male_death()$time,
                   alternative = "two.sided")
            })
        
        hearts_slider_t_test_no_death = reactive({
            t.test(hearts_slider_t_test_female()$time,
                   hearts_slider_t_test_male()$time,
                   alternative = "two.sided")
            })
        
        output$post_time_sex_stats = renderText({
            paste("The T-Test statistics between females and males (respectively) for ", 
                  "<i>", "longevity", "</i>",  
                  "while accounting only for death events is:",
                  "<br/>",
                  "<b>", signif(hearts_slider_t_test_death()$statistic, digits = 3), "</b>", 
                  "and the p-value is: ",
                  "<b>", signif(hearts_slider_t_test_death()$p.value, digits = 3), "</b>", 
                  "<br/>")
            })
        
        output$post_time_sex2_stats = renderText({
            paste("The T-Test statistic between females and males (respectively) for ", 
                  "<i>", "longevity", "</i>", 
                  "while accounting for death events ", 
                  "<i>", "and", "</i>", 
                  "loss of contact is:",
                  "<br/>",
                  "<b>", signif(hearts_slider_t_test_no_death()$statistic, digits = 3), "</b>",
                  "and the p-value is: ",
                  "<b>", signif(hearts_slider_t_test_no_death()$p.value, digits = 3), "</b>")
            })
        
        

# Plot for Tab 4 (SerNa vs SerCr) ------------------------------------------------------------------------------
        
        # Plots for tab "Serum Creatinine vs Serum Sodium"
        death_filter_high_scr = reactive({death_filter() %>% 
                filter(., 
                       ifelse(sex == "M", serum_creatinine >=1.5, serum_creatinine >=1.2))})
            

        output$sna_scr_filtered = renderPlot(
            
                death_filter_high_scr() %>% 
                ggplot(aes(x = serum_creatinine, y = serum_sodium)) +
                geom_point(aes(color = sex), alpha = 0.5) +
                geom_line(stat = "smooth", aes(color = sex), se = F, alpha = 0.25, size = 2) +
                labs(title = "Serum Creatinine vs Serum Sodium Filtered if High Serum Creatinine", 
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
        
    #* Tab 4 Filters, Statistical Calculations, and Outputs ======================================================
        
        #** Tab 4 Filters, Statistical Calculations for Male ########################################
        
        # *** MALE MALE MALE ***
        
        # preparation for statistical calculation and output by setting a reactive filter to 
        # work in concert with the hearts slider input used for the graph above
        death_filter_scr_male_high_scr = reactive({
            death_filter_high_scr() %>% 
                filter(., sex=="M") %>% 
                select(., serum_creatinine)
            })
        
        # filter serum sodium for males with high serum creatinine
        death_filter_sna_male_high_scr = reactive({
            death_filter_high_scr() %>% 
                filter(., sex=="M") %>% 
                select(., serum_sodium)
            })
        
            # filter serum creatinine for males
        death_filter_scr_male = reactive({
            death_filter() %>% 
                filter(., sex=="M") %>% 
                select(., serum_creatinine)
            })
        
            # filter serum sodium for males
        death_filter_sna_male = reactive({
            death_filter() %>% 
                filter(., sex=="M") %>% 
                select(., serum_sodium)
            })
        
        # correlation calculation for male serum creatinine and male serum sodium for high serum creatinine
        tested_4_fil_male_cor = reactive({cor.test(death_filter_scr_male_high_scr()$serum_creatinine,
                                          death_filter_sna_male_high_scr()$serum_sodium,
                                          method = "pearson")$estimate
            })

        
            # correlation calculation for male serum creatinine and male serum sodium 
        tested_4_unfil_male_cor = reactive({cor.test(death_filter_scr_male()$serum_creatinine,
                                                 death_filter_sna_male()$serum_sodium,
                                                 method = "pearson")$estimate
            })
        

        #** Tab 4 Filters, Statistical Calculations for Female ######################################
        
        # *** FEMALE FEMALE FEMALE FEMALE ***
        
        # preparation for statistical calculation and output by setting a reactive filter to 
        # work in concert with the hearts slider input used for the graph above
        # filter serum creatinine for females with high serum creatinine
        death_filter_scr_female_high_scr = reactive({
            death_filter_high_scr() %>%
                filter(., sex=="F") %>%
                select(., serum_creatinine)
            })

        # filter serum sodium for females with high serum creatinine
        death_filter_sna_female_high_scr = reactive({
            death_filter_high_scr() %>%
                filter(., sex=="F") %>%
                select(., serum_sodium)
            })
        
            # filter serum creatinine for females
        death_filter_scr_female = reactive({
            death_filter() %>% 
                filter(., sex=="F") %>% 
                select(., serum_creatinine)
            })
        
            # filter serum sodium for females
        death_filter_sna_female = reactive({
            death_filter() %>% 
                filter(., sex=="F") %>% 
                select(., serum_sodium)
            })
        
        # correlation calculation for female serum creatinine and female serum sodium for high serum creatinine
        tested_4_fil_female_cor = reactive({cor.test(death_filter_scr_female_high_scr()$serum_creatinine,
                                                 death_filter_sna_female_high_scr()$serum_sodium,
                                                 method = "pearson")$estimate
            })
        
        
            # correlation calculation for female serum creatinine and female serum sodium 
        tested_4_unfil_female_cor = reactive({cor.test(death_filter_scr_female()$serum_creatinine,
                                                   death_filter_sna_female()$serum_sodium,
                                                   method = "pearson")$estimate
            })
        

        #** Tab 4 T Test #######################################################################################
        
        # t test between male and female
        # serum creatinine with high serum creatinine
        scr_t_test_high_scr_filter = reactive({
            t.test(death_filter_scr_male_high_scr()$serum_creatinine,
                   death_filter_scr_female_high_scr()$serum_creatinine,
                   alternative = "two.sided")
            })
        
        # serum sodium with high serum creatinine
        sna_t_test_high_scr_filter = reactive({
            t.test(death_filter_sna_male_high_scr()$serum_sodium,
                   death_filter_sna_female_high_scr()$serum_sodium,
                   alternative = "two.sided")
            })
        
        # serum creatinine
        scr_t_test_unfilter = reactive({
            t.test(death_filter_scr_male()$serum_creatinine,
                   death_filter_scr_female()$serum_creatinine,
                   alternative = "two.sided")
            })
        
        # serum sodium
        sna_t_test_unfilter = reactive({
            t.test(death_filter_sna_male()$serum_sodium,
                   death_filter_sna_female()$serum_sodium,
                   alternative = "two.sided")
            })
        
        
        #** Tab 4 Output ########################################################################################### 
        
        # text output for graph 1
        output$sna_scr_filtered_stats = renderText({
            paste("The Pearson correlation coefficient between ",
                  "<i>", "Serum Creatinine levels and Serum Sodium levels ", "</i>", 
                  "for females with high Serum Creatinine levels is:", 
                  "<br/>",
                  "<b>", signif(tested_4_fil_female_cor(), digits = 3), "</b>", 
                  "<br/>",
                  "<br/>",
                  "The Pearson correlation coefficient between ",
                  "<i>", "Serum Creatinine levels and Serum Sodium levels ", "</i>",
                  "for males with high Serum Creatinine levels is:",
                  "<br/>",
                  "<b>", signif(tested_4_fil_male_cor(), digits = 3), "</b>", 
                  "<br/>",
                  "<br/>",
                  "The T-Test statistic for ", 
                  "<i>", "Serum Creatinine levels ", "</i>",
                  "between females and males with high Serum Creatinine is:",
                  "<br/>", 
                  "<b>", signif(scr_t_test_high_scr_filter()$statistic, digits = 3), "</b>", 
                  "and the p-value is: ", 
                  "<b>", signif(scr_t_test_high_scr_filter()$p.value, digits = 3), "</b>", 
                  "<br/>",
                  "<br/>",
                  "The T-Test statistic for ", 
                  "<i>", "Serum Sodium levels ", "</i>",
                  "between females and males with high Serum Creatinine is: ",
                  "<br/>",
                  "<b>", signif(sna_t_test_high_scr_filter()$statistic, digits = 3), "</b>", 
                  "and the p-value is: ",
                  "<b>", signif(sna_t_test_high_scr_filter()$p.value, digits = 3), "</b>", 
                  "<br/>")
            })
        
        # text output for graph 2
        output$sna_scr_unfiltered_stats = renderText({
            paste("The Pearson correlation coefficient between ",
                  "<i>", "Serum Creatinine levels and Serum Sodium levels ", "</i>", 
                  " for all females is:",
                  "<br/>",
                  "<b>", signif(tested_4_unfil_female_cor(), digits = 3),"</b>", 
                  "<br/>",
                  "<br/>",
                  "The Pearson correlation coefficient between ",
                  "<i>", "Serum Creatinine levels and Serum Sodium levels ", "</i>", 
                  " for all males is:",
                  "<br/>",
                  "<b>", signif(tested_4_unfil_male_cor(), digits = 3), "</b>", 
                  "<br/>",
                  "<br/>",
                  "The T-Test statistic for ", 
                  "<i>", "Serum Creatinine levels ", "</i>",
                  "between all females and all males is:",
                  "<br/>", 
                  "<b>", signif(scr_t_test_unfilter()$statistic, digits = 3), "</b>", 
                  "and the p-value is: ",
                  "<b>", signif(scr_t_test_unfilter()$p.value, digits = 3), "</b>", 
                  "<br/>",
                  "<br/>",
                  "The T-Test statistic for ", 
                  "<i>", "Serum Creatinine levels ", "</i>",
                  "between all females and all males is:",
                  "<br/>",
                  "<b>", signif(sna_t_test_unfilter()$statistic, digits = 3),"</b>", 
                  "and the p-value is: ",
                  "<b>", signif(sna_t_test_unfilter()$p.value, digits = 3), "</b>")
            })
        

        
# Plots for Tab 5 (CrPhK vs SerCr) ----------------------------------------------------------------------

        # Plots for tab "CrPhK vs Creatinine"
        output$crpk_scr_filtered = renderPlot(
            
            death_filter() %>% 
                filter(., 
                       ifelse(sex == "M", serum_creatinine >=1.5, serum_creatinine >=1.2)) %>% 
                ggplot(aes(x = creatinine_phosphokinase, y = serum_creatinine)) +
                geom_point(aes(color = sex), alpha = 0.5) +
                geom_line(stat = "smooth", aes(color = sex), se = F, alpha = 0.25, size = 2) +
                labs(title = "Creatinine Phosphokinase vs Serum Creatinine Filtered if High Serum Creatinine (by Sex)", 
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

    #* Tab 5 Filters, Statistical Calculations, and Outputs==================================================
       
        # Correlation calculation and output
        # refer to "Sodium vs Creatinine" above for filters for Serum Creatinine
        
        #** Tab 5 Filters, Statistical Calculations for Male ######################################
        
        # *** MALE MALE MALE ***
        
        # filter creatinine phosphokinase for males with high serum creatinine
        death_filter_crphk_male_high_scr = reactive({
            death_filter_high_scr() %>%
                filter(., sex=="M") %>%
                select(., creatinine_phosphokinase)
        })

        
        # filter creatinine phosphokinase for males
        death_filter_crphk_male = reactive({
            death_filter() %>%
                filter(., sex=="M") %>%
                select(., creatinine_phosphokinase)
        })
        
        # correlation calculation for male creatinine phosphokinase and male serum creatinine for high serum creatinine
        tested_5_fil_male_cor = reactive({cor.test(death_filter_scr_male_high_scr()$serum_creatinine,
                                                   death_filter_crphk_male_high_scr()$creatinine_phosphokinase,
                                                   method = "pearson")$estimate
        })
        
        
        # correlation calculation for male creatinine phosphokinase and male serum creatinine
        tested_5_unfil_male_cor = reactive({cor.test(death_filter_scr_male()$serum_creatinine,
                                                     death_filter_crphk_male()$creatinine_phosphokinase,
                                                     method = "pearson")$estimate
        })
        
        
        #** Tab 5 Filters, Statistical Calculations for Female ######################################
        
        # *** FEMALE FEMALE FEMALE FEMALE ***
        
        # filter creatinine phosphokinase for females with high serum creatinine
        death_filter_crphk_female_high_scr = reactive({
            death_filter_high_scr() %>%
                filter(., sex=="F") %>%
                select(., creatinine_phosphokinase)
        })

        
        # filter creatinine phosphokinase for females
        death_filter_crphk_female = reactive({
            death_filter() %>% 
                filter(., sex=="F") %>% 
                select(., creatinine_phosphokinase)
        })
        
        # correlation calculation for female creatinine phosphokinase and femal serum creatinine for high serum creatinine
        tested_5_fil_female_cor = reactive({cor.test(death_filter_crphk_female_high_scr()$creatinine_phosphokinase,
                                                     death_filter_scr_female_high_scr()$serum_creatinine,
                                                     method = "pearson")$estimate
        })
        
        
        # correlation calculation for female creatinine phosphokinase and female serum creatinine 
        tested_5_unfil_female_cor = reactive({cor.test(death_filter_crphk_female()$creatinine_phosphokinase,
                                                       death_filter_scr_female()$serum_creatinine,
                                                       method = "pearson")$estimate
        })
        
        
        #** Tab 5 T test #################################################################################
       
        # t statistic for Creatinine phosphokinase for high serum creatinine
        crphk_t_test_high_scr_filter = reactive({
            t.test(death_filter_crphk_male_high_scr()$creatinine_phosphokinase,
                   death_filter_crphk_female_high_scr()$creatinine_phosphokinase,
                   alternative = "two.sided")
            })
        
        # t statistic for Creatinine phosphokinase
        crphk_t_test_unfilter = reactive({
            t.test(death_filter_crphk_male()$creatinine_phosphokinase,
                   death_filter_crphk_female()$creatinine_phosphokinase,
                   alternative = "two.sided")
            })
        
        #** Tab 5 Output #################################################################################
       
         # text output for graph 1
        output$crphk_scr_filtered_stats = renderText({
            paste("The Pearson correlation coefficient between ", 
                  "<i>", "Creatinine Phosphokinase levels and Serum Creatinine levels ", "</i>",
                  "for females with high Serum Creatinine levels is:",
                  "<br/>",
                  "<b>", signif(tested_5_fil_female_cor(), digits = 3), "</b>", 
                  "<br/>",
                  "<br/>",
                  "The Pearson correlation coefficient between ", 
                  "<i>", "Creatinine Phosphokinase levels and Serum Creatinine levels ", "</i>",
                  "for males with high Serum Creatinine levels is:", 
                  "<br/>",
                  "<b>", signif(tested_5_fil_male_cor(), digits = 3), "</b>", 
                  "<br/>", 
                  "<br/>", 
                  "The T-Test statistics for ",
                  "<i>", "Creatinine Phosphokinase levels ", "</i>",
                  "between females and males with high Serum Creatinine levels is:",
                  "<br/>",
                  "<b>", signif(crphk_t_test_high_scr_filter()$statistic, digits = 3), "</b>",
                  "and the p-value is:",
                  "<b>", signif(crphk_t_test_high_scr_filter()$p.value, digits = 3), "</b>", 
                  "<br/>")
            })
        
        # text output for graph 2
        output$crphk_scr_unfiltered_stats = renderText({
            paste("The Pearson correlation coefficient between ", 
                  "<i>", "Creatinine Phosphokinase levels and Serum Creatinine levels ", "</i>",
                  "for all females is:",
                  "<br/>",
                  "<b>", signif(tested_5_unfil_female_cor(), digits = 3), "</b>", 
                  "<br/>",
                  "<br/>",
                  "The Pearson correlation coefficient between ", 
                  "<i>", "Creatinine Phosphokinase levels and Serum Creatinine levels ", "</i>",
                  "for males is:", 
                  "<br/>",
                  "<b>", signif(tested_5_unfil_male_cor(), digits = 3), "</b>", 
                  "<br/>",
                  "<br/>",
                  "The T-Test statistic for ",
                  "<i>", "Creatinine Phosphokinase levels ", "</i>",
                  "between all females and all males is:",
                  "<br/>",
                  "<b>", signif(crphk_t_test_unfilter()$statistic, digits = 3), "</b>", 
                  "and the p-value is: ",
                  "<b>", signif(crphk_t_test_unfilter()$p.value, digits = 3), "</b>")
            })


        
# 1st Plot for Tab 6 (No Evdnt Corr) ---------------------------------------------------------------------------
        
        # Plot for tab "No Evdnt Corr"
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

    #* Tab 6a Filters, Statistical Calculations, and Outputs ======================================================     
        
        # *** MALE MALE MALE ***
        
        
        # filter ejection fraction for males
        death_filter_ef_male = reactive({
            death_filter() %>%
                filter(., sex=="M") %>%
                select(., ejection_fraction)
        })
        
        # correlation calculation for male serum creatinine and male ejection fraction
        tested_6a_unfil_male_cor = reactive({cor.test(death_filter_scr_male()$serum_creatinine,
                                                     death_filter_ef_male()$ejection_fraction,
                                                     method = "pearson")$estimate
        })
        
        
        
        # *** FEMALE FEMALE FEMALE FEMALE ***
        
        # filter ejection fraction for females
        death_filter_ef_female = reactive({
            death_filter() %>% 
                filter(., sex=="F") %>% 
                select(., ejection_fraction)
        })
        
        
        # correlation calculation for female serum creatinine and female ejection fraction 
        tested_6a_unfil_female_cor = reactive({cor.test(death_filter_scr_female()$serum_creatinine,
                                                       death_filter_ef_female()$ejection_fraction,
                                                       method = "pearson")$estimate
        })
        
        # T test between male and female
        # t statistic for ejection fraction for high serum creatinine
        ef_t_test_unfilter = reactive({
            t.test(death_filter_ef_male()$ejection_fraction,
                   death_filter_ef_female()$ejection_fraction,
                   alternative = "two.sided")})
        
        #** Tab 6a Output ############################################################################################
        
        # t statistic for ejection fraction
        output$ef_scr_unfiltered_stats = renderText({
            paste("The Pearson correlation coefficient between ", 
                  "<i>", "Serum Creatinine and Ejection Fraction ", "</i>",
                  "for all females is:",
                  "<br/>",
                  "<b>", signif(tested_6a_unfil_female_cor(), digits = 3), "</b>",
                  "<br/>",
                  "<br/>",
                  "The Pearson correlation coefficient between ", 
                  "<i>", "Serum Creatinine and Ejection Fraction ", "</i>",
                  "for all males is:", 
                  "<br/>",
                  "<b>", signif(tested_6a_unfil_male_cor(), digits = 3), "</b>", 
                  "<br/>", 
                  "<br/>", 
                  "The T-Test statistic between all females and all males for ",
                  "<i>", "Ejection Fraction", "</i>",
                  "is:", 
                  "<br/>",
                  "<b>", signif(ef_t_test_unfilter()$statistic, digits = 3), "</b>",
                  "and the p-value is: ", 
                  "<b>", signif(ef_t_test_unfilter()$p.value, digits = 3), "</b>")})
        
        

# 2nd Plot for Tab 6 (No Evdnt Corr) -----------------------------------------------------------------------------------

        # 2nd Plot for tab "No Evdnt Corr"
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
        
    #* Tab 6b Filters, Statistical Calculations, and Outputs ============================================================  
        
        # *** MALE MALE MALE ***
        
        # filter platelets for males
        death_filter_plt_male = reactive({
            death_filter() %>%
                filter(., sex=="M") %>%
                select(., platelets)
        })
        
        # correlation calculation for male serum creatinine and male platelets
        tested_6b_unfil_male_cor = reactive({cor.test(death_filter_scr_male()$serum_creatinine,
                                                      death_filter_plt_male()$platelets,
                                                      method = "pearson")$estimate
        })
        
        
        
        # *** FEMALE FEMALE FEMALE FEMALE ***
        
        # filter platelets for females
        death_filter_plt_female = reactive({
            death_filter() %>% 
                filter(., sex=="F") %>% 
                select(., platelets)
        })
        
        # correlation calculation for female serum creatinine and female platelets 
        tested_6b_unfil_female_cor = reactive({cor.test(death_filter_scr_female()$serum_creatinine,
                                                        death_filter_plt_female()$platelets,
                                                        method = "pearson")$estimate
        })
        
        # T test between male and female
        # t statistic for platelets for high serum creatinine
        plt_t_test_unfilter = reactive({
            t.test(death_filter_plt_male()$platelets,
                   death_filter_plt_female()$platelets,
                   alternative = "two.sided")})
        
        #** Tab 6b Output ###################################################################################################
        
        # t statistic for platelets
        output$plt_scr_unfiltered_stats = renderText({
            paste("The Pearson correlation coefficient between ", 
                  "<i>", "Serum Creatinine and Platelets", "</i>",
                  "for all females is:",
                  "<br/>",
                  "<b>", signif(tested_6b_unfil_female_cor(), digits = 3), "</b>",
                  "<br/>",
                  "<br/>",
                  "The Pearson correlation coefficient between ", 
                  "<i>", "Serum Creatinine and Platelets", "</i>",
                  "for all males is:",
                  "<br/>",
                  "<b>", signif(tested_6b_unfil_male_cor(), digits = 3), "</b>", 
                  "<br/>",
                  "<br/>",
                  "The T-Test statistic between all females and all males for ",
                  "<i>", "Platelets ", "</i>",
                  "is:",
                  "<br/>",
                  "<b>", signif(plt_t_test_unfilter()$statistic, digits = 3), "</b>",
                  "and the p-value is: ",
                  "<b>", signif(plt_t_test_unfilter()$p.value, digits = 3), "</b>")})

})

        
        


        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        