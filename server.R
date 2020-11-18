


shinyServer(

    function(input, output, session) {

        
        # hides the age adjustment slider based on checkbox input
        observeEvent(input$adjust_age, {
            if (input$adjust_age == TRUE) {
                showElement("age")
            } else {
                hideElement("age")
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

                    toggleElement("age") &
                    showElement("adjust_age") &
                    showElement("yes_death")
            } else if (input$my_tabs == "C") {
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
        
        # allows user to filter the data (and hence, the graphs) for positive death events
        # once yes_death checkbox is checked
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
            paste("The T-Test statistics between females and males (respectively) for longevity are t-stat: ",
                  signif(hearts_slider_t_test_death()$statistic, digits = 3), "/p-value: ", signif(hearts_slider_t_test_death()$p.value, digits = 3), ".")
            })
        
        output$post_time_sex2_stats = renderText({
            paste("The T-Test statistics between females and males (respectively) for longevity are t-stat: ",
                  signif(hearts_slider_t_test_no_death()$statistic, digits = 3), "/p-value: ", signif(hearts_slider_t_test_no_death()$p.value, digits = 3), ".")
            })
        
        ########################################################################################
        
        # "Serum Creatinine vs Serum Sodium"
        death_filter_high_scr = reactive({death_filter() %>% 
                filter(., 
                       ifelse(sex == "M", serum_creatinine >=1.5, serum_creatinine >=1.2))})
            

        output$sna_scr_filtered = renderPlot(
            
                death_filter_high_scr() %>% 
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
        
        
        # Correlation calculation and output
        
        
        # *** MALE MALE MALE ***
        
        # filter serum creatinine for males with high serum creatinine
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
        

        
        # *** FEMALE FEMALE FEMALE FEMALE ***
        
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
        


        # t test between male and female
        # serum creatinine with high serum creatinine
        scr_t_test_high_scr_filter = reactive({
            t.test(death_filter_scr_male_high_scr()$serum_creatinine,
                   death_filter_scr_female_high_scr()$serum_creatinine,
                   alternative = "two.sided")})
        
        # serum sodium with high serum creatinine
        sna_t_test_high_scr_filter = reactive({
            t.test(death_filter_sna_male_high_scr()$serum_sodium,
                   death_filter_sna_female_high_scr()$serum_sodium,
                   alternative = "two.sided")})
        
        # serum creatinine
        scr_t_test_unfilter = reactive({
            t.test(death_filter_scr_male()$serum_creatinine,
                   death_filter_scr_female()$serum_creatinine,
                   alternative = "two.sided")})
        
        # serum sodium
        sna_t_test_unfilter = reactive({
            t.test(death_filter_sna_male()$serum_sodium,
                   death_filter_sna_female()$serum_sodium,
                   alternative = "two.sided")})
        
        
        output$sna_scr_filtered_stats = renderText({
            paste("The Pearson correlation coefficients between Serum Creatinine and Serum Sodium for females and males (respectively) with high Serum Creatinine are ",
                  signif(tested_4_fil_female_cor(), digits = 3), " and ", signif(tested_4_fil_male_cor(), digits = 3),".", 
                  "The T-Test statistics between females and males for Serum Creatinine and for Serum Sodium (respectively) are t-stat: ",
                  signif(scr_t_test_high_scr_filter()$statistic, digits = 3), "/p-value: ", signif(scr_t_test_high_scr_filter()$p.value, digits = 3),
                  " and t-stat: ", signif(sna_t_test_high_scr_filter()$statistic, digits = 3), "/p-value: ", signif(sna_t_test_high_scr_filter()$p.value, digits = 3), ".")})
        
        output$sna_scr_unfiltered_stats = renderText({
            paste("The Pearson correlation coefficients between Serum Creatinine and Serum Sodium for females and males (respectively) are ",
                  signif(tested_4_unfil_female_cor(), digits = 3), " and ", signif(tested_4_unfil_male_cor(), digits = 3),". ", 
                  "The T-Test statistics between females and males for Serum Creatinine and for Serum Sodium (respectively) are t-stat: ",
                  signif(scr_t_test_unfilter()$statistic, digits = 3), "/p-value: ", signif(scr_t_test_unfilter()$p.value, digits = 3),
                  " and t-stat: ", signif(sna_t_test_unfilter()$statistic, digits = 3),"/p-value: ", signif(sna_t_test_unfilter()$p.value, digits = 3), ".")})
        

        
        ########################################################################################
        
        # "CrPhK vs Creatinine"
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
        
        # Correlation calculation and output
        # refer to "Sodium vs Creatinine" above for filters for Serum Creatinine
        
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
        
        # correlation calculation for male serum creatinine and male serum sodium for high serum creatinine
        tested_5_fil_male_cor = reactive({cor.test(death_filter_scr_male_high_scr()$serum_creatinine,
                                                   death_filter_crphk_male_high_scr()$creatinine_phosphokinase,
                                                   method = "pearson")$estimate
        })
        
        
        # correlation calculation for male serum creatinine and male serum sodium 
        tested_5_unfil_male_cor = reactive({cor.test(death_filter_scr_male()$serum_creatinine,
                                                     death_filter_crphk_male()$creatinine_phosphokinase,
                                                     method = "pearson")$estimate
        })
        
        
        
        # *** FEMALE FEMALE FEMALE FEMALE ***
        
        # filter serum sodium for females with high serum creatinine
        death_filter_crphk_female_high_scr = reactive({
            death_filter_high_scr() %>%
                filter(., sex=="F") %>%
                select(., creatinine_phosphokinase)
        })

        
        # filter serum sodium for females
        death_filter_crphk_female = reactive({
            death_filter() %>% 
                filter(., sex=="F") %>% 
                select(., creatinine_phosphokinase)
        })
        
        # correlation calculation for female serum creatinine and female serum sodium for high serum creatinine
        tested_5_fil_female_cor = reactive({cor.test(death_filter_scr_female_high_scr()$serum_creatinine,
                                                     death_filter_crphk_female_high_scr()$creatinine_phosphokinase,
                                                     method = "pearson")$estimate
        })
        
        
        # correlation calculation for female serum creatinine and female serum sodium 
        tested_5_unfil_female_cor = reactive({cor.test(death_filter_scr_female()$serum_creatinine,
                                                       death_filter_crphk_female()$creatinine_phosphokinase,
                                                       method = "pearson")$estimate
        })
        
        
        # T test between male and female
        # t statistic for Creatinine phosphokinase for high serum creatinine
        crphk_t_test_high_scr_filter = reactive({
            t.test(death_filter_crphk_male_high_scr()$creatinine_phosphokinase,
                   death_filter_crphk_female_high_scr()$creatinine_phosphokinase,
                   alternative = "two.sided")})
        
        # t statistic for Creatinine phosphokinase
        crphk_t_test_unfilter = reactive({
            t.test(death_filter_crphk_male()$creatinine_phosphokinase,
                   death_filter_crphk_female()$creatinine_phosphokinase,
                   alternative = "two.sided")})
        
        output$crphk_scr_filtered_stats = renderText({
            paste("The Pearson correlation coefficients between Serum Creatinine and Creatinine Phosphokinase for females and males (respectively) with high Serum Creatinine are ",
                  signif(tested_5_fil_female_cor(), digits = 3), " and ", signif(tested_5_fil_male_cor(), digits = 3),".", 
                  "The T-Test statistics between females and males for Creatinine Phosphokinase among patients with high Serum creatinine are t-stat: ",
                  signif(crphk_t_test_high_scr_filter()$statistic, digits = 3), "/p-value: ", signif(crphk_t_test_high_scr_filter()$p.value, digits = 3), ".")})
        
        output$crphk_scr_unfiltered_stats = renderText({
            paste("The Pearson correlation coefficients between Serum Creatinine and Creatinine Phosphokinase for females and males (respectively) are ",
                  signif(tested_5_unfil_female_cor(), digits = 3), " and ", signif(tested_5_unfil_male_cor(), digits = 3),". ", 
                  "The T-Test statistics between females and males for Creatinine Phosphokinase are t-stat: ",
                  signif(crphk_t_test_unfilter()$statistic, digits = 3), "/p-value: ", signif(crphk_t_test_unfilter()$p.value, digits = 3), ".")})

        ########################################################################################
        
        # ejection fraction vs Creatinine
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
        
        # *** MALE MALE MALE ***
        
        
        # filter ejection fraction for males
        death_filter_ef_male = reactive({
            death_filter() %>%
                filter(., sex=="M") %>%
                select(., ejection_fraction)
        })
        
        # correlation calculation for male serum creatinine and male serum sodium 
        tested_6a_unfil_male_cor = reactive({cor.test(death_filter_scr_male()$serum_creatinine,
                                                     death_filter_ef_male()$ejection_fraction,
                                                     method = "pearson")$estimate
        })
        
        
        
        # *** FEMALE FEMALE FEMALE FEMALE ***
        
        # filter serum sodium for females
        death_filter_ef_female = reactive({
            death_filter() %>% 
                filter(., sex=="F") %>% 
                select(., ejection_fraction)
        })
        
        # correlation calculation for female serum creatinine and female serum sodium 
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
        
        # t statistic for ejection fraction
        output$ef_scr_unfiltered_stats = renderText({
            paste("The Pearson correlation coefficients between Serum Creatinine and Ejection Fraction for females and males (respectively) are ",
                  signif(tested_6a_unfil_female_cor(), digits = 3), " and ", signif(tested_6a_unfil_male_cor(), digits = 3),". ", 
                  "The T-Test statistic between females and males for Ejection Fraction is t-stat: ",
                  signif(ef_t_test_unfilter()$statistic, digits = 3), "/p-value: ", signif(ef_t_test_unfilter()$p.value, digits = 3), ".")})
        
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
        
        
        # *** MALE MALE MALE ***
        
        
        # filter ejection fraction for males
        death_filter_plt_male = reactive({
            death_filter() %>%
                filter(., sex=="M") %>%
                select(., platelets)
        })
        
        # correlation calculation for male serum creatinine and male serum sodium 
        tested_6b_unfil_male_cor = reactive({cor.test(death_filter_scr_male()$serum_creatinine,
                                                      death_filter_plt_male()$platelets,
                                                      method = "pearson")$estimate
        })
        
        
        
        # *** FEMALE FEMALE FEMALE FEMALE ***
        
        # filter serum sodium for females
        death_filter_plt_female = reactive({
            death_filter() %>% 
                filter(., sex=="F") %>% 
                select(., platelets)
        })
        
        # correlation calculation for female serum creatinine and female serum sodium 
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
        
        # t statistic for platelets
        output$plt_scr_unfiltered_stats = renderText({
            paste("The Pearson correlation coefficients between Serum Creatinine and Platelets for females and males (respectively) are ",
                  signif(tested_6b_unfil_female_cor(), digits = 3), " and ", signif(tested_6b_unfil_male_cor(), digits = 3),". ", 
                  "The T-Test statistic between females and males for Platelets is t-stat: ",
                  signif(plt_t_test_unfilter()$statistic, digits = 3), "/p-value: ", signif(plt_t_test_unfilter()$statistic, digits = 3), ".")})

})

        
        


        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        