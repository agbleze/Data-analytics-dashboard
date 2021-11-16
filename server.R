#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyServer(function(input, output, session) {  
    #### Dataset UI
#    output$dataset <- renderTable(all_data)
    
    ###
    # uniques_session_count <- reactive({
    #     ### Total number of unique sessions
    #     date_select = input$date_select
    #     
    #     unique_session_count <- KPI_data %>%
    #         dplyr::filter(date_booked == date_select)%>%
    #         collapse::fselect(session_id)%>%
    #         collapse::na_omit()%>%
    #         count()
    # })
    
    
    
    
    ### KPI 
    output$unique_visitors <- renderValueBox({
        date_select <- input$date_select
        unique_visitors_count<- KPI_data %>%
            dplyr::filter(date_booked == date_select) %>%
            collapse::fselect(visitor_id)%>%
            collapse::na_omit()%>%
            count()
        
        #unique_visitors_count  ### total number of unique visitors is 17,454 / 17453 when NA is removed
        valueBox(value = unique_visitors_count, color = 'green', subtitle = 'Unique site visitors')
    })
    
    output$unique_session <- renderValueBox({
        ### Total number of unique sessions
        # unique_session_count <- KPI_data %>%
        #     collapse::fselect(session_id)%>%
        #     collapse::na_omit()%>%
        #     count()
        # 
        date_select = input$date_select
        
        unique_session_count <- KPI_data %>%
            dplyr::filter(date_booked == date_select)%>%
            collapse::fselect(session_id)%>%
            collapse::na_omit()%>%
            count()
        valueBox(value = unique_session_count, color = 'green', subtitle = 'Unique session')
    })
    
    output$avg_sess_per_visitor <- renderValueBox({
        date_select = input$date_select
        
        unique_session_count <- KPI_data %>%
            dplyr::filter(date_booked == date_select)%>%
            collapse::fselect(session_id)%>%
            collapse::na_omit()%>%
            count()
        
        unique_visitors_count<- KPI_data %>%
            dplyr::filter(date_booked == date_select) %>%
            collapse::fselect(visitor_id)%>%
            collapse::na_omit()%>%
            count()
        
        ### Average session per visitor
        avg_session_per_user <- unique_session_count/unique_visitors_count
        #avg_session_per_user   ### the average session per user is 1.081
        
        
        
        valueBox(value = comma(avg_session_per_user, digits = 3), color = 'olive', 
                 subtitle = 'Average Session per Visitor')
    })
    
    output$conversion_total <- renderValueBox({
        date_select <- input$date_select
        conversion_total <- KPI_data %>%
          dplyr::select(page_type, date_booked)%>%
          na.omit()%>%
          filter(page_type == "request/success" & date_booked == date_select)%>%
          count()
        
        valueBox(value = comma(conversion_total), color = 'teal', subtitle = 'Total Conversions')
    })
    
    output$bounce_rate <- renderValueBox({
        date_select = input$date_select
        
        bounce <- KPI_data%>%
            dplyr::select(session_id, page_type, date_booked)%>%
            na.omit()%>%
            dplyr::filter(date_booked == date_select)%>%
            group_by(session_id)%>%
            count(page_type)%>%
            tally(wt = n)%>%
            mutate(num_pages = n)%>%
            mutate(bounce_status = case_when(num_pages == 1 ~ "bounce",
                                             num_pages > 1 ~ "not_bounce"))
       
        bounce_group <- bounce%>%
            group_by(bounce_status)%>%
            count(bounce_status)
       
        ## bounce rate
        bounce_rate <- (bounce_group[1,2]/sum(bounce_group$n)) * 100
       # bounce_rate  ## 13.50384% bounce rate
        
        valueBox(value = paste0(comma(bounce_rate, digits = 2), '%'), color = 'teal', subtitle = 'Bounce rate')
    })
    
    output$customer_aquisition <- renderValueBox({
        
        date_select = input$date_select
        
        customer_acquisition <- KPI_data%>%
            dplyr::select(event_type, date_booked) %>%
            na.omit()%>%
            dplyr::filter(event_type == "user_register_success" & date_booked == date_select)%>%
            count()
        
        valueBox(value = customer_acquisition, color = 'teal', subtitle = 'Customer acquisition -New customers registered')
    })
    
    
    ########################## A/B testing  ###############################
    ########## Control group  ###############
    output$rcsf_ref_conversion <- renderValueBox({
       ref_conversion <-  a_b_testgroups %>%
            filter(page_type == "request/success" & test_status == 'control' & date_booked == input$date_filter)%>%
            na.omit()%>%
            count()
       
       valueBox(value = ref_conversion, subtitle = 'Total Conversion', color = 'lime')
    })
    
    output$rcsf_ref_bounce <- renderValueBox({
        ## Bounce_rate for rcsp_ref
        bounce_rcsp_ref <- a_b_testgroups %>%
            dplyr::filter(test_status =='control' & date_booked == input$date_filter) %>%
            dplyr::select(session_id, page_type)%>%
            na.omit()%>%
            dplyr::group_by(session_id)%>%
            count(page_type)%>%
            tally(wt = n)%>%
            mutate(num_pages = n)%>%
            mutate(bounce_status = case_when(num_pages == 1 ~ "bounce",
                                             num_pages > 1 ~ "not_bounce"))
        
        #View(bounce_rcsp_ref)
        bounce_group_rcsp_ref <- bounce_rcsp_ref%>%
            group_by(bounce_status)%>%
            count(bounce_status)
        
        ## bounce rate for rcsp_ref
        bounce_rate_rcsp_ref <- (bounce_group_rcsp_ref[1,2]/sum(bounce_group_rcsp_ref$n)) * 100
        bounce_rate_rcsp_ref  ## 13.61572% bounce rate
        
       valueBox(value = paste0(comma(bounce_rate_rcsp_ref, digits = 2), '%'), subtitle = 'Bounce rate', 
                color = 'lime')     
    })
    
    ########### TEST GROUP
    output$rcsf_show_conversion <- renderValueBox({
        show_conversion <- a_b_testgroups%>%
            dplyr::filter(page_type == 'request/success' & test_status == 'test' & date_booked == input$date_filter)%>%
            na.omit()%>%
            count()
        
        valueBox(value = paste0(comma(show_conversion, digits = 0)), subtitle = 'Total Conversion', 
                 color = 'fuchsia')
            
    })
    
    
    output$rcsf_show_bounce <- renderValueBox({
        #####  Bounce_rate for A/B testing
        ## Bounce rate rcsp_show
        bounce_rcsp_show <- a_b_testgroups%>%
            dplyr::filter(test_status == 'test' & date_booked == input$date_filter)%>%
            dplyr::select(session_id, page_type)%>%
            na.omit()%>%
            dplyr::group_by(session_id)%>%
            count(page_type)%>%
            dplyr::tally(wt = n)%>%
            dplyr::mutate(num_pages = n)%>%
            dplyr::mutate(bounce_status = case_when(num_pages == 1 ~ "bounce",
                                                    num_pages > 1 ~ "not_bounce"))
        
        bounce_group_rcsp_show <- bounce_rcsp_show%>%
            group_by(bounce_status)%>%
            count(bounce_status)
        
        ## bounce rate for rcsp_show
        bounce_rate_rcsp_show <- (bounce_group_rcsp_show[1,2]/sum(bounce_group_rcsp_show$n)) * 100
        bounce_rate_rcsp_show  ## 13.59 % bounce rate for rcsp_show
        
        valueBox(
            value = paste0(comma(bounce_rate_rcsp_show, digits = 2), '%'), subtitle = 'Bounce rate',
            color = 'fuchsia'
        )
    })
    
    
    output$ab_user_jour <- renderPlotly({
        
        rcsp_funl_both_arrange
    })
    
    
    ############# clustering groups  ##########
    output$device_type <- renderPlot(
        
        ### clusters users into groups based on device type
        # users_device_class <- all_data%>%
        #     dplyr::select(visitor_id, device_class)%>%
        #     na.omit()%>%
        #    # dplyr::filter(date_booked == input$date_select) %>%
        #     dplyr::group_by(device_class)%>%
        #     dplyr::distinct(visitor_id)%>%
        #     count(visitor_id)%>%
        #     dplyr::tally(wt = n)%>%
        #     dplyr::arrange(desc(n)) %>%
        #     dplyr::rename("Number of users" = n)
        
        ggplot(data = users_device_class, mapping = aes(x = reorder(device_class, -`Number of users`), y = `Number of users`)) + geom_col() + 
            ggtitle(label = "Number of users based on devices used") + xlab("Device class")
    )
    
    output$user_country <- renderPlot({
        
        ### clusters users into groups based on user country 
        # users_country <- all_data%>%
        #     dplyr::select(visitor_id, user_location_country)%>%
        #     na.omit()%>%
        #   #  dplyr::filter(date_booked == input$date_select)
        #     dplyr::group_by(user_location_country)%>%
        #     dplyr::distinct(visitor_id)%>%
        #     count(visitor_id)%>%
        #     tally(wt = n)%>%
        #     dplyr::arrange(desc(n)) %>%
        #     dplyr::rename("Number of users" = n)
        # #View()
        # top20_users_country <- dplyr::top_n(users_country, n = 20)
        
        ggplot(data = top20_users_country, mapping = aes(x = reorder(user_location_country, -`Number of users`), 
                                                         y = `Number of users`)) + geom_col() + 
            ggtitle("Top 20 countries with highest number of visitors") + xlab("Countries of users")
        
    })
    
    output$user_browser <- renderPlot({
        
    #     users_browser <- all_data%>%
    #         dplyr::select(visitor_id, device_browser)%>%
    #         na.omit()%>%
    # #        dplyr::filter(date_booked == input$date_select)
    #         dplyr::group_by(device_browser)%>%
    #         dplyr::distinct(visitor_id)%>%
    #         count(visitor_id)%>%
    #         dplyr::tally(wt = n)%>%
    #         dplyr::arrange(desc(n)) %>%
    #         dplyr::rename("Number of users" = n)
        
        ggplot(data = top_n(users_browser, n =5), mapping = aes(x = reorder(device_browser, -`Number of users`), 
                                                                y = `Number of users`)) + geom_col() + 
            ggtitle("Top 5 browers") + xlab("Type of browser")
        
    })
    
    output$user_jour <- renderHighchart({
        ## funnel charts for user journey
        # page_type_count <- all_data%>%
        #     dplyr::select(page_type)%>%
        #     na.omit()%>%
        #     dplyr::group_by(page_type)%>%
        #     count()%>%
        #     dplyr::arrange(desc(n))
        # 
        # 
        # user_jour_funnel <- page_type_count %>%
        #     hchart(
        #         "funnel", hcaes(x = page_type, y = n),
        #         name = "user_journey"
        #     )
        # 
        user_jour_funnel
        
    })
    
    output$cluster_plot <- renderPlot({
        
        # ####################### cluster analysis
        ## pageviews per visitor
        # user_pageviews <- all_data%>%
        #     dplyr::select(visitor_id,page_type)%>%
        #     na.omit()%>%
        #     dplyr::group_by(visitor_id)%>%
        #     count(page_type)%>%
        #     dplyr::tally(wt = n)%>%
        #     dplyr::mutate(num_pages = n)%>%
        #     dplyr::select(-2)
        # 
        # 
        # user_sess <- all_data%>%
        #     dplyr::select(visitor_id, session_id) %>%
        #     na.omit()%>%
        #     dplyr::group_by(visitor_id)%>%
        #     dplyr::distinct(session_id)
        # 
        # user_sess_num <- user_sess%>%
        #     dplyr::group_by(visitor_id)%>%
        #     count()%>%
        #     dplyr::mutate(num_sessions = n)%>%
        #     dplyr::select(-2)
        # 
        ### merge num_pages and num_sessions for visitors
        # users_all <- merge(user_pageviews, user_sess_num)
        # visitor_pageviews_sessions_num <- merge(user_pageviews, user_sess_num)
        # 
    #     ## k-mean clustering
    #     visitor_pageviews_sessions_num <- na.omit(visitor_pageviews_sessions_num)
    #     ## scale data
    #     visitor_pageviews_sessions_num <- scale(visitor_pageviews_sessions_num[,c(2:3)])
    # #    head(visitor_pageviews_sessions_num)
    #    
    #     ##### K means clustering
    #     k2_visitor <- kmeans(visitor_pageviews_sessions_num, centers = 2, nstart = 25)
    #     
    #     k3_visitor <- kmeans(visitor_pageviews_sessions_num, centers = 3, nstart = 25) ## kmeans with 3 clusters
    #     k4_visitor <- kmeans(visitor_pageviews_sessions_num, centers = 4, nstart = 25) ## kmeans with 4 clusters
    #     k5_visitor <- kmeans(visitor_pageviews_sessions_num, centers = 5, nstart = 25) ## kmeans with 5 clusters
    #     # 
    #     # # plots to compare
    #     p1 <- fviz_cluster(k2_visitor, geom = "point", data = visitor_pageviews_sessions_num) + ggtitle("k = 2")
    #     p2 <- fviz_cluster(k3_visitor, geom = "point", data = visitor_pageviews_sessions_num) + ggtitle("k = 3")
    #     p3 <- fviz_cluster(k4_visitor, geom = "point", data = visitor_pageviews_sessions_num) + ggtitle("k = 4")
    #     p4 <- fviz_cluster(k5_visitor, geom = "point", data = visitor_pageviews_sessions_num) + ggtitle("k = 5")
    #      
        # ############# plot all graphs together  #######
        grid.arrange(p1, p2, p3, p4, nrow = 2)
        
    })
    
    output$optimal_clust <- renderPlot({
        ##Determing optimal number of clusters using average silhouettes method 
        # silh_visitors <- fviz_nbclust(visitor_pageviews_sessions_num, kmeans, method = "silhouette")
        silh_visitors
    })
    
    output$optimal_cluster_plot <- renderPlot({
        #######  compute k-means clustering with k = 2  // the optimal clusters according to silhoutte
        # set.seed(123)
        # optimal_clusters_visitors <- kmeans(visitor_pageviews_sessions_num, 2, nstart = 25)
        # 
        # optimal_clust_viz <- fviz_cluster(optimal_clusters_visitors, data = visitor_pageviews_sessions_num)
        optimal_clust_viz
    })
    
 ################ Naive bayes  #####################   
    ## Root Mean Squared Error (RMSE)
    output$rmse <- renderValueBox(
        valueBox(
            value = paste0(comma(test_rmse, digits = 2)),
            subtitle = "Root Mean Squared Error (RMSE)"
        )
    )
    
    
    ## Gini
    output$gini <- renderValueBox(
        valueBox(
            value = paste0(comma(test_gini, digits = 2)),
            subtitle = "Gini"
        )
    )
    
    ## Confusion matrix table 
    output$confusionmatrix <- renderTable(rownames = TRUE, hover = TRUE,
                                          h2o.confusionMatrix(h20_best_model)
    )
    
    
    ## AUC 
    output$auc_perform <- renderPlotly(
        data.frame(fpr = fpr, tpr = tpr) %>%
            ggplot(aes(fpr, tpr)) + geom_line() + ggtitle(sprintf("AUC: %f", h2o_auc))
        
    )
    
    
    ########################## Display actual use of instant booking
    output$actual <- renderValueBox({
        val <- input$predic
        
        actul <- test_predict_join %>%
            filter(rowid == val) %>%
            dplyr::select(instant_booking)
        
        valueBox(value = h5(actul), subtitle = "Actual value", color = "yellow")
    })
    
    ############# Display predicted use of instant booking
    output$predicted <- renderValueBox({
        val <- input$predic
        
        predict_value <- test_predict_join %>%
            filter(rowid == val) %>%
            dplyr::select(predict)
        
        valueBox(value = h5(predict_value), subtitle = "Prediction", color = "red")
    })
    
    
    ################# Indicates whether the model prediction is correct or wrong
    output$model_precision <- renderValueBox({
        val <- input$predic
        
    
        
        model_status <-  test_predict_join%>%
            filter(rowid = val)%>%
            dplyr::select(prediction_status)
        
        ## create different icons for correcting and wrong prediction
        model_icon<- if(model_status == "Correct"){
            correct = icon("check-circle")
        } else if (model_status == "Wrong"){
            wrong = icon("times-circle")
        } 
        
        valueBox(value = h5(model_status), subtitle = "Accuracy of Model", icon = model_icon)
    })
    
    #View(KPI_data)
    ############################ MODEL UI  ##############
    # output$dataset <- renderDataTable(
    #     task_data
    # )
    # 
    # output$ggnonparametrin <- renderPlot({
    #     data_grouped_demo_conversion <- task_data_convert %>%
    #         dplyr::select(demo_appointment_datetime, conversion, company_group, sales_script_type, is_signed)%>%
    #         tidyr::separate(col = demo_appointment_datetime, sep = ' ', into = c('demo_appointment_yyyymmdd','demo_appointment_hhmmss')) %>%
    #         dplyr::group_by(demo_appointment_yyyymmdd, sales_script_type)
    #     
    #     ## sum the total conversion for each script on each day
    #     df_grp_sum = data_grouped_demo_conversion %>% dplyr::summarize(total_conversion = sum(is_signed))
    #     
    #     
    #     ggbetweenstats(df_grp_sum, x = sales_script_type, y = total_conversion, plot.type = 'boxviolin',
    #                    type = 'nonparametric', p.adjust.method = 'bonferroni')
    # })
    # 
    # output$training_dataset <- renderPlotly({
    #     ggplot(data = train_data, aes(conversion)) + geom_bar() + 
    #         ggtitle("Training dataset") + xlab("")
    # })
    # 
    # output$test_dataset <- renderPlotly({
    #     ggplot(data = test_data, aes(conversion)) + geom_bar() + 
    #         ggtitle("Test dataset") +
    #         xlab("")
    # })
    # 
    output$regression_result <- renderPlot(
        ggcoefstats(
            x = stats::lm(formula = total_paid.EUR ~ test_status + instant_booking +
                              user_verified, data = train_all_convern.lmvar),
            ggtheme = ggplot2::theme_gray(), # changing the default theme
            title = "Regression analysis: Predicting the influence of various factors on booking sales",
        ) # + theme_grey()
    )
    # 
    output$var_inf <- renderDataTable({
         var_influeunce
    })
    # 
    ################## Model metrics ###################
    output$r2_result <- renderValueBox({
        valueBox(color = 'maroon',
                 value = paste0(comma(r2_model4, digits = 2)),
                 subtitle = h4("R-sqaured")
        )
         
     })
    
    output$resid_vs_fitted <- renderPlot({
        ggplot(data = model4_results,mapping = aes(x = .fitted, y = .resid)) + 
            geom_ref_line(h = 0) +
            geom_point() +
            geom_smooth(se = FALSE) +
            ggtitle("Residuals vs Fitted")
    })
    
    output$qqplot_resid <- renderPlot({
        plot(model4_all_convern_factors, which = 2)
    })
    
    output$scale_location <- renderPlot({
        plot(model4_all_convern_factors, which = 3)
    })
    # 
    # output$specificity_result <- renderValueBox({
    #     valueBox(color = 'maroon',
    #              value = paste0(comma(specificity_result, digits = 2), '%'),
    #              subtitle = h4("Specificity")
    #     )
    # })
    # 
    # output$mcfaddenR2_result <- renderValueBox({
    #     valueBox(color = 'maroon',
    #              value = paste0(comma(mcfadden_r2, digits = 6)), 
    #              subtitle = h4('McFadden’s pseudo R2'))
    # })
    # 
    # output$auc_perform <- renderPlot(
    #     # data.frame(fpr = fpr_tested_model, tpr = tpr_tested_model) %>%
    #     #     ggplot(aes(fpr, tpr)) + geom_line() + ggtitle(sprintf("AUC: %f", h2o.auc(test_model)))
    #     
    #     prediction(test.predicted.m1, test_data$is_signed) %>%
    #         performance(measure = "tpr", x.measure = "fpr") %>%
    #         plot() + title(main = paste0('AUC= ', comma(auc, digits = 2)))
    # )
    # 
    # 
    # 
    ########################## Display actual sales from the testing dataset
    output$actual <- renderValueBox({
        client <- input$client

        actul <- sale_prediction%>%
            dplyr::filter(visitor_id == client)%>%
            dplyr::select(total_paid.EUR)

        valueBox(value = h3(actul), subtitle = h4("Actual Revenue"), color = "yellow",
                 width = 3)
    })

    output$predicted <- renderValueBox({
        client = input$client

        predicted <- sale_prediction %>%
            dplyr::filter(visitor_id == client)%>%
            dplyr::select(pred)
        valueBox(value = h3(predicted), subtitle = h4('Model Prediction'), color = 'yellow',
                 width = 3)
    })

    # output$model_precision <- renderValueBox({
    #     client = input$client
    # 
    #     model_status <- prediction_data%>%
    #         dplyr::filter(request_id == client)%>%
    #         dplyr::select(predict_satus)
    # 
    #     color_model <- if(model_status == 'Wrong prediction') {
    #         color = 'red'
    #     } else if(model_status == 'Correct prediction'){
    #         color = 'green'
    #     }
    # 
    # 
    #     ## create different icons for correcting and wrong prediction
    #     model_icon<- if(model_status == "Correct prediction"){
    #         correct = icon("check-circle")
    #     } else if (model_status == "Wrong prediction"){
    #         wrong = icon("times-circle")
    #     }
    # 
    #     valueBox(value = h3(model_status), subtitle = h4('Model Accuracy'), width = 6,
    #              color = color_model, icon = model_icon)
    # })

    # 
    # ######################################  CONVERSION UI ############################################# 
    # ########## Sales scripts  ######################
    # output$scriptA_conv_rate <- renderValueBox({
    #     conv_rate_selectyear <- input$conv_rate_selectyear
    #     conv_rate_selectmonth <- input$conv_rate_selectmonth
    #     
    #     script_A_conversion <- task_data_convert%>%
    #         filter(request_created_year == conv_rate_selectyear & request_created_month == conv_rate_selectmonth,
    #                sales_script_type == 'script_A')%>%
    #         count(conversion)
    #     
    #     scriptA_rate<- (script_A_conversion[1,2] / sum(script_A_conversion$n)) * 100
    #     
    #     
    #     
    #     valueBox(paste0(comma(scriptA_rate$n, digits = 2), '%'), 
    #              paste0("Conversion rate ( Script A) ", conv_rate_selectmonth, ', ', conv_rate_selectyear ), 
    #              width = 6, icon = icon("funnel-dollar"))
    # })
    # 
    # output$scriptB_conv_rate <- renderValueBox({
    #     conv_rate_selectyear <- input$conv_rate_selectyear
    #     conv_rate_selectmonth <- input$conv_rate_selectmonth
    #     
    #     script_B_conversion <- task_data_convert%>%
    #         filter(request_created_year == conv_rate_selectyear & request_created_month == conv_rate_selectmonth,
    #                sales_script_type == 'script_B')%>%
    #         count(conversion)
    #     
    #     scriptB_rate<- (script_B_conversion[1,2] / sum(script_B_conversion$n)) * 100
    #     
    #     
    #     
    #     valueBox(paste0(comma(scriptB_rate$n, digits = 2), '%'), 
    #              paste0("Conversion rate ( Script B) ", conv_rate_selectmonth, ', ', conv_rate_selectyear ), 
    #              width = 6, icon = icon("funnel-dollar"))
    # })
    # 
    # 
    # output$scriptC_conv_rate <- renderValueBox({
    #     conv_rate_selectyear <- input$conv_rate_selectyear
    #     conv_rate_selectmonth <- input$conv_rate_selectmonth
    #     
    #     script_C_conversion <- task_data_convert%>%
    #         filter(request_created_year == conv_rate_selectyear & request_created_month == conv_rate_selectmonth,
    #                sales_script_type == 'script_C')%>%
    #         count(conversion)
    #     
    #     scriptC_rate<- (script_C_conversion[1,2] / sum(script_C_conversion$n)) * 100
    #     
    #     
    #     
    #     valueBox(paste0(comma(scriptC_rate$n, digits = 2), '%'), 
    #              paste0("Conversion rate ( Script C) ", conv_rate_selectmonth, ', ', conv_rate_selectyear ), 
    #              width = 6, icon = icon("funnel-dollar"))
    # })
    # 
    # 
    # ############ marketing source conversion rate  #############
    # output$marketing_source_conv_rate <- renderValueBox({
    #     conv_rate_selectyear <- input$conv_rate_selectyear
    #     conv_rate_selectmonth <- input$conv_rate_selectmonth
    #     conv_marketing_source <- input$marketing_source
    #     
    #     marketing_source_conversion <- task_data_convert%>%
    #         filter(request_created_year == conv_rate_selectyear & request_created_month == conv_rate_selectmonth,
    #                source == conv_marketing_source)%>%
    #         count(conversion)
    #     
    #     marketing_source_conv_rate<- (marketing_source_conversion[1,2] / sum(marketing_source_conversion$n)) * 100
    #     
    #     
    #     
    #     valueBox(paste0(comma(marketing_source_conv_rate$n, digits = 2), '%'), 
    #              paste0("Conversion rate (Marketing Source Lead) ", conv_rate_selectmonth, ', ', conv_rate_selectyear,
    #                     ', ', conv_marketing_source), 
    #              width = 6, icon = icon("funnel-dollar"))
    # })
    # 
    # 
    # ############### sales group conversion rate  ####################
    # output$sales_group_conv_rate <- renderValueBox({
    #     conv_rate_selectyear <- input$conv_rate_selectyear
    #     conv_rate_selectmonth <- input$conv_rate_selectmonth
    #     conv_sales_group <- input$sales_group
    #     
    #     sales_group_conversion <- task_data_convert%>%
    #         filter(request_created_year == conv_rate_selectyear & request_created_month == conv_rate_selectmonth,
    #                sales_group_name == conv_sales_group)%>%
    #         count(conversion)
    #     
    #     sales_group_conv_rate<- (sales_group_conversion[1,2] / sum(sales_group_conversion$n)) * 100
    #     
    #     
    #     
    #     valueBox(paste0(comma(sales_group_conv_rate$n, digits = 2), '%'), 
    #              paste0("Conversion rate (Sales group) ", conv_rate_selectmonth, ', ', conv_rate_selectyear,
    #                     ', ', conv_sales_group), 
    #              width = 6, icon = icon("funnel-dollar"))
    # })
    # 
    # ###################### TIMESERIES CONVERSION ############################################
    # output$accur_drift <- renderValueBox({
    #     valueBox(value = paste0(comma(accur_drift[2,2], digits = 2)), 
    #              subtitle = "Drift method RMSE of conversion",
    #              color = "yellow", icon = icon("square-root-alt"))
    # })
    # 
    # output$accur_meanf <- renderValueBox({
    #     valueBox(value = paste0(comma(accur_meanf[2,2], digits = 2)), 
    #              subtitle = "Mean method RMSE of conversion",
    #              color = "yellow", icon = icon("square-root-alt"))
    # })
    # 
    # output$accur_naive <- renderValueBox({
    #     valueBox(value = paste0(comma(accur_naive[2,2], digits = 2)), 
    #              subtitle = "Naive method RMSE of conversion",
    #              color = "yellow", icon = icon("square-root-alt"))
    # })
    # 
    # output$accur_rwf <- renderValueBox({
    #     valueBox(value = paste0(comma(accur_rwf[2,2], digits = 2)), 
    #              subtitle = "RWF method RMSE of conversion",
    #              color = "yellow", icon = icon("square-root-alt"))
    # })
    # 
    # 
    # output$avg_monthly_conversion <- renderPlot({   
    #     ggseasonplot(data_convert_transform_ts[,'avgmonthly_convert'], year.labels = T, year.label.left = T, year.label.right = T) + 
    #         ggtitle("Timeseries plot of Average Monthly Conversions") + ylab('Average monthly conversions')
    # })
    # 
    # output$polar_series <- renderPlot({   
    #     ggseasonplot(data_convert_transform_ts[,'avgmonthly_convert'], polar = T) + 
    #         ggtitle("Polar plot of Average Monthly Conversions") + ylab('Average monthly conversions')
    # })
    # 
    # output$conversion_forecast <- renderPlot({
    #     autoplot(train_data_convert_transform_forecast[,'avgmonthly_convert']) + theme_dark() +
    #         autolayer(data_convert_meanf, series = "Mean", PI = T, alpha = 0.1) +
    #         autolayer(data_convert_naive, series = "Naive", PI = T, alpha = 0.1) +
    #         #  autolayer(rev_snaive, series = "Seasonal Naive", PI = T, alpha = 0.3) +
    #         autolayer(data_convert_rwf, series = "rwf", PI = T, alpha = 0.1) +
    #         autolayer(data_convert_rwfdrift, series = "Drift", PI = T, alpha = 0.1) +
    #         ggtitle("Forecast for Conversion with various forecasting methods") +
    #         guides(colour = guide_legend(title = "Forecast")) + ylab("Average Monthly Revenue") +
    #         xlab('Years')
    # })
    # 
    # output$meanf_diagnostic <- renderPlot({
    #     checkresiduals(data_convert_meanf)
    # })
    # 
    # output$naive_diagnostic <- renderPlot({
    #     checkresiduals(data_convert_naive)
    # })
    # 
    # output$rwf_diagnostic <- renderPlot({
    #     checkresiduals(data_convert_rwfdrift)
    # })
    
    
    ################ ANIMATIONS #########
    observe(addHoverAnim(session, "accur_rwf", "rubberBand"))
    observe(addHoverAnim(session, "accur_drift", "rubberBand"))
    observe(addHoverAnim(session, "avg_monthly_conversion", "swing"))
    # observe(addHoverAnim(session, "seas", "pulse"))
    observe(addHoverAnim(session, "dataset", "pulse"))
    observe(addHoverAnim(session, "ggnonparametrin", "pulse"))
    observe(addHoverAnim(session, "revenue_change_detect", "pulse"))
    observe(addHoverAnim(session, "revenue_stl_forecast", "pulse"))
    observe(addHoverAnim(session, "revenue_trendSeasonal_forecast", "pulse"))
    observe(addHoverAnim(session, "revenue_seasonality", "pulse"))
    observe(addHoverAnim(session, "revenue_seasonality", "pulse"))
    observe(addHoverAnim(session, "regress_model", "pulse"))
    observe(addHoverAnim(session, "seasonal_forecast", "pulse"))
    
    
    observe(addHoverAnim(session, "accur_meanf", "pulse"))
    observe(addHoverAnim(session, "accur_naive", "pulse"))
    observe(addHoverAnim(session, "sensitivity_result", "pulse"))
    observe(addHoverAnim(session, "specificity_result", "pulse"))
    
    observe(addHoverAnim(session, "mcfaddenR2_result", "pulse"))
    observe(addHoverAnim(session, "auc_perform", "pulse"))
    observe(addHoverAnim(session, "var_inf", "pulse"))
    observe(addHoverAnim(session, "regression_result", "pulse"))
    
    observe(addHoverAnim(session, "scriptA_conv_rate", "rubberBand"))
    observe(addHoverAnim(session, "scriptB_conv_rate", "rubberBand"))
    observe(addHoverAnim(session, "scriptC_conv_rate", "swing"))
    observe(addHoverAnim(session, "marketing_source_conv_rate", "swing"))
    observe(addHoverAnim(session, "sales_group_conv_rate", "swing"))
})
