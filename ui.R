#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

dashboardPage(skin = "midnight", 
              dashboardHeader(title = "Envio Product Analytics"
              ),
              dashboardSidebar(
                  collapsed = TRUE, disable = TRUE, minified = FALSE 
              ),
              dashboardBody( 
                  busy_start_up(loader = spin_epic("flower", color = "#99FFC1"), text = h1("HOMELIKE PRODUCT ANALYTICS DASHBOARD loading..."),
                                mode = "auto", color = "#FFFBBB", background = "#111539"),
                  setZoom(class = "box"), setZoom(class = "gauge"), setZoom("column"), 
                  setZoom(id = "newuser_transactions"), setZoom(id = "returnuser_transactions"),
                  tabsetPanel(
                      tabPanel(title = "Dataset used", icon = icon('database'),
                               h3("Dataset"),
                               # fluidRow(
                               #     box(width = 6, collapsible = TRUE, collapsed = TRUE,
                               #         dataTableOutput("dataset") %>%
                               #             withSpinner(type = 8, color = "green")
                               #     ),
                               #     
                               #     box(width = 6, background = 'blue', collapsible = TRUE, 
                               #         plotOutput('ggnonparametrin') %>%
                               #             withSpinner(type = 8, color = "green")
                               #     )
                               #     
                               # )
                               
                               fluidRow(
                                 valueBoxOutput(outputId = "unique_visitors"),
                                 valueBoxOutput(outputId = 'unique_session'),
                                 valueBoxOutput(outputId = 'avg_sess_per_visitor')
                               ),
                               fluidRow(
                                 valueBoxOutput(outputId = 'conversion_total'),
                                 valueBoxOutput(outputId = 'bounce_rate'),
                                 valueBoxOutput(outputId = 'customer_aquisition')
                               ),
                               
                               column(width = 3,
                                      selectizeInput(inputId = 'date_select', choices = unique(KPI_data$date_booked),
                                                     label = 'Select date for KPI'))
                      ),
                      ##################### Conversion tab  
                      tabPanel("Conversion Rate",
                               fluidPage(title = "Conversion Rate",  withAnim(),### KPI Rows
                                         fluidRow(
                                             valueBoxOutput("scriptA_conv_rate")%>%
                                                 withSpinner(type = 8, color = "green"),
                                             
                                             valueBoxOutput("scriptB_conv_rate")%>%
                                                 withSpinner(type = 8, color = "green"),
                                             
                                             valueBoxOutput("scriptC_conv_rate")%>%
                                                 withSpinner(type = 8, color = "green")
                                         ),
                                         fluidRow(
                                             valueBoxOutput("marketing_source_conv_rate")%>%
                                                 withSpinner(type = 8, color = "green"),
                                             valueBoxOutput("sales_group_conv_rate")%>%
                                                 withSpinner(type = 8, color = "green")
                                         ),
                                         fluidRow(
                                             # column(width = 6,
                                             #        sliderInput("conv_rate_selectyear", label = "Select year", min = 2019, max = 2021,
                                             #                    value = 2020, sep = "")
                                             # ),
                                             
                                             column(width = 6,
                                                    selectInput("conv_rate_selectmonth", label = "Select month", choices = c(unique(KPI_data$date)))
                                             )
                                         ),
                                         fluidRow(
                                             # column(width = 6,
                                             #        selectInput("marketing_source", label = "Select Marketing source lead", 
                                             #                    choices = c(unique(task_data_convert$source)))
                                             # ),
                                             # 
                                             # column(width = 6,
                                             #        selectInput("sales_group", label = "Sales group", choices = c(unique(task_data_convert$sales_group_name)))
                                             # )
                                         ))),
                      #                   
                      ######################### Conversion UI ################################
                      tabPanel(title = "A/B testing",
                               h3('A/B testing -KPI comparison'),
                               
                               fluidRow(
                                 column(width = 6, h4('Control group KPI'),
                                        valueBoxOutput("rcsf_ref_conversion")%>%
                                          withSpinner(type = 8, color = "green"),
                                        valueBoxOutput("rcsf_ref_bounce")%>%
                                          withSpinner(type = 8, color = "green"),
                                        ),
                                 column(width = 6, h4('Test group KPI'),
                                        valueBoxOutput("rcsf_show_conversion")%>%
                                          withSpinner(type = 8, color = 'green'),
                                        
                                        valueBoxOutput("rcsf_show_bounce")%>%
                                          withSpinner(type = 8, color = "green")
                                        )
                               ),
                               
                               fluidRow(
                                 column(width = 3,
                                        selectInput('date_filter', label = 'Select date for analysis',
                                                    choices = unique(KPI_data$date_booked))
                                        ),
                                 
                                 column(width = 9,
                                        box(title = 'User journey for A/B testing', width = 12,
                                            plotlyOutput('ab_user_jour')) %>%
                                          withSpinner(type = 8, color = "green"))
                               )#,
                               
                               # fluidRow(
                               #     box(title = "Timeseries of monthly average Conversion", collapsible = TRUE,
                               #         plotOutput("avg_monthly_conversion")%>%
                               #             withSpinner(type = 8, color = "green")
                               #     ),
                               #     
                               #     box(title = "Polar chart of monthly average Conversion" , collapsible = TRUE,
                               #         plotOutput("polar_series")%>%
                               #             withSpinner(type = 8, color = "green")
                               #     )
                               # ),
                               # fluidRow(
                               #     box(title = "Forecasting conversion with various methods", collapsible = TRUE,
                               #         width = 10,
                               #         plotOutput("conversion_forecast")%>%
                               #             withSpinner(type = 8, color = "green")
                               #     )
                               # ),
                               # fluidRow(
                               #     box(title = "Diagnotics of residuals for Mean forecasting Method", collapsible = TRUE, width = 6,
                               #         plotOutput("meanf_diagnostic")%>%
                               #             withSpinner(type = 8, color = "green")
                               #     ),
                               #     
                               #     box(title = "Diagnotics of residuals for Naive forecasting Method", collapsible = TRUE, width = 6,
                               #         plotOutput("naive_diagnostic")%>%
                               #             withSpinner(type = 8, color = "green")
                               #     )
                               # ),
                               # fluidRow(
                               #     box(title = "Diagnotics of residuals for Randow walk with drift forecasting Method", collapsible = TRUE, width = 6,
                               #         plotOutput("rwf_diagnostic")%>%
                               #             withSpinner(type = 8, color = "green")
                               #     )
                               # )
                      ),
                      
                      ######## Clustering and grouping
                      tabPanel(title = 'Clustering',
                               fluidPage(title = 'cluster',
                                         fluidRow(
                                           box(collapsible = TRUE,
                                               plotOutput('device_type')%>%
                                                 withSpinner(type = 8, color = 'green')
                                               ),
                                           box(collapsible = TRUE,
                                               plotOutput('user_country')%>%
                                                 withSpinner(type = 8, color = "green")
                                               )
                                         ),
                                         
                                         fluidRow(
                                           # column(width = 3,
                                           #        selectInput('date_select', label = 'Select date',
                                           #                    choices = unique(KPI_data$date_booked))),
                                           box(collapsible = TRUE, width = 6,
                                               plotOutput('user_browser')%>%
                                                 withSpinner(type = 8, color = "green")),
                                           
                                           box(collapsible = TRUE, width = 6,
                                               highchartOutput('user_jour')%>%
                                                 withSpinner(type = 8, color = "green"))
                                           
                                         ),
                                         
                                         fluidRow(
                                           box(title = 'Clustering',
                                             plotOutput('cluster_plot')%>%
                                               withSpinner(type = 8, color = "green")
                                           ),
                                           
                                           box(title = 'optimal number of clusters',
                                               plotOutput('optimal_clust')%>%
                                                 withSpinner(type = 8, color = "green"))
                                         ),
                                         
                                         fluidRow(
                                           box(title = 'Display Optimal clusters',
                                               plotOutput('optimal_cluster_plot')%>%
                                                 withSpinner(type = 8, color = "green"))
                                         )
                                         )
                        
                      ),
                      ### Naive bayes 
                      tabPanel(title = 'Naive Bayes Classifier',
                               tabBox(id = 'response',
                                      plotOutput('response_variable')),
                               tabBox(id = 'model_perf',
                                      fluidRow(
                                        valueBoxOutput("rmse"),
                                        valueBoxOutput("gini")
                                      ),
                                      fluidRow(
                                        h5("Confusion Matrix"),
                                        tableOutput("confusionmatrix")
                                      ),
                                      fluidRow(
                                        plotlyOutput("auc_perform")
                                      )
                                      ),
                               tabBox(id = 'naive_predict',
                                      fluidPage(
                                        h5("Predict whether or not a firm has upgraded its existing product line"),
                                        fluidRow(
                                          colum(width = 2,
                                                selectInput('predic', label = 'Select Client ID',
                                                            choices = unique(test_predict_join$rowid)
                                                            )
                                                ),
                                          column(width = 2,
                                            valueBoxOutput("actual"),
                                            valueBoxOutput("predicted"),
                                            valueBoxOutput("model_precision")
                                          )
                                          
                                        )
                                      )
                                      )),
                      
                      #### Tabset to show splitting of response variable into training and test datasets
                      tabPanel( title = "Response variable", icon = icon("chart-bar"),
                                fluidPage(title = "Conversion",
                                          h4("Proportion of binary class in response variable (Conversion)"),                       
                                          fluidRow(
                                              
                                              column(width = 6, reporTheme = T, 
                                                     plotlyOutput("training_dataset")%>%
                                                         withSpinner(type = 8, color = "green")
                                              ),
                                              column(width = 6, 
                                                     plotlyOutput("test_dataset")%>%
                                                         withSpinner(type = 8, color = "green")
                                              )
                                          )
                                          
                                          
                                )
                                
                      ),
                      
                      ### Tabpanel to display performance metrics for Multiple Logistic regression model
                      tabPanel(title = "Multiple Logistic regression", value = "model_predictors", icon = icon("chart-line"),
                               fluidPage(
                                   fluidRow(
                                       column(width = 6, plotOutput(outputId = 'regression_result') %>%
                                                  withSpinner(type = 8, color = "green")),
                                       column(width = 6, fluidRow(h3('Influence of variables in predicting conversion',
                                                                     dataTableOutput('var_inf')%>%
                                                                         withSpinner(type = 8, color = "green")
                                       )))
                                   ),
                                   h2("Model validation with testing dataset"),
                                   h3("Performance Metrics"),
                                   fluidRow(
                                       valueBoxOutput("sensitivity_result")%>%
                                           withSpinner(type = 8, color = "green"),
                                       valueBoxOutput("specificity_result")%>%
                                           withSpinner(type = 8, color = "green"),
                                       valueBoxOutput("mcfaddenR2_result")%>%
                                           withSpinner(type = 8, color = "green")
                                   ),
                                   fluidRow(
                                       column(width = 6,
                                              plotOutput("auc_perform")%>%
                                                  withSpinner(type = 8, color = "green")
                                       ),
                                       column(width = 6,
                                              box(title = 'Interpretation of results', width = 12,
                                                  solidHeader = TRUE, collapsible = TRUE, background = "green",
                                                  'The confusion matrix indicated that the model correctly 
                                                  predicted 858 conversions out of a total of 2017,
which a precision rate of 42.54% (sensitivity). Also, the model correctly predicted 1119 non-conversions
out of a total of 1983 non-conversions which is a specificity rate of 56.43%. Given that we are more
interested in successfully predicting conversion, the model should be assessed based on sensitivity.
Moreover, the predictors in the model are not optimal hence the need for better predictors to improve 
the model. The model needs to be tuned to improve sensitivity. The model has AUC of 0.5012221 hence 
                                                  a poor classifying model. With McFadden pseudo-r2 value 
                                                  of 0.000587, the model has very low predictive power 
                                                  which suggests the need to consider inclusion of 
                                                  serveral other better predictors.'))
                                   )
                               )
                      ),
                      
                      ## tabpanel for model prediction
                      tabPanel(title = "Predict client conversion", value = "pred", icon = icon("wave-square"),
                               fluidPage(
                                   h3("Predict whether or not a client will sign-up"),
                                   
                                   # fluidRow(
                                   #     column(width = 4,
                                   #            selectizeInput(inputId = 'client',label = 'Client ID', selected = 11112344,
                                   #                           choices = unique(prediction_data$request_id)))
                                   # ),
                                   fluidRow(
                                       valueBoxOutput("actual"),
                                       valueBoxOutput("predicted"),  
                                       valueBoxOutput("model_precision")%>%
                                           withSpinner(type = 8, color = "green")
                                   )
                               ))
                  )))