library(readr)
library(tidyverse) # data manipulation and visualization
library(ggplot2)
library(GGally)
library(ggstatsplot)
library(plotly)
library(highcharter)
library(cluster)  ### working with clusters
library(factoextra) ## cal and visualizing clusters
library(gridExtra) ## plotting multiple graphs
library(DT)
library(stringr)
library(stringi)
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outputs
library(car)  ## for regression
#library(haven)
library(caret)
library(h2o)
library(rsample)
library(collapse)
library(magrittr)

####
library(formattable)
library(fontawesome)
library(shiny)
library(shinydashboard)
library(devtools)
library(magrittr)
library(purrr)
library(shinyWidgets)
library(tibble)
library(utils)
library(bslib)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyanimate)
library(shinydashboardPlus)
library(shinyEffects)
library(shinybusy)
library(shinyBS)
library(ggstatsplot)
library(bfast)
library(MASS)
library(yarrr)
library(ROCR)


###################################################
# bq_results_20210718 <- read_csv("bq-results-20210718.csv")
# bq_results_20210717 <- read_csv("bq-results-20210717.csv")
# bq_results_20210716 <- read_csv("bq-results-20210716.csv")
# bq_results_20210715 <- read_csv("bq-results-20210715.csv")
# bq_results_20210714 <- read_csv("bq-results-20210714.csv")
# 
# data14 <- bq_results_20210714
# data15 <- bq_results_20210715
# data16 <- bq_results_20210716
# data17 <- bq_results_20210717
# data18 <- bq_results_20210718


#########
#all_data <- rbind(data18, data17, data16, data15, data14)
#View(all_data)
#write_csv(all_data, file = 'all_data.csv')

#library(readr)
all_data <- read_csv("all_data.csv")
#View(all_data)

########## Unique visitors
KPI_data <- all_data %>%
  dplyr::select(datetime,visitor_id, user_location_country, user_location_city, session_id, page_type, 
                event_type, test_groups)%>%
  separate(col = datetime, into = c('date_booked', 'time'), sep = ' ')

#View(KPI_data)

# unique_visitors_count<- KPI_data %>%
#   collapse::fselect(visitor_id)%>%
#   collapse::na_omit()%>%
#   count()

#unique_visitors_count  ### total number of unique visitors is 17,454 / 17453 when NA is removed

#View(unique_visitors_count)

# ### Total number of unique sessions
# #date_select = input$date_select
# unique_session_count <- KPI_data %>%
#   collapse::fselect(session_id)%>%
#   collapse::na_omit()%>%
# #  dplyr::filter(date_booked == date_select)%>%
#   count()
#   
#unique_session_count
  
### Average session per visitor
# avg_session_per_user <- unique_session_count/unique_visitors_count
#avg_session_per_user   ### the average session per user is 1.081


## Conversion rate
## Conversion occurs when page_type == request/success
# find number of conversions made
# date_select <- input$date_select
# conversion_total <- KPI_data %>%
#   dplyr::select(page_type, date_booked = date)%>%
#   na.omit()%>%
#   filter(page_type == "request/success" & date_booked = date_select)%>%
#   count()
#View(conversion_total)  ## number of conversions

## conversion rate
#conversion_rate <- conversion_total / unique_session_count #* 100
#conversion_rate  ## conversion rate is 0.159 %


##############
######## Bounce_rate
# bounce <- KPI_data%>%
#   collapse::fselect(session_id, page_type)%>%
#   collapse::na_omit()%>%
#   collapse::fgroup_by(session_id)#%>%
#   as.data.frame()%>%
#   count(page_type)#%>%
#   tally(wt = n)#%>%
#   mutate(bounce_status = case_when(num_pages == 1 ~ "bounce",
#                                    num_pages > 1 ~ "not_bounce"))
# 
#   
# View(bounce)
# count(bounce, page_type)
# bounce <- KPI_data%>%
#   dplyr::select(session_id, page_type)%>%
#   na.omit()%>%
#   group_by(session_id)%>%
#   count(page_type)%>%
#   tally(wt = n)%>%
#   mutate(num_pages = n)%>%
#   mutate(bounce_status = case_when(num_pages == 1 ~ "bounce",
#                                    num_pages > 1 ~ "not_bounce"))
# #View(bounce)
# 
# bounce_group <- bounce%>%
#   group_by(bounce_status)%>%
#   count(bounce_status)
# View(bounce_group)
# 
# ## bounce rate
# bounce_rate <- (bounce_group[1,2]/sum(bounce_group$n)) * 100
# bounce_rate  ## 13.50384% bounce rate



## Customer acquisition as KPI 
# customer_acquisition <- KPI_data%>%
#   dplyr::select(event_type) %>%
#   na.omit()%>%
#   dplyr::filter(event_type == "user_register_success")%>%
#  count()   ## 76 new users register for our services



################################     Task 2: Analyse  ################################################
########## ●Analyse results of our “rcsp’ A/B test and present results to the product team. 
####   ○The “rcsp=show” test users of page_type=search_page experience a faster loading time of the page. 

a_b_testgroups <- KPI_data%>%
  mutate(test_status = case_when(grepl(pattern = '"rcsp":"show"', test_groups) ~"test",
                                 str_detect(test_groups, pattern = '"rcsp":"show"', 
                                            negate = T)~ "control"))
         
#View(a_b_testgroups)
## divide data into test group and control group
# subset test group
# rcsp_show <- subset(KPI_data, grepl('"rcsp":"show"', test_groups))
# # subset control group
# rcsp_ref <- subset(KPI_data, grepl(pattern = '"rcsp":"ref"', test_groups))
# View(rcsp_show)
# ######## conversion rate for control group
# rcsp_ref_conversion <- rcsp_ref%>%
#   filter(page_type == "request/success") %>%
#   na.omit() %>%
# count(rcsp_ref_conversion)  ## Conversions for control group is 19

# a_b_testgroups %>%
#   filter(page_type == "request/success" & test_status == 'control')%>%
#   na.omit()%>%
#   count()

## number of sessions made by rcsp_ref
# rcsp_ref_session <- rcsp_ref%>%
#   dplyr::select(session_id)%>%
#   na.omit()%>%
#   count(session_id)
# count(rcsp_ref_session) ## 9468 sessions for rcsp_ref /  9467 when NA removed
# 
# rcsp_ref_conversion_rate <- (count(rcsp_ref_conversion)/count(rcsp_ref_session)) * 100
# rcsp_ref_conversion_rate ## 0.2% conversion rate for rcsp_ref

#### bounce rate for A/B testing
# ## Bounce_rate for rcsp_ref
# bounce_rcsp_ref <- a_b_testgroups %>%
#   dplyr::filter(test_status =='control') %>%
#   dplyr::select(session_id, page_type)%>%
#   na.omit()%>%
#   dplyr::group_by(session_id)%>%
#   count(page_type)%>%
#   tally(wt = n)%>%
#   mutate(num_pages = n)%>%
#   mutate(bounce_status = case_when(num_pages == 1 ~ "bounce",
#                                    num_pages > 1 ~ "not_bounce"))
# 
# View(bounce_rcsp_ref)
# bounce_group_rcsp_ref <- bounce_rcsp_ref%>%
#   group_by(bounce_status)%>%
#   count(bounce_status)
# 
# ## bounce rate for rcsp_ref
# bounce_rate_rcsp_ref <- (bounce_group_rcsp_ref[1,2]/sum(bounce_group_rcsp_ref$n)) * 100
# bounce_rate_rcsp_ref  ## 13.61572% bounce rate

################# Test group ###################
#####  Bounce_rate for A/B testing
## Bounce rate rcsp_show
bounce_rcsp_show <- a_b_testgroups%>%
  dplyr::filter(test_status == 'test')%>%
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



####### User journey for A/B testing

##### user journey for user_ref
rcsp_ref_user_jour <- a_b_testgroups%>%
  dplyr::filter(test_status == 'control') %>%
  dplyr::select(page_type)%>%
  na.omit()%>%
  group_by(page_type)%>%
  count()
#View(rcsp_ref_user_jour)

## user journey rcsp_show 
rcsp_show_user_jour <- a_b_testgroups %>%
  dplyr::filter(test_status == 'test')%>%
  dplyr::select(page_type)%>%
  na.omit()%>%
  dplyr::group_by(page_type)%>%
  count()

#View(rcsp_show_user_jour)

rcsp_ref_user_jour.desc<- dplyr::arrange(rcsp_ref_user_jour, desc(n) )
rcsp_show_user_jour.desc <- dplyr::arrange(rcsp_show_user_jour, desc(n))

rcsp_funl <- plot_ly(
  type = "funnel",
  name = 'rcsp:ref (control group)',
  y = as.vector(rcsp_ref_user_jour.desc$page_type),
  x = as.vector(rcsp_ref_user_jour.desc$n),
  textinfo = "value+percent initial") 

rcsp_funl_both <- rcsp_funl %>%
  add_trace(
    type = "funnel",
    name = 'rcsp:show (test group)',
    orientation = "h",
    y = as.vector(rcsp_show_user_jour.desc$page_type),
    x = as.vector(rcsp_show_user_jour.desc$n),
    textposition = "inside",
    textinfo = "value+percent initial") 
rcsp_funl_both_arrange <- rcsp_funl %>%
  layout(yaxis = list(categoryarray = as.vector(rcsp_show_user_jour.desc$page_type))) %>%
  layout(hovermode = 'compare')


rcsp_funl_both

rcsp_funl_both_arrange



users_device_class <- all_data%>%
  dplyr::select(visitor_id, device_class)%>%
  na.omit()%>%
  dplyr::group_by(device_class)%>%
  dplyr::distinct(visitor_id)%>%
  count(visitor_id)%>%
  dplyr::tally(wt = n)%>%
  dplyr::arrange(desc(n)) %>%
  dplyr::rename("Number of users" = n)

# ggplot(data = users_device_class, mapping = aes(x = reorder(device_class, -`Number of users`),
#                                                 y = `Number of users`)) + geom_col() +
#   ggtitle(label = "Number of users based on devices used") + xlab("Device class")





### clusters users into groups based on user country
users_country <- all_data%>%
  dplyr::select(visitor_id, user_location_country)%>%
  na.omit()%>%
  dplyr::group_by(user_location_country)%>%
  dplyr::distinct(visitor_id)%>%
  count(visitor_id)%>%
  tally(wt = n)%>%
  dplyr::arrange(desc(n)) %>%
  dplyr::rename("Number of users" = n)
#View()
top20_users_country <- dplyr::top_n(users_country, n = 20)


### clusters users into groups based on user browser
users_browser <- all_data%>%
  dplyr::select(visitor_id, device_browser)%>%
  na.omit()%>%
  dplyr::group_by(device_browser)%>%
  dplyr::distinct(visitor_id)%>%
  count(visitor_id)%>%
  dplyr::tally(wt = n)%>%
  dplyr::arrange(desc(n)) %>%
  dplyr::rename("Number of users" = n)
#View()

#ggplot(data = top_n(users_browser, n =5), mapping= aes(x = device_browser, y = `Number of users`)) + geom_col()

ggplot(data = top_n(users_browser, n =5), mapping = aes(x = reorder(device_browser, -`Number of users`),
                                                 y = `Number of users`)) + geom_col() +
  ggtitle("Top 5 browers") + xlab("Type of browser")



## funnel charts for user journey
page_type_count <- all_data%>%
  dplyr::select(page_type)%>%
  na.omit()%>%
  dplyr::group_by(page_type)%>%
  count()%>%
  dplyr::arrange(desc(n))


user_jour_funnel <- page_type_count %>%
  hchart(
    "funnel", hcaes(x = page_type, y = n),
    name = "user_journey")
user_jour_funnel



# ####################### cluster analysis
## pageviews per visitor
user_pageviews <- all_data%>%
  dplyr::select(visitor_id,page_type)%>%
  na.omit()%>%
  dplyr::group_by(visitor_id)%>%
  count(page_type)%>%
  dplyr::tally(wt = n)%>%
  dplyr::mutate(num_pages = n)%>%
  dplyr::select(-2)

#View(user_pageviews)
## number of sessions per visitor
user_sess <- all_data%>%
  dplyr::select(visitor_id, session_id) %>%
  na.omit()%>%
  dplyr::group_by(visitor_id)%>%
  dplyr::distinct(session_id)

user_sess_num <- user_sess%>%
  dplyr::group_by(visitor_id)%>%
  count()%>%
  dplyr::mutate(num_sessions = n)%>%
  dplyr::select(-2)

### merge num_pages and num_sessions for visitors
users_all <- merge(user_pageviews, user_sess_num)
visitor_pageviews_sessions_num <- merge(user_pageviews, user_sess_num)


#View(visitor_pageviews_sessions_num)


## k-mean clustering
visitor_pageviews_sessions_num <- na.omit(visitor_pageviews_sessions_num)
## scale data
visitor_pageviews_sessions_num <- scale(visitor_pageviews_sessions_num[,c(2:3)])
#head(visitor_pageviews_sessions_num)

#cal the distance measure
#user_distance <- get_dist(visitor_pageviews_sessions_num)
#user_distance
## viz the distance measure
# fviz_dist(user_distance, gradient = list(low = "#00AFBB", mid = "white",
#                                          high = "#FC4E07"))

##### K means clustering
k2_visitor <- kmeans(visitor_pageviews_sessions_num, centers = 2, nstart = 25)
# # str(k2_visitor)
# # k2_visitor
# # remove(k2_visiotr)
# ### visualize clusters
# fviz_cluster(k2_visitor, data = visitor_pageviews_sessions_num)
# 
# ## pairwise scatter plots to visualize clusters
# visitor_pageviews_sessions_num%>%
#   as_tibble()%>%
#   mutate(cluster = k2_visitor$cluster,
#          state = row.names(visitor_pageviews_sessions_num))%>%
#   ggplot(aes(num_pages, num_sessions, color = factor(cluster), label = state)) +
# #   geom_text()
# # 
 k3_visitor <- kmeans(visitor_pageviews_sessions_num, centers = 3, nstart = 25) ## kmeans with 3 clusters
 k4_visitor <- kmeans(visitor_pageviews_sessions_num, centers = 4, nstart = 25) ## kmeans with 4 clusters
 k5_visitor <- kmeans(visitor_pageviews_sessions_num, centers = 5, nstart = 25) ## kmeans with 5 clusters
#
# # plots to compare
 p1 <- fviz_cluster(k2_visitor, geom = "point", data = visitor_pageviews_sessions_num) + ggtitle("k = 2")
 p2 <- fviz_cluster(k3_visitor, geom = "point", data = visitor_pageviews_sessions_num) + ggtitle("k = 3")
 p3 <- fviz_cluster(k4_visitor, geom = "point", data = visitor_pageviews_sessions_num) + ggtitle("k = 4")
 p4 <- fviz_cluster(k5_visitor, geom = "point", data = visitor_pageviews_sessions_num) + ggtitle("k = 5")
#
# ############# plot all graphs together  #######
 grid.arrange(p1, p2, p3, p4, nrow = 2)

 #######  compute k-means clustering with k = 2  // the optimal clusters according to silhoutte
 set.seed(123)
 optimal_clusters_visitors <- kmeans(visitor_pageviews_sessions_num, 2, nstart = 25)
 
 optimal_clust_viz <- fviz_cluster(optimal_clusters_visitors, data = visitor_pageviews_sessions_num)
# optimal_clust_viz

 
### Data has been preprocessed with the code below, exported and loaded
 
#  all_conversion<- all_data%>%
#    filter(page_type == "request/success")
# # View(all_conversion)
# 
#  all_conversion.sel <- all_conversion%>%
#    dplyr::select(test_groups, session_id, visitor_id, device_class, params)%>%
#    na.omit() %>%
#    dplyr::mutate(test_status = case_when(grepl(pattern = '"rcsp":"show"', test_groups) ~"test",
#                                   str_detect(test_groups, pattern = '"rcsp":"show"', 
#                                              negate = T)~ "control"),
#           instant_booking = case_when(grepl(pattern = '"is_instant_booking":true', params)~"Instant",
#                                       grepl(pattern = '"is_instant_booking":false', params)~"Not_instant"),
#           user_verified = case_when(grepl(pattern = '"is_user_verified":true', params)~"Verified",
#                                     grepl(pattern = '"is_user_verified":false', params)~"Not_verified"))
#  
#  
# # View(all_conversion.sel)
#  
#  all_conversion_wide <- tidyr::separate(all_conversion.sel, col = params, sep = "total", into = c("params", "total_paid"))%>%
#    tidyr::separate(col = total_paid, sep = ',"currency":"', into = c("total_paid", "rest"))%>%
#    tidyr::separate(col = rest, sep = '"', into = c("currency", "rest_info"))%>%
#    tidyr::separate(col = total_paid, sep = '":', into = c("waste", "total_paid"))%>%
#    dplyr::select(-6,-9) %>%
#    tidyr::separate(col=params, sep = '"days":', into = c("params", "days"))%>%
#    tidyr::separate(col = days, sep = ',', into = c("days", "city", "country", "price", "tenant_fee",
#                                             "landlord_fee")) %>%
#    tidyr::separate(col = city, sep = '"', into = c("a", "b", "c", "d"))%>%
#    dplyr::select(-a,-b,-c)%>%
#    tidyr::separate(col = country, sep = '"', into = c("a","b","c","e"))%>%
#    dplyr::select(-a,-b,-c)%>%
#    tidyr::separate(col = price, sep = '"', into = c("a","b","c","f"))%>%
#    dplyr::rename(price = c) %>%
#    tidyr::separate(col = price, sep = ':', into = c("f", "price"))%>%
#    dplyr::select(-a,-b,-f)%>%
#    tidyr::separate(col = tenant_fee, sep = '"', into = c("a", "b", "c"))%>%
#    tidyr::separate(col=c, sep = ":", into = c("g", "h"))%>%
#    dplyr::rename(tenant_fee = h)%>%
#    dplyr::select(-a,-b,-g)%>%
#    tidyr::separate(col = landlord_fee, sep = '"', into = c("a","b","c"))%>%
#    tidyr::separate(col = c, sep= ':', into = c("x","y"))%>%
#    dplyr::rename(landlord_fee = y)%>%
#    dplyr::select(-a,-b,-x)%>%
#    dplyr::rename(city = d, country = e)
# 
# # View(all_conversion_wide) 
# 
#  ## number of sessions per visitor who converted
#  all_conversion_var <- all_conversion_wide%>%
#    group_by(visitor_id)%>%
#    distinct(session_id, .keep_all = T )%>%
#    count()%>%
#    mutate(num_sessions = n)%>%
#    dplyr::select(-n)
# View(all_conversion_var) 
# 
# 
# all_conversions_variables<- merge(all_conversion_var, all_conversion_wide)%>%
#   distinct(visitor_id,.keep_all = T) 
# 
# 
# all_conversions_variables <- all_conversions_variables%>%
#   mutate(price = as.numeric(price), tenant_fee = as.numeric(tenant_fee), 
#          landlord_fee = as.numeric(landlord_fee), total_paid = as.numeric(total_paid),
#          device_class = factor(device_class, ordered = TRUE),
#          test_status = factor(test_status, ordered = TRUE),
#          instant_booking = factor(instant_booking, ordered = TRUE),
#          user_verified = factor(user_verified, ordered = TRUE),
#          num_sessions = as.numeric(num_sessions),
#          days = as.numeric(days))%>%
#   mutate(total_paid.EUR = case_when(currency == 'CHF' ~ total_paid*0.94,
#                                     currency == 'GBP' ~ total_paid*1.18,
#                                     currency == 'EUR' ~ total_paid))

#View(all_conversions_variables) 
#write.csv(all_conversions_variables, file = 'all_conversions_variables.csv') 


all_conversions_variables <- read_csv("all_conversions_variables.csv")

all_conversions_variables <- all_conversions_variables%>%
  mutate(price = as.numeric(price), tenant_fee = as.numeric(tenant_fee), 
         landlord_fee = as.numeric(landlord_fee), total_paid = as.numeric(total_paid),
         device_class = factor(device_class, ordered = TRUE),
         test_status = factor(test_status, ordered = TRUE),
         instant_booking = factor(instant_booking, ordered = TRUE),
         user_verified = factor(user_verified, ordered = TRUE),
         num_sessions = as.numeric(num_sessions),
         days = as.numeric(days))%>%
  mutate(total_paid.EUR = case_when(currency == 'CHF' ~ total_paid*0.94,
                                    currency == 'GBP' ~ total_paid*1.18,
                                    currency == 'EUR' ~ total_paid))
#glimpse(all_conversions_variables)

###########  naive bayes classifier  ###########
### Predicting whether a convertor will use our instant_booking feature
all_conversions.modelvars <- all_conversions_variables%>%
  dplyr::select(num_sessions, device_class, days, test_status, instant_booking, user_verified)


#glimpse(all_conversions.modelvars)

## splitting data
set.seed(123)
all_conversion_split_data <- initial_split(all_conversions.modelvars, prop = 0.7, strata = "instant_booking")
train_all_conversion <- training(all_conversion_split_data)
test_all_conversion <- testing(all_conversion_split_data)
#table(train_all_conversion$instant_booking)
#table(test_all_conversion$instant_booking)
#View(train_all_conversion)

# ###### h2o for naive bayes
h2o.init()
#
# ## h2o does not accepted ordered factors
h2o_train_allconversion_instant <- train_all_conversion %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()
#
h2o_test_allconversion_instant <- test_all_conversion%>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()
#
x_h2o = setdiff(names(train_all_conversion), "instant_booking")
#
y_h2o <-  'instant_booking'
#   #as.factor(instant_booking) #as.factor(train_all_conversion[,"instant_booking"])
#
# ## h2o naive model
h2o_naive_bayes <- h2o.naiveBayes(
  x = x_h2o,
  y = y_h2o,
  training_frame = h2o_train_allconversion_instant,
  nfolds = 10,
  laplace = 0
)
#
# ## confusionmatrix to assess result
h2o.confusionMatrix(h2o_naive_bayes)
#
#
# ## parameter tuning
# preprocess_h2o <- preProcess(train_all_conversion, method = c("BoxCox", "center", "scale", "pca"))
#
h2o_hyper_params <- list(
  laplace = seq(0, 5, by = 1)
)
#
h2o_grid <- h2o.grid(
  algorithm = "naivebayes",
  grid_id = "nb_id",
  hyper_params = h2o_hyper_params,
  training_frame = h2o_train_allconversion_instant,
  nfolds = 10,
  x = x_h2o,
  y = y_h2o
)
#
# ### sort model by accuracy
 h2o_sorted_grid <- h2o.getGrid(grid_id = "nb_id", sort_by = "accuracy", decreasing = TRUE)
 h2o_best_model_retrive <- h2o_sorted_grid@model_ids[[1]]
 h20_best_model <- h2o.getModel(h2o_best_model_retrive)
#
# ## confusinmatrx
h2o.confusionMatrix(h20_best_model)
#
# # ROC
 h2o_auc <- h2o.auc(h20_best_model, xval = TRUE)
#
# ## fpr retrieve
fpr <- h2o.performance(h20_best_model, xval = TRUE) %>%
  h2o.fpr() %>%
  .[["fpr"]]

# ## tpr retrieve
tpr <- h2o.performance(h20_best_model, xval = TRUE) %>%
  h2o.tpr() %>%
  .[["tpr"]]

data.frame(fpr = fpr, tpr = tpr) %>%
  ggplot(aes(fpr, tpr)) + geom_line() + ggtitle(sprintf("AUC: %f", h2o_auc))

# ## evaluate model with training data
# h2o.performance(h20_best_model, h2o_train_upgradeproduct)

## evaluate model with test data
test_model <- h2o.performance(h20_best_model, h2o_test_allconversion_instant, xval = TRUE)

## use model to predict
#h2o.predict(h20_best_model, newdata = h2o_test_allconversion_instant)


#h2o.confusionMatrix(h20_best_model, newdata = h2o_test_allconversion_instant)
test_confusionMatrix <- h2o.confusionMatrix(h20_best_model, newdata = h2o_test_allconversion_instant)

#h2o.confusionMatrix(h20_best_model)

 test_rmse <- h2o.rmse(test_model)
 test_gini <- h2o.giniCoef(test_model)
#
#
# # test_confusionMatrix <- test_confusionMatrix %>%
# #   dplyr::select("Upgraded existing product line" = 1, "No upgrade of existing product line" = 2, Error, Rate)
# # #View(test_confusionMatrix)
# #h2o.precision(test_model)

 test_prediction <- h2o.predict(h20_best_model, newdata = h2o_test_allconversion_instant)
#
test_prediction_with_rowid <- rowid_to_column(as.data.frame(test_prediction))
test_data_with_rowid <- rowid_to_column(as.data.frame(h2o_test_allconversion_instant))

# #View(test_prediction_with_rowid)
test_predict_join <- full_join(test_data_with_rowid, test_prediction_with_rowid) %>%
  dplyr::mutate(prediction_status = case_when(
    instant_booking == predict ~ 'Correct',
    instant_booking != predict ~ 'Wrong'
  )) %>%
  dplyr::select(rowid, instant_booking, predict, prediction_status)

#h2o.shutdown()

# #test_predict_join$upgrade_existingproduct_line[levels]
#
# #View(test_predict_join)


#################################
########################## predicting sales
## What percentage of sales is explained by the following predictors
## Assess the impact that certain predictors has on booking sales 


##### Spliting dataset into training and testing samples
set.seed(123)
sample_all_conversions <- sample(c(TRUE, FALSE), nrow(all_conversions_variables), replace = T, prob = c(0.6,0.4))
train_all_convern.lmvar <- all_conversions_variables[sample_all_conversions, ]
test_all_convern.lmvar <- all_conversions_variables[!sample_all_conversions, ]

## regression for qualitative predictors (categorical dataset -- factors) 
options('contrasts' = c('contr.treatment','contr.treatment') )
model4_all_convern_factors <- lm(total_paid.EUR ~ test_status + instant_booking +
                                   user_verified, data = train_all_convern.lmvar)


# add model diagnostics to our training data
model4_results <- augment(model4_all_convern_factors, train_all_convern.lmvar)
#View(model4_results)

# ggplot(data = model4_results,mapping = aes(x = .fitted, y = .resid)) + 
#   geom_ref_line(h = 0) +
#   geom_point() +
#   geom_smooth(se = FALSE) +
#   ggtitle("Residuals vs Fitted")


qq_plot <- qqnorm(model4_results$.resid)
qq_plot <- qqline(model4_results$.resid)

#tidy(model4_all_convern_factors)
#summary(model4_all_convern_factors)

booking_sales.predictioplot <- ggcoefstats(
  x = stats::lm(formula = total_paid.EUR ~ test_status + instant_booking +
                  user_verified, data = train_all_convern.lmvar),
  ggtheme = ggplot2::theme_gray(), # changing the default theme
  title = "Regression analysis: Predicting the influence of various factors on booking sales",
) 

#booking_sales.predictioplot

#####  Model accuracy
## R squared
#rsquare(model4_all_convern_factors, data = test_all_convern.lmvar)
r2_model4 <- rsquare(model4_all_convern_factors, data = train_all_convern.lmvar) ### This suggests that predictor
## variables used can explain only 4.2% of variability in the data.

### Residual Standard Error
sigma(model4_all_convern_factors)  ## estimate RSE

### 
# plot(model4_all_convern_factors)
# plot(model4_all_convern_factors, which = 4)
# plot(model4_all_convern_factors, which = 1)
# plot(model4_all_convern_factors, which = 2)
# plot(model4_all_convern_factors, which = 3)
# plot(model4_all_convern_factors, which = 5)

#### Make predictions
test_all_convern.lmvar%>%
  add_predictions(model = model4_all_convern_factors)%>%
  summarize(MSE = mean((total_paid.EUR - pred)^2))

train_all_convern.lmvar%>%
  add_predictions(model = model4_all_convern_factors)%>%
  summarize(MSE = mean((total_paid.EUR - pred)^2))
broom::glance(model4_all_convern_factors)

var_influeunce<- caret::varImp(model4_all_convern_factors)

sale_prediction <- test_all_convern.lmvar%>%
  add_predictions(model = model4_all_convern_factors)%>%
  as.data.frame() #%>%
  #View()
