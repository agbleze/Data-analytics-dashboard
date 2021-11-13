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
rcsp_ref_session <- rcsp_ref%>%
  dplyr::select(session_id)%>%
  na.omit()%>%
  count(session_id)
count(rcsp_ref_session) ## 9468 sessions for rcsp_ref /  9467 when NA removed

rcsp_ref_conversion_rate <- (count(rcsp_ref_conversion)/count(rcsp_ref_session)) * 100
rcsp_ref_conversion_rate ## 0.2% conversion rate for rcsp_ref

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
# k2_visitor <- kmeans(visitor_pageviews_sessions_num, centers = 2, nstart = 25)
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
#  k3_visitor <- kmeans(visitor_pageviews_sessions_num, centers = 3, nstart = 25) ## kmeans with 3 clusters
#  k4_visitor <- kmeans(visitor_pageviews_sessions_num, centers = 4, nstart = 25) ## kmeans with 4 clusters
#  k5_visitor <- kmeans(visitor_pageviews_sessions_num, centers = 5, nstart = 25) ## kmeans with 5 clusters
# # 
# # plots to compare
#  p1 <- fviz_cluster(k2_visitor, geom = "point", data = visitor_pageviews_sessions_num) + ggtitle("k = 2")
#  p2 <- fviz_cluster(k3_visitor, geom = "point", data = visitor_pageviews_sessions_num) + ggtitle("k = 3")
#  p3 <- fviz_cluster(k4_visitor, geom = "point", data = visitor_pageviews_sessions_num) + ggtitle("k = 4")
#  p4 <- fviz_cluster(k5_visitor, geom = "point", data = visitor_pageviews_sessions_num) + ggtitle("k = 5")
# # 
# ############# plot all graphs together  #######
 # grid.arrange(p1, p2, p3, p4, nrow = 2)








