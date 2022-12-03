library(tidyverse)
library(ggplot2)
library(splitstackshape)
library(ggmap)
library(maps)
library(mapproj)
library(kernlab)
library(caret)
library(DataExplorer)
library(glmnet)
library(caTools)
library(e1071)
library(shiny)
library(shinythemes)
library(DT)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  theme = "cerulean", 
                  "HMO Data Analytics",
                  tabPanel("Upload File",
                           sidebarPanel(
                             tags$h3("Input Cost Test File:"),
                             fileInput(inputId = "ABC", label = "Input File", multiple = FALSE, accept = NULL,
                                       width = NULL, buttonLabel = "Browse...",
                                       placeholder = "No file selected"),
                      
                             actionButton(inputId = "submit", label = "Submit"),
                           ),
                           mainPanel(
                             ####displaying test file on upload tab
                             dataTableOutput("test_input_display")
                           )),
                  tabPanel("Visualization",
                           mainPanel(
                             ##Count Summary
                             tags$h4("Count Summary"),
                             plotOutput("plot_summary"),
                             br(),br(),br(),
                             
                             #Missing values attributes
                             tags$h4("Missing values attributes"),
                             plotOutput("missing_plots"),
                             br(),br(),br(),
                             
                             #histograms
                             tags$h4("Histogram plots for each attribute"),
                             plotOutput("hist_plots"),
                             br(),br(),br(),
                             
                             #Scatterplot
                             tags$h4("Scatterplots: Cost vs dependent variables"),
                             plotOutput("scatter_plots"),
                             br(),br(),br(),
                             
                             ###Correlation Matrix
                             tags$h4("Correlation Heatmap"),
                             plotOutput("correlation"),
                             br(),br(),br(),
                             
                             ###Map plotting
                             tags$h5("Map"),
                             plotOutput("mapOut")
                             
                           )),
                  tabPanel("Regression", 
                           dataTableOutput("regression_output_tbl"),
                           br(),br(),br(),
                           verbatimTextOutput("reg_summary")
                  ),
                  tabPanel("Classification", 
                             tabsetPanel(
                               tabPanel("Predicted Test Output",dataTableOutput("class_output_tbl")),
                               tabPanel("Performance Analysis",
                                        
                                        #Confusion Matrix
                                        tags$h4("Confusion Matrix"),
                                        plotOutput("cm_matrix"),
                                        br(),br(),br(),
                                        
                                        #Performance Metrics
                                        tags$h4("Performance Metrics"),
                                        tableOutput("class_metrics")
                                        ),
                               #tabPanel("Performance Metrics",tableOutput("class_metrics"))
                             ))
                
                
)) # fluidPage

# Define server function  
server <- function(input, output) {
  observeEvent( input$submit, {
    
    #####datatable for test file for display  
    data_test_input <- read.csv(input$ABC$datapath, header = TRUE, sep = ",")
    output$test_input_display <- renderDataTable(datatable(data_test_input),options = list(autoWidth = FALSE))
    ###testfile for regression
    data_test_reg_input <- read.csv(input$ABC$datapath, header = TRUE, sep = ",")
    ###testfile for classification
    data_test_class_input<-read.csv(input$ABC$datapath, header = TRUE, sep = ",")
    
    
    ###reading train file HMO data
    data_shiny_input<- read_csv('https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv')
    data_shiny_input<- as.data.frame(unclass(data_shiny_input),stringsAsFactors = TRUE)
    
    ###########---EDA COD-----E###############
    
    #####DATA_PREPROCESSING FOR TRAIN DATA
    data_shiny_eda<- data_shiny_input
    
    ##replacing bmi NA values with mean value
    mean_bmi <- mean(data_shiny_eda$bmi,na.rm = TRUE)
    data_shiny_eda[,"bmi"][is.na(data_shiny_eda[,"bmi"])] <- mean_bmi
    #replacing hypertension NA values with 0
    data_shiny_eda[,"hypertension"][is.na(data_shiny_eda[,"hypertension"])] <- 0
    
    # data_shiny_eda %>% 
    #  filter(!is.na(hypertension) | !is.na(hypertension))
    
    data_shiny_eda$smoker<- ifelse(data_shiny_eda$smoker == "yes",1,0)
    data_shiny_eda$exercise<- ifelse(data_shiny_eda$exercise == "Active",1,0)
    data_shiny_eda$location_type<- ifelse(data_shiny_eda$location_type == "Urban",1,0)
    data_shiny_eda$yearly_physical<- ifelse(data_shiny_eda$yearly_physical == "Yes",1,0)
    data_shiny_eda$married<- ifelse(data_shiny_eda$married == "Married",1,0)
    data_shiny_eda$gender<- ifelse(data_shiny_eda$gender== "Male",1,0)
    
    ####Feature selection for train data for regression model
    data_train_regression<- data_shiny_eda[,c('age','bmi','children','smoker','exercise','hypertension','yearly_physical','cost')]
    
    ###VISUALIZATION FOR TRAIN DATA
    ##correlation matrix for train data
    output$correlation<- renderPlot({plot_correlation(data_train_regression)})
    ##histogram for train data
    output$hist_plots<- renderPlot(plot_histogram(data_shiny_input%>% select(2:14)))
    #scatterplot
    output$scatter_plots<- renderPlot(plot_scatterplot(data_train_regression,by="cost"))
    ##plotting missing for train data
    output$missing_plots<- renderPlot(plot_missing(data_shiny_input))
    ##plotting count summary for train data
    output$plot_summary<- renderPlot(plot_intro(data_shiny_input))
    
    
    df_map <- data_shiny_input %>% group_by(location) %>% summarise(mean(cost))
    
    us <- map_data("state")
    us$state_name <- tolower(us$region)
    coord_df2 <- data.frame(loc=tolower(df_map$location),avg_cost=df_map$`mean(cost)`)
    us_with_coords2 <- merge(us,coord_df2, by.x='state_name',by.y='loc',all.x=TRUE, all.y = TRUE)
    us_with_coords2 <- us_with_coords2 %>% arrange(order)
    
    Mymap2 <- ggplot(us_with_coords2,aes(map_id= region)) + geom_polygon(color="black",aes(x=long,y=lat,group=group,fill=avg_cost))  +
      expand_limits(x=us_with_coords2$long, y=us_with_coords2$lat)+coord_map("mercator")+
      ggtitle("USA MAP")
    
    output$mapOut<- renderPlot({Mymap2})
    
    
    
    #######---------REGRESSION MODEL--###########
    
    ###Multiple Linear Regression on train data
    lmOut<- lm(cost~smoker+age+hypertension+bmi+exercise,data=data_train_regression)
    #summary(lmOut2)
    
    ####Data preprocessing for Test data
    ####DATA PREPROCESSING FOR TEST DATA
    data_test_reg<- data_test_reg_input
    data_test_reg<- as.data.frame(unclass(data_test_reg),stringsAsFactors = TRUE)
    data_test_reg_bkp<- data_test_reg
    data_test_reg_bkp_output<-  data_test_reg_bkp
    mean_bmi_test <- mean(data_test_reg$bmi,na.rm = TRUE)
    data_test_reg[,"bmi"][is.na(data_test_reg[,"bmi"])] <- mean_bmi_test
    data_test_reg[,"hypertension"][is.na(data_test_reg[,"hypertension"])] <- 0
    data_test_reg$smoker<- ifelse(data_test_reg$smoker == "yes",1,0)
    data_test_reg$exercise<- ifelse(data_test_reg$exercise == "Active",1,0)
    data_test_reg$location_type<- ifelse(data_test_reg$location_type == "Urban",1,0)
    data_test_reg$yearly_physical<- ifelse(data_test_reg$yearly_physical == "Yes",1,0)
    data_test_reg$married<- ifelse(data_test_reg$married == "Married",1,0)
    data_test_reg$gender<- ifelse(data_test_reg$gender== "Male",1,0)
    data_test_reg<- data_test_reg[,c('age','bmi','smoker','yearly_physical','exercise','hypertension')]
    
    ####predicting cost on test data using regression
    pred_test_cost <-predict(lmOut, new = data_test_reg)
    data_test_reg_output<- data_test_reg_input
    data_test_reg_output$predicted_cost<- round(pred_test_cost,2)
    output$regression_output_tbl <- DT::renderDataTable(datatable(data_test_reg_output),options = list(autoWidth = TRUE))
    output$reg_summary <- renderPrint({summary(lmOut)})
    
    #-------!!!!!! Need to draw boxplot and remove outliers----~~
    
    #boxplot(data_classification%>% select (2:5,10,12,14))
    
    
    ###########----CLASSIFICATION MODEL-----###############
    
    ####Data preprocessing for classification
    data_classification_input <- data_shiny_eda
    
    
    
    
    #### Considering cost above 75 % quantile as expensive
    cost_filter<- quantile(data_classification_input$cost,na.rm = T,probs = c(0.75))
    data_classification_input$expensive<- ifelse(data_classification_input$cost>cost_filter ,1,0)
    
    ####Feature selection for input data for classification model
    data_classification_input<- data_classification_input[,c('age','bmi','children','smoker','exercise','hypertension','expensive')]
    #View(data_classification_input)
    
    
    ###splitting dataset:
    # Create train and test data sets
    set.seed(321)
    
    #use 80% of dataset as training set and 20% as test set 
    intrain <- createDataPartition(y = data_classification_input$expensive, p= 0.8, list = FALSE)
    training <- data_classification_input[intrain,]
    testing <- data_classification_input[-intrain,]
    
    ###Running SVM Classification model:
    training[['expensive']] = factor(training[['expensive']])
    svm<-ksvm(expensive ~ ., data=training, kernel= "rbfdot", kpar = "automatic",
              C = 5, cross = 3, prob.model = TRUE)
    svmPred<- predict(svm,testing)
    
    
    ####Classification for test sample data:
    
    data_test_class<- data_test_class_input
    data_test_class<- as.data.frame(unclass(data_test_class),stringsAsFactors = TRUE)
    #data_test_class<- data_test_reg
    #data_test_reg_bkp_output<-  data_test_reg_bkp
    mean_bmi_test <- mean(data_test_class$bmi,na.rm = TRUE)
    data_test_class[,"bmi"][is.na(data_test_class[,"bmi"])] <- mean_bmi_test
    data_test_class[,"hypertension"][is.na(data_test_class[,"hypertension"])] <- 0
    data_test_class$smoker<- ifelse(data_test_class$smoker == "yes",1,0)
    data_test_class$exercise<- ifelse(data_test_class$exercise == "Active",1,0)
    data_test_class$location_type<- ifelse(data_test_class$location_type == "Urban",1,0)
    data_test_class$yearly_physical<- ifelse(data_test_class$yearly_physical == "Yes",1,0)
    data_test_class$married<- ifelse(data_test_class$married == "Married",1,0)
    data_test_class$gender<- ifelse(data_test_class$gender== "Male",1,0)
    data_test_class<- data_test_class[,c('age','bmi','children','smoker','exercise','hypertension')]
    
    #test prediction 
    svmPred_test<- predict(svm,data_test_class)
    data_test_class_output<- data_test_class_input
    data_test_class_output$predicted_expensive<- svmPred_test
    data_test_class_output$predicted_expensive<- ifelse(data_test_class_output$predicted_expensive == 1,"expensive","not expensive")
    output$class_output_tbl<- DT::renderDataTable(datatable(data_test_class_output),options = list(autoWidth = TRUE))
    
    
    
    ######performance metrics for train data:
    #confuston matrix
    confusion_tbl<- table(testing$expensive,svmPred)
    confusion_tbl
    
    TP= confusion_tbl[1,1] #True Negative
    FP = confusion_tbl[1,2] #False Negative
    FN = confusion_tbl[2,1] #False Positive
    TN = confusion_tbl[2,2] #True Positive
    
    ##Accuracy:
    acc<-  (TP+TN)/(TP+TN+FP+FN)
    
    #Precision:
    prec<- (TP)/(TP+FP)
    
    #Sensitivity:
    sens<- (TP)/(TP+FN)
    
    #Recall:
    rec<- TP/(TP+FN)
    #F1-score:
    f1<- (2*prec*sens)/(prec+sens)
    
    output$class_metrics<- renderTable({df <- data.frame(Sensitivity  = sens,Accuracy = acc,Precision = prec,F1_Score = f1)})
    
    output$cm_matrix<- renderPlot(
      fourfoldplot(confusion_tbl, color = c("blue", "light green"),
                   conf.level = 0, margin = 1, main = "Confusion Matrix"))
  })}



# Create Shiny object
shinyApp(ui = ui, server = server)