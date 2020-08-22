setwd('')

###############################
###### Package Loads ##########
#------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(caret)
library(randomForest)
library(ggpubr)

#
#
###################################
## Loading the dataset and Model ##
#------------------------------------------------------------------
df <- read.csv('dados_func.csv')

# Changing column names to english
colnames(df) <- c('satisfaction_level','last_evaluation', 'number_projects','avg_hours_month',
                  'employed_years','work_accident', 'left_company', 'last_promotion_5years',
                  'area','salary')

# Load the Predictive Model
model_turnover <- readRDS("model_turnover.rds")

#
#
###############################
####### User Interface ########
#------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("cosmo"),
                # This is the panel with all the tabs on top of the pages
                navbarPage(
                  theme = 'cosmo',
                  'HR Analytics - Predicting Turnover',
                  
                  
                  # Tab About
                  tabPanel("About",
                           mainPanel(fluidRow(
                             h3('The Dataset'),
                             p('Project developed with a dataset provided by the Data Science Academy school from Brazil.
                                      The dataset has 14.999 observations and 10 variables. The variables are: 1. satisfaction_level, 2. last_evaluation, 3. number_projects,
                                      4. avg_hours_month,  5. employed_years, 6. work_accident, 7. left_company, 8. last_promotion_5years, 9. area and 10. salary.
                                This project is part of the Data Scientist Formation Program.'),
                             
                             h3('Author'),
                             p('My name is', strong('Gustavo R Santos'), '. I am a Data Analyst | Data Scientist and work with R, Python, MS Excel and SQL to extract insights from data, 
                                      helping companies to make better decisions.'),
                             p('See more projects in my GitHub page:', a('GitHub Portfolio',href='https://bit.ly/3j43yOM')),
                             
                             h3('The Project'),
                             p('In this project, I have worked with HR Analystics, creating a classification predictor that returns to a HR manager
                                      what is the probability of a person to leave the company.'),
                             p('The work was divided in 2 parts:'),
                             p('1. Exploring the dataset to understand the data distribution and creating some visualizations to extract the best business insights from it.
                                      The result of the Part 1 can be found in the tabs', strong('REPORT and PLOTS.')),
                             p('2. The Part 2 part was to create a Classification Interactive Tool using Shiny App that allows the user to input some values and have a prediction
                                      whether the employee will or will not leave the company, as well as the probability of each option to happen.
                                      You can interact with the tool in the tab ', strong('PREDICTIONS'), '. The accuracy is 98%.')
                             
                             ) # fluidRow-About
                             ) # mainPanel-About
                           ), #tabPanel-About
                  
                  # This is the tab panel for REPORT
                  tabPanel("REPORT",
                           mainPanel(
                             h4('Descriptive Statistics'),
                             p('The initial exploration of the dataset was mainly about descriptive statistics. It was observed 14999 rows and 10 columns,
                               being 5 quantitative variables (satisfaction_level, last_evaluation, number_projects, avg_hours_month, employed_years) and 
                               5 categorical variables (work_accident, left_company, last_promotion_5years, area, salary)'),
                             p('From the stats summary, other insights were extracted:'),
                             p('* Most people allocated in the Sales department, followed by Technical and Support teams.', 
                             div('* Satisfaction level average is above 6 out of 10, however the mean dropped since the last evaulation.'),
                             div('* People work with an average of 4 projects at a time.'),
                             div('* The average of hours worked per month is approx. 200, what gives us around 50hs per week, which is high, if considered a journey of 40hs/week.'),
                             div('* A person works in that company for an average of 3.5 to 4 years.'),
                             div('* There is more people with low salaries and very few with high.'),
                             div('* Current turnover rate: 24% of the people leave the company.')),
                             br(), #line break = blank line
                             h4('Exploratory Analysis'),
                             h5('The impact of the satisfaction level'),
                             p('The histograms show us that the satisfaction within the company is a little bit more to the 
                               good half (50%+) than to the bad half. In the analysis, it was observed that this number is 
                               homogeneous within the company, as all the areas have similar satisfaction level mean.'),
                             p('There is a negative correlation of 39% between the satisfaction level and leaving the company. 
                             Although we know correlation does not mean cause, we know that those variables are related.
                             Analyzing deeper, it was learned that the satisfaction level only goes down each year an employee 
                               is in that company - possibly an opportunity for the HR department to act with motivation booster campaigns. 
                               Usually around the 3rd and 4th years of employment is when most of the employees decide to leave.'),
                             p('For this company, the money variable plays a strong part, because people with lower salaries are those 
                               less motivated and those who more leave the company (30%).'),
                             br(),
                             h5('Work & Life balance'),
                             p('There is an increasing preoccupation regarding the balance between working hours and leisure hours. 
                               Both are very important for one’s health and the data also reflects it.'),
                             p('The analysis showed us that HR should allocate between 3 and 5 project per person, being those 
                               numbers where it was noted the highest satisfaction levels. People with 2 projects, on the other hand, 
                               are less motivated - possibly because they feel undervalued - and are those who more leave the company.'),
                             p('Now, regarding worked hours per month, taking as base a week with 40 hours of labor journey (160 hs/week), 
                               numbers like 200 to 250 hours should be considered high. But ‘even though it is not clear the relationship 
                               between hours worked and satisfaction level, the trend line shows that for high numbers like 250+ hours the
                               satisfaction level drops quickly as well as the chance of that person to leave the company.'),
                             br(),
                             h5('The impact of the Promotions'),
                             p('To promote an employee is more than giving him or her more money. 
                               It is also about recognizing that person’s competence and potential.'),
                             p(' It becomes visible that there is an impact from the promotions in the company’s turnover.
                              From those who were promoted within the last 5 years, only 5% left. When the group not promoted is analyzed, 
                               that number increases to 24%.'),
                             p('Furthermore, there is a low number of employees that stay longer than 5 years in the company.'),
                             p('Once again, another pain point to be worked and developed by the HR dept. This company needs clear career
                              paths and new promotion criteria.'),
                             br(),
                             h4('Feature Selection'),
                             p('It was created a random forest model with the hyperparameter Variable Importante activated, so we could analyze
                               the most relevant ones to be used for our model.'),
                             img(src="FeatureEngineering.png",  width = "500px", height = "250px"),
                             p('After plotting the importance of the variables for the model, the following variables were chosen:
                              satisfaction_level, number_projects, last_evaluation, avg_hours_month, employed_years'),
                             br(),
                             h4('The Prediction Model'),
                             p('I have created thre models for comparison. The first was a Random Forest without balancing the dataset. The target,
                               variable was distributed 76% for Not Leaving the Company vs. 24% Leaving. This first model had 99% of accuracy.'),
                             p('The second model was another Random Forest, but this time with a balanced dataset. The accuracy slightly dropped
                               to 98%, but that was the chosen model as it has better chances to learn equaly about both labels it is predicting.'),
                             p('The last one was a model with the XGBoost algorithm, what presented 97% of accuracy.')
                             
                             
                           ) # mainPanel-Plots
                           ), # tabPanel-Report

                  # This is the tab panel for PLOTS
                  tabPanel("PLOTS",
                           sidebarPanel(
                             selectInput('graphic', h5('Select a Graphic to Display'),
                                         choices = c('Histograms', 'Barplots', 
                                                     'Satisfaction vs Left',
                                                     'Number Projects vs Left',
                                                     'Hours Worked vs Left',
                                                     'Area vs Left',
                                                     'Employed Years vs Left',
                                                     'Promotion vs. Left',
                                                     'Salary vs Left'),
                                         selected = )
                           ), #sidebarPanel 
                           mainPanel(
                             plotOutput(outputId = 'plots')
                           ) # mainPanel-Plots
                  ), # tabPanel-Plots                           
                           
                  # This is the tab panel for PREDICTIONS
                  tabPanel("PREDICTIONS",h3("HR Turnover Prediction Tool"),
                           fluidRow(column(10, p('Interact with the sliders to get some predictions from a Random Forest model
                      trained and validated with * 98% * of accuracy. Choose any values on the sliders below and see 
                      what is the prediction of the model regarding the probability of an employee to leave the company.'),
                                           p(strong('0 means NOT LEAVE the company | 1 means LEAVE the company.')),
                                           br())),
                           sidebarPanel(
                             sliderInput("slidersat", 'Satisfaction Level',
                                         min = min(df$satisfaction_level), max = max(df$satisfaction_level), value = 0.5),
                             sliderInput("slidereval", "Last Evaluation",
                                         min = min(df$last_evaluation), max = max(df$last_evaluation), value = 0.65),
                             sliderInput("sliderproj", 'Number of Projects',
                                         min = min(df$number_projects), max = max(df$number_projects), value = 3, step = 1),
                             sliderInput("sliderhours", "Hours Worked by Month",
                                         min = min(df$avg_hours_month), max = max(df$avg_hours_month), value = 155),
                             sliderInput("slideryears", 'Years in the Company',
                                         min = min(df$employed_years), max = max(df$employed_years), value = 3, step = 1)
                           ), #sidebar-predictions
                           mainPanel(
                             h3('Your Choices'),
                             tableOutput('showchoices'),
                             h3(tableOutput('prediction'))
                           ) # mainPanel-Predictions
                  ) # tabPanel-Predictions
                  
                  
                ) # navbarPage
  
) #MainfluidPage-close





#
#
#############################
########## Server ###########
#------------------------------------------------------------------

server <- function(input, output) {
  
  #Code for the Histograms and Barplots
  output$plots <- renderPlot(
    if (input$graphic == 'Histograms') {
    # Setup plotting area 2 rows of 4
    par(mfrow = c(2,3))
    # Function to create the 5 histograms for quantitative variables
    for (col in c(1,2,3,4,5)) {
      hist(df[,col], main = paste('Histogram of',colnames(df[col])),
           xlab = colnames(df[col]), col = 'skyblue2', prob=T)
      lines(density(df[,col]), col = 'blue', lwd = 2)} #for loop
      } #if
    else if (input$graphic == 'Barplots') {
      par(mfrow = c(2,3))
      barplot(table(df$work_accident), main = 'Barplot of Work Accident',
              xlab= 'work_accident', ylab = 'Count', col = c('blue3','tomato'))
      
      barplot(table(df$left_company), main = 'Barplot of Left Company',
              xlab= 'Left_Company', ylab = 'Count', col = c('blue3','tomato'))

      barplot(table(df$last_promotion_5years), main = 'Barplot of Last Promotion in 5 Years',
              xlab= 'last_promotion_5years', ylab = 'Count', col = c('blue3','tomato'))
      
      barplot(table(df$salary), main = 'Barplot of Salary',
              xlab= 'Salary', ylab = 'Count', col = c('blue3','tomato'))
      
      barplot(table(df$area), horiz = T, main = 'Barplot of Area',col = c('blue3'), las=2,cex.lab=0.5)
    } #else if 1
    
    else if (input$graphic == 'Satisfaction vs Left') {
      # Satisfaction level by Left Company
      ggplot(data=df, aes(x=satisfaction_level,y=left_company)) +
        geom_point() + geom_smooth(method='lm') +
        labs(title = 'Satisfaction Level by Left Company')
    } #else if 2
    
    else if (input$graphic == 'Number Projects vs Left') {
      # Satisfaction level by Number of Projects
      g1=ggplot(data=df, aes(x=as.factor(number_projects),y=satisfaction_level)) +
        geom_boxplot(aes(group=number_projects), fill='coral2') +
        labs(title = 'Satisfaction Level by Number of Projects')
      
      # Left_Company by Number of Projects
      g2 = ggplot(data=df, aes(x=as.factor(number_projects),y=left_company)) +
        geom_col(fill='coral2') +
        labs(title = 'Letf the Company by Number of Projects')
      ggarrange(g1, g2, ncol = 1, nrow = 2)
    } #else if 3
    
    else if (input$graphic == 'Hours Worked vs Left') {
      # Satisfaction level by avg hours worked
      g1= ggplot(data=df, aes(x=avg_hours_month,y=satisfaction_level)) +
        geom_point(fill='coral2') + geom_smooth() +
        labs(title = 'Satisfaction Level by Hours Worked in a month')
      
      # Left_Company by avg_hours
      g2 = ggplot(data=df, aes(x=avg_hours_month, y=left_company)) +
        geom_point(fill='coral2') + geom_smooth()
      labs(title = 'Letf the Company by Avg Hours Worked')
      ggarrange(g1, g2, ncol = 1, nrow = 2)
    } #else if 4
    
    else if (input$graphic == 'Area vs Left') {
      # Satisfaction level by area
      g1= ggplot(data=df, aes(x=area,y=satisfaction_level)) +
        geom_boxplot(aes(group=area), fill='coral2') +
        labs(title = 'Satisfaction Level by Area')
      
      # Left_Company by area
      g2= ggplot(data=df, aes(x=area,y=left_company)) +
        geom_col(fill='coral2') +
        labs(title = 'Letf the Company by Area')
      ggarrange(g1, g2, ncol = 1, nrow = 2)
    } #else if 5
    
    else if (input$graphic == 'Employed Years vs Left') {
      # Satisfaction level by employed years
      g1 = ggplot(data=df, aes(x=as.factor(employed_years),y=satisfaction_level)) +
        geom_boxplot(aes(group=employed_years), fill='coral2') +
        labs(title = 'Satisfaction Level by Years in the Company')
      
      # Left_Company by employed years
      g2 = ggplot(data=df, aes(x=as.factor(employed_years),y=left_company)) +
        geom_col(fill='coral2') +
        labs(title = 'Letf the Company by Years Employed')
      ggarrange(g1, g2, ncol = 1, nrow = 2)
    } #else if 6
    
    else if (input$graphic == 'Promotion vs. Left') {
      # Left_Company by last promotion 5 years
      ggplot(data=df, aes(x=as.factor(last_promotion_5years),y=left_company)) +
        geom_col(fill='coral2') +
        labs(title = 'Letf the Company by Promotion in the last 5 years ')
    } #else if 7
    
    else if (input$graphic == 'Salary vs Left') {
      # Satisfaction level by Salary
      g1 = ggplot(data=df, aes(x=as.factor(salary),y=satisfaction_level)) +
        geom_boxplot(aes(group=salary), fill='coral2') +
        labs(title = 'Satisfaction Level by Salary')
      
      # Left_Company by Salary
      g2 = ggplot(data=df, aes(x=as.factor(salary),y=left_company)) +
        geom_col(fill='coral2') +
        labs(title = 'Letf the Company by Salary')
      ggarrange(g1, g2, ncol = 1, nrow = 2)
    } #else
  ) #renderPlot
  
  # Piece of code for the User Choices Table
  output$showchoices <- renderTable({
    # Create DF
    choices <- data.frame(Sat_Level=input$slidersat,
                          Last_eval=input$slidereval,
                          Num_Projs=input$sliderproj, 
                          Hours_month=input$sliderhours,
                          Years_inComp=input$slideryears)
    
    # Show Table
    choices
    
  }) #output showcoices
  
  # Piece of code for the Prediction
  output$prediction <- renderTable({
    # Create DF
    datapred <- data.frame(satisfaction_level=input$slidersat, last_evaluation=input$slidereval,
                           number_projects=input$sliderproj,avg_hours_month=input$sliderhours,
                           employed_years= input$slideryears)
    
    # Predict
    prediction <- predict(model_turnover,datapred, type='prob')
    data.frame('Probability' = prediction)
    
  }) #output prediction
  
} #function



#
#
#
#
##############################
######### Shiny App ##########
#------------------------------------------------------------------

shinyApp(ui = ui, server = server)
