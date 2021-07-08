
library("tidyverse")
library("patchwork")
library("plotly")
library("shiny")
library("rsconnect")
library("shinythemes")
library("markdown")
library("reshape2")

# IMPORT DATA

heart_data <- read_csv('data/heart.csv')

heart_data <- as.data.frame(heart_data)

#Renaming columns.
data_col_names <- c('Age', 'Sex', 'Chest Pain Type', 'Resting Blood Pressure', 'Cholesterol', 'Fasting Blood Sugar', 'Resting ECG', 'Max. Heart Rate',
                    'Exercise Induced Angina', 'Previous Peak', 'Slope', 'No. Major Blood Vessels', 'Thal Rate', 'Condition')
colnames(heart_data) <- data_col_names

# Select numerical and categorical data
Numerical <- heart_data %>% select('Age','Resting Blood Pressure','Cholesterol','Max. Heart Rate','Previous Peak')
Categorical <- heart_data %>% select('Sex','Chest Pain Type','Fasting Blood Sugar','Resting ECG','Exercise Induced Angina','Slope','No. Major Blood Vessels','Thal Rate')

# separate x and y of the dataset
Samples <- heart_data %>% select(!Condition)
Labels <- heart_data %>% select(Condition)

#plot the correlation_matrix
Correlation_matrix <- cor(heart_data) %>% round(3)


get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
}
upper_tri <- get_upper_tri(Correlation_matrix)
melted_cormat <- melt(upper_tri,na.rm = TRUE)


Correlation_matrix_plot <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 8, hjust = 1))+
    
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
#From the above correlation matrix, we can see that the correlation between features is less.
#Chest Pain Type with Condition and Max. Heart Rate with Condition have high correlated features in our dataset; Correlation Coefficient of 0.43 and 0.42 respectively.
#Our features have a lot of negative correlation coefficient indicating that two individual variables have a statistical relationship such that generally move in opposite directions from one another.
##==============================================================================
# create categorical plots with condition
heart_data_copy <- data.frame(heart_data)
colnames(heart_data_copy) <- data_col_names



heart_data_copy$Slope <- as.factor(heart_data_copy$Slope)
heart_data_copy$`No. Major Blood Vessels`<- as.factor(heart_data_copy$`No. Major Blood Vessels`)
heart_data_copy$`Thal Rate`<- as.factor(heart_data_copy$`Thal Rate`)


heart_data_copy$Condition <-factor(heart_data_copy$Condition,
                                levels = c(0,1),
                                labels = c("less chance of heart attack","more chance of heart attack"))

heart_data_copy$Sex <- factor(heart_data_copy$Sex,
                         levels = c(0,1),
                         labels = c("female","male"))
heart_data_copy$`Chest Pain Type` <- factor(heart_data_copy$`Chest Pain Type`,
                                       levels =c(0,1,2,3),
                                       labels = c("typical angina","atypical angina","non-anginal pain","asymptomatic"))
heart_data_copy$`Fasting Blood Sugar`<-factor(heart_data_copy$`Fasting Blood Sugar`,
                                         levels = c(0,1),
                                         labels = c("false","true"))
heart_data_copy$`Resting ECG`<- factor(heart_data_copy$`Resting ECG`,
                                  levels = c(0,1,2),
                                  labels = c("normal","having ST-T wave abnormality","showing probable or definite left ventricular hypertrophy"))

heart_data_copy$`Exercise Induced Angina`<-factor(heart_data_copy$`Exercise Induced Angina`,
                                             levels = c(0,1),
                                             labels = c("no","yes"))


Sex_plot <- ggplot(heart_data_copy,aes(x=Sex,fill=Condition))+
    geom_bar(position = "dodge")

Chest_plot <- ggplot(heart_data_copy,aes(x=`Chest Pain Type`,fill=Condition))+
    geom_bar(position = "dodge")+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 8, hjust = 1))

Sugar_plot <- ggplot(heart_data_copy,aes(x=`Fasting Blood Sugar`,fill=Condition))+
    geom_bar(position = "dodge")

ECG_plot <- ggplot(heart_data_copy,aes(x=`Resting ECG`,fill=Condition))+
    geom_bar(position = "dodge")+
    theme(axis.text.x = element_text(angle = 30, vjust = 1, 
                                     size = 8, hjust = 1))

Exercise_plot <- ggplot(heart_data_copy,aes(x=`Exercise Induced Angina`,fill=Condition))+
    geom_bar(position = "dodge")

Slope_plot <- ggplot(heart_data_copy,aes(x=Slope,fill=Condition))+
    geom_bar(position = "dodge")

Vessels_plot <- ggplot(heart_data_copy,aes(x=`No. Major Blood Vessels`,fill=Condition))+
    geom_bar(position = "dodge")

Thal_plot <- ggplot(heart_data_copy,aes(x=`Thal Rate`,fill=Condition))+
    geom_bar(position = "dodge")


# create numerical plot with condition

heart_data_copy$Age <- as.numeric(heart_data_copy$Age)
heart_data_copy$`Resting Blood Pressure` <- as.numeric(heart_data_copy$`Resting Blood Pressure`)
heart_data_copy$Cholesterol <- as.numeric(heart_data$Cholesterol)
heart_data_copy$`Max. Heart Rate` <- as.numeric(heart_data_copy$`Max. Heart Rate`)
heart_data_copy$`Previous Peak` <- as.numeric(heart_data_copy$`Previous Peak`)

Age_plot <- ggplot(heart_data_copy,aes(x=Age,fill=Condition))+
    geom_density(alpha=0.3)

Pressure_plot <- ggplot(heart_data_copy,aes(x=`Resting Blood Pressure`,fill=Condition))+
    geom_density(alpha=0.3)

Cholesterol_plot <- ggplot(heart_data_copy,aes(x=Cholesterol,fill=Condition))+
    geom_density(alpha=0.3)

HeartRate_plot <- ggplot(heart_data_copy,aes(x=`Max. Heart Rate`,fill=Condition))+
    geom_density(alpha=0.3)

Peak_plot<-ggplot(heart_data_copy,aes(x=`Previous Peak`,fill=Condition))+
    geom_density(alpha=0.3)



## import trained model 
logistic_model <- load(file = "model/logistic.rda",.GlobalEnv)
# predict(fit_boost, data)

smp_size <- floor(0.8 * nrow(heart_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(heart_data)), size = smp_size)

train <- heart_data[train_ind, ]
test <- heart_data[-train_ind, ]

glm.fit <- glm(Condition ~ Age + Sex + `Chest Pain Type` + `Max. Heart Rate`, data = train, family = binomial)

# heart_data$Condition <-as.factor(heart_data$Condition)
# heart_data$Sex<-as.factor(heart_data$Sex)
# heart_data$`Chest Pain Type`<-as.factor(heart_data$`Chest Pain Type`)
# summary(heart_data)
# set.seed(1)
# sample <- sample(c(TRUE,FALSE),nrow(heart_data),replace=TRUE,prob = c(0.7,0.3))
# train <- heart_data[sample,]
# test <- heart_data[!sample,]
# 
# model <- glm(Condition~Age+Sex+`Chest Pain Type`+`Max. Heart Rate`, family = "binomial",data=train)
# options(scipen = 999)
# summary(model)
# 
# new <- data.frame(Age = 67, Sex = as.factor(1) , `Chest Pain Type` = as.factor(0), `Max. Heart Rate`= 129)
# col_names <- c('Age','Sex','Chest Pain Type','Max. Heart Rate')
# colnames(new) <- col_names
# predict(model,new,type="response")
# 
# predicted <- predict(model,test,type="response")
# predicted
# APP UI

# Design UI for app
ui <- navbarPage("Heart Attack Prediction",
                 
                 tabPanel("Feature Analysis",
                          # App title
                          titlePanel(strong("Feature Analysis")),
                          
                          # Captions for top of app, explains what is going on
                          h4(p("This page is to visualize our dataset, we display the categorical feature count plots and numerical feature density plots")),
                          h5(p("Here we show the relationship between each feature and the chance of suffering heart attack")),
        
                          
                          br(),
                          
                          sidebarLayout(
                              sidebarPanel(
                                  width = 3,
                                  fluidRow(selectInput(
                                      inputId = "Categorical",
                                      label = "Choose one categorical feature to display:",
                                      choices = c(
                                          "Sex"= 1,
                                          "Chest Pain Type"= 2 ,
                                          "Fasting Blood Sugar" = 3,
                                          "Resting Electrocardiographic Results"= 4,
                                          "Exercise Induced Angina" = 5,
                                          "Number of Major Blood Vessels" =6,
                                          "Thal Rate" = 7
                                          
                                      ),
                                      selected = 1)
                                  ),
                                  
                                  fluidRow(selectInput(
                                      inputId = "Numerical",
                                      label = "Choose one numerical feature to display:",
                                      choices = c("Age" = 1,
                                                  "Resting Blood Pressure" =2,
                                                  "Cholesterol" =3,
                                                  "Max. Heart Rate"=4,
                                                  "Previous Peak" =5),
                                      selected = 1
 
 
                                  )
                                  )
                              ),
                              
                              # Display the Plotly plot in the main panel
                              
                              mainPanel(width =9,
                                        tabsetPanel(
                                        tabPanel("Feature Plots",
                                                 fluidRow(plotlyOutput("Categorical_plot",height="300px")),
                                                 fluidRow(plotlyOutput("Numerical_plot",height = "300px"))
                                                 ),
                                        tabPanel("Correlation Matrix",
                                                 fluidRow(plotlyOutput("Correlation_Matrix_plot",height = "600px")))
                                        )         
                                  )
                              )
                 ),
                 
                 tabPanel("Predictions",
                          # App title
                          titlePanel(strong("Prediction")),
                          h4(p("This page is to predict the risk of having heart attack with the information provided")),
                          h5(p("Here we use a logistic regression model trained with the data from the dataset")),
                          br(),
                          sidebarLayout(sidebarPanel(
                                               width = 3,
                                               fluidRow(selectInput(
                                                   inputId = "sex",
                                                   label = "Choose the sex ",
                                                   choices = c(
                                                       "Male"= 1,
                                                       "Female"= 0),
                                                   selected = 1)
                                               ),
                                               fluidRow(numericInput(
                                                    inputId = "age",
                                                    label = "Put in the age",
                                                    value = 20)
                                                    ),
                                               fluidRow(numericInput(
                                                    inputId = "mhr",
                                                    label = "Maximum heart rate",
                                                    value = 20)
                                                       ),
                                               fluidRow(selectInput(
                                                    inputId = "chpt",
                                                    label = "Choose the chest pain type",
                                                    choices = c("typical angina" = 0,
                                                                "atypical angina" = 1,
                                                                "non-anginal pain" = 2,
                                                                "asymptomatic" = 3),
                                                    selected = 1)
                                               )),
                                        mainPanel(width =9,      
                                            img(src = 'heart-attack-anatomy.jpg'),
                                            h1(strong("The predicted possibility of heart attack risk for the data input is: "),
                                               style = "font-size:21px;"
                                               ),
                                            textOutput("predicted")
                                        )
                          )        
                 )
)

# ==============================================================================
# APP SERVER
# Create R code for app functions
server <- function(input, output) {
    #    Create reactive Plotly plot for app
    library(caret)
    
    output$Categorical_plot <- renderPlotly({
        if(input$Categorical==1){
            Target_plot=Sex_plot
        }else if(input$Categorical==2){
            Target_plot=Chest_plot
        }else if(input$Categorical==3){
            Target_plot=Sugar_plot
        }else if(input$Categorical==4){
            Target_plot=ECG_plot
        }else if(input$Categorical==5){
            Target_plot=Exercise_plot
        }else if(input$Categorical==6){
            Target_plot=Vessels_plot
        }else if(input$Categorical==7){
            Target_plot=Thal_plot
        }
        plotly_build(Target_plot)
    })
    output$Numerical_plot <- renderPlotly({
        if(input$Numerical==1){
            Num_plot=Age_plot
        }else if(input$Numerical==2){
            Num_plot=Pressure_plot
        }else if(input$Numerical==3){
            Num_plot=Cholesterol_plot
        }else if(input$Numerical==4){
            Num_plot=HeartRate_plot
        }else if(input$Numerical==5){
            Num_plot=Peak_plot
        }
        plotly_build(Num_plot)
    })
    
    output$Correlation_Matrix_plot <- renderPlotly({
        plotly_build(Correlation_matrix_plot)
    })
    
    # new_features <- reactive({
    #     # this is how you fetch the input variables from ui component
    #     Var1 <- as.numeric(input$sex)
    #     Var2 <- as.numeric(input$age)
    #     Var3 <- as.numeric(input$mhr)
    #     Var4 <- as.numeric(input$chpt)
    #     new_features <- cbind(Var1, Var2, Var3, Var4)
    #     new_features <- as.data.frame(new_features)
    #     new_f_col_names <- c('Age', 'Sex', 'Chest Pain Type', 'Max. Heart Rate')
    #     colnames(new_features) <- new_f_col_names
    #     new_features 
    #     # Model action button
    # })
    output$predicted = renderText({
        # this is how you fetch the input variables from ui component
        Var1 <- as.numeric(input$age)
        Var2 <- as.numeric(input$sex)
        Var3 <- as.numeric(input$chpt)
        Var4 <- as.numeric(input$mhr)
        coeffs = glm.fit$coefficients;
        p = coeffs[1] + coeffs[2] * Var1 + coeffs[3] * Var2 + coeffs[4] * Var3 + coeffs[5] * Var4; 
        # Model action button
        p = as.numeric(p);
        predicted = exp(p)/(1+exp(p))
        predicted
        })
}
# ==============================================================================
# BUILD APP

# Knit UI and Server to create app
shinyApp(ui = ui, server = server)