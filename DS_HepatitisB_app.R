
library(readr)
library(dplyr)
library(tidyverse)
library(pROC)
library(regclass)
library(shiny)
library(caret)
library(ggplot2)
library(MASS)
library(randomForest)
library(DT) # for visualizing RShiny

library(glmtoolbox)
library(lmtest)

# 1. Make Age a categorical variables because it isn't linear
# 2. Rename Variables to be user friendly
# 3. Change app appearance to appear appealing
# 4. Give context of the app
# 5. Don't make LBXHBS an input
# rownames(bag.tree$importance)[1:4]  

# setwd("~/Villanova 2025/Data Science/Project/RShinyProjectApp_RD1")

subdat1 <- read_csv("subdat.csv")
# View(subdat)

subdat1$LBXHBS <- subdat1$LBXHBS - 1

subdat1$Age_Categorical <- cut(subdat1$Age, 
                    breaks = c(0, 20, 30, 40, 50, 60, 70, 80, Inf), 
                    labels = c("0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"), right= FALSE)

subdat1 <- subdat1 |>
  rename(
    Anti_HBs = LBXHBS,
    Hepatitis_B_Diagnosis = Told_to_have_Hepatitis_B,
    Phlebotomy = Phlebotomy_2_Year_Weight,
    Race = Race_and_Hispanic_Origin,
    Military = Military_Background,
    Married = Marital_Status,
    Poor_Liver_Condition = Liver_Condition ,
    Poor_Appetite = Poor_appetite_or_overeating ,
    Fatigue = Tired_or_little_energy ,
    Weight_kg = Weight_kg
  )

# subdat1 <- subdat1 |> relocate(Age_Categorical, .after = Age)
subdat1 <- dplyr::select(subdat1, sort(names(subdat1)))
subdat <- subdat1


# Relevel categorical variables ####################################
relevel_data  <- function(data) {
  # Race
  if ("Race" %in% names(data)) {data$Race <- factor(data$Race)          # Convert character to factored variables to allow releveling.
  if ("Non-Hispanic White" %in% levels(data$Race)) {
    data$Race <- relevel(data$Race, ref = "Non-Hispanic White")
    }} 
  # Married
  if ("Married" %in% names(data)) {data$Married <- factor(data$Married)     # Convert character to factored variables to allow releveling.
  if ("Never Married" %in% levels(data$Married)) {
    data$Married <- relevel(data$Married, ref = "Never Married")
    }}
  # Gender
  if ("Gender" %in% names(data)) {data$Gender <- factor(data$Gender)     # Convert character to factored variables to allow releveling.
  if ("Male" %in% levels(data$Gender)) {
    data$Gender <- relevel(data$Gender, ref = "Male")
    }}
  # Generic Yes/No variables
  YN_vars <- c(
    "Hepatitis_B_Diagnosis","Military","Poor_Liver_Condition","Poor_Appetite","Fatigue")
  
  for (i in YN_vars) {
  if (i %in% names(data)) {data[[i]] <- factor(data[[i]])      # Convert character to factored variables to allow releveling.
    if ("No" %in% levels(data[[i]])) {data[[i]] <- relevel(data[[i]], ref = "No")}
    }}
  
  return(data)
} ################################


ui <- fluidPage(
  titlePanel("Best Variables in Model for Hepatitis B Surface Antibody"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Outcome Variable: Hepatitis B Surface Antibody"),
      
      selectInput("predictors", "Predictor Variables:", 
                  choices = names(subdat)[names(subdat) != "Anti_HBs"],
                  multiple = TRUE, 
                  selected = c("Hepatitis_B_Diagnosis")),
      
      checkboxInput("center_vars", "Center numeric predictors", value = FALSE), 
      checkboxInput("relevel_vars", "Relevel categorical variables to preferred reference levels", value = FALSE),      ###############################################
      actionButton("run_stepwise", "Run Stepwise Selection"), actionButton("run_RF", "Run Random Forest")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Instructions", 
                 
                 h3(tags$b("Overview")),
                 p("This app aims to create multivariable logistic regression models for Hepatitis B Surface Antibody (anti-HBs) status in the USA, using the 2021-2023 NHANES demographic, laboratory, and questionnaire datasets to identify predictors associated with anti-HBs status. You can validate the best logistic regression model fit with the highest predictability by comparing a full model with all the predictors, a nested model via automated stepwise selection, and a nested model via random forest."),
                 tags$ul(
                   tags$li(tags$a(href = "https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?Cycle=2021-2023", "NHANES 2021-2023 Datasets", target = "_blank")), #_blank opens to new browser
                   tags$li("The dataset contains 3,869 observations of participants with or without anti-HBs, containing 13 predictors (3 numeric, 10 categorical)."),
                   tags$li("You can select predictor variables on the left to automatically generate a logistic regression model. I recommend centering the quantitative predictors for interpretation."),
                   tags$li("You can also relevel categorical variables on the left for easier interpretation."),
                   tags$li("The Full Model Summaries tab, the Stepwise Selection tab, and the Random Forest tab will include the outputs of the logistic regression models and the VIFs of the predictors. Click the buttons on the left to generate a nested model based on automated stepwise selection and random forest.")
                 ),
                 
                 h4(tags$b("Multicollinearity")),
                 p("A predictor with a VIF > 5 indicates potential concerns of multicollinearity, and VIF > 10 indicates the predictors suffer from multicollinearity. For predictors with too high multicollinearity, the model will not run."),
                 
                 h4(tags$b("Model Comparisons")),
                 p(
                   "The model comparison tables analyzes the best fitted models based on the AIC, BIC, McFadden R",
                   tags$sup(2),
                   ", the Adjusted McFadden R",
                   tags$sup(2),
                   "the Area Under the Curve (AUC), and the Hosmer Lemeshow p-value. It also reports the p-values of the likelihood ratio test between the Full and Stepwise models and the Full and Random Forest models. Note that the table will not load until you run the stepwise selection and random forest methods."
                 ),
                 
                 h4(tags$b("Notes")),
                 tags$ul(
                   tags$li(tags$b("Poor Living Condition"), "suffers from severe multicollinearity, and you will receive an error running a model with it included."),
                   tags$li("I've included", tags$b("Age"), "as numeric and categorical variables. Age suffers from nonlinearity, and I strongly recommend including Age only as a categorical variable."),
                   tags$li("I strongly recommend including", tags$b("Phlebotomy Weight"), ", since it is necessary for measuring anti-HBs status for those who had the virus or had vaccination."),
                   tags$li("You will not get an optimal model with the predictors available, since the adjusted R", tags$sup(2), "will still be < 50%. For future studies, I will consider adding in more predictors that could be associated with anti-HBs status.")
                 ),
              ),
        
        tabPanel("Run Full Model Summaries", verbatimTextOutput("model_summary"), verbatimTextOutput("Multicollinearity")),
        tabPanel("Run StepWise Selection", 
                 h3(tags$b("Automated Stepwise Selection")),
                 p("The bidirectional stepwise selection automatically looks for a 'better' logistic regression model by adding and removing predictors, based on the AIC criterion. It aims to find the best model with the lowest AIC."),
                 verbatimTextOutput("stepwise_summary"), verbatimTextOutput("Multicollinearity2")),
        tabPanel("Run Random Forest",  
                 h3(tags$b("Random Forest")),
                 p("Random forest is a machine learning method to construct a classification model for anti-HBs status. Given a low sample size of 3,879, the performance of random forest would not be as powerful for modeling the covariates."),
                 tags$ul(
                   tags$li("The variable importance is evaluated by the mean decrease in accuracy and the mean decrease in the Gini impurity index for each predictor. The mean decrease in accuracy assesses the loss of accuracy for a certain predictor removed, and the mean decrease in the Gini impurity index assesses the loss of quality of the split in a decision tree. Both measurements determine how much the effect of leaving out a certain covariate would have on the overall performance of the model; the higher the mean decrease, the lower the overall performance of the model."),
                   tags$li("The method will run 500 decision trees based on the mtry parameter, before combining all trees under one optimized model. Mtry is the number of variables randomly sampled to different simulated decision trees, and the best mtry is based on the lowest out-of-bag (OOB) error. In this case, the best mtry will be 4, but you can still run with 3 or fewer predictors.")
                 ),  
                 verbatimTextOutput("RF_summary"), plotOutput("rfImpPlot"), verbatimTextOutput("RFtop4"), verbatimTextOutput("Multicollinearity3")),
        tabPanel("Model Comparisons", DTOutput("run_AIC_BIC_R2"))
      )
    )
  )
)

server <- function(input, output) {
  #### FULL MODEL
  model <- reactive({
    req(input$predictors) # Ensure at least one predictor is selected
    
    data_used <- subdat ######################################
    
    if (input$relevel_vars) {
      data_used <- relevel_data(data_used)
    }
    
    # Apply centering if selected              
    # Identify numeric predictors
    numeric_vars <- input$predictors[sapply(data_used[input$predictors], is.numeric) ]  # picks predictors, and sapply checks which predictors are numeric. Then subsets numeric columns only.
    
    # Building Predictor terms
    # I had another centering code that did work, but once I've run the stepwise selection, I can't disable centering the variables. Despite the ugliness, this will show if centering is enabled or not.
      
    predictor_terms <- sapply(input$predictors, function(var) {
      if (input$center_vars && var %in% numeric_vars) {
        paste0("scale(", var, ", center = TRUE, scale = FALSE)")
      } else {var}})
    
    names(predictor_terms) <- input$predictors
    
    formula <- as.formula(paste("Anti_HBs", "~", paste(predictor_terms, collapse = "+")) )   # Check to improve this? Collapse "+" means to include these predictors as "x1+x2+x3+..."
    
    glm(formula, data = data_used, family = binomial, model = TRUE)
  })
  
  #### STEPWISE MODEL
  stepwise_model <- eventReactive(input$run_stepwise, {
    req(input$predictors)
    
    data_used <- subdat ######################################
    
    if (input$relevel_vars) {
      data_used <- relevel_data(data_used)
    }
    
    # Apply centering if selected
    # Identify numeric predictors
    numeric_vars <- input$predictors[sapply(data_used[input$predictors], is.numeric) ]
    
    # Building Predictor terms
    predictor_terms <- sapply(input$predictors, function(var) {
      if (input$center_vars && var %in% numeric_vars) {
        paste0("scale(", var, ", center = TRUE, scale = FALSE)")
      } else {var}})
    
    names(predictor_terms) <- input$predictors
    
    full_formula <- as.formula(paste("Anti_HBs", "~", paste(predictor_terms, collapse = "+")) )   # Check to improve this?
    
    full_model <- glm(full_formula, data = data_used, family = binomial, model = TRUE)
    
    step(full_model, direction = "both", trace = 0)
  })
  
  #### RANDOM FOREST MODEL
  RF_model <- eventReactive(input$run_RF, {
    req(input$predictors)
    
    data_used <- subdat[, c("Anti_HBs", input$predictors)]
    
    if (input$relevel_vars) {
      data_used <- relevel_data(data_used)
    }
    
    # Force classification RF
    data_used[["Anti_HBs"]] <- as.factor(data_used[["Anti_HBs"]]) # Forces model to run RF. we have to convert outcome of 0/1 into a factor for RF
    
    # Apply centering if selected
    # Identify numeric predictors
    
    # The previous center code does not work. Let's use a new one.
    if (input$center_vars) {
      numeric_vars <- input$predictors[sapply(data_used[input$predictors], is.numeric) ]
      data_used[numeric_vars] <- lapply(data_used[numeric_vars], function(x) {scale(x, center = TRUE, scale = FALSE)}) }
    
    full_formula2 <- as.formula(paste("Anti_HBs", "~", paste(input$predictors, collapse = "+")) )   # Check to improve this?
    # full_model2 <- glm(full_formula2, data = data_used, family = binomial, model = TRUE)
    
    #Try random forest
    # library(caret)
    split <- createDataPartition(y=data_used$Anti_HBs, p=0.8, list=FALSE)
    train <- data_used[split,]
    test <- data_used[-split,]
    
    bag.tree <- randomForest(full_formula2, data=train, mtry = min(4, length(input$predictors)), importance=TRUE) # total number of predictors. Lower value for mtry
    # bag.tree
    
    # Best 4
    # Variable importance
    imp <- importance(bag.tree)
    # top4 <- names(sort(imp[, "MeanDecreaseGini"], decreasing = TRUE))[1:4]
    n_top <- min(4, length(input$predictors))
    top4 <- names(sort(imp[, "MeanDecreaseGini"], decreasing = TRUE))[1:n_top]
    top4_data = subdat |> dplyr::select(Anti_HBs, all_of(top4))
    modeltop4 <- glm(Anti_HBs ~ ., data = top4_data, family="binomial", model = TRUE) #all_of uses the values inside the variable top4 as column names.

    
    return(list(
      rf = bag.tree,
      top4_model = modeltop4,
      top4 = top4,
      top4_data = top4_data
    ))
    
  })
  
  ###FULL MODEL
  output$model_summary <- renderPrint({
    summary(model())
  })
  
  output$Multicollinearity <- renderPrint({ 
    vif_values <- car::vif(model())
    print(vif_values)
  })
  
  ### STEPWISE
  output$stepwise_summary <- renderPrint({
    req(stepwise_model())
    summary(stepwise_model())
  })
  
  output$Multicollinearity2 <- renderPrint({ 
    vif_values <- car::vif(stepwise_model())
    print(vif_values)
  })
  
  ### RANDOM FOREST
  output$RF_summary <- renderPrint({
    req(RF_model())
    rf <- RF_model()$rf
    print(rf)
    cat("\n\nVariable Importance:\n")
    print(importance(rf))
  })
  
  output$rfImpPlot <- renderPlot({
    req(RF_model())
    varImpPlot(RF_model()$rf)
  })
  
  output$RFtop4 <- renderPrint({
    req(RF_model())
    summary(RF_model()$top4_model)
  })
  
  output$Multicollinearity3 <- renderPrint({ 
    vif_values <- car::vif(RF_model()$top4_model)
    print(vif_values)
  })
  
  ### MODEL COMPARISONS. This won't show unless all three models have an output
  output$run_AIC_BIC_R2 <- renderDT({
    
    # Because stepwise and RF are event reactive, add req
    req(model())
    req(stepwise_model())
    req(RF_model()$top4_model)
    
    mod <- model()
    mod2 <- stepwise_model()
    mod3 <- RF_model()$top4_model
    rf_data <- RF_model()$top4_data
    
    # Create null models explicitly
       null_mod1 <- glm(Anti_HBs ~ 1,data = mod$model,family = binomial)
       null_mod2 <- glm(Anti_HBs ~ 1,data = mod2$model,family = binomial)
    
    # RF-selected logistic model. Builds null model using rf_data, since top4_data only exists in list by RF_model()
        null_mod3 <- glm(Anti_HBs ~ 1, data = rf_data, family = binomial
        ) # Sets Anti_HBS as outcome in model eqn. For fixing the denominator
        
    # McFadden pseudo R2
    # r2 <- 1 - (as.numeric(logLik(mod)) / as.numeric(logLik(update(mod, . ~ 1))))
    r2 <- 1 - (as.numeric(logLik(mod)) / as.numeric(logLik(null_mod1)))
    # r2.2 <- 1 - (as.numeric(logLik(mod2)) / as.numeric(logLik(update(mod2, . ~ 1))))
    r2.2 <- 1 - (as.numeric(logLik(mod2)) / as.numeric(logLik(null_mod2)))
    # r2.3 <- 1 - (as.numeric(logLik(mod3)) / as.numeric(logLik(update(mod3, . ~ 1)))) # mod3 doesn't work in denominator
    r2.3 <- 1 - (as.numeric(logLik(mod3)) / as.numeric(logLik(null_mod3)))
    
    # Adjusted McFadden pseudo R2
    # adj_r2 <- 1 - ((as.numeric(logLik(mod)) - length(coef(mod))) / as.numeric(logLik(update(mod, . ~ 1))))
    adj_r2 <- 1 - ((as.numeric(logLik(mod)) - length(coef(mod))) / as.numeric(logLik(null_mod1)))
    # adj_r2.2 <- 1 - ((as.numeric(logLik(mod2)) - length(coef(mod2))) / as.numeric(logLik(update(mod2, . ~ 1))))
    adj_r2.2 <- 1 - ((as.numeric(logLik(mod2)) - length(coef(mod2))) / as.numeric(logLik(null_mod2)))    
    # adj_r2.3 <- 1 - ((as.numeric(logLik(mod3)) - length(coef(mod3))) / as.numeric(logLik(update(mod3, . ~ 1)))) # mod3 doesn't work in denominator
    adj_r2.3 <- 1 - ((as.numeric(logLik(mod3)) - length(coef(mod3))) / as.numeric(logLik(null_mod3)))
    
    # AUC
    roc.object <- roc(mod$y~fitted(mod))
    roc.object2 <- roc(mod2$y~fitted(mod2))
    roc.object3 <- roc(mod3$y~fitted(mod3))
    
    # Hosmer Lemeshow model goodness of fit
    # library(glmtoolbox)
    ht1 <- hltest(mod)
    HL1 <- round(as.numeric(ht1[["p.value"]]), 5)
    ht2 <- hltest(mod2)
    HL2 <- round(as.numeric(ht2[["p.value"]]), 5)
    ht3 <- hltest(mod3)
    HL3 <- round(as.numeric(ht3[["p.value"]]), 5)
    
    # library(lmtest)
    LR1 = round(lrtest(mod, mod2)[2, "Pr(>Chisq)"], 5)
    LR2 = round(lrtest(mod, mod3)[2, "Pr(>Chisq)"], 5)
    
    # Create comparison table using DT package
    comparison_table <- data.frame(
      Model = c("Full Model", "Stepwise Model", "RF Model"),
      AIC = c( round(AIC(mod), 2), round(AIC(mod2), 2), round(AIC(mod3), 2) ),
      BIC = c( round(BIC(mod), 2), round(BIC(mod2), 2), round(BIC(mod3), 2) ),
      McFadden_R2 = c( round(r2, 4), round(r2.2, 4), round(r2.3, 4) ),
      Adjusted_McFadden_R2 = c( round(adj_r2, 4), round(adj_r2.2, 4), round(adj_r2.3, 4) ),
      AUC = c( round(auc(roc.object), 4), round(auc(roc.object2), 4), round(auc(roc.object3), 4) ),
      Hosmer_Lemeshow_p_value = c(HL1, HL2, HL3),
      Likelihood_Ratio_Test_p_value = c("---", LR1, LR2)
      )
    
    datatable( comparison_table, rownames = FALSE, options = list( pageLength = 5, dom = 't', autoWidth = TRUE ) )
    
  })
}

shinyApp(ui = ui, server = server)
