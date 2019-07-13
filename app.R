library(plotly)
library(shiny)
library(e1071)
library(rpart)
library(randomForest)

#ui component
ui <- fluidPage(
  h1("The Machine Learning Explorer"),
  h4("More Than Just a Black Box"),
  h5("Machine Learning algorithms represent some of the most exciting advances in technology in recent years.  
     When used correctly, the ability of these programs to detect patterns in data 
     and make predictions is nothing short of amazing.  Unfortunately, 
     many of these algorithms are 'black boxes' that 
     are difficult to understand without prior experience with advanced math. The goal of this web app 
     is to provide data science students and enthusiasts with an often not seen view into how many 
     common machine learning models work so that they can begin to understand how they 'fit' to different 
     data sets and what makes them different (or similar.)"),
  h4("How Visualization Can Help"),
  h5("Often, explanations of machine learning models begin with mathematical foundations and theory.  
     Students are presented with complex formulas full of Greek symbols and esoteric notation. 
     While a skilled expert can wade through these hieroglyphics and interpret them,
     most people are left confused and discouraged.  
     However, visualization helps bring math to life and can support learning just as, 
     if not more, than studying pure theory.  In the spirit of this concept, 
     all the information presented within this app is stripped down and designed 
     to be accessible to everyone, and not just experts.  "),
  h4("Just Enough Math"),
  h5("Ok, so we can't fully escape math, this is data science after all, but, 
      we keep it to purely applicable concepts.  So let's make up a scenario...  
      let's say you've collected some data about a sine wave, think of it as a bunch 
      of points that represent the same pattern that you would see in a wave in the ocean.  
      In fact, this data is shown in the picture below. Try interacting with the image by 
      clicking on it and using the tools to pan, zoom in, and hover over specific points to learn more.  
      Keep going until you're able to find the distinct 'S' or wave-shaped pattern in the data.  
      Now think about you would draw an S that would capture as many of these data points as possible 
      Now let's say we want to use machine learning to understand what patterns exist within the data.  
      We can see that an algorithm is working if it can create the same S shape we saw earlier.  
      If the algorithm can understand the shape of the data, then it can make predictions about how new 
      data will look, and once we can predict the future, we can take actions to help ensure that the 
      future will be the best possible one for us."),  
     
  h5("To model the data, we're going to use four of the most common types of machine 
     learning algorithms; linear regression, decision trees, random forest, and support 
     vector machines.  Each one of these takes a very different approach to build an S to fit our data, 
     so how can we know if they're working? To choose which model is the best, 
     we're going to use one standard metric to grade each one of them.  
     This metric is called root-mean-squared-error or RMSE.  
     Think of it as a measure of spread for how close our model predictions are to the actual 
     data points.  A lower number is good and means we're not missing by too much, while a larger number
     is bad because we're over or underestimating.  "),
  
  h4("Let's Go!"),
  h5("Using the drop-down below, choose a different type of model, the image at the lower
     right will change to that model's interpretation of the data, you'll also see 
     a short description of the model and its RMSE score.  Check out different types 
     of models to see how they try and solve the problem of predicting the perfect S shape.  
     Now we have our data, models, and a common way to evaluate them, let's create 
     some machine learning algorithms!"),
  h4("Model Comparison Widget"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(
        "Start Here!"
      ),
      helpText(
        "Choose what type of machine learning model you would like to run.
        Note: Some models like random forest may take a few moments to load."
      ),
      selectInput(
        "var",
        "Model Family:",
        c(
          "Source Data"  = "source",
          "Linear Regression" = "lm",
          "Decision Tree" = "dt",
          "Random Forest" = "rf",
          "Support Vector Machine" = "svm"
        ),
        selected = "source"
      ),
      textOutput("score"),
      textOutput("var")
      ),
    
    mainPanel(plotlyOutput("plot"))
)
)

#reactive server component
server <- function(input, output, session) {
  observe({
    n <- 5000
    rtf <- function(x1, x2) {
      sin(x1 + x2) / (x1 + x2)
    }
    
    xgrid <- seq(1, 6, length = 31)
    ygrid <- seq(1, 6, length = 31)
    zgrid <- outer(xgrid, ygrid, rtf)
    
    df <-
      data.frame(x1 = (runif(n, min = 1, max = 6)), x2 = (runif(n, min = 1, max =
                                                                  6)))
    df$m <-  rtf(df$x1, df$x2)
    df$y <-  df$m + rnorm(n, sd = .07)
    
    #helper function to render the various models
    build_mod <- function(name) {
      #linear regression
      if (name == "lm") {
        reg = lm(y ~ I(x1 ^ 6) + I(x2 ^ 6), data = df)
        actual <- df$y
        predicted <- predict(reg, newdata = df)
        p <-  function(x1, x2)
          predict(reg, newdata = data.frame(x1 = x1, x2 = x2))
        grid <-  seq(1, 6, length = 31)
        ygrid <-  seq(1, 6, length = 31)
        zgrid <-  outer(xgrid, ygrid, p)
        return(zgrid)
      } else if (name == "dt") {
        #decision tree
        reg <- rpart(y ~ x1 + x2, data = df, method = "anova")
        actual <- df$y
        predicted <- predict(reg, newdata = df)
        p <- function(x1, x2)
          predict(reg,
                  newdata = data.frame(x1 = x1, x2 = x2),
                  type = "vector")
        zgrid <- outer(xgrid, ygrid, p)
        return(zgrid)
        #random forest
      } else if (name == "rf") {
        reg = randomForest(y ~ x1 + x2, data = df)
        actual <- df$y
        predicted <- predict(reg, newdata = df)
        p <- function(x1, x2)
          as.numeric(predict(
            reg,
            newdata = data.frame(x1 = x1, x2 = x2),
            type = "response"
          ))
        zgrid <- outer(xgrid, ygrid, p)
        return(zgrid)
      } else if (name == "svm") {
        reg = svm(y ~ x1 + x2, data = df)
        actual <- df$y
        predicted <- predict(reg, newdata = df)
        p <-function(x1, x2)
          as.numeric(predict(
            reg,
            newdata = data.frame(x1 = x1, x2 = x2),
            type = "response"
          ))
        zgrid <- outer(xgrid, ygrid, p)
        return(zgrid)
      }
      
    }
    
    #draw the windows for the source data and models
    if (input$var == "source") {
      df <-
        data.frame(x1 = (runif(n, min = 1, max = 6)), x2 = (runif(n, min = 1, max =
                                                                    6)))
      df$m <- rtf(df$x1, df$x2)
      df$y <- df$m + rnorm(n, sd = .07)
      
      #source data
      output$plot <- renderPlotly({
        plot_ly(
          x = df$x1,
          y = ~ df$y,
          z = ~ df$x2,
          width = 750,
          height = 650
        ) %>%
          add_markers(size = 0.25, opacity = 0.5)  %>%
          layout(
            title = "3d Scatterplot of Source Data",
            scene = list(
              xaxis = list(title = "x"),
              yaxis = list(title = "y"),
              zaxis = list(title = "Z")
            ))
      })
      
    } else {
      zgrid <- build_mod(input$var)
      
      #models
      output$plot <- renderPlotly({
        plot_ly(
          x = ~ xgrid,
          y = ~ ygrid,
          z = ~ zgrid,
          type = 'surface'
          ,
          width = 750,
          height = 650
        ) %>%
          layout(
            title = "3d Plot of Model Fit",
            scene = list(
              xaxis = list(title = "x"),
              yaxis = list(title = "y"),
              zaxis = list(title = "Z")
            ))
      })
      
    }
    
   #model text
    output$var <- renderText({
      if (input$var == "lm") {
        paste("Linear regression works by fitting a single line or curve to the data.
              Since our example consists of three dimensions instead of two, the 
              model will fit what's called a hyperplane. Linear regression 
              is simple, straightforward, and will work with a large variety 
              of data types. However, one drawback is that it may not be as 
              flexible as other modeling techniques.  Our regression model cannot 
              capture all of the twists and turns of the data and does a poor job 
              of capturing the nuances of our example data set. This poor fit is 
              reflected in a high RMSE score.  Nonetheless, linear regression should 
              be the starting point for most data science work, and don't be 
              surprised if the simple linear model outperforms a more complicated one!")
      } else if (input$var == "dt") {
        paste("Decision trees work by creating a series of rules about the data.  
              Each rule determines whether the model should stay the course, or make a change.  
              Since it's using simple rules like yes or no, the model can only change directions 
              at 90-degree angles.  The building up of these rules is shown by the straight lines 
              and abrupt angles in the model.  These rules can be very robust and often do an 
              excellent job of describing the data, but sometimes cannot account for subtle changes, 
              which is shown the lack of smoothness in the model.  ")
      } else if (input$var == "rf") {
        paste("Random forest takes decision trees to the next level by building dozens of individual 
              decision tree models and then having them vote on the best possible outcome.  
              The result of these votes can be combined in what is called an ensemble to make
              more accurate predictions.  You can see by the image to the right that these 
              ensembles can capture much more variation in the data (but not all of it) 
              resulting in the model output that could easily be mistaken for a topographic 
              map of a mountain range.")
      } else if (input$var == "svm") {
        paste("Model Output: RMSE = ")
      }
    })
    output$score <- renderText({
      if (input$var == "lm") {
        paste("Model Output: RMSE = ")
      } else if (input$var == "dt") {
        paste("Model Output: RMSE = ")
      } else if (input$var == "rf") {
        paste("Model Output: RMSE = ")
      } else if (input$var == "svm") {
        paste("Model Output: RMSE = ")
      }
    })
  })
}

#launch the app
shinyApp(ui, server)