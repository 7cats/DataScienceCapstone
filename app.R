#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## load package, data and code
library(shiny)
library(tm)
library(RWeka)
library(tidyr)
library(stringr)
library(ggplot2)
library(dplyr)
load("allData2.RData")
load("allFigures.RData")
source('PreProcessFun.R')
source('FunPredictiveWordBackOff.R')
source('funCalculateProbs.R')


# define UI
ui <- fluidPage( 
    tabsetPanel(
    tabPanel("Introduction", 
             verticalLayout( 
                 h2("Introduction"),
                 h4('Author:Zixin Zhang'),
                 h4('Date:30/11/2020'),
                 p("This is a shiny app that present a predictive model that suggest the next word based on previous given words.\\n 
                   This model is trained based on text from blogs, news and twitter. The original data source is from:"),
                 a(href = 'data source', 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'),
                 p('\n\nThis app contains three part. The first one is a summary of original data. 
                   Second part allows you to predict the next word based on previous given words.
                   And the last part could compare the probablities between different words to follow a certain phrase. 
                   To see the original code for this project, please refer to:'),
                 a(href = 'Code&Report', 'https://github.com/7cats/DataScienceCapstone')
             )),
    
    tabPanel("Data Summary",
             verticalLayout( 
                 plotOutput("gCount",
                            width = "70%",
                            height = "400px"),
                 plotOutput("gTopTenTwitter",
                            width = "70%",
                            height = "400px"),
                 plotOutput("gTopTenBlog",
                            width = "70%",
                            height = "400px"),
                 plotOutput("gTopTenNew",
                            width = "70%",
                            height = "400px"),
                 textOutput('Analysis'))),

    
    tabPanel("Predict the next word", 
             # Application title
             titlePanel("Predict the Next Word"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
                 sidebarPanel(
                     textInput('gram1',
                               label = 'First gram',
                               # value = ' ',
                               placeholder = 'Please give your first word'),
                     
                     textInput('gram2',
                               label = 'Second gram',
                               # value = ' ',
                               placeholder = 'Please give your second word'),
                     
                     textInput('gram3',
                               label = 'Third gram',
                               # value = ' ',
                               placeholder = 'Please give your third word')
                 ),
                 # Show a plot of the generated distribution
                 mainPanel(
                     textOutput('hint'),
                     textOutput('nextWord'),
                     tableOutput('predictResult'))
             )
             ),
    
    tabPanel("Comparing probabilities", 

         titlePanel("Comparing the probabilities between two words to following one phrase"),
         sidebarLayout(
             sidebarPanel(
                 textInput('gram1c',
                           label = 'First gram',
                           # value = ' ',
                           placeholder = 'Please give your first word'),
                 
                 textInput('gram2c',
                           label = 'Second gram',
                           # value = ' ',
                           placeholder = 'Please give your second word'),
                 
                 textInput('gram3c',
                           label = 'Third gram',
                           # value = ' ',
                           placeholder = 'Please give your third word')),
             # Show a plot of the generated distribution
             mainPanel(
                 textInput('word1',
                           label = 'Word to be compared',
                           # value = ' ',
                           placeholder = 'Give a word you want to compare'),
                 
                 textInput('word2',
                           label = 'Word to be compared',
                           # value = ' ',
                           placeholder = 'Give a word you want to compare'),
                 
                 textOutput('hintGram'),
                 textOutput('hintMessage'),
                 tableOutput('predictProb'))
         ))
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    inputText <- reactive({
        ## predict next word output
        givenGrams <- cleanText(paste(input$gram1, input$gram2, input$gram3, sep = ' '))
        n <- sum(c(input$gram1 != '', input$gram2 != '', input$gram3 != ''))

        hint <- switch (as.character(n),
                        '3' = 'Three grams are given to predict the next one. Using the quadgram model.',
                        '2' = 'Two gram are given to predict the next one. Using the trigram model.',
                        '1' = 'One grams is given to predict the next one. Using the bigram model.',
                        '0' = 'No gram are given to predict the next one. Simply give words with highest probability.')
        if(n == 0) textPredict <- 'Words with the highest probabilities'
        else textPredict <- paste(
            'The word predicted to be next following', givenGrams, 'is', sep = ' ')
        predictResult <- predictWordBackOff(smoothGT, textMatrixAll, givenGrams, returnNum = 100)
        predictResult <- mutate(predictResult, prob = prob*100/sum(prob))
        colnames(predictResult) <- c('counts','probability(\\%)','word')
        predictResult <- predictResult[1:10,]
        
        ## compare probability output
        phrase1 <- cleanText(paste(input$gram1c, input$gram2c, input$gram3c, input$word1, sep = ' '))
        phrase2 <- cleanText(paste(input$gram1c, input$gram2c, input$gram3c, input$word2, sep = ' '))
        n_gram <- sum(c(input$gram1c != '', input$gram2c != '', input$gram3c != ''))
        n_word <- sum(c(input$word1 != '', input$word2 != ''))
        if(n_word == 2)
        {
            if(n_gram == 0){
                phrase1 <- gsub('\\s','',phrase1)
                phrase2 <- gsub('\\s','',phrase2)}
            predProb <- data.frame(word = c(input$word1,input$word2),
                                   probbability = c(calculateProbs(smoothGT, textMatrixAll, phrase1),
                                            calculateProbs(smoothGT, textMatrixAll, phrase2))*100)
            wordOrder <- sort(predProb$prob, decreasing = T, index.return = T)$ix
            hintMessage <- paste('The word', predProb$word[wordOrder[1]],'has a larger probability than the word',
                                 predProb$word[wordOrder[2]])
        }
        else
        {
            predProb <- NULL
            hintMessage <-'Please give two words that you want to predict'
        }
        hintGram <- switch (as.character(n_gram),
                            '3' = 'Three grams are given to predict the next one. Using the quadgram model.',
                            '2' = 'Two gram are given to predict the next one. Using the trigram model.',
                            '1' = 'One grams is given to predict the next one. Using the bigram model.',
                            '0' = 'No gram are given. Simply give the probabilities of these two words.')
        return(list(hint = hint, textPredict = textPredict, predictResult = predictResult,
                    hintGram = hintGram, predProb = predProb, hintMessage = hintMessage))
    })
    
    output$hint <- renderText({
        inputText <- inputText()
        inputText$hint})
    
    output$nextWord <- renderText({
        inputText <- inputText()
        inputText$textPredict}) 
    
    output$predictResult <- renderTable({
        inputText <- inputText()
        inputText$predictResult})
    
    ## compare probability output
    output$hintGram <- renderText({
        inputText <- inputText()
        inputText$hintGram})
    
    output$hintMessa <- renderText({
        inputText <- inputText()
        inputText$hintMessage}) 
    
    output$predictProb <- renderTable({
        inputText <- inputText()
        if (!is.null(inputText$predPr))
            colnames(inputText$predPro) <- c('word', 'probability(\\%)')
        inputText$predProb})
    
    ## data summary output
    output$gCount <- renderPlot({gCount})
    output$gTopTenTwitter <- renderPlot({gTopTen[[1]]})
    output$gTopTenBlog <- renderPlot({gTopTen[[2]]})
    output$gTopTenNew <- renderPlot({gTopTen[[3]]})
    output$Analysis <- renderText(
        paste('Most frequently used words of these three sources are presented in the figures. We could draw some conclusions:',
              '1) The word the has the highest frequency of all media sources, but with much higher frequencies for News and Blogs than Twitter. It indicates that when people are writing on Twitter, they give less attention to using grammar correctly.',
              '2) The personal pronoun I, you appear much more times in Text from Twitter than the other media source. It is also expected since people tends to talk about themselves on Twitter.\n',
              sep = '\n'))
}

# Run the application 
shinyApp(ui = ui, server = server)
