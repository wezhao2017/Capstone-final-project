

library(shiny)
library(stringr)
library(tm)


bi <- readRDS("bigram.RData")
tri <- readRDS("trigram.RData")
qua <- readRDS("quadgram.RData")


names(bi)[names(bi) == 'word1'] <- 'w1'; names(bi)[names(bi) == 'word2'] <- 'w2';
names(tri)[names(tri) == 'word1'] <- 'w1'; names(tri)[names(tri) == 'word2'] <- 'w2'; names(tri)[names(tri) == 'word3'] <- 'w3';
names(qua)[names(qua) == 'word1'] <- 'w1'; names(qua)[names(qua) == 'word2'] <- 'w2'; names(qua)[names(qua) == 'word3'] <- 'w3';
names(qua)[names(qua) == 'word4'] <- 'w4';
message <- "" 



predictWord <- function(the_word) {
  
        word_add <- stripWhitespace(removeNumbers(removePunctuation(tolower(the_word),preserve_intra_word_dashes = TRUE)))
        
        the_word <- strsplit(word_add, " ")[[1]]
        
        n <- length(the_word)
 
        if (n == 1) {the_word <- as.character(tail(the_word,1)); functionBigram(the_word)}
        
        else if (n == 2) {the_word <- as.character(tail(the_word,2)); functionTrigram(the_word)}
        
        else if (n >= 3) {the_word <- as.character(tail(the_word,3)); functionQuadgram(the_word)}
        
}


functionBigram <- function(the_word) {
        
        if (identical(character(0),as.character(head(bi[bi$w1 == the_word[1], 2], 1)))) {
                
                message<<-"The most used word 'the' will be returned if no word found" 
                
                as.character(head("the",1))
                
        }
        
        else {
                
                message <<- "Using Bigram Freqeuncy Matrix to Predict next Word"
                
                as.character(head(bi[bi$w1 == the_word[1],2], 1))
                
        }
        
}



functionTrigram <- function(the_word) {
        
        if (identical(character(0),as.character(head(tri[tri$w1 == the_word[1]
                                                        
                                                        & tri$w2 == the_word[2], 3], 1)))) {
                
                as.character(predictWord(the_word[2]))
                
        }
        
        else {
                
                message<<- "Using Trigram Freqeuncy Matrix to Predict next Word" 
                
                as.character(head(tri[tri$w1 == the_word[1]
                                     
                                     & tri$w2 == the_word[2], 3], 1))
                
        }
        
}



functionQuadgram <- function(the_word) {
        
        if (identical(character(0),as.character(head(qua[qua$w1 == the_word[1]
                                                        
                                                        & qua$w2 == the_word[2]
                                                        
                                                        & qua$w3 == the_word[3], 4], 1)))) {
                
                as.character(predictWord(paste(the_word[2],the_word[3],sep=" ")))
                
        }
        
        else {
                
                message <<- "Using Quadgram Freqeuncy Matrix to Predict next Word" 
                
                as.character(head(qua[qua$w1 == the_word[1] 
                                     
                                     & qua$w2 == the_word[2]
                                     
                                     & qua$w3 == the_word[3], 4], 1))
                
        }       
        
}



shinyServer(function(input, output) {
        
        output$prediction <- renderPrint({
                
                result <- predictWord(input$inputText)
                
                output$sentence2 <- renderText({message})
                
                result
                
        });
        
        output$sentence1 <- renderText({
                
                input$inputText});
}
)

