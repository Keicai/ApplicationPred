library(dplyr)
library(tidytext)
library(stringi)
library(stringr)
library(tm)

#Loading corpus data cleaned by cleanInput function
bigram <- readRDS("all_bigram.rds")
trigram <- readRDS("all_trigram.rds")
quadgram <- readRDS("all_quadgram.rds")
quingram <- readRDS("all_quingram.rds")

#en_badwords downloaded from the web.
profanity <- read.table("en_badwords.txt", header=FALSE, sep="\n", strip.white=TRUE)
names(profanity) <- "badword"

#Cleaning input to extract specific words
cleanInput <- function(text){
        textInput <- tolower(text)
        textInput <- removePunctuation(textInput)
        textInput <- removeNumbers(textInput)
        textInput <- str_replace_all(textInput, "[^[:alnum:]]", " ")
        textInput <- stripWhitespace(textInput)
        textInput <- txt.to.words.ext(textInput, language="English.all", preserve.case = TRUE)
        textInput <- removeWords(textInput, c("a","and","the","an", "i","e","g","x","id","be","he","she","we","you","to","been","has","had","have","am","im","are","is"))
        #need to allow some stop words to make sense of the next word
        #textInput <- removeWords(textInput, stopwords('english'))
        textInput <- removeWords(textInput, profanity[,1])
        final <- textInput[textInput != ""]
        return(final)
}

#Match string in five Gram and get probable word
fivegram <- function (inputWord1,inputWord2,inputWord3,inputWord4)
        
{
        predictWord <- filter(quingram,(word1 == inputWord1 & word2 == inputWord2 & word3 == inputWord3 & word4 == inputWord4))$word5
        
        if(length(predictWord) == 0)
        {
                predictWord <- filter(quingram,( word2 == inputWord2 & word3 == inputWord3 & word4 == inputWord4))$word5
                
                if(length(predictWord) == 0)
                {
                        predictWord <- filter(quingram,( word1 == inputWord2 & word2 == inputWord3 & word3 == inputWord4))$word4
                        
                        if(length(predictWord) == 0)
                        {
                                predictWord <- filter(quingram,( word1 == inputWord3 & word2 == inputWord4))$word5
                        }
                        
                        
                        if(length(predictWord) == 0)
                        {
                                predictWord <- fourgram(inputWord2,inputWord3, inputWord4)
                        }
                        
                }
                
        }
        
        predictWord
        
}

#Match string in four Gram and get probable word
fourgram <- function (inputWord1,inputWord2,inputWord3)
        
{
        predictWord <- filter(quadgram,(word1 == inputWord1 & word2 == inputWord2 & word3 == inputWord3))$word4
        if(length(predictWord) == 0)
        {
                
                predictWord <- filter(quadgram,( word2 == inputWord2 & word3 == inputWord3))$word4
                if(length(predictWord) == 0)
                {
                        predictWord <- filter(quadgram,( word1 == inputWord2 & word2 == inputWord3))$word3
                        
                        
                        if(length(predictWord) ==0)
                        {
                                predictWord <- threegram(inputWord2,inputWord3)
                        }
                        
                }
                
        }
        
        predictWord
        
}

#Match string in three Gram and get probable word
threegram <- function(inputWord1,inputWord2)
{
        predictWord <- filter(trigram,( word1 == inputWord1 & word2 == inputWord2))$word3
        if(length(predictWord)==0)
        {
                predictWord <- filter(trigram,(word2 == inputWord2))$word3 
                
                if(length(predictWord)== 0)
                {
                        predictWord <- filter(trigram,(word1 == inputWord2))$word2 
                        
                        if(length(predictWord) ==0 )
                        {
                                predictWord <- twogram(inputWord2)
                        }
                        
                }
        }
        predictWord
}

#Match string in two Gram and get probable word
twogram <- function(inputWord1)
{
        predictWord <- filter(bigram,( word1 == inputWord1 ))$word2
        
        predictWord
        
}

predictNextWord <- function(input)
{
        
        #Cleaning the input
        wordInput <- cleanInput(input)
        #Getting the number of words in the input
        wordCount <- length(wordInput)
        #Initializing response
        prediction <- c()
        
        #Trimming input to the last four words
        if(wordCount>4)
        {
                wordInput <- wordInput[(wordCount-3):wordCount]
                prediction <- fivegram(wordInput[1],wordInput[2],wordInput[3],wordInput[4])
        }
        
        #Five Gram Match
        if(wordCount ==4)
        {
                prediction <- fivegram(wordInput[1],wordInput[2],wordInput[3],wordInput[4])
        }
        
        #Four Gram Match
        if(wordCount ==3)
        {
                prediction <- fourgram(wordInput[1],wordInput[2],wordInput[3])
        }
        
        #Three Gram Match
        if(wordCount ==2)
        {
                prediction <- threegram(wordInput[1],wordInput[2])
        }
        #Two gram match
        if(wordCount ==1)
        {
                prediction <- twogram(wordInput[1])
        }
        
        #No word entered
        if(wordCount == 0)
        {
                prediction <- "Please enter at least one word."
        }
        
        #Unable to predict
        if(length(prediction)==0)
        {
                prediction <- "I'm still learning, please pardon me."
        }
        
        #Returning at most 5 responses
        if(length(prediction) < 5)
        {
                prediction
        }
        
        else
        {
                unique(prediction[1:3])
        }
        
        
}