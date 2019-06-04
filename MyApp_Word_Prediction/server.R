#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(RWeka)
library(ngram)
library(dplyr)
library(stringr)

### Load dataset
en_us_tw <- readRDS("~/data/en_us_tw.RDS")
en_us_blog <- readRDS("~/data/en_us_blog.RDS")
en_us_news <- readRDS("~/data/en_us_news.RDS")

### Create function - Tokenization - clean text
tokenmaker <- function(x) {
  corpus <- VCorpus(VectorSource(x))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, PlainTextDocument)
}  

### Create function - Tokenization - Bigrams
bigramtoken <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bigrams <- function(x) {
  bigrams <- TermDocumentMatrix(x, control = list(tokenize = bigramtoken)) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rowSums() %>%
    sort(decreasing=TRUE)
  return(data.frame(Word=names(bigrams), Count=bigrams, row.names=(1:length(bigrams))))
}

### Create function - Tokenization - Trigrams
trigramtoken <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
trigrams <- function(x) {
  trigrams <- TermDocumentMatrix(x, control = list(tokenize = trigramtoken)) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rowSums() %>%
    sort(decreasing=TRUE)
  return(data.frame(Word=names(trigrams), Count=trigrams, row.names=(1:length(trigrams))))
}

### Create function - Tokenization - 4-grams
forthgramtoken <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
forthgrams <- function(x) {
  forthgrams <- TermDocumentMatrix(x, control = list(tokenize = forthgramtoken)) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rowSums() %>%
    sort(decreasing=TRUE)
  return(data.frame(Word=names(forthgrams), Count=forthgrams, row.names=(1:length(forthgrams))))
}

### Prediction Algorithm 
nextword <- function(x) {
  inputtextclean <- gsub("[^a-zA-Z ]+", "", x)
  inputtextclean <- gsub("( )( )+", " ", inputtextclean)
  inputtextclean <- gsub("^ ", "", inputtextclean)
  inputtextclean <- gsub(" $", "", inputtextclean)
  
  totalword <- wordcount(inputtextclean)
  m <- 4            ## m-grams
  max_loop <- 3     ## possible max loop to check all m-grams 
  cur_loop <- 1     ## initial loop
  
  while (cur_loop <= max_loop) {
    
    previoustext <- ifelse(m <= totalword, word(inputtextclean, -m+1 , -1), inputtextclean)
    
    tw <- en_us_tw[grepl(previoustext, en_us_tw, ignore.case=TRUE)]
    blog <- en_us_blog[grepl(previoustext, en_us_blog, ignore.case=TRUE)]
    news <- en_us_news[grepl(previoustext, en_us_news, ignore.case=TRUE)]
    alltext <- c(tw, blog, news)
    rm(tw, blog, news)
    
    if(length(alltext) == 0) {
      cur_loop <- cur_loop + 1
      m <- m-1
    } else {
      cur_loop <- 10
    }
  }  
  
  if(length(alltext) > 0) {
    ### Choose the most high frequency word based on: if number of words of previous text is n, then search from (n+1)grams until bigrams. 
    ctext <- tokenmaker(alltext)
    
    if (m == 4) {
      forthfreq <- forthgrams(ctext)
      return(word(as.character(head(forthfreq[startsWith(as.character(forthfreq$Word), previoustext),],1)$Word),-1))
    } 
    
    if (m == 3) {
      trifreq <- trigrams(ctext)
      return(word(as.character(head(trifreq[startsWith(as.character(trifreq$Word), previoustext),],1)$Word),-1))
    }
    
    if (m == 2) {
      bifreq <- bigrams(ctext)
      return(word(as.character(head(bifreq[startsWith(as.character(bifreq$Word), previoustext),],1)$Word),-1))
    }
  }
}

# Upcoming Word Prediction
shinyServer(function(input, output) {
  
  ntext <- eventReactive(input$button1, {
    #runif(input$text1)
    nextword(input$text1)
  })
    
  output$pred1 <- renderText({
    ntext()
  })
    
})
