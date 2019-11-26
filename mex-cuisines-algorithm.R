#USA dataset
library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(dplyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(sqldf)
library(tidyr)
library(udpipe)
udmodel = udpipe_download_model(language = "english")   #load language for POS model

#PART 1-------------------
mex = read.csv("mex.csv")
mex_df = mex[,c(8:19,21:45,52:108)]
mex_df_ing = mex_df[,-c(20,27,37)]

#PART 2------------------
mex_df_free = mex_df_ing %>% distinct()
colnames(mex_df_free) <- gsub("\\.","_",colnames(mex_df_free))

#PART 3-----------------
tm_df_mex = unite(mex_df_free, ingredients, c(2:11,13:91), remove=FALSE, sep = " ") 

#dataframe with courses
full_df_mex = data.frame(tm_df_mex$name,tm_df_mex$ingredients,tm_df_mex$course_0)  
names(full_df_mex)[1] = "name"
names(full_df_mex)[2] = "ingredients"
names(full_df_mex)[3] = "course"


#PART 4-----------------


#---LOAD DICTIONARY of COMMON CULINARY TERMS AND UNITS
dic_unit = read.csv("dict/cooking_units.txt",encoding = "UTF-8")
names(dic_unit)="unit"

dic_terms = read.csv("dict/cooking_terms.txt",encoding = "UTF-8")
names(dic_terms)="term"

#REMOVE FRACTIONS
stop_measures = c("1/2","1/3","1/4","1/8","1/16","2/3","2/4","3/4","3/8","about","half","one","quarter", "and", "or", "of", "if", "to", "with", "each", "nbsp")


# CREATE FILTER FUNCTIONS ---------------------------------------
stopw_filter <- function(string, stopwords=c()){
  # Create something like:  "\\b( the|Jura)\\b"
  new_regex <- paste0("\\b( ", paste0(stopwords, collapse="|"), ")\\b")
  x = gsub(new_regex, " ", string)
}

all_but_hyphen_filter_comma <- function(string){
  x = gsub("[^-a-zA-Z[:space:]]+"," ",string, perl = TRUE)
}
no_numbers_filter <- function(string){
  x = gsub("[0-9]"," ",string)
}
no_newline_filter <- function(string){
  x = gsub("\n"," ",string)
}

units_filter <- function(string, dic_unit=c()){
  
  units_regex <- paste0("\\b( ", paste0(dic_unit$unit, collapse="|"), ")\\b")
  y = gsub(units_regex, " ", string)
}

common_filter <- function(string, dic_terms=c()){
  
  common_regex <- paste0("\\b( ", paste0(dic_terms$term, collapse="|"), ")\\b")
  gsub(common_regex, " ", string)  
}


#TOPIC MODELLING FUNCTION-----------------------
top_terms_by_topic_LDA <- function(input_text,  plot = T,  number_of_topics = 5, plottitle)
{
  Corpus <- Corpus(VectorSource(input_text)) 
  DTM <- DocumentTermMatrix(Corpus)
  
  unique_indexes <- unique(DTM$i)
  DTM <- DTM[unique_indexes,]
  
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  top_terms <- topics  %>%
    group_by(topic) %>% 
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      ggtitle(plottitle)+
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
  
}


#SELECT THE COURSE TO ANALYSE -------------------------------------------------
#list all courses to know which Course to input into the query    
sqldf("SELECT DISTINCT(course) FROM full_df_usa")

#extract the course
full_df_course_usa = sqldf("SELECT * FROM full_df_usa WHERE course='Salads' ")  #******change here******



#LOAD DATASET AS CORPUS-----------------------------
recipesCorpus <- Corpus(VectorSource(full_df_course_usa$ingredients))   
recipesDTM <- DocumentTermMatrix(recipesCorpus)           #CONVERT CORPUS TO DOC-TERM-MATRIX
recipesDTM_tidy <- tidy(recipesDTM)                       #TIDY DTM
recipesDTM_tidy_cleaned <- recipesDTM_tidy 

#FILTER FRACTIONS --- DICTIONARY WORDS (TERMS AND UNIS)
recipesDTM_tidy_cleaned=recipesDTM_tidy[!grepl(paste(stop_measures, collapse="|"), recipesDTM_tidy$term),]
recipesDTM_tidy_cleaned=recipesDTM_tidy_cleaned[!grepl(paste(dic_unit$unit, collapse="|"),  recipesDTM_tidy_cleaned$term),]
recipesDTM_tidy_cleaned=recipesDTM_tidy_cleaned[!grepl(paste(dic_terms$term, collapse="|"), recipesDTM_tidy_cleaned$term),]

#remove punctuations
clean_recipesDTM_tidy_cleaned = recipesDTM_tidy_cleaned %>%
  mutate_all(funs(gsub("[[:punct:]]", "", .))) 

clean_recipesDTM_tidy_cleaned = sqldf("SELECT * FROM clean_recipesDTM_tidy_cleaned WHERE count>1 ") #select count greater than 1
clean_recipesDTM_tidy_cleaned = sqldf("SELECT * FROM clean_recipesDTM_tidy_cleaned WHERE LENGTH(term)>2 ") #select character greater than 2


#create dataframe grouped (tidy)
cc_cleaned_documents <- clean_recipesDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

#FILTER
x = stopw_filter(cc_cleaned_documents)
x1 = no_numbers_filter(x)
x2 = no_newline_filter(x1)
x3 = all_but_hyphen_filter_comma(x2)
x4 = common_filter(x3)
x5 = units_filter(x4)


c_lean_POS = udpipe(x5[2],object=udmodel)

#INTERCHANGEABLE QUERIES (q1,q2,q3)-------------------------------BEGINS----------HERE-------
#--------for general topics (q1)
#c_l_ean_POS = sqldf("SELECT * FROM c_lean_POS WHERE upos NOT IN ('ADJ','ADV','PART','VERB','PROPN','INTJ','') ") #REMOVE Some Parts of Speech

#--------for adjective topics (q2)
c_l_ean_POS = sqldf("SELECT * FROM c_lean_POS WHERE upos='ADJ' ") #FILTER ONLY ADJECTIVES

#--------for noun topics (q3)
#c_l_ean_POS = sqldf("SELECT * FROM c_lean_POS WHERE upos='NOUN' ") #FILTER ONLY NOUNS

#INTERCHANGEABLE QUERIES (q1,q2,q3)-------------------------------ENDS-----------HERE-------

#plot after filtering PARTS OF SPEECH - LEMMA -------------------------------topics lemmas
top_terms_by_topic_LDA(c_l_ean_POS$lemma, number_of_topics = 5, plottitle = "Top Adjectives associated with Salads in Mexican Cuisines")    #lemma topics plot


