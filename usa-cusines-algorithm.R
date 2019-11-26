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
usa = read.csv("usa.csv")
usa_measures = usa[-1,c(2,14:19)]
usa_df = usa[-1,-c(14:19)]


usa_df_info = data.frame(usa_df$name,usa_df$course.0,usa_df$course.1,usa_df$course.2,usa_df$course.3,usa_df$course.4)
usa_df_ingredients = data.frame(c(usa_df[2],usa_df[,3:11],usa_df[,14:27], usa_df[,30:34],usa_df[,36:48]))


usa_measures_clean = na.omit(usa_measures)
usa_df_clean = merge(usa,usa_measures)
usa_df_clean = na.omit(usa_df_clean)
usa_df_clean = usa_df_clean[,-19]

#PART 2-------------------

#sum of all courses in american meals
summary(usa_df_clean[,c(18,33,34,40,54)])
course_usa =summary(usa_df_clean[,c(18,33,34,40,54)])
summary.factor(usa_df_clean$course.1)

#remove duplicate recipes
usa_df_free = usa_df_clean %>% distinct()
#replace dots with dashes in names
colnames(usa_df_free) <- gsub("\\.","_",colnames(usa_df_free)) 


#PART 3----------------------------
#merge all individual ingredient columns into one column
tm_df_usa = unite(usa_df_free, ingredients, c(ingredients_0,ingredients_1,ingredients_2,ingredients_3,ingredients_4,ingredients_5,
                                              ingredients_6,ingredients_7,ingredients_8,ingredients_9,ingredients_10,ingredients_11,
                                              ingredients_12,ingredients_13,ingredients_14,ingredients_15,ingredients_16,ingredients_17,
                                              ingredients_18,ingredients_19,ingredients_20,ingredients_21,ingredients_22,ingredients_23,
                                              ingredients_24,ingredients_25,ingredients_26,ingredients_27,ingredients_28,ingredients_29,
                                              ingredients_30,ingredients_31,ingredients_32,ingredients_33,ingredients_34,ingredients_35,
                                              ingredients_36,ingredients_37,ingredients_38,ingredients_39,ingredients_40), remove=FALSE, sep = " ")
tm_df_usa = tm_df_usa[, c(1,9)]

#dataframe with courses
full_df_usa = data.frame(tm_df_usa, usa_df_free$course_0)  
names(full_df_usa)[3] = "course"


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

#--------for general topics
#c_l_ean_POS = sqldf("SELECT * FROM c_lean_POS WHERE upos NOT IN ('ADJ','ADV','PART','VERB','PROPN','INTJ','') ") #REMOVE Some Parts of Speech

#--------for adjective topics
c_l_ean_POS = sqldf("SELECT * FROM c_lean_POS WHERE upos='ADJ' ") #FILTER ONLY ADJECTIVES

#--------for noun topics
#c_l_ean_POS = sqldf("SELECT * FROM c_lean_POS WHERE upos='NOUN' ") #FILTER ONLY NOUNS

#plot after filtering PARTS OF SPEECH - LEMMA -------------------------------topics lemmas
top_terms_by_topic_LDA(c_l_ean_POS$lemma, number_of_topics = 5, plottitle = "Top Adjectives associated with Salads in American Cuisines")    #lemma topics plot
