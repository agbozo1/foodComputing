<<<<<<< HEAD
﻿# foodComputing
A study by colleague Maija Kale (University of Latvia) & Ebenezer Agbozo (Ural Federal University) on Complexity of food consumption: deconstructing recipes.
=======
# foodComputing
A study by colleague Maija Kale (University of Latvia) - <https://github.com/Maijyta> & Ebenezer Agbozo (Ural Federal University) on Complexity of food consumption: deconstructing recipes.
>>>>>>> 5cbbebb034268735edfaa2c1210b3072f2269aab
This algorithm is the basis for our presentation at the *14th International Symposium of Cognition, Logic and Communication
"Linking Senses: Cross-Modality in Perceptual Domains across Cultures"*

### Code is run in R
The following libraries are essential to running the algorithms
* library(tidyverse)
* library(tidytext) 
* library(topicmodels) 
* library(tm)
* library(SnowballC) 
* library(dplyr)
* library(ggplot2)
* library(dplyr)
* library(stringr)
* library(sqldf)
* library(tidyr)
* library(udpipe)

###Running the code:
1. Mexican Cusisines (Mexican.json)
2. American Cusisines (American.json) 

<<<<<<< HEAD

Publication: http://ceur-ws.org/Vol-2612/paper4.pdf 
arXiv: https://arxiv.org/abs/2007.05552

=======
The called function (i.e. the last line of code) is where the magic happens
```
top_terms_by_topic_LDA(c_l_ean_POS$lemma, number_of_topics = 5, plottitle = "Top Adjectives associated with Salads in Mexican Cuisines")  
#lemma topics plot
```
>>>>>>> 5cbbebb034268735edfaa2c1210b3072f2269aab
Dataset source : 
Yummly 66k (*W. Min, B. K. Bao, S. Mei, Y. Zhu, Y. Rui, and S. Jiang. 2018a. You are what you eat: Exploring rich recipe information for cross-region food analysis. IEEE
Transactions on Multimedia 20, 4 (2018), 950–964.*) 
http://isia.ict.ac.cn/dataset/Yummly-66K.html. 


http://123.57.42.89/FoodComputing__Dataset.html
https://github.com/minweiqing/You-Are-What-You-Eat-Exploring-Rich-Recipe-Information-for-Cross-Region-Food-Analysis