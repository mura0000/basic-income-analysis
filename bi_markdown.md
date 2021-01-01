---
title: "BI_Analysis"
author: "Yasuaki Murai"
date: "1/17/2020"
output: 
  html_document:
    keep_md: yes
  md_document:
    variant: markdown_github
---

<style type="text/css">
.main-container {
  max-width: 1800px;
  margin-left: auto;
  margin-right: auto;
}
</style>





## Basic Income Analysis
The goal is to extract knowledge on what factors are possibly associated with the opinions on basic income, based on a dataset provided by Dalia Research's poll on basic income in European countries in 2016. Specifically, the aim of this project is to gain knowledge about the follwing topics:
<ol>
  <li>How awareness of BI is associated with socio-economic factors</li>
  <li>How the thoughts on the effects of BI is associated with socio-economic factors</li>
  <li>Whether there is an association between awareness and whether a person would vote for BI</li>
  <li>How awareness is related to thoughts on the effects </li>
  <li>What makes a respondent be for or be against BI </li>
</ol>

### Import libraries

```r
library(tidyverse)
library(MASS)
library(nnet)
library(scales)
library(GGally)
library(VGAM)
```

### Import data

```r
df = read.csv("basic_income_dataset_dalia.csv")
```

### Define Functions which are going to be used for data preprocesing and other tasks

```r
length_sum = function(lst){
  return(lst %>%
           sapply(function(x) return(length(x))) %>%
           sum())
}

string_to_lst = function(strings){
  lst = strings %>% sapply(function(st){
    return(strsplit(x=as.character(st), split = " | ", fixed=TRUE))
  })
  return(lst)
}

indicator_category = function(strings, category){
  lst = string_to_lst(strings)
  indicator = rep(0, length(lst))
  for(i in 1:length(lst)){
    if(category %in% lst[[i]]) indicator[i]=1 
  }
  return(indicator)
}

unique_strings = function(strings){
  lst = string_to_lst(strings)
  dat_vec = character(length = length_sum(lst))
  ind = 1
  for(i in 1:length(lst)){
    for(j in 1:length(lst[[i]])){
      dat_vec[ind] = lst[[i]][j]
      ind = ind + 1
    }
  }
  return(unique(dat_vec))
}

pie_chart_generator = function(var_name, facet_col="none", title){
  p = ggplot(df, aes_string(x=factor(1), fill=var_name)) +
    geom_bar(width=1, position = "fill") +
    labs(title=title) +
    coord_polar(theta="y") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0.1,0.1,0.1,0.1), "mm")
  ) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75),
                       labels=percent) 
  if(facet_col %in% colnames(df)){
    if(class(df[,facet_col])=="factor") p = p + facet_grid(reformulate(facet_col, "."))
  }
  return(p)
}

bar_prop_generator = function(fill_var, x_var, title, with_n=TRUE, facet_col="none"){
  p = ggplot(df, aes_string(x=x_var)) + 
      geom_bar(aes_string(fill=fill_var), position="fill") +
      geom_text(aes(label=..count..), stat = "count", position = "fill") +
      labs(title=title, y="prop")
  if(facet_col %in% colnames(df)){
    if(class(df[,facet_col])=="factor") p = p + facet_grid(reformulate(facet_col, "."))
  }
  return(p)
}

reason_plots = function(for_or_against = "for"){
  with_parenthesis = sprintf(" (%s)", for_or_against) 
  vars = colnames(df)[grep(with_parenthesis, colnames(df), fixed=TRUE)]
  reasons_stat = df[,vars] %>% 
    apply(2,sum) %>%
    data.frame()
  colnames(reasons_stat) = "n"
  reasons_stat$reason = rownames(reasons_stat) %>%
   sapply(function(x) return(sub(with_parenthesis, "", x, fixed=TRUE)))
  rownames(reasons_stat) = 1:nrow(reasons_stat)
  num_none_of_the_above = reasons_stat$n[reasons_stat$reason=="None of the above"]
  num_not_none_of_the_above = nrow(df) - num_none_of_the_above
  whether_there_are_reasons = 
    data.frame(response=c("yes", "no"),
              n = c(num_not_none_of_the_above, num_none_of_the_above))
  reasons_stat$prop = reasons_stat$n / num_not_none_of_the_above
  reasons_stat = reasons_stat[,c("reason", "n", "prop")]
  
  plot1 = whether_there_are_reasons %>%
    ggplot(aes(x=response, y=n)) +
    geom_bar(stat="identity") +
    labs(title=sprintf("Number of respondents who did/ did not gave at least one reason for being %s BI", for_or_against))
  
  plot2 = reasons_stat %>%
    ggplot(aes(x=reorder(reason, prop), y=prop)) +
    geom_bar(stat="identity") +
    labs(x="reason", title=sprintf("Reasons for being %s BI", for_or_against)) +
    coord_flip()
  
  return(list(plot1, plot2))
}
```

### Take a quick look at the data
#### Dimension of dataset

```r
dim(df)
```

```
## [1] 9649   15
```

#### Names of Columns

```r
colnames(df)
```

```
##  [1] "country_code"                                        "uuid"                                                "age"                                                 "gender"                                              "rural"                                               "dem_education_level"                                 "dem_full_time_job"                                   "dem_has_children"                                    "question_bbi_2016wave4_basicincome_awareness"        "question_bbi_2016wave4_basicincome_vote"             "question_bbi_2016wave4_basicincome_effect"           "question_bbi_2016wave4_basicincome_argumentsfor"     "question_bbi_2016wave4_basicincome_argumentsagainst" "age_group"                                           "weight"
```

Since columns related to opinions each have a long name, they can be shortened.

```r
c_name_responses = c("awareness", "vote", "effect", "arguments_for",
           "arguments_against")
colnames(df)[9:13] = c_name_responses
colnames(df)
```

```
##  [1] "country_code"        "uuid"                "age"                 "gender"              "rural"               "dem_education_level" "dem_full_time_job"   "dem_has_children"    "awareness"           "vote"                "effect"              "arguments_for"       "arguments_against"   "age_group"           "weight"
```

#### Take a look at the data

```r
for(i in 1:ncol(df)){
  cat("\n----", colnames(df)[i], "---- \n")
  for(j in 1:5){
     cat("   ", as.character(df[j, i]), "\n")
  }
}
```

```
## 
## ---- country_code ---- 
##     AT 
##     AT 
##     AT 
##     AT 
##     AT 
## 
## ---- uuid ---- 
##     f6e7ee00-deac-0133-4de8-0a81e8b09a82 
##     54f0f1c0-dda1-0133-a559-0a81e8b09a82 
##     83127080-da3d-0133-c74f-0a81e8b09a82 
##     15626d40-db13-0133-ea5c-0a81e8b09a82 
##     24954a70-db98-0133-4a64-0a81e8b09a82 
## 
## ---- age ---- 
##     61 
##     57 
##     32 
##     45 
##     41 
## 
## ---- gender ---- 
##     male 
##     male 
##     male 
##     male 
##     female 
## 
## ---- rural ---- 
##     rural 
##     urban 
##     urban 
##     rural 
##     urban 
## 
## ---- dem_education_level ---- 
##     no 
##     high 
##     NA 
##     high 
##     high 
## 
## ---- dem_full_time_job ---- 
##     no 
##     yes 
##     no 
##     yes 
##     yes 
## 
## ---- dem_has_children ---- 
##     no 
##     yes 
##     no 
##     yes 
##     yes 
## 
## ---- awareness ---- 
##     I know something about it 
##     I understand it fully 
##     I have heard just a little about it 
##     I have heard just a little about it 
##     I have heard just a little about it 
## 
## ---- vote ---- 
##     I would not vote 
##     I would probably vote for it 
##     I would not vote 
##     I would probably vote for it 
##     I would probably vote for it 
## 
## ---- effect ---- 
##     None of the above 
##     A basic income would not affect my work choices 
##     ‰Û_ gain additional skills 
##     ‰Û_ work less 
##     None of the above 
## 
## ---- arguments_for ---- 
##     None of the above 
##     It increases appreciation for household work and volunteering | It encourages financial independence and self-responsibility | It reduces anxiety about financing basic needs 
##     It creates more equality of opportunity 
##     It reduces anxiety about financing basic needs 
##     It reduces anxiety about financing basic needs 
## 
## ---- arguments_against ---- 
##     None of the above 
##     It might encourage people to stop working 
##     Foreigners might come to my country and take advantage of the benefit 
##     None of the above 
##     It is impossible to finance | It might encourage people to stop working | It increases dependence on the state 
## 
## ---- age_group ---- 
##     40_65 
##     40_65 
##     26_39 
##     40_65 
##     40_65 
## 
## ---- weight ---- 
##     1.105.534.474 
##     1.533.248.826 
##     0.9775919155 
##     1.105.534.474 
##     58.731.136
```

It seems values of response-related variables can be fixed so that the values are not too long and reflect, for arguments_for and argements_against columns, 
the fact that those are based on multiple choices of opinions.

### Preprecessing
#### Check for missingness

```r
df %>%
  apply(2, function(x) sum(is.na(x)))
```

```
##        country_code                uuid                 age              gender               rural dem_education_level   dem_full_time_job    dem_has_children           awareness                vote              effect       arguments_for   arguments_against           age_group              weight 
##                   0                   0                   0                   0                   0                 663                   0                   0                   0                   0                   0                   0                   0                   0                   0
```

Only education level has missing values. For now, let the missing values for this variable be treated as "unknown".


```r
df$dem_education_level = as.character(df$dem_education_level)
df$dem_education_level[is.na(df$dem_education_level)] = "unknown"
df$dem_education_level = factor(df$dem_education_level,
                                   levels=c("unknown", "no", "low", "medium", "high"), ordered=TRUE)
```


#### awareness
Based on the unique values in the awareness column, shown below, values can be renamed so they are not too long and have no spaces.

```r
unique(df$awareness)
```

```
## [1] I know something about it           I understand it fully               I have heard just a little about it I have never heard of it           
## Levels: I have heard just a little about it I have never heard of it I know something about it I understand it fully
```

```r
df$awareness = df$awareness %>%
  sapply(function(x){
    if(x == "I understand it fully"){return("fully_understand")}
    else if(x == "I know something about it"){return("know_something_about_it")}
    else if(x=="I have heard just a little about it"){return("heard_a_little")}
    else{return("never_heard")}
  }) %>%
  factor(levels=c("never_heard", "heard_a_little" , "know_something_about_it", "fully_understand" ), ordered=TRUE)
```

#### Vote

```r
###vote###
unique(df$vote)
```

```
## [1] I would not vote                 I would probably vote for it     I would vote against it          I would vote for it              I would probably vote against it
## Levels: I would not vote I would probably vote against it I would probably vote for it I would vote against it I would vote for it
```

```r
df$vote = df$vote %>%
sapply(function(x){
  if(x == "I would vote for it"){return("yes")}
  else if(x == "I would probably vote for it"){return("prob_yes")}
  else if(x=="I would probably vote against it"){return("prob_no")}
  else if(x=="I would vote against it"){return("no")}
  else{return("no_vote")}
}) %>%
  factor(levels=c("no_vote", "no", "prob_no", "prob_yes", "yes"))
```

#### effect
For this variable, the letters "‰Û_ " are included in some of the observations, which are not neccesary and can be removed.

```r
unique(df$effect)
```

```
## [1] None of the above                               A basic income would not affect my work choices ‰Û_ gain additional skills                      ‰Û_ work less                                   ‰Û_ work as a freelancer                        ‰Û_ do more volunteering work                   ‰Û_ stop working                                ‰Û_ spend more time with my family              ‰Û_ look for a different job                   
## Levels: ‰Û_ do more volunteering work ‰Û_ gain additional skills ‰Û_ look for a different job ‰Û_ spend more time with my family ‰Û_ stop working ‰Û_ work as a freelancer ‰Û_ work less A basic income would not affect my work choices None of the above
```

```r
df$effect = df$effect %>% sapply(function(x){
  x = as.character(x)
  sub("‰Û_ ", "", x, fixed=TRUE)
}) %>%
  factor()
```

#### arguments_for and arguments_against
For arguments_for and arguments_against, since the values are based on a multiple choice with each of the chosen options separated by the character "|", columns for each option can be created so that for each of the respondents, a cell in a column corresponding to one of the choice options takes 1 if that option is checked and 0 otherwise.

```r
###arguments_for###
arguments_for_unique = df$arguments_for %>% 
  unique_strings()
for(i in 1:length(arguments_for_unique)){
  df[paste(arguments_for_unique[i], "(for)")] = df$arguments_for %>% 
     indicator_category(category = paste(arguments_for_unique[i]))
}

###arguments_against###
arguments_against_unique = df$arguments_against %>% 
  unique_strings()
for(i in 1:length(arguments_against_unique)){
  df[paste(arguments_against_unique[i], "(against)")] = df$arguments_against %>% 
    indicator_category(category = paste(arguments_against_unique[i]))
}
```

Now, the columns arguments_for and arguments_against can be deleted

```r
df = df[setdiff(colnames(df), c("arguments_for", "arguments_against"))]
```

### Fix factor orderings
For factor variables with more than 2 levels, levels can be possibly reordered.

#### Create country column
Regarding the country, since there is only a variable for country code, creating a culumn for the country name based on the code could help in making visualization clear in meaning.

```r
df$country_code %>% unique()
```

```
##  [1] AT BE BG CY CZ DE DK EE ES FI FR GB GR HR HU IE IT LT LU LV MT NL PL PT RO SE SI SK
## Levels: AT BE BG CY CZ DE DK EE ES FI FR GB GR HR HU IE IT LT LU LV MT NL PL PT RO SE SI SK
```

```r
map_code_name = list(
  AT = "Austria", BE = "Belgium", BG = "Bulgaria",
  CY = "Cyprus", CZ = "Czechia", DE = "Germany",
  DK = "Denamrk", EE = "Estonia", ES = "Spain",
  FI = "Finland", FR = "France", GB = "Britain",
  GR = "Greece", HR = "Croatia", HU = "Hungary",
  IE = "Ireland", IT = "Italy", LT = "Lithuania",
  LU = "Luxembourg", LV = "Latvia", MT = "Malta",
  NL = "Netherlands", PL = "Poland", PT = "Portugal",
  RO = "Romania", SE = "Sweden", SI = "Slovenia",
  SK = "Slovenia"
)
df$country = sapply(df$country_code, function(x)
  return(map_code_name[[as.character(x)]])) %>% 
  as.factor()
```

In addition to this, countries can be divided based on region.

```r
map_code_region = list(
  AT = "Central", BE = "Western", BG = "Southeastern",
  CY = "Southeastern", CZ = "Central", DE = "Central",
  DK = "Nordic", EE = "Eastern", ES = "Southern",
  FI = "Nordic", FR = "Western", GB = "Western",
  GR = "Southeastern", HR = "Southeastern", HU = "Central",
  IE = "Western", IT = "Southern", LT = "Eastern",
  LU = "Western", LV = "Eastern", MT = "Southern",
  NL = "Western", PL = "Central", PT = "Southern",
  RO = "Southeastern", SE = "Nordic", SI = "Southeastern",
  SK = "Southeastern"
)
df$region = sapply(df$country_code, function(x)
  return(map_code_region[[as.character(x)]])) %>% 
  as.factor()
```

Now take a look at the first six rows again

```r
for(i in 1:ncol(df)){
  cat("----", colnames(df)[i], "---- \n  ")
  for(j in 1:5){
     cat(as.character(df[j, i]), ",")
  }
  cat(as.character(df[6, i]), "\n\n")
}
```

```
## ---- country_code ---- 
##   AT ,AT ,AT ,AT ,AT ,AT 
## 
## ---- uuid ---- 
##   f6e7ee00-deac-0133-4de8-0a81e8b09a82 ,54f0f1c0-dda1-0133-a559-0a81e8b09a82 ,83127080-da3d-0133-c74f-0a81e8b09a82 ,15626d40-db13-0133-ea5c-0a81e8b09a82 ,24954a70-db98-0133-4a64-0a81e8b09a82 ,28583060-dbe2-0133-ff9c-0a81e8b09a82 
## 
## ---- age ---- 
##   61 ,57 ,32 ,45 ,41 ,26 
## 
## ---- gender ---- 
##   male ,male ,male ,male ,female ,female 
## 
## ---- rural ---- 
##   rural ,urban ,urban ,rural ,urban ,rural 
## 
## ---- dem_education_level ---- 
##   no ,high ,unknown ,high ,high ,high 
## 
## ---- dem_full_time_job ---- 
##   no ,yes ,no ,yes ,yes ,yes 
## 
## ---- dem_has_children ---- 
##   no ,yes ,no ,yes ,yes ,no 
## 
## ---- awareness ---- 
##   know_something_about_it ,fully_understand ,heard_a_little ,heard_a_little ,heard_a_little ,fully_understand 
## 
## ---- vote ---- 
##   no_vote ,prob_yes ,no_vote ,prob_yes ,prob_yes ,no 
## 
## ---- effect ---- 
##   None of the above ,A basic income would not affect my work choices ,gain additional skills ,work less ,None of the above ,A basic income would not affect my work choices 
## 
## ---- age_group ---- 
##   40_65 ,40_65 ,26_39 ,40_65 ,40_65 ,26_39 
## 
## ---- weight ---- 
##   1.105.534.474 ,1.533.248.826 ,0.9775919155 ,1.105.534.474 ,58.731.136 ,0.6888730839 
## 
## ---- None of the above (for) ---- 
##   1 ,0 ,0 ,0 ,0 ,0 
## 
## ---- It increases appreciation for household work and volunteering (for) ---- 
##   0 ,1 ,0 ,0 ,0 ,0 
## 
## ---- It encourages financial independence and self-responsibility (for) ---- 
##   0 ,1 ,0 ,0 ,0 ,1 
## 
## ---- It reduces anxiety about financing basic needs (for) ---- 
##   0 ,1 ,0 ,1 ,1 ,1 
## 
## ---- It creates more equality of opportunity (for) ---- 
##   0 ,0 ,1 ,0 ,0 ,0 
## 
## ---- It reduces bureaucracy and administrative expenses (for) ---- 
##   0 ,0 ,0 ,0 ,0 ,0 
## 
## ---- It increases solidarity, because it is funded by everyone (for) ---- 
##   0 ,0 ,0 ,0 ,0 ,0 
## 
## ---- None of the above (against) ---- 
##   1 ,0 ,0 ,1 ,0 ,0 
## 
## ---- It might encourage people to stop working (against) ---- 
##   0 ,1 ,0 ,0 ,1 ,1 
## 
## ---- Foreigners might come to my country and take advantage of the benefit (against) ---- 
##   0 ,0 ,1 ,0 ,0 ,0 
## 
## ---- It is impossible to finance (against) ---- 
##   0 ,0 ,0 ,0 ,1 ,1 
## 
## ---- It increases dependence on the state (against) ---- 
##   0 ,0 ,0 ,0 ,1 ,0 
## 
## ---- It is against the principle of linking merit and reward (against) ---- 
##   0 ,0 ,0 ,0 ,0 ,1 
## 
## ---- Only the people who need it most should get something from the state (against) ---- 
##   0 ,0 ,0 ,0 ,0 ,0 
## 
## ---- country ---- 
##   Austria ,Austria ,Austria ,Austria ,Austria ,Austria 
## 
## ---- region ---- 
##   Central ,Central ,Central ,Central ,Central ,Central
```
Now, the dataset is in a more analyzable form.

## EDA

### Respondents' countries

```r
df %>% 
  group_by(by=country) %>%
  summarise(n=n()) %>% 
  ggplot(aes(x=reorder(by, n), y=n)) +
  geom_bar(stat="identity") +
  xlab("Country") +
  coord_flip() 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

![](bi_markdown_files/figure-html/unnamed-chunk-18-1.png)<!-- -->



### education levels

```r
df %>% 
  group_by(by=dem_education_level) %>%
  summarise(n=n()) %>% 
  ggplot(aes(x=by, y=n)) +
  geom_bar(stat="identity") +
  xlab("Education Level") 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

![](bi_markdown_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

### education levels and full time job for each gender

```r
bar_prop_generator("dem_full_time_job", "gender", "dem_full_time_job", with_n=TRUE, facet_col="none")
```

![](bi_markdown_files/figure-html/unnamed-chunk-20-1.png)<!-- -->


### Awareness

```r
df %>% 
  group_by(by=awareness) %>%
  summarise(n=n()) %>% 
  ggplot(aes(x=by, y=n)) +
  geom_bar(stat="identity") +
  xlab("Awareness") 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

![](bi_markdown_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

Based on the fact that the majority of the survey respondents fall in either the category "fully_understand"or "know_something_about_it", it seems that, despite the possibility that the awareness is highly dependent on other variables, including social status and political and ecnonomic attributes of a region, BI is overall a well-known idea at least to some extent.

### Vote

```r
df %>% 
  group_by(by=vote) %>%
  summarise(n=n()) %>% 
  ggplot(aes(x=by, y=n)) +
  geom_bar(stat="identity") +
  xlab("vote") 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

![](bi_markdown_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

It can be seen that a majority of respondents of the survey at least probably would vote for BI. Again, this could be dependent on the social status of a respondent, political or economic attributes of the region they reside in, etc.

### Reasons for for

```r
for_plots = reason_plots()
for_plots[[1]]
```

![](bi_markdown_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

```r
for_plots[[2]]
```

![](bi_markdown_files/figure-html/unnamed-chunk-23-2.png)<!-- -->

From the above plots, a number of people seem to think BI reduces financial anxiety.

### Reasons for against

```r
against_plots = reason_plots("against")
against_plots[[1]]
```

![](bi_markdown_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

```r
against_plots[[2]]
```

![](bi_markdown_files/figure-html/unnamed-chunk-24-2.png)<!-- -->

From the above plots, it can be seen that for vast majority of people, there is at least one reason for being against BI in the answer choices.

### Effect

```r
df %>%
  group_by(by=effect) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(by, n), y=n)) +
  geom_bar(stat="identity") +
  labs(title="Opinions on effects",
       x="choice") +
  coord_flip() 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

![](bi_markdown_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

It seems that many of the respondents think BI would not lead to job loss, and few people think BI would make them stop working.

## Answering quastions
### 1. How social status is related to whether a person knows about BI
Whether a respondent knows about BI corresponds to the variable awareness. The variable awareness can be considered an ordinal variable with the categories, "never_heard", "heard_a_little", "know_something_about_it", and "fully_understand".
Firstly, let's explore this topic through grapshs.

### Graphical Analysis
#### Vs regrion

```r
bar_prop_generator("awareness", "region", "awareness vs education level")
```

![](bi_markdown_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

##### Plot awareness vs eduction level

```r
bar_prop_generator("awareness", "dem_education_level", "awareness vs education level")
```

![](bi_markdown_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

Despite slight differences, people who have a high education level on average are more knowledgeable in BI. Let's partition by whether the resident is from a rural or urban area.


```r
bar_prop_generator("awareness", "dem_education_level", "awareness vs education level by rural", 
                   facet_col = "rural")
```

![](bi_markdown_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

It seems the trend seems overall not that different, and that whether a person lives in a rural area does not confound the relationship between the education level and the level of awareness. For urban resiednts, however, a respondent from an urban area with "no" education level are more likely to not have heard about BI than a same type of respondent from a rural area. 

##### plot vs age group

```r
bar_prop_generator("awareness", "age_group", "awareness vs age group")
```

![](bi_markdown_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

Slightly, people belonging to older age groups seem to tend to be more knowledgeable in BI.

#### Modelling
Since the level of awareness can be considered an ordered categorical variable, a cumulative logit model can be suitable. The coefficients of the model can be too complex to interpret. One of the possible ways to mitigate this possible problem is to have a proportional odds assumption, in which the coefficient for each level only differs by the intercept term. A model with this assumption and without it can be compared based on a certain metric. Since the models are nested, Likelihood ratio test can be used for comparing the models.

##### Multinomial logit model

```r
reg_formula_generator = function(output, predictors){
  
  formula_right = paste(predictors, collapse=" + ")
  formula = paste(output, formula_right, sep = " ~ ")
  
  return(as.formula(formula))
}

explanatory_vars = c("age_group", "region", "gender", "rural", 
                     "dem_education_level", "dem_full_time_job", "dem_has_children")
output_var = "awareness"

##Fit models##
#prop odds
mod_po = vglm(reg_formula_generator(output_var, explanatory_vars),
     data=df, family=cumulative(parallel = TRUE))

#non-prop-odds
mod_npo = vglm(reg_formula_generator(output_var, explanatory_vars),
     data=df, family=cumulative())

summary(mod_po)
```

```
## 
## Call:
## vglm(formula = reg_formula_generator(output_var, explanatory_vars), 
##     family = cumulative(parallel = TRUE), data = df)
## 
## Coefficients: 
##                         Estimate Std. Error z value Pr(>|z|)    
## (Intercept):1         -8.814e-01  6.435e-02 -13.697  < 2e-16 ***
## (Intercept):2          4.086e-01  6.325e-02   6.461 1.04e-10 ***
## (Intercept):3          1.988e+00  6.638e-02  29.947  < 2e-16 ***
## age_group26_39        -1.063e-01  5.728e-02  -1.856 0.063445 .  
## age_group40_65        -1.948e-01  5.588e-02  -3.486 0.000490 ***
## regionEastern         -4.583e-01  1.819e-01  -2.519 0.011763 *  
## regionNordic           2.052e-01  9.607e-02   2.136 0.032712 *  
## regionSoutheastern    -5.343e-01  6.988e-02  -7.646 2.07e-14 ***
## regionSouthern        -4.672e-01  5.130e-02  -9.108  < 2e-16 ***
## regionWestern         -1.678e-01  4.753e-02  -3.530 0.000416 ***
## gendermale            -2.705e-01  3.816e-02  -7.089 1.35e-12 ***
## ruralurban            -2.948e-02  4.158e-02  -0.709 0.478316    
## dem_education_level.L -5.476e-01  6.095e-02  -8.986  < 2e-16 ***
## dem_education_level.Q -2.637e-01  5.517e-02  -4.779 1.76e-06 ***
## dem_education_level.C -4.105e-05  7.084e-02  -0.001 0.999538    
## dem_education_level^4 -6.363e-02  5.943e-02  -1.071 0.284319    
## dem_full_time_jobyes  -1.917e-01  4.133e-02  -4.638 3.52e-06 ***
## dem_has_childrenyes   -9.002e-02  4.182e-02  -2.153 0.031343 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Names of linear predictors: logitlink(P[Y<=1]), logitlink(P[Y<=2]), logitlink(P[Y<=3])
## 
## Residual deviance: 25488.14 on 28929 degrees of freedom
## 
## Log-likelihood: -12744.07 on 28929 degrees of freedom
## 
## Number of Fisher scoring iterations: 4 
## 
## No Hauck-Donner effect found in any of the estimates
## 
## 
## Exponentiated coefficients:
##        age_group26_39        age_group40_65         regionEastern          regionNordic    regionSoutheastern        regionSouthern         regionWestern            gendermale            ruralurban dem_education_level.L dem_education_level.Q dem_education_level.C dem_education_level^4  dem_full_time_jobyes   dem_has_childrenyes 
##             0.8991412             0.8230045             0.6323766             1.2277317             0.5860514             0.6267341             0.8455317             0.7629779             0.9709474             0.5783076             0.7682349             0.9999589             0.9383483             0.8255612             0.9139162
```

```r
summary(mod_npo)
```

```
## 
## Call:
## vglm(formula = reg_formula_generator(output_var, explanatory_vars), 
##     family = cumulative(), data = df)
## 
## Coefficients: 
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept):1           -0.907331   0.086356 -10.507  < 2e-16 ***
## (Intercept):2            0.396330   0.070752   5.602 2.12e-08 ***
## (Intercept):3            1.940820   0.087815  22.101  < 2e-16 ***
## age_group26_39:1        -0.209877   0.081458  -2.576 0.009981 ** 
## age_group26_39:2        -0.110425   0.064840  -1.703 0.088564 .  
## age_group26_39:3         0.010784   0.075817   0.142 0.886893    
## age_group40_65:1        -0.399291   0.080405  -4.966 6.84e-07 ***
## age_group40_65:2        -0.240885   0.063322  -3.804 0.000142 ***
## age_group40_65:3         0.004428   0.073970   0.060 0.952262    
## regionEastern:1         -0.109203   0.274634  -0.398 0.690902    
## regionEastern:2         -0.255850   0.208104  -1.229 0.218909    
## regionEastern:3         -0.840398   0.213736  -3.932 8.43e-05 ***
## regionNordic:1           0.548535   0.124586   4.403 1.07e-05 ***
## regionNordic:2           0.272029   0.108174   2.515 0.011912 *  
## regionNordic:3          -0.302483   0.128790  -2.349 0.018841 *  
## regionSoutheastern:1     0.005392   0.104537   0.052 0.958867    
## regionSoutheastern:2    -0.507431   0.082082  -6.182 6.33e-10 ***
## regionSoutheastern:3    -0.792761   0.086340  -9.182  < 2e-16 ***
## regionSouthern:1        -0.377610   0.082432  -4.581 4.63e-06 ***
## regionSouthern:2        -0.457863   0.059054  -7.753 8.96e-15 ***
## regionSouthern:3        -0.621233   0.067804  -9.162  < 2e-16 ***
## regionWestern:1          0.016697   0.069863   0.239 0.811104    
## regionWestern:2         -0.130393   0.053780  -2.425 0.015326 *  
## regionWestern:3         -0.375811   0.065625  -5.727 1.02e-08 ***
## gendermale:1            -0.243999   0.057475  -4.245 2.18e-05 ***
## gendermale:2            -0.327019   0.043624  -7.496 6.57e-14 ***
## gendermale:3            -0.226933   0.049518  -4.583 4.59e-06 ***
## ruralurban:1            -0.028926   0.061250  -0.472 0.636738    
## ruralurban:2             0.012807   0.047373   0.270 0.786904    
## ruralurban:3            -0.081588   0.055409  -1.472 0.140895    
## dem_education_level.L:1 -0.910586   0.080838 -11.264  < 2e-16 ***
## dem_education_level.L:2 -0.503250   0.068411  -7.356 1.89e-13 ***
## dem_education_level.L:3 -0.276324   0.082927  -3.332 0.000862 ***
## dem_education_level.Q:1 -0.288637   0.073598  -3.922 8.79e-05 ***
## dem_education_level.Q:2 -0.299150   0.062112  -4.816 1.46e-06 ***
## dem_education_level.Q:3 -0.224983   0.075388  -2.984 0.002842 ** 
## dem_education_level.C:1  0.064748   0.089594   0.723 0.469877    
## dem_education_level.C:2 -0.001765   0.079479  -0.022 0.982286    
## dem_education_level.C:3 -0.149693   0.098474  -1.520 0.128479    
## dem_education_level^4:1 -0.065850   0.076253  -0.864 0.387824    
## dem_education_level^4:2 -0.017320   0.066614  -0.260 0.794865    
## dem_education_level^4:3 -0.070612   0.082352  -0.857 0.391204    
## dem_full_time_jobyes:1  -0.099675   0.061456  -1.622 0.104826    
## dem_full_time_jobyes:2  -0.188871   0.046873  -4.029 5.59e-05 ***
## dem_full_time_jobyes:3  -0.236203   0.054723  -4.316 1.59e-05 ***
## dem_has_childrenyes:1   -0.048784   0.064343  -0.758 0.448334    
## dem_has_childrenyes:2   -0.101937   0.048005  -2.123 0.033716 *  
## dem_has_childrenyes:3   -0.084465   0.053850  -1.569 0.116759    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Names of linear predictors: logitlink(P[Y<=1]), logitlink(P[Y<=2]), logitlink(P[Y<=3])
## 
## Residual deviance: 25320.77 on 28899 degrees of freedom
## 
## Log-likelihood: -12660.38 on 28899 degrees of freedom
## 
## Number of Fisher scoring iterations: 5 
## 
## No Hauck-Donner effect found in any of the estimates
## 
## 
## Exponentiated coefficients:
##        age_group26_39:1        age_group26_39:2        age_group26_39:3        age_group40_65:1        age_group40_65:2        age_group40_65:3         regionEastern:1         regionEastern:2         regionEastern:3          regionNordic:1          regionNordic:2          regionNordic:3    regionSoutheastern:1    regionSoutheastern:2    regionSoutheastern:3        regionSouthern:1        regionSouthern:2        regionSouthern:3         regionWestern:1         regionWestern:2         regionWestern:3            gendermale:1            gendermale:2            gendermale:3            ruralurban:1            ruralurban:2            ruralurban:3 dem_education_level.L:1 dem_education_level.L:2 dem_education_level.L:3 dem_education_level.Q:1 dem_education_level.Q:2 dem_education_level.Q:3 dem_education_level.C:1 dem_education_level.C:2 dem_education_level.C:3 dem_education_level^4:1 dem_education_level^4:2 dem_education_level^4:3  dem_full_time_jobyes:1  dem_full_time_jobyes:2 
##               0.8106839               0.8954539               1.0108423               0.6707956               0.7859317               1.0044381               0.8965486               0.7742581               0.4315388               1.7307154               1.3126252               0.7389810               1.0054061               0.6020402               0.4525933               0.6854980               0.6326340               0.5372818               1.0168374               0.8777501               0.6867323               0.7834887               0.7210703               0.7969742               0.9714883               1.0128889               0.9216515               0.4022883               0.6045627               0.7585674               0.7492844               0.7414480               0.7985301               1.0668900               0.9982369               0.8609726               0.9362714               0.9828296               0.9318237               0.9051316               0.8278934 
##  dem_full_time_jobyes:3   dem_has_childrenyes:1   dem_has_childrenyes:2   dem_has_childrenyes:3 
##               0.7896206               0.9523865               0.9030863               0.9190036
```

```r
anova(mod_po, mod_npo, type=1)
```

```
## Analysis of Deviance Table
## 
## Model 1: awareness ~ age_group + region + gender + rural + dem_education_level + 
##     dem_full_time_job + dem_has_children
## Model 2: awareness ~ age_group + region + gender + rural + dem_education_level + 
##     dem_full_time_job + dem_has_children
##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
## 1     28929      25488                          
## 2     28899      25321 30   167.38 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


Since the goal is to see what factors are related to the level of awareness, despite the fact that causal relationship cannot be guaranteed, another way to possibly infer this is to fit a Random Forest classifier and see the importance measure of each explanatory variable considered, based on the mean decrease in Gini coef. 

```r
library(randomForest)
```

```
## randomForest 4.6-14
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```r
explanatory_vars = c("age_group", "region", "gender", "rural", 
                     "dem_education_level", "dem_full_time_job", "dem_has_children")
rf = randomForest::randomForest(x=df[explanatory_vars], y=df$awareness, ntree=5000)
rf$importance
```

```
##                     MeanDecreaseGini
## age_group                   63.92106
## region                     120.73942
## gender                      39.06537
## rural                       38.82371
## dem_education_level        112.59750
## dem_full_time_job           39.35872
## dem_has_children            35.87831
```

Based on the importance measures above, education level and region might be related to the level of awareness more than other variables in the dataset. 

### 2. How notion on the effects of BI is associated with social status
#### Graphical Analysis
##### Plot 

```r
bar_prop_generator("effect", "age_group", "effect vs age group", facet_col="gender")
```

![](bi_markdown_files/figure-html/unnamed-chunk-32-1.png)<!-- -->


```r
bar_prop_generator("effect", "dem_education_level", "effect vs age group")
```

![](bi_markdown_files/figure-html/unnamed-chunk-33-1.png)<!-- -->


##### Modelling
Since there are many choices for the effects, a multinomial logit model would be too complex to interpret. 


```r
explanatory_vars = c("age_group", "region", "gender", "rural", 
                     "dem_education_level", "dem_full_time_job", "dem_has_children")
library(randomForest())
rf_effect = randomForest(x= df[explanatory_vars], y=df$effect, ntree = 1000)
rf_effect$importance
```

```
##                     MeanDecreaseGini
## age_group                   73.43267
## region                     112.68672
## gender                      43.14444
## rural                       39.40985
## dem_education_level        106.18290
## dem_full_time_job           38.77812
## dem_has_children            38.73170
```

### 3. Whether there is an association between awareness and whether a person would vote for BI
#### graphical Analysis

```r
bar_prop_generator("vote", "awareness", "vote vs awareness")
```

![](bi_markdown_files/figure-html/unnamed-chunk-35-1.png)<!-- -->

The above plot shows that the more knowledge one has about BI, the more likely he would vote for BI. It cna be inferred from the above plot that the vote and awareness are not independent or homogeneous, let's do Chi square test for independence

#### Chi-square test

```r
chisq.test(x=df$vote, y=df$awareness)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  df$vote and df$awareness
## X-squared = 1519.5, df = 12, p-value < 2.2e-16
```

#### Correspondence analysis

```r
library(FactoMineR)
library(factoextra)
```

```
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
```

```r
table_awareness_vote = table(df$awareness, df$vote)
CA(table_awareness_vote)
```

![](bi_markdown_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

```
## **Results of the Correspondence Analysis (CA)**
## The row variable has  4  categories; the column variable has 5 categories
## The chi square of independence between the two variables is equal to 1519.526 (p-value =  2.324287e-318 ).
## *The results are available in the following objects:
## 
##    name              description                   
## 1  "$eig"            "eigenvalues"                 
## 2  "$col"            "results for the columns"     
## 3  "$col$coord"      "coord. for the columns"      
## 4  "$col$cos2"       "cos2 for the columns"        
## 5  "$col$contrib"    "contributions of the columns"
## 6  "$row"            "results for the rows"        
## 7  "$row$coord"      "coord. for the rows"         
## 8  "$row$cos2"       "cos2 for the rows"           
## 9  "$row$contrib"    "contributions of the rows"   
## 10 "$call"           "summary called parameters"   
## 11 "$call$marge.col" "weights of the columns"      
## 12 "$call$marge.row" "weights of the rows"
```




```r
#head(df)
```

### 4. How awareness is related to thoughts on the effects
#### Graphical Analysis

```r
bar_prop_generator("effect", "awareness", "vote vs awareness")
```

![](bi_markdown_files/figure-html/unnamed-chunk-39-1.png)<!-- -->

It can be seen that those who says they understand BI tend to have the opinion that BI would not affect their work choices. Based on this observations, people knowledgeable about BI might tend to be more optimistic about the BI's effects.

#### Chi square test

```r
chisq.test(x=df$effect, y=df$awareness)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  df$effect and df$awareness
## X-squared = 728.72, df = 24, p-value < 2.2e-16
```


#### Correspondence analysis

```r
table_awareness_effect = table(df$effect, df$awareness)
CA(table_awareness_effect)
```

![](bi_markdown_files/figure-html/unnamed-chunk-41-1.png)<!-- -->

```
## **Results of the Correspondence Analysis (CA)**
## The row variable has  9  categories; the column variable has 4 categories
## The chi square of independence between the two variables is equal to 728.718 (p-value =  2.237916e-138 ).
## *The results are available in the following objects:
## 
##    name              description                   
## 1  "$eig"            "eigenvalues"                 
## 2  "$col"            "results for the columns"     
## 3  "$col$coord"      "coord. for the columns"      
## 4  "$col$cos2"       "cos2 for the columns"        
## 5  "$col$contrib"    "contributions of the columns"
## 6  "$row"            "results for the rows"        
## 7  "$row$coord"      "coord. for the rows"         
## 8  "$row$cos2"       "cos2 for the rows"           
## 9  "$row$contrib"    "contributions of the rows"   
## 10 "$call"           "summary called parameters"   
## 11 "$call$marge.col" "weights of the columns"      
## 12 "$call$marge.row" "weights of the rows"
```

### 5. What are the possible reasons for a respondent to not vote for BI
The columns for the reasons for being for/against the BI along with the variable vote.
In order to simplify analysis, the levels for vote can be collapsed into two levels, "Yes", which includes the levels "yes" and "prob_yes" in the original formulation, and "No", which includes "no" and "prob_no".

Since the goal is to see what factors possibly would make a person vote for or against BI, not to make a model for precise predictions, the rows with the label for the vote "no_vote" can be discarded

There can be loss of information by doing the above things. However, this leads to the reduction of number of the levels down to 2, which then enables us to fit a binary logistic regression, in which the interpretation of coefficients is much simpler than when there are more than 2 levels.




