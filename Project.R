#'---
#' title: "TSCI 5050: Mock Data Analysis"
#' author: 'Kristi Dietert ^1^, TSCI 5050 Class'
#' abstract: |
#'  | Analysis of stock esophageal cancer clinical data that includes tobacco use, alcohol use and the prevalence of the cancer. 
#'  
#'  
#' documentclass: article
#' css: "production.css"
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     code_folding: show
#' ---
#'
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# init ----
# This part does not show up in your rendered report, only in the script,
# because we are using regular comments instead of #' comments
debug <- 0;
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0);
library(ggplot2);#visualization&plotting
library(dplyr);#data manipulation/processing
library(GGally);#adds to the capabilities of ggplot -- more types of plots
library(rio);#ability to save dataframes in different formats (word/excel etc)
library(pander);#reverse engineer for table making
library(printr);
library(broom); #summarize data 
options(max.print=42); #sets the limit of how many lines are printed in console
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);
whatisthis <- function(xx){
  list(class=class(xx),info=c(mode=mode(xx),storage.mode=storage.mode(xx)
                              ,typeof=typeof(xx)))};

#' # Importing Data
#' 
#' 
#' 
data(esoph)


#' # Summarize Data
#' 
#' 
#' 
#' 
summary(esoph) %>% pander()
?esoph #help file for this dataset
?'+' #help file for arithmetic operators

#' # Data Visualization
#' 
#' 
#'
#' 
ggpairs(esoph)

#' # Data Analysis
#' 
#'
#' 
model1 <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp * alcgp,
              data = esoph, family = binomial())
summary(model1)
tidy(model1)
