#'---
#' title: "TSCI 5050: Introduction to Data Science"
#' author: 'Author One ^1^, Author Two ^1^'
#' abstract: |
#'  | Provide a summary of objectives, study design, setting, participants,
#'  | sample size, predictors, outcome, statistical analysis, results,
#'  | and conclusions.
#' documentclass: article
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
library(broom)
options(max.print=42); #sets the limit of how many lines are printed in console
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);
whatisthis <- function(xx){
  list(class=class(xx),info=c(mode=mode(xx),storage.mode=storage.mode(xx)
                              ,typeof=typeof(xx)))};
# R basic syntax ----
#'
#' # R basic syntax
#'
#' ## Assignment, Variables, and Data types.
#'
#' To store a value as variable `foo` in R the convention is to use the
#' `<-` operator, not `=`. This makes it easier to distinguish stand-alone
#' expressions from function arguments.

#+ assignment_operator
foo <- 365;
bar <- foo;
bar <- foo <- 365;
#' It's not a formal rule, it's rarely even written down, but `foo`, `baz`,
#' `bat`, etc. are throw-away variables people use for testing. If you need more
#' test variables, just make up three letter ones that start with `b`.
#' If you see one of those in a script you're reviewing it means it is left over
#' from when that code was being debugged. Example code shared with this class
#' will usually use `foo` and friends to represent the parts of an expression
#' that should be replaced with whatever values you are using instead of being
#' used literally. Shorter than having to write `YOURFUNCTIONHERE` or
#' `YOURARGUMENTHERE` each time.
#'
#' This is not specific to R-- it's just a little quirk of programming culture
#' in general. A quirk with a practical consequence: _never use `foo`, `bar`,
#' `baz`, `bat`, etc. in your production (i.e. finalized) code_ because
#' otherwise you or somebody else debugging your code will attempt to use those
#' names as test variables and in some situations this could overwrite the
#' existing variables!
#'
#'
#' ## Simple data types
#'
#' Numeric. You can do arithmetic on them: `+`, `-`, `/`, `*`, `^`, `log()`,
#' `exp()`, `sqrt()`

#+ assignment_numeric
print(foo <- 2 + 2); foo;
print(baz <- 5-2); baz;
print(bat <- 72/6); bat;
print(bar<-5*5); bar;
print(var<-log(67)); var;
whatisthis(bar);
log(((var^2)*(foo+baz)/bar)-bat) 
#' Character strings. Create these by wrapping single (`'`) or double (`"`)
#' quotes around the value.

#+ assignment_string
fry <- "Do Not Panic";
j <-'Do Not Panic';
philip <- "Don't Panic";
professor <- 'The "Heart of Gold" comes equipped with heated leather seats and an infinite improbability drive';
leela <- '42';

#' Logical values are `TRUE` or `FALSE`. Can also be created using `>`, `<`,
#' `==`, `!=`, `>=`, `<=`

#+ assignment_logical
fry == philip
fry != leela

#' Missing values are represented by `NA` (no quotes for any of these). Null
#' values are _not_ the same as missing and they are represented by `NULL`. In
#' some circumstances you might also run into `Inf`, `-Inf`, and `NaN`. These
#' often indicate errors somewhere else in your code.

#' Dates and times. Can be created with the `Sys.Date()` or `Sys.time()`
#' functions or converted from a character string using `as.Date()`.

#+ assignment_datetime
Sys.Date()
Sys.time()
as.Date(70467, origin = "1986-08-04") #this is in the format of number of days since the date given in m/d/y

#' Factors are basically integers that have labels. They are a human-readable
#' alternative to using integer codes for discrete data. These will make more
#' sense after we talk about vectors in the next section.

#+ factor_example

#+ assignment_wierd

#' ## Data structures
#'
#' Of course we usually need to work with variables bundled together, not single
#' values.
#'
#' ### Vectors
#'
#' The default data structure in R is a `vector`. You create one with the `c()`
#' command with any number of arguments. All items in a vector have to be the
#' same type.

#+ vectors_c
print(foo <- c(56,78,34,97,2,86))
print(baz <- c(34,23,94,3,12,53))

#' Since the default data structure in R is a `vector`, if you
#' create some sort of simple value you are creating a `vector` even if you are
#' not using `c()`... it just happens to be a `vector` of length 1. These
#' are identical, and both return `1` when used with the `length()` function.

#+ vectors_length1
length(foo)
#' If you want to create a sequence of consecutive integers, you can use the `:`
#' operator.

#+ vectors_sequence
25:76
65:38
-32:12
seq_len(12)
#' In most other languages, you need to use a `for` loop in order to perform
#' some sort of change to a series of values. In R, you often don't have to
#' when you are working with vectors because a lot of functions (including all
#' the arithmetic and logical ones above) and be applied to a vector and they
#' work. If the function involves multiple vectors (e.g. `+`), usually you'll
#' want all of them to be either the same length or length 1.

#+ vectors_operators
foo+6
foo+baz
foo/baz
foo>=34
baz<=23
bob <- baz<=23 

c(baz,foo)
c(baz,foo,"76")
#these work with all arithmetic operators
#' You can assign names to some or all members of a vector when you create it.
#' You can also assign or re-assign names later using the `names()` function.

#+ vectors_names1
print(zoidberg <- c(a="lobster",bb="egg",larvae="baby"))
zoidberg["bb"]
zoidberg[c("a","larvae")]
#' You can also use it to see the currently assigned names.

#+ vectors_names2
names(zoidberg)
names(zoidberg) <- c("more","sardines", "please") #renaming
zoidberg
names(zoidberg)[3]
names(zoidberg)[1]
names(zoidberg)[3] <- "pretty"
names(zoidberg)[3]
zoidberg
#' You can subset a vector by using `[...]` with the `...` replaced by _another_
#' vector, of integers indicating which positions you want to extract. Or you
#' could use a vector of names.

#+ vectors_subset1
foo[3]

#' If you just need a single value, use a single name or number.

#+ vectors_subset2

#' If you need a series of adjacent values, use `:`. If you need several
#' adjacent series with interruptions between them, use `c()` with `:`
#' expressions separated by commas `,`.

#+ vectors_subset3
foo[c(1,2,3)]
foo[1:3]
foo[c(1:3,5:6)]
baz
bob
baz[bob] #pulled the values less than or equal to 23
#' Other useful functions for exploring vectors: `length()`, `summary()`,
#' `table()`, `head()`, `tail()`, `sum()`, `diff()`, `seq_along()`.

#+ vectors_explore
summary(foo) #summary gives you as  much info as it can about the item
summary(zoidberg)
table(zoidberg)
table(foo)
table(bob)
summary(bob)
#creating a new kind of vector to use these explorations better
futurama <- sample(1:10,30,replace = TRUE) #sequence of 1-10; drawing 30 times, replace fxn allows repeats after 10 have been drawn
table(futurama)
futurama <- sample(1:10,30,replace = TRUE)*1000
table(futurama)
futurama
head(futurama) #first 6 elements of the vector
tail(futurama) #last 6 elements of the vector
diff(futurama) #difference between two consecutive values
sum(futurama) #sum of all values
sum(futurama,na.rm=TRUE) #omits missing values (NA) in the sum
seq_along(futurama) #generate ID numbers unique for each element

#' Here are some aggregation functions. For these, make sure to use `na.rm=T` if
#' your vector has `NA`s in it... `max()`, `min()`, `mean()`, `median()`,
#' `quantile()`.

#+ vectors_aggregate

quantile(futurama)
quantile(futurama,na.rm=TRUE)


#' ### Data Frames
#'
#' You can bundle several vectors of the same length together into a
#' `data.frame` using the `data.frame()` command. A `data.frame` is a tabular
#' data structure where the columns can be different types from each other
#' (though within each column the type will be uniform because each column is
#' still a vector). Most data in R is in the form of a `data.frame` or a class
#' that inherits from `data.frame`. The `dplyr` package makes working with
#' `data.frames` easier and a lot of attention will be devoted to `dplyr`
#' [below](#data-frames-indepth). For now, here are some basic commands for
#' exploring `data.frames`: `dim()`, `nrow()`, `ncol()`, `names()` and (for
#' small datasets) `plot()`.

#+ df_explore
dim(iris) #dimensions of the dataframe, rows and columns
nrow(iris) #number of rows
ncol(iris) #number of columns
names(iris) #names of the columns
head(iris) #first 6 values -- can change number by using: head(iris,#)
tail(iris) #last 6 values


#' how to select rows
#+ df_subset, results="hide"
iris[3:20,] #leaving column blank 
iris[c(2:10,34,40:50,34,34,34),]
iris[-c(3:20),]
seq_len(nrow(iris))
sample(seq_len(nrow(iris)),10) #sampling without replacement
sample(seq_len(nrow(iris)),250,replace=TRUE) #sampling with replacement

iris0 <- iris[sample(seq_len(nrow(iris)),10),]
dim(iris0)

#' how to select columns
#+ df_columns, error=TRUE, results="hide"
iris[,4:5] #leaving rows blank
iris[,c("Petal.Length","Petal.Width")]
petalcolumnnames <- c("Petal.Length","Petal.Width") 
iris[,petalcolumnnames]
#iris[,c(petalcolumnnames,randoname)] -- showing how it won't pull nonexistent variable

iris$Sepal.Length
outcome <- "Sepal.Length"
iris$outcome #can't use columns held within variable with $ 
iris[[outcome]]
iris[["Sepal.Length"]]

#' how to select columns and rows at the same time
#+ df_columnsandrows
iris[3:12,petalcolumnnames]

#'## Comment Types
#'
#'`#` This is an ordinary comment. Everything after it on the same line is not
#'executed.
#'
#'`#'`This indicates that this line should be formatted as text. It must be the
#'first two characters in that line in order to work
#'
#'`#+` This indicates that the following lines (until the next #' or #+) should
#'be treated as a "code chunk". I.e. the next lines (but not this one) will be
#'run, the code will be displayed according to your settings and the results
#'will be displayed according to your settings.
#'
#'`#+` for these, you can add "chunk options" separated by commas. The first one
#'has no name and is always the label of the chunk. E.g. df_subset. The rest
#'need names, e.g. error=TRUE.

#' ## Linear Models
#' 
#+ linear_models
#+ 
example(lm) #example of a linear model
summary(lm.D9) #summary of dataset in example
#' library(broom) #added a library
tidy(lm.D9) #gives small summary of dataset
head(mtcars) #other dataset to play with
lm(mpg~cyl+disp+qsec,mtcars) #linear model of these specific columns of dataset
performance <- lm(mpg~cyl+disp+qsec,mtcars) #saving lm to an object
summary(performance <- lm(mpg~cyl+disp+qsec,mtcars)) #getting stats on dataset predictions
tidy(performance) #more "tidy" version of this data
tidy(performance)$p.value #extracting the pvalues from that
tidy(performance)[-1,c("estimate","p.value")] #extracting the pvalues except from the first line
performancedf <- lm(mpg~cyl+disp+qsec,mtcars) %>% tidy() %>% select(c("estimate","p.value")) %>% slice(-1)
whatisthis(performance)

#' ## Multiple Comparisons
#' 
#+ multiple_comparisons
#'
#'
performance %>%tidy() %>% select(c("p.value")) %>% slice(-1)
performance %>%tidy() %>% select(c("p.value")) %>% slice(-1) %>% unlist() %>% 
  p.adjust()

#' ## Working With Datasets and dplyr 
#' define location of your files
#' 
example1 <-list.files("C:/Users/krist/Dropbox/My PC (DESKTOP-6EA3C71)/Desktop/sample_data", 
                      full.names = TRUE) %>% sapply(import) %>% setNames(.,basename(names(.)))
#'first part names the sample data etc as "example1" and imported the sample data,
#' the "setnames" part simplified the names of the files 
#' 
birthweight <- example1$Birthweight.sav
#' renamed birthweight data to birthweight in order to make it easier to work with 
#' 
#'the process for importing only one file
#'"most concise way to do it"
#'type in the name of the variable
birthweight <- import("C:/Users/krist/Dropbox/My PC (DESKTOP-6EA3C71)/Desktop/sample_data/Birthweight.sav")

#' ## Intro to dplyr
#' 
#' mutate command: changing/transforming an entire column
mutate(birthweight,AGE_Months=AGE*12) %>% head() #adding age to be reported in months
# if wanted to merely change the column: mutate(birthweight,AGE=AGE*12) 
mutate(birthweight,AGE_Months=AGE*12,AGE_Days=AGE_Months*30.4)

table(birthweight$RACE) #determining how many levels exist and what their values are to be able to transform etc

with(birthweight, case_when(RACE == 1 ~ "Caucasian", RACE == 2 ~ "Asian", RACE == 3 ~ "African American/Black", TRUE ~ as.character(RACE))) 
# ^this "with" command tells the command with what to work with
# This command is reassigning the numerical values in this data to these specific names

mutate(birthweight,AGE_Months=AGE*12,AGE_Days=AGE_Months*30.4,RACE_Name=case_when(RACE == 1 ~ "Caucasian", RACE == 2 ~ "Asian", RACE == 3 ~ "African American/Black", TRUE ~ as.character(RACE))) %>% head()


#' # The Summary Function
#' 
summary(birthweight$BWT)
summary(birthweight)

summarize(birthweight, age= median(AGE))
summarize(birthweight, age= median(AGE), height=median(HT), meanage= mean(AGE))
group_by(birthweight, SMOKE)
group_by(birthweight, SMOKE) %>% summarize(birthweight, age= median(AGE), height=median(HT), meanage= mean(AGE))
group_by(birthweight, SMOKE) %>% summarize(across(where(is.numeric),mean))
group_by(birthweight, SMOKE) %>% summarize(across(where(is.numeric),sd))
group_by(birthweight, SMOKE) %>% summarize(across(where(is.numeric),mean, .names = '{.col}_mean')
                                          ,(across(where(is.numeric),sd, .names = '{.col}_sd')))
    
group_by(birthweight, SMOKE) %>% summarize(across(where(is.numeric),list(mean,sd)))                                         
group_by(birthweight, SMOKE) %>% summarize(across(where(is.numeric),list(Mn=mean,StD=sd,Md=median,InQR=IQR)))  %>% View                                     
                                                                                




