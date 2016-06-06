---
title: "Standard Functions for Basic Statistical Analysis"
author: "Bruno Fischer Colonimos"
date: "30 May 2016"
output:
  html_document:
    highlight: textmate
    theme: journal
    toc: yes
    toc_float: no
  pdf_document:
    toc: yes
  word_document: default
---


# Overview

## Motivation  
Here is an attempt to build a set of very basic tools for statistical analysis. For
each situation, ie each combination of types of thr variables being analyzed , 
one function will provide the most frequently useful analysis elements.

Each main function should return a list of the following elements:

* The name(s) of the variable(s) analyzed 
* The number of (valid) cases
* some summaries
* one or more tables 
* one or more classical tests most often performed in this situation.
* one or more graphs (ggplot)


## The main functions  

### One variable  
* cat1 :  1 categorical variable (factor)
* num1c :  1 (continuous) numeric variable
* num1d :  1 (discrete) numeric variable

### Two variables  

*  cat2 :  2 factors
*  cat1num1 :  1 categorical variable (factor) + 1 (discrete) numeric variable
*  num2 :  2 numeric variables




# Functions Desctiption: Helper functions  
  

### condfreqtable: Make a conditional frequency table for 2 factors  

#### Usage  
``` 
condfreqtable(dataf, nomfact1, nomfact2, useNA = "no")
```

#### Formal arguments  
`dataf`         the dataframe  
`nomfact1`      first factor name (string)   
`nomfact2`      2nd factor name  (string)   
`useNA = "no"`  keep or remove missing values ("no" or anything else)  

#### Value 
a dataframe: for each value of `nomfact1`, the conditional frequency  for `nomfact2`  

Dataframe columns : 

*  nomfact1, 
*  nomfact2, 
*  joint frequency ("num") , 
*  conditional relative frequency ("perc") for nomfact2 given nomfact1

#### Details   
Only works correctly with data.frames, not tbl_df. tbl_df's should
be treated with as.data.frame() before calling the function.



### orderfact : reordering the levels of one factor 

#### Usage

``` 
orderfact(dataf, nomfact, orderfreq = TRUE, orderdesc = TRUE,  
          ordervar = "c..nt", orderval = NA, orderfun = sum,nlevels = NULL)
``` 

#### Arguments

`dataf`  The dataframe  
`nomfact`  The factor's name  
`orderfreq = TRUE`      should the factor's levels be reordered ?  
`orderdesc = TRUE`      in descending order if TRUE  
`ordervar = "c..nt"`    the ordering variable (if "c..nt", the number of rows)  
`orderval = NA`         According to the frequency of a value ? (if supplied)  
`orderfun = sum`        summary function to use on `ordervar`  
`nlevels = NULL`  if supplied, directly reorder the levels as given here. Overrides everything else.

#### Value  
The factor, with reordered levels

#### Details   
Only works correctly with data.frames, not tbl_df. tbl_df's should
be treated with as.data.frame() before calling the function.



# Functions Description: Main functions


### cat1 : Analyze one categorical variable (factor)  

#### Usage  
``` 
cat1(dataf, nomfact, useNA = "no", orderfreq = TRUE, orderdesc = TRUE, 
     ordervar = "c..nt", orderval = NA, orderfun = sum, 
     rfreq = TRUE, digits = 2, cfill = "steelblue")
``` 

#### Arguments  
`dataf`  The dataframe   
`nomfact`  The factor's name   
`useNA = "no"`  keep or remove missing values ("no" or "ifany" or "always")  
`orderfreq = TRUE` ....... Arguments for `orderfreq` ..... 
`orderdesc = TRUE`  
`ordervar = "c..nt"`  = variable to use for ordering,  
`orderval = NA`  = value if the ordering variable is the frequency of ordervar == value
`orderfun = sum`  
`rfreq = TRUE` resulting yscale of barchart = relative frequency or absolute  
`digits = 2`    rounding of relative frequency (2 = whole percentages)  
`cfill = "steelblue"`   fill of barchart  

#### Value
named list:  

*   name = factor name
*   levels = factor levels
*   table = frequency table.
*   num = number of observations
*   uchisq = Chi-square test for GoF with the uniform distribution (done with chisq.test)
*   plot =  ggplot2 bar chart

#### Details    

#### Example  

```
# example 1
mc <- cat1(as.data.frame(mpg), "class")

# with plot value labels and cosmetic changes
mc$plot +
    geom_text(data=mc$table, aes(x=class, y = 100 * rfreq - 1.5, 
                                 label = ifelse(index <= 6, perclabs, "")) + 
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    labs(title = "Class",
         x = "",
         y = "percentage")
                        
        
# 2nd example, with ordering by the frequency of a value in another variable
mc <- cat1(as.data.frame(mpg), "class", 
           ordervar = "drv", orderval = "4", orderdesc = FALSE)
mc$plot +
    geom_text(data=mc$table, aes(x=class, y = 100 * rfreq - 1.5, 
                                 label=ifelse(rfreq >= 0.19, perclabs, ""))) +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    labs(title = "Class", 
         x = "",
         y = "percentage")
        
# checking if differences may be significant
mc$uchisq$p.value
```
 

### num1d : Analyze one numerical variable (discrete)

#### Usage  
``` 
num1d(dataf, nomvar, digits = 2, sumdigits = 2,
      useNA ="no", rfreq = TRUE, 
      width = .5, cfill = "steelblue")
``` 

#### Arguments  

`dataf`  
`nomvar`        name of the numeric variable  
`digits` = 2    number of digits for relative frequency  
`sumdigits` = 2 number of digits for the summaries  
`useNA` ="no",  Show missing values  
`rfreq` = TRUE  Use relative frequency Y-axis  
`width` = .5    Width of bars  
`cfill` = "steelblue"   default fill colour  

#### Value 

A named list:

* name = name of variable
* summaries = named vector [number of cases (Num), mean (Mean), std dev (St.dev), 
                        Min., 1st Qu.,Median,3rd Qu., Max., NA's]
*  table = Frequency table.  
        Table columns:  
    * variable name
    * num = frequency
    * rfreq = relative frequency
    * perclabs  = percentage labels
    * numlabs = frequency labels
    * index = row index
*  num = number of cases
*  uchisq = chisquare test for GoF with a uniform distribution
*  plot = ggplot2 bar chart

#### Details  

#### Example
``` 
# test with mpg
cyl <- num1d(as.data.frame(mpg), "cyl")
cyl$plot + xlab("Cylinders") + ylab("Percentage")
``` 


### cat2 : Analyze two categorical variable (factors) together

#### Usage 
```
cat2(dataf, nomfact1, nomfact2,  useNA = "no",
         orderfreq1 = TRUE, orderdesc1 = TRUE, ordervar1 = "c..nt",
         orderval1 = NA, orderfun1 = sum, nlevel1 =NULL,
         orderfreq2 = TRUE, orderdesc2 = TRUE, ordervar2 = "c..nt",
         orderval2 = NA, orderfun2 = sum, nlevel2 =NULL,
         rfreq = TRUE, digits = 2, cfill = "steelblue")
```

#### Arguments 

`dataf`  The dataframe   
`nomfact`  The 1st factor's name   
`nomfact2`  The 2nd factor's name  
`useNA` = "no"  
`orderfreq1` = TRUE  
`orderdesc1` = TRUE  
`ordervar1` = "c..nt"  
`orderval1` = NA  
`orderfun1` = sum   
`nlevel1` =NULL   
`orderfreq2` = TRUE  
`orderdesc2` = TRUE  
`ordervar2` = "c..nt"  
`orderval2` = NA   
`orderfun2` = sum  
`nlevel2` = NULL  
`rfreq` = TRUE unused but should mean relative frequency (vs absolute, ie position = "fill" or "stack")  
`digits` = 2  
`cfill` = "steelblue" (unused)  
 

#### Value  


#### Details  


#### Example  
```  
mp2 <- cat2(as.data.frame(mpg), "class", "drv", 
                orderfreq1 =TRUE, ordervar1 = "drv" , orderval1 = "f", orderfun1 = mean)
mp2
mp2$plot +
        geom_text(data = mp2$table$tbl1 , aes(x = class, y = -.05, label = numlabs)) +
        geom_text(data = mp2$table$tbl1 , aes(x = class,
                                             y = percval - 0.03,
                                             label = ifelse( index <= 4, perclabs, ""))) +
        theme(axis.text.x = element_text(angle=45, hjust=1)) +
        labs(title = "Drive per class",
             x = "Class",
             y = "percentage")




mp3 <- cat2(as.data.frame(mpg), "class", "drv",
                orderfreq1 =TRUE, ordervar1 = "drv" , orderval1 = "4", orderfun1 = mean,
                orderfreq2 =TRUE, nlevel2 = c("4","r","f"))
mp3
mp3$plot +
        geom_text(data = mp3$table$tbl1 , aes(x = class, y = -.05, label = numlabs)) +
        geom_text(data = mp3$table$tbl1 , aes(x = class,
                                              y = percval - 0.03,
                                              label = ifelse( index <= 4, perclabs, ""))) +
        theme(axis.text.x = element_text(angle=45, hjust=1))
```


### Namefun : Template ---

#### Usage 

#### Arguments

#### Value  

#### Details  

#### Example  
```
print(try(log("a"), TRUE)) # dummy code
```
  

### Namefun : Template

#### Usage

#### Arguments

#### Value

#### Details

#### Example
