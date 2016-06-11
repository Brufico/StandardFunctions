--- 
title: "Standard Functions for Basic Statistical Analysis"
author: "Bruno Fischer Colonimos"
date: "4 juin 2016"
output:
  pdf_document: 
    toc: yes
  html_document:
    number_sections: yes
    theme: readable
  word_document: default
---

-------------------------------------------------------------

Overview
=======================

Motivation
-----------------------

Here is an attempt to build a set of very basic tools for (exploratory) statistical analysis.
For each situation, (ie each combination of types of the variables being analyzed)
, one would call a standard function, which will provide a set of most frequently useful analysis elements.

Each main function should return a list of the following elements:

* The name(s) of the variable(s) analyzed (vector)
* The number of (valid) cases 
* some summaries 
* one or more tables 
* one or more classical tests most often performed in this situation. 
* one or more graphs (ggplot)


The main functions
-----------------------

### One variable 

cat1 
~ 1 categorical variable (factor)

num1c
~ 1 (continuous) numeric variable

num1d
~ 1 (discrete) numeric variable


### Two variables

cat2 :
~ 2 factors 

cat1num1d
~ 1 categorical variable (factor) + 1 (discrete) numeric variable 

cat1num1c
~ 1 categorical variable (factor) + 1 (continuous) numeric variable

num2
~ 2 numeric variables


-------------------------------------------------------------

Code Description: Constants and settings 
=======================================


### Function sfinitdefaults : Initialize global constants and default values

#### Usage {-}

``` 
sfdefault <- sfinitdefaults() 
```

#### Arguments {-}

None

#### Value {-}

The function `sfdefault`

#### Details {-}

`sfinitdefaults(name)`   constructs the initial list of
defaults and returns the function to access and modify it

#### Example {-} 

```
```

### Function sfdefaults() : set global constants and default values

#### Usage {-}

``` 
sfdefaults(name) 
sfdefaults(name, value) 
```

#### Arguments {-}

`name`     the name of the default to set/get  
`value`    the value to set the default to

#### Value {-}

The value of the default `name`

#### Details {-}

`sfdefaults(name)`    gets the value associated with `name` 
`sfdefaults(name, value)` sets the value associated with `name`

#### Example {-} 

```
# set the language 
sfdefault("language","french")

# set the summaries names 
if(sfdefault("language") == "french") { 
        sfdefault("namesum", sfdefault("namesumfrench")) 
} else { sfdefault("namesum", sfdefault("namesumeng")) 
}

# report number of NA's in a variable? 
# sfdefault("reportNA", FALSE) 
```


-------------------------------------------------------------

Functions Description: Helper functions 
=======================================

Simple and multiple summary tables
----------------------------------

### Function sumvector(): generate a named vector of summaries

#### Usage {-}

``` 
sumvector(var, dnames = sfdefault("namesum"), 
          reportNA = sfdefault("reportNA")) 
```

#### Arguments {-}

`var`     the variable (a numeric vector)  
`dnames`  The names for the summary vector  
`reportNA` report the number of NA's ?  

#### Value {-}

a vector of summaries

#### Details {-}

The names of the summaries ("namesum") are set with sfdefaults. 2 sets of names
are defined initially:

``` 
namesumeng = c("n", "Mean", "St.dev", 
                "Min.", "1st Qu.","Median", "3rd Qu.", "Max.", " NA's") 
namesumfrench  = c("n", "Moyenne", "Ecart-type", 
                "Min.", "Q1","Médiane", "Q3",  "Max.", " NA's"),
```


#### Example {-} 

``` 
sumvector(mpg[["hwy"]])

##          n    Moyenne Ecart-type        Min.        Q1    Médiane        Q3      Max. 
## 234.000000  23.440000   5.954643  12.000000  18.000000  24.000000 27.000000  44.000000

```

### Function cbsummaries() : combined summaries tables for different variables in a dataframe, for all individuals

#### Usage {-}

``` 
cbsummaries(dataf, vnames)
```

#### Arguments {-}

dataf  
vnames = vector of variable names 

#### Value {-}

the summary vectors, as columns of a dataframe

#### Details {-}

#### Example {-} 

```
cbsummaries(mpg, c("hwy", "cty")) 

```


### Function condsummaries() : combined summaries for one variable, conditional to the values of a factor

#### Usage {-}

```
condsummaries(dataf, vname, fname)
```

#### Arguments {-}

dataf ==> the dataframe  
vname ==> the numeric variable name  
fname ==> the factor's name  

#### Value {-}

a dataframe combining as columns the summaries of vname, for each level of fname

#### Details {-}

#### Example {-}

```
condsummaries(mpg,"hwy","class")

##             2seater  compact  midsize   minivan   pickup subcompact       suv
## n           5.00000 47.00000 41.00000 11.000000 33.00000  35.000000 62.000000
## Moyenne    24.80000 28.30000 27.29000 22.360000 16.88000  28.140000 18.130000
## Ecart-type  1.30384  3.78162  2.13593  2.062655  2.27428   5.375012  2.977973
## Min.       23.00000 23.00000 23.00000 17.000000 12.00000  20.000000 12.000000
## Q1         24.00000 26.00000 26.00000 22.000000 16.00000  24.500000 17.000000
## Médiane    25.00000 27.00000 27.00000 23.000000 17.00000  26.000000 17.500000
## Q3         26.00000 29.00000 29.00000 24.000000 18.00000  30.500000 19.000000
## Max.       26.00000 44.00000 32.00000 24.000000 22.00000  44.000000 27.000000

```




Data manipulation : Tables, ordering, error/warning testing
-----------------------------------------------------------

### Function condfreqtable(): Make a conditional frequency table for 2 factors

#### Usage {-} 

``` 
condfreqtable(dataf, nomfact1, nomfact2, useNA = "no") 
```

#### Arguments {-} 

`dataf`         the dataframe  
`nomfact1`      first factor name (string)  
`nomfact2`      2nd factor name  (string)  
`useNA = "no"`  keep or remove missing values ("no" or anything else)  

#### Value {-} 

a dataframe: for each value of `nomfact1`, the conditional frequency  for `nomfact2`
Dataframe columns :

*  nomfact1
*  nomfact2, 
*  joint frequency ("num")
*  conditional relative frequency ("perc") for nomfact2 given nomfact1

#### Details{-} 

~~currently, Only works correctly with data.frames, not tbl_df. 
tbl_df's should be treated with as.data.frame() before calling the function.
working on repairing this~~. Corrected. Should work ok.


### Function orderfact() : reorder the levels of a factor

#### Usage {-}

```
orderfact(dataf, nomfact, orderfreq = TRUE, orderdesc = TRUE, 
                ordervar = "c..nt", orderval = NA, orderfun = sum, nlevels = NULL)
```

#### Arguments {-}

`dataf` ==> The dataframe  
`nomfact` ==>   The factor's name  
`orderfreq` ==> should the factor's levels be reordered according to frequency or another var?  
`orderdesc` ==>  in descending order if TRUE  
`ordervar` ==> the ordering variable (if not supplied or "c..nt", the number of rows)  
`orderval` ==> According to the frequency of a certain value ? (if supplied)  
`orderfun` ==> summary function to use on `ordervar`  
`nlevels`==> if supplied, directly reorder the levels as given here. Overrides everything else.  

#### Value {-} 

The factor, with reordered levels, according to either:

* the frequency of the levels
* a function of the values of another (numeric) variable
* the relative frequency of a value of another variable, within all the rows of a level

#### Details  
Do not reorder with this function an ordered factor ?!!  
~~Currently, only works correctly with data.frames, not tbl_df. 
tbl_df's shouldbe treated with as.data.frame() before calling the function.~~ 
Not true anymore. Should work ok.

#### Example {-}

```
missing example code
```


Shortcut & utility functions
----------------------------

### Utility function: is.warning() : Is this a warning ?

#### Usage {-}

```
is.warning(x)
```

#### Arguments {-}

x ==> the value to be tested

#### Value {-}

TRUE if x is a warning else FALSE

#### Details {-}

definition: 

```
is.warning <- function(x) {"warning" %in% class(x)}
```

#### Example {-}

```
None
```

### utility function: nonavect() : removes NA's from a vector

#### Usage {-}

```
nonavect(x)
```

#### Arguments {-}

x ==> the vector

#### Value {-}

the vector with NA's removed

#### Details {-}

definition: 

```
nonavect <- function(vect) {vect[which(!is.na(vect))]}
```

#### Example {-}

```
None
```

Statistical functions
---------------------


### Function try.chisq.test() : chisquare test with warning detection

#### Usage {-}

```
try.chisq.test(..., keep.all = TRUE)
```

#### Arguments {-}

..., ==> The arguments to chisq.test
keep.all = TRUE ==> return all the results (if TRUE), or only a valid test 

#### Value {-} 

if keep_all = TRUE : 
    a named list : 
    * test1 , 
    * test2, 
    * warning = TRUE/FALSE ,
    * warningmsg = ww )
Else,                             
    a named list : 
    *    test1 ,  #(only)

#### Details {-}

The function calls chisquare.test and checks for a warning. if there is no warning:

* test1 = classical chi2 test
* test2 = simulation chi2 test

else (If there is a warning), the reverse:

* test1 = simulation chi2 test
* test2 = classical chi2 test

then, if keep-all = true, everything is returned, if not, only test1 is returned  
In all cases, test1 is normally the most valid test.  
Applies to all the chisquare tests


#### Example {-}

```
No example yet
```



-------------------------------------------------------------

Functions Description: Main functions
=====================================

### Function cat1() : Analyze one categorical variable (factor)

#### Usage {-} 

```
cat1(dataf, nomfact, useNA = "no", 
        orderfreq = TRUE,orderdesc = TRUE, ordervar = "c..nt", 
        orderval = NA, orderfun = sum, 
        rfreq = TRUE, digits = 2, cfill = "steelblue")
```

#### Arguments {-} 

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

#### Value {-} 

named list:

* name = factor name 
* levels = factor levels 
* table = frequency table. 
* num = number of observations 
* uchisq = Chi-square test for GoF with the uniform distribution (done with chisq.test) 
* plot =  ggplot2 bar chart

#### Details {-}

#### Example {-}

```
# example 1 mc <- cat1(as.data.frame(mpg), "class")

# with plot value labels and cosmetic changes 
mc$plot + 
        geom_text(data=mc$table, 
                  aes(x=class, y = 100 * rfreq - 1.5, label = ifelse(index <= 6, perclabs, ""))) + 
        theme(axis.text.x = element_text(angle=45, hjust=1)) + 
        labs(title = "Class", x ="", y = "percentage")

# 2nd example, with ordering by the frequency of a value in another variable 
mc <- cat1(as.data.frame(mpg), "class", ordervar = "drv", 
           orderval = "4", orderdesc = FALSE) 
mc$plot + geom_text(data=mc$table, aes(x=class, y = 100 * rfreq - 1.5, 
                                       label=ifelse(rfreq >= 0.19, perclabs, ""))) + 
        theme(axis.text.x =element_text(angle=45, hjust=1)) + 
        labs(title = "Class", x = "", y = "percentage")

# checking if differences may be significant mc$uchisq$p.value

```


### num1d : Analyze one numerical variable (discrete)

#### Usage {-} 

```
num1d(dataf, nomvar, digits = 2, sumdigits = 2, useNA ="no", 
      rfreq = TRUE, width = .5, cfill = "steelblue")
```

#### Arguments {-}

`dataf` 
`nomvar`        name of the numeric variable  
`digits` = 2    number of digits for relative frequency  
`sumdigits` = 2 number of digits for the summaries  
`useNA` ="no",  Show missing values  
`rfreq` = TRUE  Use relative frequency Y-axis  
`width` = .5    Width of bars  
`cfill` = "steelblue"   default fill colour  

#### Value {-}

A named list:

* name = name of variable 
* summaries = named vector [number of cases (Num), mean (Mean), std dev (St.dev), Min., 1st Qu.,Median,3rd Qu., Max., NA's] 
* table = Frequency table. Table columns: 
    * variable name * num = frequency 
    * rfreq = relative frequency 
    * perclabs  = percentage labels * numlabs = frequency labels 
    * index = row index 
*  num = number of cases 
*  uchisq = chisquare test for GoF with a uniform distribution 
*  plot = ggplot2 bar chart

#### Details {-}

#### Example {-} 

``` 
# test with mpg 
# cyl <- num1d(as.data.frame(mpg), "cyl") 
cyl$plot + xlab("Cylinders") + ylab("Percentage") 
```

### cat2 : Analyze two categorical variable (factors) together

#### Usage {-} 
```
cat2(dataf, nomfact1, nomfact2,  useNA = "no", 
     orderfreq1 = TRUE, orderdesc1 = TRUE, ordervar1 = "c..nt", 
     orderval1 = NA, orderfun1 = sum, nlevel1 =NULL, 
     orderfreq2 = TRUE, orderdesc2 = TRUE, ordervar2 = "c..nt", 
     orderval2 = NA, orderfun2 = sum, nlevel2 =NULL, 
     rfreq = TRUE, digits = 2, cfill = "steelblue")
```

#### Arguments {-}

`dataf`  The dataframe  
`nomfact`  The 1st factor's name  
`nomfact2`  The 2nd factor's name  
`useNA` = "no" `orderfreq1` = TRUE  
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


#### Value {-}


#### Details {-}


#### Example {-} 

```
# ex 1
mp2 <- cat2(as.data.frame(mpg), "class", "drv", orderfreq1 =TRUE, 
            ordervar1 = "drv" , orderval1 = "f", orderfun1 = mean) 
mp2 
mp2$plot + 
        geom_text(data = mp2$table$tbl1 , aes(x = class, y = -.05, label = numlabs)) + 
        geom_text(data = mp2$table$tbl1 , aes(x = class, 
                                              y = percval - 0.03, 
                                              label = ifelse( index <= 4, perclabs, ""))) +
        theme(axis.text.x = element_text(angle=45,hjust=1)) + 
        labs(title = "Drive per class", x = "Class", y = "percentage")



# ex 2
mp3 <- cat2(as.data.frame(mpg), "class", "drv", orderfreq1 =TRUE, 
            ordervar1 = "drv" , orderval1 = "4", orderfun1 = mean,
            orderfreq2 =TRUE, nlevel2 = c("4","r","f"))
mp3

mp3$plot +
        geom_text(data = mp3$table$tbl1 , 
                  aes(x = class, y = -.05, label = numlabs)) +
        geom_text(data = mp3$table$tbl1 , 
                  aes(x = class,
                      y = percval - 0.03,
                      label = ifelse( index <= 4, perclabs, ""))) +
        theme(axis.text.x = element_text(angle=45, hjust=1))
```


Templates
===========================


### Namefun : Template ---

#### Usage {-}

```
code
```

#### Arguments {-}

#### Value {-}

#### Details {-}

#### Example {-}

```
print(try(log("a"), TRUE)) # dummy code
```


### Function Namefun : Template ---

#### Usage {-}

```
code
```

#### Arguments {-}

#### Value {-}

#### Details {-}

#### Example {-}

```
missing example code
```
