# Standard Functions for basic statistical analysis

Here is an attempt to build a set of basic tools for statistical analysis.
For each situation , one function will provide the most frequent analysis results.

Each function should return a list of the following elements:  

*   The name(s) of the variable(s) analyzed  
*   one or more tables  
*   one or more graphs  
 
 ## _______________________________________________________

## Main functions 

### One variable

*  cat1         : 1 categorical variable (factor)  
*  num1c        : 1 (continuous) numeric variable  
*  num1d        : 1 (discrete) numeric variable  


### Two variables

*  cat2         :       2 factors
*  cat1num1     :       1 categorical variable (factor) + 1 (discrete) numeric variable 
*  num2         :       2 numeric variables  


## _______________________________________________________  

##  Description

###  Helper functions

#### condfreqtable: Make a conditional frequency table for 2 factors  
##### Syntax  
        condfreqtable(dataf, nomfact1, nomfact2, useNA = "no") 

##### Formal arguments  
`dataf`         the dataframe  
`nomfact1`      first factor name (string)
`nomfact2`      2nd factor name  (string)
`useNA = "no"`  keep or remove missing values ("no" or anything else)

##### Returns
a dataframe: conditional frequency  for `nomfact2` for each value of `nomfact1`.
Table columns : nomfact1, nomfact2, joint frequency ("num") , conditional relative frequency ("perc")

  
#### orderfact : reordering the levels of one factor
##### Syntax 
        orderfact(dataf, nomfact, orderfreq = TRUE, orderdesc = TRUE, ordervar = "c..nt", 
                        orderval = NA, orderfun = sum,nlevels = NULL)

##### Formal arguments  

`dataf` The dataframe  
`nomfact` The factor's name
`orderfreq = TRUE`      should the factor's levels be reordered  
`orderdesc = TRUE`      in descending order  
`ordervar = "c..nt"`    the ordering variable (if "c..nt", the number of rows)  
`orderval = NA`         According to the frequency of a value ? (if supplied)  
`orderfun = sum`        summary function to use on `ordervar`  
`nlevels = NULL`        if supplied, directly reorder the levels as given here. Overrides everything else.  

##### Returns  
The factor, with reordered levels


###  Main functions

#### cat1 : Analyze one categorical variable (factor)


