# Standard Functions for Basic Statistical Analysis

Here is an attempt to build a set of very basic tools for statistical analysis. For
each situation , one function will provide the most frequent analysis results.

Each function should return a list of the following elements:

*   The name(s) of the variable(s) analyzed 
*   one or more tables 
*   one or more graphs
*   one or more classical tests most often performed in this situation.


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
a dataframe: conditional frequency  for `nomfact2` for each value
of `nomfact1`. Table columns : nomfact1, nomfact2, joint frequency ("num") ,
conditional relative frequency ("perc")


#### orderfact : reordering the levels of one factor 
##### Syntax 
        orderfact(dataf, nomfact, orderfreq = TRUE, orderdesc = TRUE, 
                ordervar = "c..nt", orderval = NA, orderfun = sum,nlevels = NULL)

##### Formal arguments

`dataf`  The dataframe  
`nomfact`  The factor's name  
`orderfreq = TRUE`      should the factor's levels be reordered  
`orderdesc = TRUE`      in descending order if TRUE  
`ordervar = "c..nt"`    the ordering variable (if "c..nt", the number of rows)  
`orderval = NA`         According to the frequency of a value ? (if supplied)  
`orderfun = sum`        summary function to use on `ordervar`  
`nlevels = NULL`  if supplied, directly reorder the levels as given here. Overrides everything else.

##### Returns  
The factor, with reordered levels

##### Details   
Only works correctly with data.frames, not tbl_df. tbl_df's should
be treated with as.data.frame() before calling the function.


#### _______________________________________________________

###  Main functions

#### cat1 : Analyze one categorical variable (factor)  

##### Usage  
        cat1(dataf, nomfact, useNA = "no", orderfreq = TRUE, orderdesc = TRUE, ordervar = "c..nt",
                orderval = NA, orderfun = sum, rfreq = TRUE, digits = 2, cfill = "steelblue")

##### Arguments  
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

##### Value
named list:  

*   name = factor name  
*   levels = factor levels   
*   table = frequency table  
*   num = number of observations  
*   uchisq = Chi-square test for GoF with the uniform distribution (done with chisq.test)  
*   plot =  ggplot bar chart  

##### Details    

##### Example  

        mc <- cat1(as.data.frame(mpg), "class")
        
        # with plot value labels and cosmetic changes
        mc$plot +
                geom_text(data=mc$table , aes( x=class, y = 100 * rfreq - 1.5, 
                                                label = ifelse(index <= 6, perclabs, ""))) +
                theme(axis.text.x = element_text(angle=45, hjust=1)) +
                labs(title = "Class",
                        x = "",
                        y = "percentage")
                        
        
        # 2nd example, with ordering by the frequency of a value in another variable
        mc <- cat1(as.data.frame(mpg), "class", ordervar = "drv", orderval = "4", orderdesc = FALSE)
        mc$plot +
                geom_text(data=mc$table , aes( x=class, y = 100 * rfreq - 1.5, 
                                                label=ifelse(rfreq >= 0.19, perclabs, ""))) +
                theme(axis.text.x = element_text(angle=45, hjust=1)) +
                labs(title = "Class", 
                        x = "",
                        y = "percentage")
        # checking if differences may be significant
        mc$uchisq$p.value

 
#### Namefun : Template

##### Usage 

##### Arguments  

##### Value  

##### Details  

##### Example   


#### Namefun : Template

##### Usage 

##### Arguments

##### Value  

##### Details  

##### Example  



#### Namefun : Template

##### Usage 

##### Arguments

##### Value  

##### Details  

##### Example  
