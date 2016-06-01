# *******************************************************
# Essais pour le développement de fonctions standard d'analyse
# **********************************************************



library(ggplot2)
library(dplyr)

#library(ggplot2movies)

# réutilisation des données de ggplot ----------------------------------
# # tries: get data in ggplot
# ggplot_build(p2$plot)


# ===============================================================================
# Tries of graphs for the analysis of discrete x num variables, or factor x num===
# ===============================================================================

# using mpg
mpg

p <- ggplot(mpg, aes(displ, ..density..))+geom_histogram(bins=10)
p
m <- ggplot_build(p) # ==> to get the data out of a plot
m



# discrete num x num, with boxplot =============================================================
# ggplot(mpg ) +
#         geom_boxplot(aes(6, hwy,group = 1, color = "All"), width = 7, fill = "lightyellow2") +
#         geom_boxplot(aes(cyl , hwy, group = cyl, color = "by cylinders"), fill = "paleturquoise") +
#         geom_jitter(aes(cyl, hwy, group=cyl), width =.5, alpha=.5) +
#         labs(title = "Highway Miles/gallon",x = "Cylinders", y= "miles/gallon", color="") +
#         scale_color_brewer(palette = "Set1")
#
# # or better :
# ggplot(mpg ) +
#         geom_boxplot(aes(6.05, hwy, group = 1, fill =  "All"), width = 6) +
#         geom_boxplot(aes(cyl , hwy, group = cyl,  fill = "by cylinders", color=factor(cyl)), varwidth = TRUE) +
#         geom_jitter(aes(cyl, hwy, group=cyl, color=factor(cyl)), width =.5, alpha=.5) +
#         scale_color_brewer(palette = "Set1", guide=FALSE) +
#         scale_fill_hue() +
#         labs(title = "Highway Miles/gallon",x = "Cylinders", y= "miles/gallon", fill="")
#

# or better2 : (num discrete x num) boxplot & jitter --------------------------------
ggplot(mpg,aes(cyl , hwy, color=factor(cyl))) +
        geom_boxplot(aes(group = 1, fill =  "All")) +
        geom_boxplot(aes(fill = "by cylinders"), varwidth = TRUE) +
        geom_jitter( width =.5, alpha=.5) +
        scale_color_brewer(palette = "Set1", guide = FALSE) +
        scale_fill_hue("group") +
        labs(title = "Highway Miles/gallon",x = "Cylinders", y= "miles/gallon")


# creating function for base plot -------------------------------------------------------------


# Thus:
cbydboxjit <- function(dataf, vard, varc, useNA = "no",
                       labellayer = "", labelall = "All values", labelgroups = "by goup") {
        dataf <- as.data.frame(dataf) # same problem with tbl_df
        if (useNA == "no") {
                dataf <- dataf[!is.na(dataf[ , vard]) & !is.na(dataf[ , varc]), ]
        }
        dataf$fact_vard<- factor(dataf[, vard])
        ggplot(dataf,aes_(as.name(vard) , as.name(varc), color=quote(fact_vard))) +
                geom_boxplot(aes(group = 1, fill =  labelall), outlier.colour = "gray") +
                geom_boxplot(aes(fill = labelgroups), varwidth = TRUE, outlier.colour = "gray") +
                geom_jitter( width =.5, alpha=.5) +
                labs(fill = labellayer)
}

# usage
cbydboxjit(mpg, vard = "cyl", varc = "hwy")

cbydboxjit(mpg, vard = "cyl", varc = "hwy", labellayer = "Legend", labelall = "together", labelgroups = "by cylinders") +
        scale_color_brewer(palette = "Set1", guide = FALSE) +
        labs(title = "Highway Miles/gallon",x = "Cylinders", y= "miles/gallon")

cbydboxjit(dtf, vard = "dval1", varc = "dval2", labelall = "Together", labelgroups= "by Dval1")+
        scale_color_brewer(palette = "Set1", guide = FALSE) +
        scale_fill_brewer(palette = "Set3")


# factor x num, with boxplot =========================================================================

# tries

ggplot(mpg ) +
        geom_boxplot(aes(class, hwy, group = 1, fill =  "All"), width = 7) +
        geom_boxplot(aes(class , hwy, group = class,  fill = "by cylinders", color=factor(class))) +
        geom_jitter(aes(class, hwy, group=class, color=factor(class)), width =.5, alpha=.5) +
        scale_color_brewer(palette = "Set1", guide=FALSE) +
        scale_fill_hue() +
        labs(title = "Highway Miles/gallon",x = "Cylinders", y= "miles/gallon", fill="")

# better
ggplot(mpg, aes(class, hwy)) +
        geom_boxplot(aes(group = 1, fill =  "All", color=class)) +
        geom_boxplot(aes( group = class,  fill = "by cylinders", color=class), varwidth = TRUE, outlier.colour = "gray") +
        geom_jitter(aes( group=class, color=class), width =.5, height = 0, alpha=.6) +
        scale_color_brewer(palette = "Set1", guide=FALSE) +
        scale_fill_brewer(palette = "Set3") +
        labs(title = "Highway Miles/gallon",x = "Cylinders", y= "miles/gallon", fill="")

## better2 factor x num, with boxplot & jitter ----------------------------------------------
ggplot(mpg, aes(class, hwy, color=class)) +
        geom_boxplot(aes(group = 1, fill =  "All")) +
        geom_boxplot(aes(fill = "by class"), varwidth = TRUE, outlier.colour = "gray") +
        geom_jitter(width =.5, height = 0, alpha=.6) +
        scale_color_brewer(palette = "Set1", guide=FALSE) +
        scale_fill_brewer(palette = "Set3") +
        labs(title = "Highway Miles/gallon",x = "Vehicle Class", y = "miles/gallon", fill="")


# creating function for base plot -------------------------------------------------------------


cbyfboxjit <- function(dataf, varf, varc, useNA = "no",
                       labellayer = "", labelall = "All values", labelgroups = "by goup") {
        # dataf <- as.data.frame(dataf) # same problem with tbl_df
        # dataf$fact_vard<- factor(dataf[, vard])
        if (useNA == "no") {
                dataf <- dataf[!is.na(dataf[ , varf]) & !is.na(dataf[ , varc]), ]
                }
        ggplot(dataf,aes_(as.name(varf) , as.name(varc), color=as.name(varf))) +
                geom_boxplot(aes(group = 1, fill =  labelall), outlier.colour = "gray") +
                geom_boxplot(aes(fill = labelgroups), varwidth = TRUE, outlier.colour = "gray") +
                geom_jitter( width =.5, alpha=.5) +
                labs(fill = labellayer)
}

# usage
cbyfboxjit(mpg, varf = "class", varc = "hwy")

cbyfboxjit(mpg, varf = "class", varc = "hwy", labellayer = "Legend", labelall = "together", labelgroups = "by class") +
        scale_color_brewer(palette = "Set1", guide = FALSE) +
        scale_fill_brewer(palette = "Set3") +
        labs(title = "Highway Miles/gallon",x = "Vehicle class", y= "miles/gallon")


cbyfboxjit(dtf, varf = "nam1", varc = "cval1")




# ==================================================================================================================

# multiple summary tables

# vector of summaries for 1 variable
sumvector <- function (var) {
        if (length(var) == 0) {
                sapply(numeric(length = 9), function(x) NA)
        }else {
                s <- summary(var)
                if (length(s) < 7) {s <- c(s, rep(0, times=7-length(s)))}
                ret <- numeric(3)
                ret[1] <- sum(!is.na(var))
                ret[2] <-  s["Mean"]
                ret[3] <- sd(var, na.rm = TRUE)
                c(ret,s[-4])
        }
}
# names for the vector
namesum = c("n", "Mean", "St.dev", "Min.", "1st Qu.",
            "Median", "3rd Qu.",  "Max.",  " NA's")
namesumfrench  = c("n", "Moyenne", "Ecart-type", "Min.", "Q1",
                   "Médiane", "Q3",  "Max.",  " NA's")

# table of summaries for 1 variable

sumtable <- function (var) {
        sv <- sumvector(var)
        st <- matrix(sv,nrow = 1)
        st <- as.data.frame(st)
        colnames(st) <- namesum
        st
}


#test

summary(mpg$hwy)

sumvector(dtf$cval1)
sumvector(dtf[["cval1"]])
sumvector(dtf[ ,"cval1"])
sumvector(mpg$hwy)
# mpgdf <- as.data.frame(mpg)
# sumvector(mpgdf[ , "hwy"])

sumvector(mpg[["hwy"]])

sumtable(mpg[["hwy"]])



# combined summaries for different variables in a dataframe
cbsummaries <- function (dataf, vnames,
                namesum = c("n", "Mean", "St.dev", "Min.", "1st Qu.",
                            "Median", "3rd Qu.",  "Max.",  " NA's")) {
        # vnames = a vector of variable names (each a numeric variable of dataf)
        lsum = lapply(vnames, function(nam) sumvector(dataf[[nam]]))
        df <- do.call(what = data.frame, args = lsum)
        colnames(df) <- vnames
        rownames(df) <- namesum
        df
}

# test
cb <- cbsummaries(dtf, c("cval1", "cval2", "dval1", "dval2"))
round(cb,2)
cb2 <- cbsummaries(mpg, c("hwy", "cty")) # OK
round(cb2,2)


# combined summaries for one variable, conditional to the values of a factor

condsummaries <- function (dataf, vname, fname,
                           namesum = c("n", "Mean", "St.dev", "Min.", "1st Qu.",
                                       "Median", "3rd Qu.",  "Max.",  " NA's")) {
        # vname = the variable name
        # fname = the factor name
        # levels: if not factor, make it a factor and take the levels
        if (is.factor(dataf[[fname]])) {
                lv <- levels(dataf[[fname]])
        } else {
                lv <- levels(factor(dataf[[fname]]))
        }
        lsum = lapply(lv ,
                      FUN=function(lev) {
                              dt <- dataf[dataf[[fname]]==lev , ]
                              sumvector(dt[[vname]])
                      } )
        df <- do.call(what = data.frame, args = lsum)
        colnames(df) <- lv
        rownames(df) <- namesum
        df
}

#tests
condsummaries(dtf, "cval1", "nam1")

condsummaries(mpg,"hwy","class")
condsummaries(mpg,"hwy","manufacturer") # ok

diamonds
condsummaries(diamonds,"price","cut") # ok

