# *******************************************************
# Essais pour le développement de fonctions standard d'analyse
# **********************************************************



library(ggplot2)
library(dplyr)
# library(reshape2)

#library(ggplot2movies)

# réutilisation des données de ggplot ----------------------------------
# # tries: get data in ggplot
# ggplot_build(p2$plot)

# ===============================================================================
# centralisation des valeurs par défaut
# ===============================================================================

# Constantes et options : encapsulation
#
# initdefaults <- function () {
#         # structure definition
#         defaultvalue <- list( #namesum
#                 namesumeng = c("n", "Mean", "St.dev",
#                                "Min.", "1st Qu.","Median", "3rd Qu.",  "Max.",
#                                " NA's"),
#                 namesumfrench  = c("n", "Moyenne", "Ecart-type",
#                                    "Min.", "Q1","Médiane", "Q3",  "Max.",
#                                    " NA's"),
#                 namesum ="",
#                 language = "",
#                 filldefault = "steelblue")
#         # access function
#         function(name=NULL, value = NULL){
#                 if(is.null(name)) {
#                         warning("initdefaults : You must supply a name",
#                                 immediate. = TRUE, call. = TRUE)
#                         NULL
#                 } else {
#                         if (!is.null(value)) {
#                                 defaultvalue[[name]] <<- value
#                                 defaultvalue[[name]]
#                         } else {defaultvalue[[name]]}
#                 }
#         }
# }
#
#
# # set up access function and some defaults
# sfdefault <- initdefaults()
# sfdefault("language","french")
# if(sfdefault("language") == "french") {
#         sfdefault("namesum", sfdefault("namesumfrench"))
# }
# sfdefault("reportNA", FALSE)
#

# # testing
# sfdefault()
# sfdefault("language")
# sfdefault("blabla", 6)
# sfdefault("blabla")
#
# sfdefault("namesumfrench")
# sfdefault("namesum", default("namesumfrench"))
# sfdefault("namesum")


# # try...
# gl <- list(ha = 2)
#
# gl[["ha"]]
# gl[["be"]] <- 3
# gl[["be"]]
# vn <- "be"
# gl[[vn]]
# gl[[vn]] <- 6
# gl[[vn]]
#
# gl[vn]

# ================================================================================
# simple and multiple summary tables # =======================================


# # vector of summaries for 1 variable
# sumvector <- function (var, dnames = sfdefault("namesum"), reportNA = sfdefault("reportNA")) {
#         if (length(var) == 0) {
#                 sapply(numeric(length = 9), function(x) NA)
#         }else {# construct a more complete summary vector
#                 s <- summary(var)
#                 if (length(s) < 7) {s <- c(s, rep(0, times=7-length(s)))}
#                 ret <- numeric(3)
#                 ret[1] <- sum(!is.na(var))
#                 ret[2] <-  s["Mean"]
#                 ret[3] <- sd(var, na.rm = TRUE)
#                 s <- c(ret,s[-4])
#                 names(s) <- dnames
#                 if (reportNA) {s} else {s[1:(length(s) - 1)] }
#         }
# }
#
#
#
# # summary(mpg$hwy)
# #
# # sumvector(dtf$cval1)
# # sumvector(dtf[["cval1"]])
# # sumvector(dtf[ ,"cval1"])
# # sumvector(mpg$hwy)
# #
# # sv <- sumvector(mpg[["hwy"]])
# # sv
# # t(sv)
# #
# # sumtable(mpg[["hwy"]])
#
#
#
# # combined summaries for different variables in a dataframe, for the same individuals
# cbsummaries <- function (dataf, vnames) {
#         # vnames = a vector of variable names (each a numeric variable of dataf)
#         lsum = lapply(vnames, function(nam) sumvector(dataf[[nam]]))
#         df <- do.call(what = data.frame, args = lsum)
#         colnames(df) <- vnames
#         # rownames(df) <- namesum
#         df
# }
#
# # # test
# # cb <- cbsummaries(dtf, c("cval1", "cval2", "dval1", "dval2"))
# # round(cb,2)
# # cb2 <- cbsummaries(mpg, c("hwy", "cty")) # OK
# # round(cb2,2)
# # rownames(cb2)
#
# # combined summaries for one variable, conditional to the values of a factor
#
# condsummaries <- function (dataf, vname, fname) {
#         # vname = the variable name
#         # fname = the factor name
#         # levels: if not factor, make it a factor and take the levels
#         if (is.factor(dataf[[fname]])) {
#                 lv <- levels(dataf[[fname]])
#         } else {
#                 lv <- levels(factor(dataf[[fname]]))
#         }
#         lsum = lapply(lv ,
#                       FUN=function(lev) {
#                               dt <- dataf[dataf[[fname]]==lev , ]
#                               sumvector(dt[[vname]])
#                       } )
#         df <- do.call(what = data.frame, args = lsum)
#         colnames(df) <- lv
#         # rownames(df) <- namesum # rownames are preserved
#         df
# }

#tests
# condsummaries(dtf, "cval1", "nam1")
#
condsummaries(mpg,"hwy","class")
# condsummaries(mpg,"hwy","manufacturer") # ok

# diamonds
# cs <- condsummaries(diamonds,"price","cut")
# str(cs)
# tb <- t(cs) # ok
# tb
# str(tb)
# tb <- as.data.frame(tb)
# str(tb)
# tb$cut <-  rownames(tb)
# tb
# tb[["cut"]]
# tb[,"cut"]
# tb <- cbind("cut"= tb[,"cut"], select(tb, 1:8))
# tb
# tb1 <- melt(tb, id.vars = "cut")
# tb1

# is.ordered(diamonds$cut)











# ===============================================================================
# Tries of graphs for the analysis of discrete x num variables, or factor x num===
# ===============================================================================

# using mpg
# mpg
#
# p <- ggplot(mpg )+geom_histogram(aes(hwy, ..density..), bins=10)
# p
# m <- ggplot_build(p) # ==> to get the data out of a plot
# m

# One continuous variable, Histogram =============================================================
# fonctions de détermination du nombre de classes
nclass.Sturges(mpg$hwy)
nclass.FD(mpg$hwy)
nclass.scott(mpg$hwy)

#essais
# mpg

# p <- ggplot(mpg, aes(hwy))+
#         geom_histogram(bins = nclass.Sturges(mpg$hwy))
# p
# m <- ggplot_build(p)$data[[1]]
# m[ ,1:8]
#
#
#
# p <- ggplot(mpg, aes(hwy))+
#         geom_histogram(bins = nclass.FD(mpg$hwy))
# p
# m <- ggplot_build(p)$data[[1]]
# m[ ,1:8]

# stem(mpg$hwy)

# fonction
# num1c <- function(dataf, nomvar, usedensity = FALSE, plot_density = FALSE,
#                  fillhist = "steelblue", color_density = "red", digits = 2, # à modifier
#                  bins = NULL, ...) {  # ... = addtl arguments for geom_hist
#         if (plot_density) {usedensity <- TRUE} # plot_density overrides usedensity
#         p <- ggplot(mpg, aes_(as.name(nomvar))) +
#                 if (usedensity) {geom_histogram(aes(y=..density..),bins = bins, fill = fillhist,...)
#                 } else {geom_histogram(bins = bins, fill = fillhist, ...)}
#         if (plot_density) {p <- p + geom_density(color=color_density) }
#          # make summaries vector
#         s = sumvector(dataf[[nomvar]])
#         num = s["n"] # number of cases
#
#         # get the frequency table
#         tb <- ggplot_build(p)$data[[1]][ , 1:8]
#         # add  columns to it
#         tb$rfreq <- tb$count/num
#         tb$numlabs <-  paste0("n=", tb$count)
#         tb$perclabs <- paste0(100* round(tb$rfreq, digits), "%")
#         tb$index <- ave(1:nrow(tb),  FUN = function(x) 1:length(x)) # rank
#
#         # Uniform Chi2 test
#         uchisq <- try.chisq.test(tb$count)
#         if (length(unique(round(tb$xmax-tb$xmin,digits))) >= 2) {
#                         warning(paste0("Uchisq ", nomvar, " with different class widths!!", call. = TRUE)) }
#
#         # return named list
#         list(name = nomvar,
#              summaries = s,
#              table = tb,
#              num = num,
#              uchisq = uchisq,
#              plot = p)
# }


# return examples from num1d, modified
# * name = name of variable
# * summaries = named vector [number of cases (Num), mean (Mean), std dev (St.dev),
#                             Min., 1st Qu.,Median,3rd Qu., Max., NA's]
# *  table = Frequency table.
#         Table columns:
#     * x
#     * xmin
#     * xmax
#     * y
#     * count = frequency
#     * density
#     * rfreq = relative frequency
##     * numlabs = frequency labels
#     * perclabs  = percentage labels
#     * index = row index
# *  num = number of cases
# *  uchisq = chisquare test for GoF with a uniform distribution
# *  plot = ggplot2 histogram




# # ======================================



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
        # dataf <- as.data.frame(dataf) # same problem with tbl_df
        if (useNA == "no") {
                dataf <- dataf[!is.na(dataf[[vard]]) & !is.na(dataf[[varc]]), ]
        }
        dataf$fact_vard. <- factor(dataf[[vard]])
        ggplot(dataf,aes_(as.name(vard) , as.name(varc), color=quote(fact_vard.))) +
                geom_boxplot(aes(group = 1, fill =  labelall), outlier.colour = "gray") +
                geom_boxplot(aes(fill = labelgroups), varwidth = TRUE, outlier.colour = "gray") +
                geom_jitter( width =.5, alpha=.5) +
                labs(fill = labellayer)
}

# usage
cbydboxjit(mpg, vard = "cyl", varc = "hwy")

cbydboxjit(mpg, vard = "cyl", varc = "hwy", labellayer = "Legend", labelall = "together", labelgroups = "by cylinders") +
        scale_color_brewer(palette = "Set1", guide = FALSE) +
        labs(title = "Highway Miles/gallon",x = "Cylinders", y= "miles/gallon") +
        scale_fill_brewer(palette = "Set2")

cbydboxjit(dtf, vard = "dval1", varc = "dval2", labelall = "Together", labelgroups= "by Dval1")+
        scale_color_brewer(palette = "Set1", guide = FALSE) +
        scale_fill_brewer(palette = "Set3")




# factor x num, with boxplot =========================================================================

# tries

# ggplot(mpg ) +
#         geom_boxplot(aes(class, hwy, group = 1, fill =  "All"), width = 7) +
#         geom_boxplot(aes(class , hwy, group = class,  fill = "by cylinders", color=factor(class))) +
#         geom_jitter(aes(class, hwy, group=class, color=factor(class)), width =.5, alpha=.5) +
#         scale_color_brewer(palette = "Set1", guide=FALSE) +
#         scale_fill_hue() +
#         labs(title = "Highway Miles/gallon",x = "Cylinders", y= "miles/gallon", fill="")
#
# # better
# ggplot(mpg, aes(class, hwy)) +
#         geom_boxplot(aes(group = 1, fill =  "All", color=class)) +
#         geom_boxplot(aes( group = class,  fill = "by cylinders", color=class), varwidth = TRUE, outlier.colour = "gray") +
#         geom_jitter(aes( group=class, color=class), width =.5, height = 0, alpha=.6) +
#         scale_color_brewer(palette = "Set1", guide=FALSE) +
#         scale_fill_brewer(palette = "Set3") +
#         labs(title = "Highway Miles/gallon",x = "Cylinders", y= "miles/gallon", fill="")

## better2 factor x num, with boxplot & jitter ----------------------------------------------
# ggplot(mpg, aes(class, hwy, color=class)) +
#         geom_boxplot(aes(group = 1, fill =  "All")) +
#         geom_boxplot(aes(fill = "by class"), varwidth = TRUE, outlier.colour = "gray") +
#         geom_jitter(width =.5, height = 0, alpha=.6) +
#         scale_color_brewer(palette = "Set1", guide=FALSE) +
#         scale_fill_brewer(palette = "Set3") +
#         labs(title = "Highway Miles/gallon",x = "Vehicle Class", y = "miles/gallon", fill="")


# creating function for base plot -------------------------------------------------------------


# continuous x factor boxplot & jitter plot

cbyfboxjit <- function(dataf, varf, varc, useNA = "no",
                       labellayer = "", labelall = "All values", labelgroups = "by goup") {
        # dataf <- as.data.frame(dataf) # same problem with tbl_df
        # dataf$fact_vard<- factor(dataf[, vard])
        if (useNA == "no") {
                dataf <- dataf[!is.na(dataf[[varf]]) & !is.na(dataf[[varc]]), ]
                }
        ggplot(dataf,aes_(as.name(varf) , as.name(varc), color=as.name(varf))) +
                geom_boxplot(aes(group = 1, fill =  labelall), outlier.colour = "gray") +
                geom_boxplot(aes(fill = labelgroups), varwidth = TRUE, outlier.colour = "gray") +
                geom_jitter( width =.5, alpha=.5) +
                labs(fill = labellayer)
}

# # usage
cbyfboxjit(mpg, varf = "class", varc = "hwy")

cbyfboxjit(mpg, varf = "class", varc = "hwy", labellayer = "Legend", labelall = "together", labelgroups = "by class") +
        scale_color_brewer(palette = "Set1", guide = FALSE) +
        scale_fill_brewer(palette = "Set3") +
        labs(title = "Highway Miles/gallon",x = "Vehicle class", y= "miles/gallon")


cbyfboxjit(dtf, varf = "nam1", varc = "cval1")




# ==================================================================================================================

cbyfdensity <- function(dataf, varf, varc, useNA = "no") {
        if (useNA == "no") {
                dataf <- dataf[!is.na(dataf[[varf]]) & !is.na(dataf[[varc]]), ]
        }
        if (!is.factor(dataf[[varf]])) {dataf[[varf]] <- factor(dataf[[varf]])}
        ggplot(dataf,aes_(as.name(varc), y=quote(..density..), fill=as.name(varf))) +
                geom_density(alpha = 0.3)
}

#use
cbyfdensity(mpg, "drv", "hwy")
cbyfdensity(mpg, "cyl", "hwy")
cbyfdensity(mpg, "cyl", "cty")

windows()
cbyfdensity(mpg, "drv", "hwy") +facet_grid(.~class)
dev.off()






cbyfhistogram <- function(dataf, varf, varc, useNA = "no", usedensity = FALSE, ...) {
        if (useNA == "no") {
                dataf <- dataf[!is.na(dataf[[varf]]) & !is.na(dataf[[varc]]), ]
        }
        if (!is.factor(dataf[[varf]])) {dataf[[varf]] <- factor(dataf[[varf]])}
        s <- condsummaries(dataf,fname = varf, varc=varc)
        p <- if (usedensity) {ggplot(dataf,aes_(as.name(varc), y=quote(..density..), fill=as.name(varf)))
                } else {ggplot(dataf,aes_(as.name(varc), fill=as.name(varf)))}
        p <- p+ geom_histogram(..., position = "dodge")
        # return named list
        list(name = c(varc , varf)
             summaries = s,
             table = NULL, #tb,
             num = NA, #num
             uchisq = NULL, # uchisq
             plot = p)
}

p <- cbyfhistogram(mpg, "drv", "hwy", bins=10)

m <- ggplot_build(p)
m
