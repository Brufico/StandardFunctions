# **********************************************************************
# Standard functions for basic stat analysis
# *********************************************************************


library(ggplot2)
library(dplyr)
library(reshape2)


# Generate test data ===================================================

# helper functions -----------------------------------------------------

# vlookup
vlookup <- function(value, searchtable, searchcol = 1, returncol= 2){
        searchtable[match(value, searchtable[ , searchcol]), returncol]
}

# data generator -------------------------------------------------------
make_testdata <- function(tbsize = 40, seed = 98756, naratio = .5) {
        # preparation
        # a) letter sets
        setlet1 <- letters[1:4]
        setlet2 <- letters[10:13]
        # b) auxiliary tables
        tbm1 <- data.frame(setlet1, mean = sample(c(5,15,10,12)), size = length(setlet1), replace = TRUE)
        tbs1 <- data.frame(setlet1, s = sample(c(2,1,3,2)), size = length(setlet1), replace = TRUE)
        tbm2 <- data.frame(setlet2, mean = 10 - seq_along(setlet2))
        tbs2 <- data.frame(setlet2, s = 5 - seq_along(setlet2)*.5)
        tbm3a <- data.frame(setlet1, min = c(5,10,8,12))
        tbm3b <- data.frame(setlet2, max = 15 - seq_along(setlet2))
        # generate random data
        set.seed(seed)
        nam1 <- factor(sample(letters[1:4], tbsize, replace = TRUE))
        nam2 <- factor(sample(letters[10:12], tbsize, replace = TRUE))
        cval1 <- rnorm(n = tbsize, mean = 5, sd = 1)
        cval2 <- sapply(X = nam1, function(x) {rnorm(n = 1, mean = vlookup(x,tbm1), sd = vlookup(x,tbs1))} )
        dval1 = round(runif(n = tbsize, min = 1, max = 5),0)
        dval2 <- mapply(nam1, nam2,
                        FUN = function(x,y) {round(runif(n = 1,
                                                         min = vlookup(x,tbm3a),
                                                         max = vlookup(y,tbm3b)),0)} )
        dtf <- data.frame(nam1, nam2, cval1, cval2, dval1, dval2)
        # put some NA's'
        numna <- round(tbsize * naratio)
        nastab <- data.frame(
                x = sample(1:tbsize, numna, replace=TRUE),
                y = sample(1:6, numna, replace=TRUE)
        )
        for (i in 1:numna) {
                dtf[nastab[i,1], nastab[i,2]] <- NA
        }
        # return dataframe
        dtf
}

## actually make data ---------------------------------------------------
dtf <- make_testdata()
# dput(make_testdata(tbsize = 10, naratio = .5))



# ***************** ======================================================
# helper functions =======================================================


# TEST condfreqtable()

# aa <- condfreqtable(dtf, "nam1", "nam2")
# condfreqtable(dtf, "nam1", "nam2")
# condfreqtable(dtf, "nam1", "dval1", useNA = "yes")
# Another way: reshape in a crosstab
# dcast(aa, formula = nam1 ~ nam2 , value.var = "perc", drop=FALSE, fill = 0)


# my version of reorder (inutile maintenant que j'ai la solution)
summaryunique <- function(df, nomfact, nomvar, fun = mean, decreasing=FALSE, outputdf = FALSE) {
        # print("summaryunique") ; print("df=") ;print(df); print(nomfact); print(nomvar)
        x <- df[, nomfact]
        u <- unique(x[!is.na(x)])
        values <- split(df, df[ , nomfact]) %>%
                lapply(function(d) fun(d[, nomvar], na.rm=TRUE) ) %>%
                unsplit(u)
        if (outputdf) {
                data.frame( level = u, values)
        } else {
                u[order(values, decreasing = decreasing)]
        }
}



reorderlevels <- function(df, nomfact, nomvar,fun = mean, decreasing = FALSE) {
        lv <- summaryunique(df, nomfact, nomvar, fun=fun, decreasing=decreasing)
        factor(df[, nomfact], levels = lv)
}


# test reorderlevels
# reorderlevels(dtf,"nam1", "cval1", decreasing = TRUE, fun=max)


#  reorder factor  ===========================================================


# Tests ==============================================================================================
# TEST reorder
mymean <- function(x) mean(x, na.rm = TRUE)
mysum <- function(x) sum(x, na.rm = TRUE)

# dtf <- make_testdata()
table(dtf$nam1)
table(dtf$nam1,dtf$nam2)
mcv2 <- dtf %>% group_by(nam1) %>% summarise(val=mymean(cval2)) ; mcv2

aba <- condfreqtable(dtf, "nam1", "nam2")
aba <- as.data.frame(aba)
abay <- aba[aba[, "nam2"] == "j", ]
abaz <- rbind(abay,abay,abay,abay,abay,abay)
colnames(abay)


levels(dtf$nam1)
dtf$nam1 <- orderfact(dtf,"nam1") ; levels(dtf$nam1) #ok
dtf$nam1 <- orderfact(dtf,"nam1", orderdesc = FALSE) ; levels(dtf$nam1) #ok
dtf$nam1 <- orderfact(dtf,"nam1", ordervar = "dval2", orderfun = sum, orderdesc = TRUE) ; levels(dtf$nam1)#ok
dtf$nam1 <- orderfact(dtf,"nam1", ordervar = "dval2", orderfun = sum, orderdesc = FALSE) ; levels(dtf$nam1)#ok
dtf$nam1 <- orderfact(dtf,"nam1", ordervar = "cval2", orderfun = mean, orderdesc = TRUE) ; levels(dtf$nam1)#ok
dtf$nam1 <- orderfact(dtf,"nam1", ordervar = "cval2", orderfun = sum, orderdesc = TRUE) ; levels(dtf$nam1)#ok
dtf$nam1 <- orderfact(dtf,"nam1", ordervar = "nam2", orderval = "j") ; levels(dtf$nam1)

xy$nam1
xy[,"nam1"]
xy[,"perc"]

dtf$nam1 <- orderfact(dtf,"nam1"); levels(dtf$nam1)
aba$nam1 <- orderfact(aba,"nam1"); levels(aba$nam1)
abaz$nam1 <- orderfact(abaz,"nam1"); levels(abaz$nam1)

xy$nam1 <- orderfact(xy, "nam1", ordervar = "perc", orderfun = mean, orderdesc = TRUE) ; levels(xy$nam1)
dtf$nam1 <- orderfact(dtf, "nam1", ordervar = "dval2", orderfun = mean, orderdesc = TRUE) ; levels(dtf$nam1)

# reordering in one dframe and transferring order to another
aa$nam1 <- orderfact(aa, "nam1", ordervar = "perc", orderfun = mean, orderdesc = TRUE) ; levels(aa$nam1)
dtf$nam1 <- orderfact(dtf, "nam1", nlevels = levels(aa$nam1)); levels(dtf$nam1)


# ************************ ================================================================
# fonctions de tri simples ================================================================
#
# * une variable ***********************
#
# cat1  1 facteur
# num1c 1 variable continue
# num1d 1 variables discrete
#
# # * deux variables ***********************
#
# cat2  2 facteurs
# cat1num1
# num2



#  cat1  =================================================================================

# # usage, essais et tests
# # ##a
# tp <- cat1(dtf, "nam1", useNA= "no", rfreq = TRUE, orderfreq = TRUE, orderdesc = TRUE, cfill = "red")
# tp$plot
# tp$table
# # ##b
# tp <- cat1(dtf, "nam1", useNA= "no", rfreq = TRUE, orderfreq = TRUE, orderdesc = FALSE,
#             ordervar = "cval2", orderfun = mean, cfill = "steelblue")
# tp$plot
# tp$table
#
# # plot annotation
# tp$plot + geom_text(data=tp$table , aes( x=nam1, y = 100 * rfreq - 1.5, label=numlabs))
# tp$plot + geom_text(data=tp$table , aes( x=nam1, y = 100 * rfreq - 1.5, label=perclabs))
# tp$plot +
#         geom_text(data=tp$table , aes( x=nam1, y = 100 * rfreq - 1.5, label=ifelse(index <=2,perclabs, "")))+
#         theme(axis.text.x = element_text(angle=45, hjust=1)) +
#                  labs(title = "Statut",
#                       x = "",
#                       y = "pourcentage")

# tp$levels are for transferring level order if needed:




# num1d ==================================================================

# exploration
# dtf
# tb <- table(dtf$dval1)
# tbf <-tb/sum(tb)
# tbflabs <- paste0(100* round(tbf,2), "%")
# data.frame(tb, tbf, tbflabs)
# ggplot(dtf, aes(x = dval1)) +
#         geom_bar(width = .5, fill = "steelblue" )
#
#
# s <- summary(dtf$dval1, digits = 2)
# str(s)
# s["Min."]
# s["St.dev"] <- sd(dtf$dval1, na.rm = TRUE)
# s <-s[c("Mean", "St.dev",  "Min.", "1st Qu.", "Median", "3rd Qu.",  "Max.", "NA's") ]
# sd(dtf$dval1, na.rm = TRUE)
# mean(dtf$dval1, na.rm = TRUE)
# s
# round(s, 3)


# Function definition

# tests
# res <- num1d(dtf, "dval1")
#
# res$plot + xlab("Exemple") + ylab("Pourcentage")
#
# #
# # res <- num1d(dtf, "dval2", rfreq = FALSE)
# # res$plot




# cat2 ==================================================================


# ***************************************************************************************
# TESTS
# p2 <- cat2(dtf, "nam1", "nam2")
# p2
#
#
# p2 <- cat2(dtf, "nam1", "nam2", orderfreq1 =TRUE, ordervar1 = "nam2" , orderval1 = "j", orderfun1 = mean)
# p2
# print(p2$plot)
#
# # plot annotation and data labels
# p2$plot +
#         geom_text(data = p2$table$tbl1 , aes(x = nam1, y = .05, label = numlabs)) +
#         geom_text(data = p2$table$tbl1 , aes(x = nam1,
#                                              y = percval - 0.03,
#                                              label = ifelse( index <= 2, perclabs, ""))) +
#         theme(axis.text.x = element_text(angle=45, hjust=1)) +
#         labs(title = "Statut",
#              x = "",
#              y = "pourcentage")

# # tries: get data in ggplot
# ggplot_build(p2$plot)



