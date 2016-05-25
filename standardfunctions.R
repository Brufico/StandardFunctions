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


# make conditional frequency table
condfreqtable <- function(dataf, nomfact1, nomfact2, useNA = "no") {
        if (useNA == "no") {
                dataf <- dataf[!is.na(dataf[ ,nomfact1]) & !is.na(dataf[ ,nomfact2]) ,]
        }
        dt <-dataf %>%
                group_by_(as.name(nomfact1), as.name(nomfact2)) %>%
                summarise(num = n()) %>%
                mutate(perc = num / sum(num)) %>%
                ungroup
        as.data.frame(dt)
}

# TEST condfreqtable()

# aa <- condfreqtable(dtf, "nam1", "nam2")
# condfreqtable(dtf, "nam1", "dval1")
# reshape in a crosstab
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

# Attention: ne marche correctement qu'avec des data.frame. Les tbl_df doivent
# être traitées par as.data.frame avant appel de la fonction

orderfact <- function(dataf, nomfact, orderfreq = TRUE, orderdesc = TRUE,
                      ordervar = "c..nt", orderval = NA, orderfun = sum,
                      nlevels = NULL) {
        if (is.null(nlevels)) {
                direction <- ifelse(orderdesc,-1, 1) ; # print(direction) #dbg

                if (orderfreq & ordervar == "c..nt") {
                        dataf$c..nt <- c(1)
                }
                if (is.na(orderval) & ordervar == "c..nt") {
                        dataf$c..nt <- c(1)
                } else if (is.na(orderval) & ordervar != "c..nt") {
                        # dataf$c..nt <- ifelse(is.na(dataf[, ordervar]), 0, 1)
                        #
                        # ordervar <- "c..nt"
                } else {
                        dataf$c..nt <-
                                ifelse(is.na(dataf[, ordervar]), 0 , ifelse(dataf[, ordervar] == orderval, 1, 0))
                        ordervar <- "c..nt"
                }
                # facteur réordonné
                # resfact <- reorder(dataf[,nomfact], direction * dataf[ , ordervar], orderfun, na.rm = TRUE)


                # resfact <- reorderlevels(dataf,nomfact, nomvar=ordervar, decreasing=orderdesc , fun=orderfun)
                # resfact <- reorderlevels(dataf[,nomfact], dataf[ , ordervar], decreasing=orderdesc , fun=orderfun)
                xx <- dataf[,nomfact]
                xxx <- direction * dataf[ , ordervar]
                # print(ordervar); print(xx) ; print(xxx)
                xx <- as.vector(xx)
                xxx <- as.vector(xxx)
                # print(ordervar); print(xx) ; print(xxx); print(names(xx));print(names(xxx))
                resfact <- reorder(xx, xxx, orderfun, na.rm = TRUE)

#                 factor(dataf[,nomfact], levels = dataf[order(xxx), nomfact])
                # resfact <- with(dataf, reorder(eval(as.name(nomfact)), as.name("perc"), sum, na.rm = TRUE))

        } else {
                resfact <- factor(dataf[,nomfact], levels = nlevels)
        }
        # retour
        resfact
}

# Tests ==============================================================================================
# # TEST reorder
# mymean <- function(x) mean(x, na.rm = TRUE)
# mysum <- function(x) sum(x, na.rm = TRUE)
#
# # dtf <- make_testdata()
# table(dtf$nam1)
# table(dtf$nam1,dtf$nam2)
# mcv2 <- dtf %>% group_by(nam1) %>% summarise(val=mymean(cval2)) ; mcv2
#
# aba <- condfreqtable(dtf, "nam1", "nam2")
# aba <- as.data.frame(aba)
# abay <- aba[aba[, "nam2"] == "j", ]
# abaz <- rbind(abay,abay,abay,abay,abay,abay)
# colnames(abay)
#
#
# levels(dtf$nam1)
# dtf$nam1 <- orderfact(dtf,"nam1") ; levels(dtf$nam1) #ok
# dtf$nam1 <- orderfact(dtf,"nam1", orderdesc = FALSE) ; levels(dtf$nam1) #ok
# dtf$nam1 <- orderfact(dtf,"nam1", ordervar = "dval2", orderfun = sum, orderdesc = TRUE) ; levels(dtf$nam1)#ok
# dtf$nam1 <- orderfact(dtf,"nam1", ordervar = "dval2", orderfun = sum, orderdesc = FALSE) ; levels(dtf$nam1)#ok
# dtf$nam1 <- orderfact(dtf,"nam1", ordervar = "cval2", orderfun = mean, orderdesc = TRUE) ; levels(dtf$nam1)#ok
# dtf$nam1 <- orderfact(dtf,"nam1", ordervar = "cval2", orderfun = sum, orderdesc = TRUE) ; levels(dtf$nam1)#ok
# dtf$nam1 <- orderfact(dtf,"nam1", ordervar = "nam2", orderval = "j") ; levels(dtf$nam1)
#
# xy$nam1
# xy[,"nam1"]
# xy[,"perc"]
#
# dtf$nam1 <- orderfact(dtf,"nam1"); levels(dtf$nam1)
# aba$nam1 <- orderfact(aba,"nam1"); levels(aba$nam1)
# abaz$nam1 <- orderfact(abaz,"nam1"); levels(abaz$nam1)
#
# xy$nam1 <- orderfact(xy, "nam1", ordervar = "perc", orderfun = mean, orderdesc = TRUE) ; levels(xy$nam1)
# dtf$nam1 <- orderfact(dtf, "nam1", ordervar = "perc", orderfun = mean, orderdesc = TRUE) ; levels(dtf$nam1)
# aa$nam1 <- orderfact(aa, "nam1", ordervar = "perc", orderfun = mean, orderdesc = TRUE) ; levels(aa$nam1)



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


cat1 <- function(dataf, nomfact, useNA = "no",
                 orderfreq = TRUE, orderdesc = TRUE, ordervar = "c..nt",
                 orderval = NA, orderfun = sum,
                 rfreq = TRUE, digits = 2, cfill = "steelblue") {
        # useNA = "always, "ifany" or "no", orderfreq = TRUE  or FALSE, descorder =TRUE or FALSE
        # ordervar = variable to use for ordering,
        # orderval = value if the ordering variable is the frequency of ordervar == value

        # reordering the levels:
        dataf[,nomfact] <-
                orderfact(dataf, nomfact, orderfreq, orderdesc, ordervar, orderval, orderfun)

        # make table as dataframe
        tbl <- table(dataf[, nomfact], useNA = useNA)
        tbl <- data.frame(num = tbl, rfreq = tbl / sum(tbl))
        tbl <- tbl[, c(1,2,4)]
        names(tbl) <- c(nomfact, "num","rfreq")
        tbl$numlabs <- paste0("n=" ,tbl$num)
        tbl$perclabs <- paste0(100 * round(tbl$rfreq, digits),"%")
        tbl$index <- ave(1:nrow(tbl),  FUN = function(x) 1:length(x)) # rank
        num <- sum(tbl$num)
        #
        # bar chart with ggplot2
        # the data
        dataf1 <- if (useNA == "no") {
                dataf[which(!is.na(dataf[, nomfact])), ]
        } else {
                dataf
        }
        # base ggplot
        pt <- if (rfreq) {
                ggplot(dataf1,
                       aes_(as.name(nomfact), quote(100 * ..count.. / sum(..count..))))
        } else {
                ggplot(dataf1,
                       aes_(as.name(nomfact)))
        }
        # geom
        pt <- pt + geom_bar(fill = cfill)
        # ylabel
        if (rfreq) {pt <- pt + ylab(label = "percent")}


        list(   name = nomfact,
                levels = levels(dataf[, nomfact]),
                table = tbl, num = num, plot = pt
        )
}


# # usage, essais et tests
# # ##a
# tp <- cat1(dtf, "nam1", useNA= "no", rfreq = TRUE, orderfreq = TRUE, orderdesc = TRUE, cfill = "red")
# tp$plot
# tp$table
# # ##b
tp <- cat1(dtf, "nam1", useNA= "no", rfreq = TRUE, orderfreq = TRUE, orderdesc = FALSE,
            ordervar = "cval2", orderfun = mean, cfill = "steelblue")
tp$plot
tp$table

# plot annotation
tp$plot + geom_text(data=tp$table , aes( x=nam1, y = 100 * rfreq - 1.5, label=numlabs))
tp$plot + geom_text(data=tp$table , aes( x=nam1, y = 100 * rfreq - 1.5, label=perclabs))
tp$plot +
        geom_text(data=tp$table , aes( x=nam1, y = 100 * rfreq - 1.5, label=ifelse(index <=2,perclabs, "")))+
        theme(axis.text.x = element_text(angle=45, hjust=1)) +
                 labs(title = "Statut",
                      x = "",
                      y = "pourcentage")

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
num1d <- function(dataf, nomvar, digits = 2, sumdigits = 2,
                  rfreq = TRUE, useNA ="no",
                  width = .5, cfill = "steelblue") {
        # make a table (with nb of rows)
        tb <- table(dataf[, nomvar])
        num <- sum(tb)
        tbf <- tb/sum(tb)
        tbflabs <- paste0(100* round(tbf,digits), "%")
        tbl <- data.frame(tb, tbf, tbflabs)
        tbl <- tbl[ , c(1,2,4,5)]
        colnames(tbl) <- c(nomvar, "num", "perc", "perclabs")
        tbl$numlabs = paste0("n=", tbl$num)
        tbl$index <- ave(1:nrow(tbl),  FUN = function(x) 1:length(x)) # rank
        # print(tbl) #dbg

        # make summaries
        s = summary(dataf[ , nomvar])
        s["Num."] <- num
        s["St.dev"] <- sd(dataf[ , nomvar], na.rm = TRUE)
        s <- round(s[c( "Num.", "Mean", "St.dev", "Min.", "1st Qu.", "Median", "3rd Qu.", "Max.", "NA's") ],
                   sumdigits)
        # print(s)

        # bar chart
        # data+aes
        if (rfreq) {
                pt <- ggplot( if (useNA == "no") { dataf[which(!is.na(dataf[ , nomvar])), ]} else {dataf},
                              aes_(as.name(nomvar), quote(100 * ..count.. / sum(..count..))) )
        } else {
                pt <- ggplot( if (useNA == "no") { dataf[which(!is.na(dataf[ , nomvar])), ]} else {dataf},
                              aes_(as.name(nomvar)) )
        }
        # geom
        pt <- pt + geom_bar(width = width, fill = cfill )
        # ylabel
        if (rfreq) {pt <- pt + ylab("percent")}


        # return a list of values
        list( name = nomvar,
              summaries = s,
              table = tbl,
              num = num,
              plot = pt)
}


# tests
res <- num1d(dtf, "dval1")

res$plot + xlab("Exemple") + ylab("Pourcentage")

#
# res <- num1d(dtf, "dval2", rfreq = FALSE)
# res$plot




# cat2 ==================================================================

cat2 <- function(dataf, nomfact1, nomfact2,  useNA = "no",
                 orderfreq1 = TRUE,  orderfreq2 = TRUE, orderdesc1 = TRUE, orderdesc2 = TRUE,
                 ordervar1 = "c..nt", ordervar2 = "c..nt", orderval1 = NA, orderval2 = NA,
                 orderfun1 = sum, orderfun2 = sum,
                 rfreq = TRUE, digits = 2, cfill = "steelblue"){
        # useNA = "always, "ifany" or "no", orderfreq = TRUE  or FALSE, descorder =TRUE or FALSE
        # ordervar = variable to use for ordering

        # reordering the levels:
        # nomfact2 first
        dataf[ ,nomfact2] <-  orderfact(dataf, nomfact2, orderfreq2, orderdesc2, ordervar2, orderval2, orderfun2)
        # nomfact1
        if(orderfreq1 == TRUE & ordervar1 == nomfact2 & !is.na(orderval1)){  # cas de l'ordre des fréquences conditionnelles
                #print("Frequ cond")
                tbl <- condfreqtable(dataf, nomfact1, nomfact2,  useNA = "no")
                #print("apres Frequ cond table")
                tbl <- tbl[tbl[,nomfact2] == orderval1, ]
                # print("tbl") #dbg
                # print(tbl) #dbg
                tbl[ , nomfact1] <- orderfact(tbl, nomfact1,
                                              orderfreq1, orderdesc1, ordervar = "perc",
                                              orderfun = orderfun1) #***************
                dataf[ , nomfact1] <- orderfact(dataf, nomfact1, nlevels = levels(tbl[,nomfact1]))
        } else { # autres cas
                dataf[ , nomfact1] <- orderfact(dataf, nomfact1, orderfreq1, orderdesc1, ordervar1, orderval1, orderfun1)
        }

#         print(levels(dataf[, nomfact1])) #debug
#         print(levels(dataf[, nomfact2])) #debug
        # make table as dataframe
        tblcrois <- table(dataf[ , nomfact1], dataf[ , nomfact2], useNA = useNA)

             tbl <- as.data.frame(tblcrois)
             colnames(tbl) <- c(nomfact1,nomfact2,"num")
             # print(tbl) #debug
             num <- sum(tbl$num)

             tbl1 <- summarize_(group_by_(tbl,as.name(nomfact1)), num=quote(sum(num))) # shit with non-standard eval
             tbl2 <- summarize_(group_by_(tbl,as.name(nomfact2)), num=quote(sum(num))) # shit with non-standard eval

             # supplement tbl1
             tbl1$numlabs = paste0("n=", tbl1$num)
             if (!is.na(orderval1)){
                     tbl1$numval <- tblcrois[ ,orderval1]
                     tbl1$percval <- tbl1$numval / tbl1$num
                     tbl1$perclabs <- paste0(100 * round(tbl1$percval, digits), "%")
             }
             tbl1$index <- ave(1:nrow(tbl1),  FUN = function(x) 1:length(x)) # rank

             #  bar chart with ggplot2
             #  data
             dataf2 <- if (useNA == "no") {
                     dataf[which(!is.na(dataf[, nomfact1]) &
                                         !is.na(dataf[, nomfact2])),]
             } else {dataf
             }
             #  plot
             pt <- ggplot(dataf2) +
                     geom_bar(aes_(as.name(nomfact1), fill = as.name(nomfact2)), position = "Fill") +
                     guides(fill = guide_legend(reverse = TRUE)) +
                     ylab("percent")

             #retourner les éléments
             list(name = c(nomfact1, nomfact2),
                  levels = list(levels1 =levels(dataf[ , nomfact1]),
                                levels2 =levels(dataf[ , nomfact2]) ),
                  tables =list(tbl=tbl, tblcrois=tblcrois, tbl1=tbl1, tbl2=tbl2),
                  num = num, plot = pt
                  )
}

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



