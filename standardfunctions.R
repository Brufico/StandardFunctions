# **********************************************************************
# Standard functions for basic stat analysis
# *********************************************************************


library(ggplot2)
library(dplyr)
library(reshape2)


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



