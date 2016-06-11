# **********************************************************************
# Standard functions for basic stat analysis
# *********************************************************************


library(ggplot2)
library(dplyr)
# library(ggplot2movies)

# library(reshape2)


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




# ***************** ======================================================
# helper functions =======================================================
# ========================================================================

# TEST sfdefault --------------------------------------------------
# sfdefault()
# sfdefault("language")
# sfdefault("blabla", 6)
# sfdefault("blabla")
#
# sfdefault("namesumfrench")
# sfdefault("namesum", default("namesumfrench"))
# sfdefault("namesum")


# TEST condfreqtable() -----------------------------------------------

# aa <- condfreqtable(dtf, "nam1", "nam2")
condfreqtable(dtf, "nam1", "nam2")
condfreqtable(dtf, "nam1", "dval1", useNA = "always")
condfreqtable(dtf, "nam1", "dval1", useNA = "no")

# try with mpg
condfreqtable(mpg, "class", "drv") # ==> Problem within prop.table(), only works with dataframes
condfreqtable(as.data.frame(mpg), "class", "drv")




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

aba$nam1
aba[["nam1"]]
aba[["perc"]]

dtf$nam1 <- orderfact(dtf,"nam1"); levels(dtf$nam1)
aba$nam1 <- orderfact(aba,"nam1"); levels(aba$nam1)
abaz$nam1 <- orderfact(abaz,"nam1"); levels(abaz$nam1)

aba$nam1 <- orderfact(aba, "nam1", ordervar = "perc", orderfun = mean, orderdesc = TRUE) ; levels(aba$nam1)
dtf$nam1 <- orderfact(dtf, "nam1", ordervar = "dval2", orderfun = mean, orderdesc = TRUE) ; levels(dtf$nam1)

# reordering in one dframe and transferring order to another
aba$nam1 <- orderfact(aba, "nam1", ordervar = "perc", orderfun = mean, orderdesc = TRUE) ; levels(aba$nam1)
dtf$nam1 <- orderfact(dtf, "nam1", nlevels = levels(aba$nam1)); levels(dtf$nam1)


levels(diamonds[["cut"]])
diamonds[["cut"]] <- orderfact(diamonds , "cut", ordervar = "price", orderfun = mean, orderdesc = TRUE); levels(diamonds[["cut"]])

mpg[["manufacturer"]] <- factor(mpg[["manufacturer"]])
levels(mpg[["manufacturer"]])
mpg[["manufacturer"]] <- orderfact(mpg , "manufacturer", ordervar = "hwy",
                                   orderfun = mean, orderdesc = TRUE); levels(mpg[["manufacturer"]])


mpg[["drv"]] <- factor(mpg[["drv"]]) ; levels(mpg[["drv"]])
levels(mpg[["manufacturer"]])

mpg[["manufacturer"]] <- orderfact(mpg , "manufacturer", ordervar = "drv", orderval = "4",
                                   orderfun = sum, orderdesc = TRUE); levels(mpg[["manufacturer"]])



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
# ##a
tp <- cat1(dtf, "nam1", useNA= "no", rfreq = TRUE, orderfreq = TRUE, orderdesc = TRUE, cfill = "red")
tp
tp$plot
tp$table


# ##b
tp <- cat1(dtf, "nam1", useNA= "no", rfreq = TRUE, orderfreq = TRUE, orderdesc = FALSE,
            ordervar = "cval2", orderfun = mean, cfill = "steelblue")
tp
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

## testing with mpg
mc <- cat1(mpg, "class")
mc
# with plot value labels and cosmetic changes
mc$plot +
        geom_text(data=mc$table , aes( x=class, y = 100 * rfreq - 1.5, label=ifelse(index <=6, perclabs, "")))+
        theme(axis.text.x = element_text(angle=45, hjust=1)) +
        labs(title = "Class",
             x = "",
             y = "percentage")


mc <- cat1(mpg, "class", ordervar = "drv", orderval = "4", orderdesc = FALSE)
mc$plot +
        geom_text(data=mc$table , aes( x=class, y = 100 * rfreq - 1.5, label=ifelse(rfreq >= 0.19, perclabs, "")))+
        theme(axis.text.x = element_text(angle=45, hjust=1)) +
        labs(title = "Class",
             x = "",
             y = "percentage")

# get pvalue
mc$uchisq$test1$p.value




# num1d ==================================================================


# tests
res <- num1d(dtf, "dval1")
res
res$plot + xlab("Exemple") + ylab("Pourcentage")

#
res <- num1d(dtf, "dval2", rfreq = FALSE)
res$plot
res$uchisq$test1

# test with mpg
res <- num1d(mpg, "cyl")
res
res$plot + xlab("Cylinders") + ylab("Percentage")


# # tests for num1c ===================================================
# tests for mkclabs
mkclabs( c(1,2,4,6))

# test for nonavect
nonavect(c(2,4, 6, NA, 10))


# tests for num1c
num1c(mpg,"hwy")
num1c(mpg,"hwy", plot_density = TRUE)
num1c(mpg,"hwy", bins = "nclass.FD")
num1c(mpg,"hwy", binwidth=3, plot_density = TRUE)
num1c(mpg,"hwy", breaks = seq(from=10,to=46, by=4), plot_density = TRUE)
num1c(mpg,"hwy", breaks = seq(from=11,to=46, by=3), plot_density = TRUE)
num1c(mpg,"hwy", breaks = seq(from=11,to=46, by=3), closed = "left")

num1c(mpg,"hwy", breaks = c(seq(from=11,to=38, by=3), 44), plot_density = TRUE)
num1c(mpg,"hwy", usedensity = TRUE, breaks = seq(from=10,to = 45, by=5))
num1c(mpg,"hwy", plot_density = TRUE, breaks = seq(from=10,to=46, by=4))


# stem(mpg$cty)
#
num1c(mpg,"cty", plot_density = TRUE)
num1c(mpg,"cty", bins = "nclass.FD", plot_density = TRUE )
num1c(mpg,"cty", binwidth = 2, plot_density = TRUE )
num1c(mpg,"cty", breaks = seq(from=8,to=36, by=2), plot_density = TRUE)

num1c(mpg,"cty", breaks = seq(from=9,to=35, by=2), plot_density = TRUE)

num1c(mpg,"cty", bins = "nclass.scott", plot_density = TRUE )
num1c(mpg,"cty", breaks = seq(from=7,to=37, by= 3), plot_density = TRUE)
num1c(mpg,"cty", breaks = seq(from=8,to=38, by= 3), plot_density = TRUE)


ctypt <- num1c(mpg,"cty", breaks = c(seq(from=8,to=28, by=2),36), plot_density = TRUE)

ctypt$plot + geom_text(data=ctypt$table$ggtable, aes(x, y/2, label=perclabs)) +
        labs(title="City mpg distribution")



# explore
ctypt$table$ggtable$ndensity  /ctypt$table$ggtable$rfreq



num1c(dtf,"cval1", bins = "nclass.FD")
num1c(dtf,"cval1", binwidth = 1)
num1c(dtf,"cval1", breaks=seq(2, 7, by=1), plot_density = TRUE)
num1c(dtf,"cval1", breaks=seq(1.5, 7.5, by=1), plot_density = TRUE)
num1c(dtf,"cval1", breaks=seq(2, 7, by=.5), plot_density = TRUE)

num1c(dtf,"cval2", bins = "nclass.FD", plot_density = TRUE)
num1c(dtf,"cval2", binwidth = 3, plot_density = TRUE)

num1c(dtf,"cval2", breaks=seq(3, 18, by=3), plot_density = TRUE)
c2r <- num1c(dtf,"cval2", breaks=seq(1.5, 19.5, by=3), plot_density = TRUE)

print(c2r$plot)
print(c2r$tables$ptable[c(1,3, 4)])


# new tries
# data
dat <- data.frame(f = rep(c("a", "b"), 8),
                  x = c( rep(2, 3), rep(3, 4),rep(4, 5),rep(5, 3),6 ))
dat

p <-


# cat2 ==================================================================


# ***************************************************************************************
# TESTS
p2 <- cat2(dtf, "nam1", "nam2")
p2


p2 <- cat2(dtf, "nam1", "nam2", orderfreq1 =TRUE, ordervar1 = "nam2" , orderval1 = "j", orderfun1 = mean)
p2
print(p2$plot)

# plot annotation and data labels
p2$plot +
        geom_text(data = p2$table$tbl1 , aes(x = nam1, y = .05, label = numlabs)) +
        geom_text(data = p2$table$tbl1 , aes(x = nam1,
                                             y = percval - 0.03,
                                             label = ifelse( index <= 2, perclabs, ""))) +
        theme(axis.text.x = element_text(angle=45, hjust=1)) +
        labs(title = "Statut",
             x = "",
             y = "pourcentage")


mp2 <- cat2(as.data.frame(mpg), "class", "drv", orderfreq1 =TRUE, ordervar1 = "drv" , orderval1 = "f", orderfun1 = mean)
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




mp3 <- cat2(as.data.frame(mpg), "class", "drv", orderfreq1 =TRUE, ordervar1 = "drv" , orderval1 = "4", orderfun1 = mean,
            orderfreq2 =TRUE, nlevel2 = c("4","r","f"))
mp3
mp3$plot +
        geom_text(data = mp3$table$tbl1 , aes(x = class, y = -.05, label = numlabs)) +
        geom_text(data = mp3$table$tbl1 , aes(x = class,
                                              y = percval - 0.03,
                                              label = ifelse( index <= 4, perclabs, ""))) +
        theme(axis.text.x = element_text(angle=45, hjust=1))


# verification
levels(factor(mpg$drv))

# # chisquare
# mp2c <- chisq.test(mp2$tables$tblcrois,simulate.p.value=TRUE)
# mp2c$method
# str(mp2c)
# mp2c$expected



