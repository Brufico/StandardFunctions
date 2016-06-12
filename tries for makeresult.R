
#' tries for make-results
#' ======================


# six ========================
#
mkres <- function(a = NULL , b = NULL , c = NULL, d = NULL, e = NULL, x = 2, y = NULL) {
        # get arg - values list
        lenv <- as.list(environment())
        # remove NULL values from list
        if (length(which(sapply(lenv,is.null))) == 0) {
                lenv
        } else {
                lenv[-(which(sapply(lenv,is.null), arr.ind=FALSE))]
        }
}


# tries
mkres( c=3, a=1)

bobm <- matrix(c(1,2,3,4), nrow = 2)
colnames(bobm) <- c("a","y")
bobm

bobj1 <- list(d=1, list(a= NULL, e=2))


mkres( b= bobj1 , c= 4, y=3)

kk <- mkres( b= bobj1 , c= 4, y=3, a = bobm)

names(kk)
kk

ks <- mkres(a = "" , b = 2 , c = 3, d = 4, e = 5, x = 2, y = 7)
ks


make.result <- function(name = NULL,
                        numcases = NULL,
                        summaries = NULL,
                        levels = NULL,
                        breaks = NULL,
                        closed= NULL,
                        table = NULL,
                        tabledf = NULL,
                        ptable = NULL,
                        chi2 = NULL,
                        anova = NULL,
                        plot = NULL ) {
        # get arg - values list
        lenv <- as.list(environment())
        # remove NULL values from list
        if (length(which(sapply(lenv,is.null))) == 0) {
                lenv
        } else {
                lenv[-(which(sapply(lenv,is.null), arr.ind=FALSE))]
        }
}


