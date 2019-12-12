
mplotgvp <- function(x, col = "one", var = "auc") {


        if(dim(x)[1] == 0) {

                stop("Dimension of data-set must be higher than 0.")

        }

        if(var == "auc") {

                var <- c("lauc", "hauc", "auc")

        } else if(var == "bgi") {

                var <- c("lbgi", "hbgi", "bgi")

        } else if(var == "mage") {

                var <- c("ge", "lmage", "hmage", "mage")

        } else if(var == "pstr") {

                var <- c("lpstr", "hpstr", "npstr", "pstr")

        }

        names <- c("date", "time", var)
        names <- match(names, names(x))
        if(any(is.na(names))) {

                stop(paste("Names of data-set must be date, time and
                           one of these groups: c(lauc, hauc, auc), c(lbgi, hbgi, bgi),
                           c(ge, lmage, hmage, mage), or c(lpstr, hpstr, npstr, pstr)."))

        }

        if(any(is.na(as.character(x$date))) || any(is.na(as.character(x$time)))) {

                stop("Variables date and time must be non-NA values.")

        }

        date.time <- as.POSIXct(paste(as.character(x$date), as.character(x$time)), format = "%Y/%m/%d %H:%M:%S")
        position <- match(var, names(x))
        variable <- c()
        for(i in 1:length(var)){

                variable <- c(variable, !is.numeric(x[,position[3]]))

        }

        if(any(is.na(date.time))) {

                stop("Variable date and time must have yyyy/mm/dd and hh:mm:ss format.")

        }

        if(all(variable)) {

                stop("Variable must be numeric.")

        }

        if(col != "one" && col != "multiple") {

                stop("col must be one or multiple.")

        }

        suppressWarnings(if(all(var != c("lauc", "hauc", "auc")) &&
                            all(var != c("lbgi", "hbgi", "bgi")) &&
                            all(var != c("lmage", "hmage", "ge", "mage")) &&
                            all(var != c("lpstr", "hpstr", "npstr", "pstr"))) {

                stop("var must be auc, bgi, mage or pstr.")

        })


        if(length(var) == 3) {

                g1 <- plotgvp(x, col = col, var = var[1])
                g2 <- plotgvp(x, col = col, var = var[2])
                g3 <- plotgvp(x, col = col, var = var[3])
                g <- grid.arrange(grid.arrange(g1, g2, ncol = 2), grid.arrange(g3))

        } else if(length(var) == 4) {

                g1 <- plotgvp(x, col = col, var = var[1])
                g2 <- plotgvp(x, col = col, var = var[2])
                g3 <- plotgvp(x, col = col, var = var[3])
                g4 <- plotgvp(x, col = col, var = var[4])
                g <- grid.arrange(g1, g2, g3, g4, ncol = 2)

        }

        suppressWarnings(plot(g))

}

