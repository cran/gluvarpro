
avggvp <- function(x, var = "glucose", sd = FALSE) {


        if(dim(x)[1] == 0) {

                stop("Dimension of data-set must be higher than 0.")

        }

        names <- c("date", "time", var)
        names <- match(names, names(x))
        if(any(is.na(names))) {

                stop(paste("Names of data-set must be date, time and
                           one of these: glucose, adrr, lauc, hauc, auc, lbgi, hbgi, bgi,
                           conga, cv, iqr, ji, li, ge, lmage, hmage, mage,
                           mean, mv, sd, lpstr, hpstr, npstr or pstr."))

        }

        if(any(is.na(as.character(x$date))) || any(is.na(as.character(x$time)))) {

                stop("Variables date and time must be non-NA values.")

        }

        date.time <- as.POSIXct(paste(as.character(x$date), as.character(x$time)), format = "%Y/%m/%d %H:%M:%S")
        position <- which(names(x) == var)
        variable <- !is.numeric(x[,position])

        if(any(is.na(date.time))) {

                stop("Variable date and time must have yyyy/mm/dd and hh:mm:ss format.")

        }

        if(variable) {

                stop("Variable must be numeric.")

        }

        if(all(var != c("glucose", "adrr", "lauc", "hauc", "auc", "lbgi", "hbgi", "bgi",
                        "conga", "cv", "iqr", "ji", "li", "ge", "lmage", "hmage", "mage",
                        "mean", "mv", "sd", "lpstr", "hpstr", "npstr", "pstr"))) {

                stop("var must be glucose, adrr, lauc, hauc, auc, lbgi, hbgi, bgi,
                     conga, cv, iqr, ji, li, ge, lmage, hmage, mage,
                     mean, mv, sd, lpstr, hpstr, npstr or pstr.")

        }


        # Calculate mean
        x.value <- mean(as.numeric(as.character(x[, var])), na.rm = T)
        x.value <- round(x.value, digits = 2)

        if(sd == FALSE) {

                x <- x.value

        } else {

                x.error <- sd(as.numeric(as.character(x[, var])), na.rm = T)
                x.error <- round(x.error, digits = 2)
                x <- paste(x.value, "\u00B1", x.error)

        }

        return(x)

}

