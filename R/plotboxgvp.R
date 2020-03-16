
plotboxgvp <- function(x, var = "glucose") {


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


        variable <- c("glucose", "adrr", "lauc", "hauc", "auc", "lbgi", "hbgi", "bgi",
                      "conga", "cv", "iqr", "ji", "li", "ge", "lmage", "hmage", "mage",
                      "mean", "mv", "sd", "lpstr", "hpstr", "npstr", "pstr")
        position <- which(variable == var)
        ylabel <- c("GLUCOSE [mg/dl]", "ADRR", "LAUC [mg/dl]", "HAUC [mg/dl]", "AUC [mg/dl]",
                    "LBGI", "HBGI", "BGI",
                    "CONGA [mg/dl]", "CV [mg/dl]", "IQR [mg/dl]", "JI [(mg/dl)^2]", "LI [(mg/dl)^2/h]",
                    "GE", "LMAGE [mg/dl]", "HMAGE [mg/dl]", "MAGE [mg/dl]",
                    "MEAN [mg/dl]", "MV", "SD [mg/dl]",
                    "LPSTR [%]", "HPSTR [%]", "NPSTR [%]", "PSTR [%]")
        ylabel <- ylabel[position]

        y.plot.min <- floor(min(x[, var]))-floor(min(x[, var]))%%10
        y.plot.max <- floor(max(x[, var]))+floor(min(x[, var]))%%10

        y <- x
        y$y <- x[, var]
        y$x <- "x1"

        g <- ggplot(y, aes(y = y, x = x)) + theme_classic() +
                geom_boxplot(fill = "lightblue", color = "blue") +
                stat_summary(fun.y = mean, geom = "point", shape = 20, size = 3, color = "red", fill = "red") +
                labs(y = ylabel)  +
                theme(axis.text.y=element_text(size = 15)) +
                theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) +
                theme(legend.position="none") +
                theme(strip.text.y = element_text(size = 15)) +
                theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
                scale_y_continuous(breaks= pretty_breaks(), limits=c(y.plot.min, y.plot.max))

        suppressWarnings(plot(g))

}

