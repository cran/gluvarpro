
plotboxmgvp <- function(x, var = "auc") {


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

        suppressWarnings(if(all(var != c("lauc", "hauc", "auc")) &&
                            all(var != c("lbgi", "hbgi", "bgi")) &&
                            all(var != c("lmage", "hmage", "ge", "mage")) &&
                            all(var != c("lpstr", "hpstr", "npstr", "pstr"))) {

                stop("var must be auc, bgi, mage or pstr.")

        })



        if(length(var) == 3) {

                glist <- list()

                y.plot.min <- floor(min(x[, var]))-floor(min(x[, var]))%%10
                y.plot.max <- floor(max(x[, var]))+floor(min(x[, var]))%%10

                for(k in 1:3) {

                        variable <- c("glucose", "adrr", "lauc", "hauc", "auc", "lbgi", "hbgi", "bgi",
                                      "conga", "cv", "iqr", "ji", "li", "ge", "lmage", "hmage", "mage",
                                      "mean", "mv", "sd", "lpstr", "hpstr", "npstr", "pstr")
                        position <- which(variable == var[k])
                        ylabel <- c("GLUCOSE [mg/dl]", "ADRR", "LAUC [mg/dl]", "HAUC [mg/dl]", "AUC [mg/dl]",
                                    "LBGI", "HBGI", "BGI",
                                    "CONGA [mg/dl]", "CV [mg/dl]", "IQR [mg/dl]", "JI [(mg/dl)^2]", "LI [(mg/dl)^2/h]",
                                    "GE", "LMAGE [mg/dl]", "HMAGE [mg/dl]", "MAGE [mg/dl]",
                                    "MEAN [mg/dl]", "MV", "SD [mg/dl]",
                                    "LPSTR [%]", "HPSTR [%]", "NPSTR [%]", "PSTR [%]")
                        ylabel <- ylabel[position]

                        y <- x
                        y$y <- x[, var[k]]
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

                        glist[k] <- list(ggplotGrob(g))

                }

                g <- grid.arrange(grid.arrange(glist[[1]], glist[[2]], ncol = 2), grid.arrange(glist[[3]]))

        } else if(length(var) == 4) {

                glist <- list()

                y.plot.min <- floor(min(x[, var]))-floor(min(x[, var]))%%10
                y.plot.max <- floor(max(x[, var]))+floor(min(x[, var]))%%10

                for(k in 1:4) {

                        variable <- c("glucose", "adrr", "lauc", "hauc", "auc", "lbgi", "hbgi", "bgi",
                                      "conga", "cv", "iqr", "ji", "li", "ge", "lmage", "hmage", "mage",
                                      "mean", "mv", "sd", "lpstr", "hpstr", "npstr", "pstr")
                        position <- which(variable == var[k])
                        ylabel <- c("GLUCOSE [mg/dl]", "ADRR", "LAUC [mg/dl]", "HAUC [mg/dl]", "AUC [mg/dl]",
                                    "LBGI", "HBGI", "BGI",
                                    "CONGA [mg/dl]", "CV [mg/dl]", "IQR [mg/dl]", "JI [(mg/dl)^2]", "LI [(mg/dl)^2/h]",
                                    "GE", "LMAGE [mg/dl]", "HMAGE [mg/dl]", "MAGE [mg/dl]",
                                    "MEAN [mg/dl]", "MV", "SD [mg/dl]",
                                    "LPSTR [%]", "HPSTR [%]", "NPSTR [%]", "PSTR [%]")
                        ylabel <- ylabel[position]

                        y <- x
                        y$y <- x[, var[k]]
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

                        glist[k] <- list(ggplotGrob(g))

                }

                g <- grid.arrange(glist[[1]], glist[[2]], glist[[3]], glist[[4]], ncol = 2)

        }

        suppressWarnings(plot(g))

}

