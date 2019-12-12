
plotgvp <- function(x, col = "one", var = "glucose") {


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

        if(col != "one" && col != "multiple") {

                stop("col must be one or multiple.")

        }

        if(all(var != c("glucose", "adrr", "lauc", "hauc", "auc", "lbgi", "hbgi", "bgi",
                        "conga", "cv", "iqr", "ji", "li", "ge", "lmage", "hmage", "mage",
                        "mean", "mv", "sd", "lpstr", "hpstr", "npstr", "pstr"))) {

                stop("var must be glucose, adrr, lauc, hauc, auc, lbgi, hbgi, bgi,
                     conga, cv, iqr, ji, li, ge, lmage, hmage, mage,
                     mean, mv, sd, lpstr, hpstr, npstr or pstr.")

        }


        xt <- data.frame(matrix(nrow = 24, ncol = 6))
        names(xt) <- c("hour", "4h", "6h", "8h", "12h", "24h")
        xt$hour <- 0:23
        xt$"4h" <- c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4), rep(6, 4))
        xt$"6h" <- c(rep(1, 6), rep(2, 6), rep(3, 6), rep(4, 6))
        xt$"8h" <- c(rep(1, 8), rep(2, 8), rep(3, 8))
        xt$"12h" <- c(rep(1, 12), rep(2, 12))
        xt$"24h" <- 1

        t <- 24
        t <- paste(t, "h", sep = "")
        x$serie <- NA

        date <- levels(as.factor(as.character(x$date)))
        for(i in 1:length(date)) {

                position <- which(x$date == date[i])
                x$serie[position] <- as.numeric(i)

        }

        hour <- separate(x, time, into = c("hour", "minutes", "seconds"), ":")
        hour <- as.numeric(hour$hour)
        for(i in 1:length(hour)) {

                for(j in 0:23) {

                        if(hour[i] == j) {

                                x$serie[i] <- paste(x$serie[i], ".", xt[j + 1, t], sep = "")

                        }

                }

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

        u <- as.numeric(unclass(date.time))
        u <- u - min(u)
        y <- as.numeric(unclass(as.Date(as.character(x$date), format = "%Y/%m/%d")))
        y <- as.numeric(levels(as.factor(as.character(y))))
        y <- y - min(y)
        y <- 0:max(y)

        if(length(y) > 1) {

                position <- 0:round(max(u) / (60 * 60 * 24))

                z <- list()
                for(i in 1:length(position)) {

                        z[i] <- list(position[i])
                }

                names(z) <- 60 * 60 * 24 * position

                if(length(z) > 20) {

                        max <- length(z) + (10 - length(z)%%20)
                        position <- abs(floor(- length(z)/20))
                        if(position %% 2 != 0) {

                                position <- position + 1
                        }

                        position <- round(seq(1, max, position))

                }

                z <- z[position]

                if(col == "one") {

                        g <- qplot(u, x[, var], data = x, geom = "point", size = I(0.5), fill = factor(x$serie)) + geom_line(color = "blue") +
                                theme_classic() +
                                labs(x = "TIME [days]", y = ylabel)  +
                                scale_x_continuous(breaks = as.numeric(names(z)), labels = z) +
                                theme(axis.text.x=element_text(size = 15), axis.text.y=element_text(size = 15)) +
                                theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) +
                                theme(legend.position="none") +
                                theme(strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15)) +
                                scale_y_continuous(breaks= pretty_breaks())

                } else if(col == "multiple") {

                        g <- qplot(u, x[, var], data = x, geom = "point", size = I(0.5), fill = factor(x$serie)) + geom_line(color = factor(x$serie)) +
                                theme_classic() +
                                labs(x = "TIME [days]", y = ylabel)  +
                                scale_x_continuous(breaks = as.numeric(names(z)), labels = z) +
                                theme(axis.text.x=element_text(size = 15), axis.text.y=element_text(size = 15)) +
                                theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) +
                                theme(legend.position="none") +
                                theme(strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15)) +
                                scale_y_continuous(breaks= pretty_breaks())

                }

        } else {

                if(max(u) >= (60 * 60)) {

                        position <- 0:round(max(u) / (60 * 60))

                        z <- list()
                        for(i in 1:length(position)) {

                                z[i] <- list(position[i])
                        }

                        names(z) <- 60 * 60 * position

                        if(length(z) > 20) {

                                max <- length(z) + (10 - length(z)%%20)
                                position <- abs(floor(- length(z)/20))
                                if(position %% 2 != 0) {

                                        position <- position + 1
                                }

                                position <- round(seq(1, max, position))

                        }

                        z <- z[position]

                        g <- qplot(u, x[, var], data = x, geom = "point", size = I(0.5), fill = factor(x$serie)) + geom_line(color = "blue") +
                                theme_classic() +
                                labs(x = "TIME [hours]", y = ylabel)  +
                                scale_x_continuous(breaks = as.numeric(names(z)), labels = z) +
                                theme(axis.text.x=element_text(size = 15), axis.text.y=element_text(size = 15)) +
                                theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) +
                                theme(legend.position="none") +
                                theme(strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15)) +
                                scale_y_continuous(breaks= pretty_breaks())

                } else {

                        position <- 0:round(max(u) / (60))

                        z <- list()
                        for(i in 1:length(position)) {

                                z[i] <- list(position[i])
                        }

                        names(z) <- 60 * position

                        if(length(z) > 20) {

                                max <- length(z) + (10 - length(z)%%20)
                                position <- abs(floor(- length(z)/20))
                                if(position %% 2 != 0) {

                                        position <- position + 1
                                }

                                position <- round(seq(1, max, position))

                        }

                        z <- z[position]

                        g <- qplot(u, x[, var], data = x, geom = "point", size = I(0.5), fill = factor(x$serie)) + geom_line(color = "blue") +
                                theme_classic() +
                                labs(x = "TIME [minutes]", y = ylabel)  +
                                scale_x_continuous(breaks = as.numeric(names(z)), labels = z) +
                                theme(axis.text.x=element_text(size = 15), axis.text.y=element_text(size = 15)) +
                                theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) +
                                theme(legend.position="none") +
                                theme(strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15)) +
                                scale_y_continuous(breaks= pretty_breaks())

                }

        }

        suppressWarnings(plot(g))

}

