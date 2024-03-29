
magegvp <- function (x, t = 24, n = 1, type = "auto") {

        if (dim(x)[1] == 0) {

                stop("Dimension of data-set must be higher than 0.")

        }

        names <- c("date", "time", "glucose")
        names <- match(names, names(x))
        if (any(is.na(names))) {

                stop("Names of data-set must be date, time and glucose.")

        }

        if (any(is.na(as.character(x$date))) || any(is.na(as.character(x$time)))) {

                stop("Variables date and time must be non-NA values.")

        }

        date.time <- as.POSIXct(paste(as.character(x$date), as.character(x$time)), format = "%Y/%m/%d %H:%M:%S")
        if (any(is.na(date.time))) {

                stop("Variable date and time must have yyyy/mm/dd and hh:mm:ss format.")

        }

        if (any(is.na(x$glucose))) {

                stop("Variable glucose must be non-NA value.")

        }

        if (!is.numeric(x$glucose)) {

                stop("Variable glucose must be numeric.")

        }

        if (!is.numeric(t)) {

                stop("t must be numeric.")

        }

        if (all(t != c(4, 6, 8, 12, 24))) {

                stop("t must be 4, 6, 8, 12 or 24.")

        }

        if (!is.numeric(n)) {

                stop("n must be numeric.")

        }

        if (n <= 0) {

                stop("n must be positive values.")

        }

        if(all(type != c("auto", "nardin2peak", "peak2nardin"))) {

                stop("type must be auto, nardin2peak or peak2nardin")

        }

        xt <- data.frame(matrix(nrow = 24, ncol = 6))
        names(xt) <- c("hour", "4h", "6h", "8h", "12h", "24h")
        xt$hour <- 0:23
        xt$"4h" <- c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4), rep(6, 4))
        xt$"6h" <- c(rep(1, 6), rep(2, 6), rep(3, 6), rep(4, 6))
        xt$"8h" <- c(rep(1, 8), rep(2, 8), rep(3, 8))
        xt$"12h" <- c(rep(1, 12), rep(2, 12))
        xt$"24h" <- 1

        t <- paste(t, "h", sep = "")
        x$serie <- NA

        date <- levels(as.factor(as.character(x$date)))
        for (i in 1:length(date)) {

                position <- which(x$date == date[i])

                x$serie[position] <- as.numeric(i)

        }

        hour <- separate(x, time, into = c("hour", "minutes", "seconds"), ":")
        hour <- as.numeric(hour$hour)
        for (i in 1:length(hour)) {

                for (j in 0:23) {

                        if (hour[i] == j) {

                                x$serie[i] <- paste(x$serie[i], ".", xt[j + 1, t], sep = "")

                        }

                }

        }


        serie <- levels(as.factor(as.numeric(as.character(x$serie))))

        x$ge <- x$mage <- 0
        for (i in 1:length(serie)) {

                position <- which(x$serie == serie[i])
                aux <- x[position, ]

                if (!any(is.na(aux$glucose))) {

                        sd <- round(sd(as.numeric(as.character(aux$glucose)) * n, na.rm = TRUE), digits = 2)
                        minmax <- c()

                        if(length(as.numeric(as.character(aux$glucose))) > 2) {

                                for(j in 1:(length(as.numeric(as.character(aux$glucose)))-2)) {

                                        sign1 <- sign(diff(c(as.numeric(as.character(aux$glucose[j])),as.numeric(as.character(aux$glucose[j+1])))))
                                        sign2 <- sign(diff(c(as.numeric(as.character(aux$glucose[j+1])), as.numeric(as.character(aux$glucose[j+2])))))

                                        if(sign1 != sign2){

                                                minmax <- c(minmax, j+1)
                                        }

                                }

                                minmax <- c(1, minmax, length(as.numeric(as.character(aux$glucose))))
                                odirection <- direction <- sign(diff(c(as.numeric(as.character(aux$glucose[minmax[1]])), as.numeric(as.character(aux$glucose[minmax[2]])))))
                                lge <- hge <- ge <- lmage <- hmage <- mage <- 0

                                for(j in 1:(length(minmax)-1)) {

                                        difference <- diff(c(as.numeric(as.character(aux$glucose[minmax[j]])), as.numeric(as.character(aux$glucose[minmax[j+1]]))))

                                        if(abs(difference) > sd) {

                                                if(direction > 0) {

                                                        hmage <- hmage + round(abs(difference), digits = 2)
                                                        hge <- hge + 1
                                                        direction <- -1

                                                        if(odirection > 0) {

                                                                mage <- mage + round(abs(difference), digits = 2)
                                                                ge <- ge + 1

                                                        }

                                                } else {

                                                        lmage <- lmage + round(abs(difference), digits = 2)
                                                        lge <- lge + 1
                                                        direction <- 1

                                                        if(odirection < 0) {

                                                                mage <- mage + round(abs(difference), digits = 2)
                                                                ge <- ge + 1

                                                        }

                                                }

                                        } else {

                                                if(direction > 0) {

                                                        direction <- -1

                                                } else {

                                                        direction <- 1

                                                }

                                        }

                                }

                                if(type == "auto") {

                                        x$ge[position] <- ge

                                        if(mage == 0) {

                                                x$mage[position] <- 0

                                        } else {

                                                x$mage[position] <- round(mage / ge, digits = 2)

                                        }

                                } else if (type == "nardin2peak") {

                                        x$ge[position] <- hge

                                        if(hmage == 0) {

                                                x$mage[position] <- 0

                                        } else {

                                                x$mage[position] <- round(hmage / hge, digits = 2)

                                        }

                                } else {

                                        x$ge[position] <- lge

                                        if(lmage == 0) {

                                                x$mage[position] <- 0

                                        } else {

                                                x$mage[position] <- round(lmage / lge, digits = 2)

                                        }

                                }

                        }

                }

        }

        if (any(is.na(x$ge))) {

                position <- is.na(x$ge)

                x$ge[position] <- 0

        }

        if (any(is.na(x$mage))) {

                position <- is.na(x$mage)

                x$mage[position] <- 0

        }

        x <- x[, -which(names(x) == "serie")]

        return(x)

}

