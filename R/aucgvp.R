
aucgvp <- function(x, t = 24, tdown = 70, tup = 180) {


        if(dim(x)[1] == 0) {

                stop("Dimension of data-set must be higher than 0.")

        }

        names <- c("date", "time", "glucose")
        names <- match(names, names(x))
        if(any(is.na(names))) {

                stop("Names of data-set must be date, time and glucose.")

        }

        if(any(is.na(as.character(x$date))) || any(is.na(as.character(x$time)))) {

                stop("Variables date and time must be non-NA values.")

        }

        date.time <- as.POSIXct(paste(as.character(x$date), as.character(x$time)), format = "%Y/%m/%d %H:%M:%S")
        if(any(is.na(date.time))) {

                stop("Variable date and time must have yyyy/mm/dd and hh:mm:ss format.")

        }

        if(any(is.na(x$glucose))) {

                stop("Variable glucose must be non-NA value.")

        }

        if(!is.numeric(x$glucose)) {

                stop("Variable glucose must be numeric.")

        }

        if(!is.numeric(t)) {

                stop("t must be numeric.")

        }

        if(all(t != c(4, 6, 8, 12, 24))) {

                stop("t must be 4, 6, 8, 12 or 24.")

        }

        if(!is.numeric(tdown) || !is.numeric(tup)) {

                stop("tdown and tup must be numeric.")

        }

        if(tdown < 0 || tup < 0) {

                stop("tdown and tup must be positive values.")

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


        serie <- levels(as.factor(as.numeric(as.character(x$serie))))

        x$lauc <- x$hauc <- x$auc <- 0
        for(i in 1:length(serie)) {

                position <- which(x$serie == serie[i])
                aux <- x[position, ]

                if(!any(is.na(aux$glucose))) {

                        u <- as.numeric(as.character(row.names(aux)[aux$glucose < tdown]))
                        y <- as.numeric(as.character(aux$Glucosa.free[aux$glucose < tdown]))
                        diff <- diff(c(u[1] - 1, u)) == 1

                        u <- u[diff != FALSE]
                        y <- u[diff != FALSE]

                        if(length(u) > 1) {

                                integral <- sum(abs(cumtrapz(u, y)), na.rm = T)

                        } else {

                                integral <- 0

                        }

                        x$lauc[position] <- round(mean(integral, na.rm = TRUE), digits = 2)


                        u <- as.numeric(as.character(row.names(aux)[aux$glucose > tup]))
                        y <- as.numeric(as.character(aux$glucosa[aux$glucose > tup]))
                        diff <- diff(c(u[1] - 1, u)) == 1

                        u <- u[diff != FALSE]
                        y <- u[diff != FALSE]

                        if(length(u) > 1) {

                                integral <- sum(abs(cumtrapz(u, y)), na.rm = TRUE)

                        } else {

                                integral <- 0

                        }

                        x$hauc[position] <- round(mean(integral, na.rm = TRUE), digits = 2)


                        integral <- c(x$lauc[position], x$hauc[position])

                        if(length(integral) > 0) {

                                x$auc[position] <- round(mean(integral, na.rm = TRUE), digits = 2)

                        }

                }

        }

        if(any(is.na(x$lauc))) {

                position <- is.na(x$lauc)
                x$lauc[position] <- 0

        }

        if(any(is.na(x$hauc))) {

                position <- is.na(x$hauc)
                x$hauc[position] <- 0

        }

        if(any(is.na(x$auc))) {

                position <- is.na(x$auc)
                x$auc[position] <- 0

        }

        x <- x[,-which(names(x) == "serie")]

        return(x)

}

