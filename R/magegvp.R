
magegvp <- function(x, t = 24, n = 1) {


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

        if(all(is.na(x$glucose))) {

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

        if(!is.numeric(n)) {

                stop("n must be numeric.")

        }

        if(n < 0) {

                stop("n must be a positive value.")

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

        x$ge <- x$lmage <- x$hmage <- x$mage <- 0
        for(i in 1:length(serie)) {

                position <- which(x$serie == serie[i])
                aux <- x[position, ]

                if(!all(is.na(aux$glucose))) {

                        sd <- round(sd(as.numeric(as.character(aux$glucose)), na.rm = TRUE), digits = 2)

                        diff <- c()
                        for(j in 1:(dim(aux)[1] - 1)) {

                                diff <- c(diff, as.numeric(as.character(aux$glucose[j])) - as.numeric(as.character(aux$glucose[j + 1])))

                        }

                        diff.up <- diff[diff >= 0]
                        diff.up <- diff.up[abs(diff.up) > n * sd]
                        diff.down <- diff[diff < 0]
                        diff.down <- diff.down[abs(diff.down) > n * sd]

                        if(all(is.na(diff.down)) || length(diff.down) == 0) {

                                diff.down <- 0

                        }

                        if(all(is.na(diff.up)) || length(diff.up) == 0) {

                                diff.up <- 0

                        }

                        x$lmage[position] <- round(mean(abs(diff.down), na.rm = TRUE), digits = 2)
                        x$hmage[position] <- round(mean(abs(diff.up), na.rm = TRUE), digits = 2)
                        x$mage[position] <- round(mean(c(x$hmage[position], abs(x$lmage[position])), na.rm = TRUE), digits = 2)

                        if(length(diff.down) == 1) {

                                if(diff.down == 0) {

                                        diff.down <- NA

                                }

                        }

                        if(length(diff.up) == 1) {

                                if(diff.up == 0) {

                                        diff.up <- NA

                                }

                        }

                        x$ge[position] <- sum(c(length(diff.up[!is.na(diff.up)]), length(diff.down[!is.na(diff.down)])), na.rm = TRUE)

                }

        }

        if(any(is.na(x$lmage))) {

                position <- is.na(x$lmage)
                x$lmage[position] <- 0

        }

        if(any(is.na(x$hmage))) {

                position <- is.na(x$hmage)
                x$hmage[position] <- 0

        }

        if(any(is.na(x$ge))) {

                position <- is.na(x$ge)
                x$ge[position] <- 0

        }

        if(any(is.na(x$mage))) {

                position <- is.na(x$mage)
                x$mage[position] <- 0

        }

        x <- x[,-which(names(x) == "serie")]

        return(x)

}

