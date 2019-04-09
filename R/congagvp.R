
congagvp <- function(x, t = 24, ts = 15, h = 1) {


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

        if(!is.numeric(ts)) {

                stop("ts must be numeric.")

        }

        if(ts != 5 & ts != 15) {

                stop("t must be 5 or 15.")

        }

        if(!is.numeric(h)) {

                stop("h must be numeric.")

        }

        if(all(h != 1:24)) {

                stop("h must be 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23 or 24.")

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


        n <- h * (60 / ts)
        serie <- levels(as.factor(as.numeric(as.character(x$serie))))

        x$conga <- 0
        for(i in 1:length(serie)) {

                position <- which(x$serie == serie[i])
                aux <- x[position, ]


                if(!all(is.na(aux$glucose))) {

                        CONGA <- c()
                        for(j in n:dim(aux)[1]) {

                                diff <- as.numeric(as.character(aux$glucose[j])) - as.numeric(as.character(aux$glucose[j - (n - 1)]))
                                CONGA <- c(CONGA, diff)

                        }

                        x$conga[position] <- round(sd(CONGA, na.rm = TRUE), digits = 2)

                }

        }

        if(any(is.na(x$conga))) {

                position <- is.na(x$conga)
                x$conga[position] <- 0

        }

        x <- x[,-which(names(x) == "serie")]

        return(x)

}

