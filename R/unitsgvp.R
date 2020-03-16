
unitsgvp <- function(x) {


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


        x$glucose <- x$glucose * 18

        return(x)

}

