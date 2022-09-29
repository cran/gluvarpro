
strgvp <- function(x) {


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

        if(!is.numeric(x$glucose)) {

                stop("Variable glucose must be numeric.")

        }


        # Summary glucose values, days, range and NA-values.
        y <- data.frame(matrix(nrow = 1, ncol = 4))
        y[1,1] <- length(levels(as.factor(as.character(x$date))))
        y[1,2] <- dim(x)[1]
        y[1,3] <- dim(x[!is.na(x$glucose),])[1]
        y[1,4] <- dim(x[is.na(x$glucose),])[1]
        y[1,5] <- paste("[", range(x$glucose, na.rm = T)[1], ",", range(x$glucose, na.rm = T)[2], "]", sep = "")
        names(y) <- c("Days", "Registers", "Glucose.values", "Glucose.NA-values", "Glucose.range")

        # Summary glucose calendar
        x <- separate(x, date, into = c("year", "month", "day"), "/", remove = F)
        x$id <- paste(x$year, x$month, x$day, sep = "")
        year <- split(x, as.character(x$id))
        count.days <- 0
        z <- data.frame()
        for(k in 1:length(year)) {

                year.day <- year[[k]]
                month <- split(year.day, as.character(year.day$month))

                for(i in 1:length(month)) {

                        month.day <- split(month[[i]], as.character(month[[i]]$day))
                        month.days <- levels(as.factor(as.character(month[[i]]$day)))
                        for(j in 1:length(month.days)) {

                                locale <- Sys.getlocale("LC_TIME")
                                Sys.setlocale("LC_TIME", "C")
                                month <- levels(as.factor(as.character(months(as.Date(month.day[[j]]$date,'%Y/%m/%d')))))
                                Sys.setlocale("LC_TIME", locale)
                                min <- substr(as.character(month.day[[j]]$time[1]), start = 1, stop = 5)
                                max <- substr(as.character(month.day[[j]]$time[dim(month.day[[j]])[1]]), start = 1, stop = 5)
                                frame2 <- data.frame(Year = substr(names(year)[k], start = 1, stop = 4), Month = month,
                                                     Day = names(month.day)[j], Time = paste(min, "-", max, sep = ""), "Registers" = dim(month.day[[j]])[1],
                                                     "Glucose.values" = dim(month.day[[j]][!is.na(month.day[[j]]$glucose),])[1],
                                                     "Glucose.NA-values" = dim(month.day[[j]][is.na(month.day[[j]]$glucose),])[1])
                                z <- rbind(z, frame2)

                        }

                        count.days <- length(month.days) + count.days

                }

        }

        return(list(y,z))

}

