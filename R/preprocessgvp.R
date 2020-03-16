
preprocessgvp <- function(x, dp = 2, tp = 3, gp = 31, ts = 5, df = "yyyy/mm/dd",
                          tf = "hh:mm:ss", all = FALSE, type = "normal") {


        if(dim(x)[1] == 0) {

                stop("Dimension of data-set must be higher than 0.")

        }

        if(!is.numeric(dp)) {

                stop("dp must be numeric.")

        }

        if(!is.numeric(tp)) {

                stop("tp must be numeric.")

        }

        if(!is.numeric(gp)) {

                stop("gp must be numeric.")

        }

        if(dp < 0) {

                stop("dp must be positive values.")

        }

        if(tp < 0) {

                stop("tp must be positive values.")

        }

        if(gp < 0) {

                stop("gp must be positive values.")

        }

        if(dp > dim(x)[2]) {

                stop(paste("dp must be less or equal to", dim(x)[2]))

        }

        if(tp > dim(x)[2]) {

                stop(paste("tp must be less or equal to", dim(x)[2]))

        }

        if(gp > dim(x)[2]) {

                stop(paste("gp must be less or equal to", dim(x)[2]))

        }

        if(!is.numeric(ts)) {

                stop("ts must be numeric.")

        }

        if(ts != 5 & ts != 15) {

                stop("t must be 5 or 15.")

        }

        if(df != "yyyy/mm/dd" && df != "dd/mm/yyyy") {

                stop("Variable date must be yyyy/mm/dd or dd/mm/yyyy.")

        }

        if(tf != "hh:mm:ss" && tf != "hh:mm") {

                stop("Variable time must be hh:mm:ss or hh:mm.")

        }

        if(!is.logical(all)){

                stop("all must be logical.")

        }

        if(all != FALSE && all != TRUE) {

                stop("all must be FALSE or TRUE.")

        }

        if(type != "normal" && type != "round" && type != "complete") {

                stop("Variable type must be normal, round or complete.")

        }


        y <- data.frame(matrix(nrow = dim(x)[1], ncol = 3))
        names(y) <- c("date", "time", "glucose")

        if(dp == tp) {

                if(df == "yyyy/mm/dd" && tf == "hh:mm:ss") {

                        y$date <- as.character(substr(x[,dp], start = 1, stop = 10))
                        y$time <- as.character(substr(x[,dp], start = 12, stop = 19))
                        y$glucose <- x[,gp]

                        date.time <- as.POSIXct(paste(as.character(y$date), as.character(y$time)), format = "%Y/%m/%d %H:%M:%S")
                        if(any(is.na(date.time))) {

                                stop("Variable date and time must have yyyy/mm/dd and hh:mm:ss format.")

                        }

                } else if(df == "yyyy/mm/dd" && tf == "hh:mm") {

                        y$date <- as.character(substr(x[,dp], start = 1, stop = 10))
                        y$time <- as.character(substr(x[,dp], start = 12, stop = 16))
                        y$time <- paste(y$time, ":", "00", sep = "")
                        y$glucose <- x[,gp]

                        date.time <- as.POSIXct(paste(as.character(y$date), as.character(y$time)), format = "%Y/%m/%d %H:%M")
                        if(any(is.na(date.time))) {

                                stop("Variable date and time must have yyyy/mm/dd and hh:mm format.")

                        }

                } else if(df == "dd/mm/yyyy" && tf == "hh:mm:ss") {

                        y$date <- as.character(substr(x[,dp], start = 1, stop = 10))
                        y$date <- format(strptime(as.character(y$date), "%d/%m/%Y"), "%Y/%m/%d")
                        y$time <- as.character(substr(x[,dp], start = 12, stop = 19))
                        y$glucose <- x[,gp]

                        date.time <- as.POSIXct(paste(as.character(y$date), as.character(y$time)), format = "%d/%m/%Y %H:%M:%S")
                        if(any(is.na(date.time))) {

                                stop("Variable date and time must have dd/mm/yyyy and hh:mm:ss format.")

                        }

                } else if(df == "dd/mm/yyyy" && tf == "hh:mm") {

                        y$date <- as.character(substr(x[,dp], start = 1, stop = 10))
                        y$date <- format(strptime(as.character(y$date), "%d/%m/%Y"), "%Y/%m/%d")
                        y$time <- as.character(substr(x[,dp], start = 12, stop = 16))
                        y$time <- paste(y$time, ":", "00", sep = "")
                        y$glucose <- x[,gp]

                        date.time <- as.POSIXct(paste(as.character(y$date), as.character(y$time)), format = "%d/%m/%Y %H:%M")
                        if(any(is.na(date.time))) {

                                stop("Variable date and time must have dd/mm/yyyy and hh:mm format.")

                        }

                }

        } else {

                if(df == "yyyy/mm/dd" && tf == "hh:mm:ss") {

                        y$date <- as.character(x[,dp])
                        y$time <- as.character(x[,tp])
                        y$glucose <- x[,gp]

                        date.time <- as.POSIXct(paste(as.character(y$date), as.character(y$time)), format = "%Y/%m/%d %H:%M:%S")
                        if(any(is.na(date.time))) {

                                stop("Variable date and time must have yyyy/mm/dd and hh:mm:ss format.")

                        }

                } else if(df == "yyyy/mm/dd" && tf == "hh:mm") {

                        y$date <- as.character(x[,dp])
                        y$time <- as.character(x[,tp])
                        y$time <- paste(y$time, ":", "00", sep = "")
                        y$glucose <- x[,gp]

                        date.time <- as.POSIXct(paste(as.character(y$date), as.character(y$time)), format = "%Y/%m/%d %H:%M")
                        if(any(is.na(date.time))) {

                                stop("Variable date and time must have yyyy/mm/dd and hh:mm format.")

                        }

                } else if(df == "dd/mm/yyyy" && tf == "hh:mm:ss") {

                        y$date <- as.character(x[,dp])
                        y$date <- format(strptime(as.character(y$date), "%d/%m/%Y"), "%Y/%m/%d")
                        y$time <- as.character(x[,tp])
                        y$glucose <- x[,gp]

                        date.time <- as.POSIXct(paste(as.character(y$date), as.character(y$time)), format = "%d/%m/%Y %H:%M:%S")
                        if(any(is.na(date.time))) {

                                stop("Variable date and time must have dd/mm/yyyy and hh:mm:ss format.")

                        }

                } else if(df == "dd/mm/yyyy" && tf == "hh:mm") {

                        y$date <- as.character(x[,dp])
                        y$date <- format(strptime(as.character(y$date), "%d/%m/%Y"), "%Y/%m/%d")
                        y$time <- as.character(x[,tp])
                        y$time <- paste(y$time, ":", "00", sep = "")
                        y$glucose <- x[,gp]

                        date.time <- as.POSIXct(paste(as.character(y$date), as.character(y$time)), format = "%d/%m/%Y %H:%M")
                        if(any(is.na(date.time))) {

                                stop("Variable date and time must have dd/mm/yyyy and hh:mm format.")

                        }

                }

        }

        if(all == TRUE) {

                y <- cbind(y, x)

        }

        for(i in 1:dim(y)[1]) {

                y$id[i] <- paste(substr(y$date[i], start = 3, stop = 4),
                                 substr(y$date[i], start = 6, stop = 7),
                                 substr(y$date[i], start = 9, stop = 10),
                                 substr(y$time[i], start = 1, stop = 2),
                                 substr(y$time[i], start = 4, stop = 5), sep = "")

        }

        y <- y[order(y$id), ]
        row.names(y) <- c(1:dim(y)[1])

        id <- levels(as.factor(as.character(y$id)))

        y3 <- data.frame()
        for(i in 1:length(id)) {

                position <- NULL
                position <- which(id[i] == y$id)

                if(length(position) == 1) {

                        y3 <- rbind(y3, y[position,])

                } else if(length(position) > 1) {

                        y2 <- y[position[1],]

                        for(j in 2:length(position)) {

                                for(k in 1:dim(y)[2]) {

                                        if(is.na(y2[,k]) || y2[,k] == "" || y2[,k] == "0") {

                                                y2[,k] <- y[position[j],k]

                                        }

                                }

                        }

                        y3 <- rbind(y3, y2)

                }

        }

        y <- y3

        y <- y[order(y$id), ]
        row.names(y) <- c(1:dim(y)[1])

        if(ts == 5) {

                if(type == "normal") {

                        y <- y[,-dim(y)[2]]

                } else if(type == "round") {

                        y$hour <- substr(y$time, start = 1, stop = 2)
                        y$minutes <- substr(y$time, start = 4, stop = 5)

                        for(i in 1:dim(y)[1]) {

                                if(y$minutes[i] >= "00" && y$minutes[i] < "03") {

                                        y$minutes[i] <- "00"

                                } else if(y$minutes[i] >= "03" && y$minutes[i] < "08") {

                                        y$minutes[i] <- "05"

                                } else if(y$minutes[i] >= "08" && y$minutes[i] < "13") {

                                        y$minutes[i] <- "10"

                                } else if(y$minutes[i] >= "13" && y$minutes[i] < "18") {

                                        y$minutes[i] <- "15"

                                } else if(y$minutes[i] >= "18" && y$minutes[i] < "23") {

                                        y$minutes[i] <- "20"

                                } else if(y$minutes[i] >= "23" && y$minutes[i] < "28") {

                                        y$minutes[i] <- "25"

                                } else if(y$minutes[i] >= "28" && y$minutes[i] < "33") {

                                        y$minutes[i] <- "30"

                                } else if(y$minutes[i] >= "33" && y$minutes[i] < "38") {

                                        y$minutes[i] <- "35"

                                } else if(y$minutes[i] >= "38" && y$minutes[i] < "43") {

                                        y$minutes[i] <- "40"

                                } else if(y$minutes[i] >= "43" && y$minutes[i] < "48") {

                                        y$minutes[i] <- "45"

                                } else if(y$minutes[i] >= "48" && y$minutes[i] < "53") {

                                        y$minutes[i] <- "50"

                                } else if(y$minutes[i] >= "53" && y$minutes[i] < "58") {

                                        y$minutes[i] <- "55"

                                } else if(y$minutes[i] >= "58") {

                                        y$minutes[i] <- "00"
                                        y$hour[i] <- as.character(as.numeric(y$hour[i]) + 1)

                                        if(y$hour[i] == "24") {

                                                y$hour[i] <- "23"
                                                y$minutes[i] <- "45"

                                        }

                                }

                                if(nchar(as.character(y$hour[i])) == 1) {

                                        y$hour[i] <- paste("0", as.character(y$hour[i]), sep = "")

                                }

                                if(nchar(as.character(y$minutes[i])) == 1) {

                                        y$minutes[i] <- paste("0", as.character(y$minutes[i]), sep = "")

                                }

                        }

                        y$time <- paste(y$hour, ":", y$minutes, ":", "00", sep = "")

                        for(i in 1:dim(y)[1]) {

                                y$id[i] <- paste(substr(y$date[i], start = 3, stop = 4),
                                                 substr(y$date[i], start = 6, stop = 7),
                                                 substr(y$date[i], start = 9, stop = 10),
                                                 substr(y$time[i], start = 1, stop = 2),
                                                 substr(y$time[i], start = 4, stop = 5), sep = "")

                        }

                        y <- y[order(y$id), ]
                        row.names(y) <- c(1:dim(y)[1])

                        id <- levels(as.factor(as.character(y$id)))

                        y3 <- data.frame()
                        for(i in 1:length(id)) {

                                position <- NULL
                                position <- which(id[i] == y$id)

                                if(length(position) == 1) {

                                        y3 <- rbind(y3, y[position,])

                                } else if(length(position) > 1) {

                                        y2 <- y[position[1],]

                                        for(j in 2:length(position)) {

                                                for(k in 1:dim(y)[2]) {

                                                        if(is.na(y2[,k]) || y2[,k] == "" || y2[,k] == "0") {

                                                                y2[,k] <- y[position[j],k]

                                                        }

                                                }

                                        }

                                        y3 <- rbind(y3, y2)

                                }

                        }

                        y <- y3

                        y <- y[order(y$id), ]
                        row.names(y) <- c(1:dim(y)[1])

                        y <- y[,-c(dim(y)[2], dim(y)[2]-1, dim(y)[2]-2)]

                } else if(type == "complete") {

                        y$hour <- substr(y$time, start = 1, stop = 2)
                        y$minutes <- substr(y$time, start = 4, stop = 5)

                        for(i in 1:dim(y)[1]) {

                                if(y$minutes[i] >= "00" && y$minutes[i] < "03") {

                                        y$minutes[i] <- "00"

                                } else if(y$minutes[i] >= "03" && y$minutes[i] < "08") {

                                        y$minutes[i] <- "05"

                                } else if(y$minutes[i] >= "08" && y$minutes[i] < "13") {

                                        y$minutes[i] <- "10"

                                } else if(y$minutes[i] >= "13" && y$minutes[i] < "18") {

                                        y$minutes[i] <- "15"

                                } else if(y$minutes[i] >= "18" && y$minutes[i] < "23") {

                                        y$minutes[i] <- "20"

                                } else if(y$minutes[i] >= "23" && y$minutes[i] < "28") {

                                        y$minutes[i] <- "25"

                                } else if(y$minutes[i] >= "28" && y$minutes[i] < "33") {

                                        y$minutes[i] <- "30"

                                } else if(y$minutes[i] >= "33" && y$minutes[i] < "38") {

                                        y$minutes[i] <- "35"

                                } else if(y$minutes[i] >= "38" && y$minutes[i] < "43") {

                                        y$minutes[i] <- "40"

                                } else if(y$minutes[i] >= "43" && y$minutes[i] < "48") {

                                        y$minutes[i] <- "45"

                                } else if(y$minutes[i] >= "48" && y$minutes[i] < "53") {

                                        y$minutes[i] <- "50"

                                } else if(y$minutes[i] >= "53" && y$minutes[i] < "58") {

                                        y$minutes[i] <- "55"

                                } else if(y$minutes[i] >= "58") {

                                        y$minutes[i] <- "00"
                                        y$hour[i] <- as.character(as.numeric(y$hour[i]) + 1)

                                        if(y$hour[i] == "24") {

                                                y$hour[i] <- "23"
                                                y$minutes[i] <- "45"

                                        }

                                }

                                if(nchar(as.character(y$hour[i])) == 1) {

                                        y$hour[i] <- paste("0", as.character(y$hour[i]), sep = "")

                                }

                                if(nchar(as.character(y$minutes[i])) == 1) {

                                        y$minutes[i] <- paste("0", as.character(y$minutes[i]), sep = "")

                                }

                        }

                        y$time <- paste(y$hour, ":", y$minutes, ":", "00", sep = "")

                        for(i in 1:dim(y)[1]) {

                                y$id[i] <- paste(substr(y$date[i], start = 3, stop = 4),
                                                 substr(y$date[i], start = 6, stop = 7),
                                                 substr(y$date[i], start = 9, stop = 10),
                                                 substr(y$time[i], start = 1, stop = 2),
                                                 substr(y$time[i], start = 4, stop = 5), sep = "")

                        }

                        y <- y[order(y$id), ]
                        row.names(y) <- c(1:dim(y)[1])

                        id <- levels(as.factor(as.character(y$id)))

                        y3 <- data.frame()
                        for(i in 1:length(id)) {

                                position <- NULL
                                position <- which(id[i] == y$id)

                                if(length(position) == 1) {

                                        y3 <- rbind(y3, y[position,])

                                } else if(length(position) > 1) {

                                        y2 <- y[position[1],]

                                        for(j in 2:length(position)) {

                                                for(k in 1:dim(y)[2]) {

                                                        if(is.na(y2[,k]) || y2[,k] == "" || y2[,k] == "0") {

                                                                y2[,k] <- y[position[j],k]

                                                        }

                                                }

                                        }

                                        y3 <- rbind(y3, y2)

                                }

                        }

                        y <- y3

                        y <- y[order(y$id), ]
                        row.names(y) <- c(1:dim(y)[1])

                        y <- y[,-c(dim(y)[2], dim(y)[2]-1)]

                        days <- length(levels(as.factor(as.character(y$date))))
                        n <- 288
                        y2 <- data.frame(matrix(nrow = days * n, ncol = dim(y)[2]))
                        names(y2) <- names(y)

                        y2$date <- rep(levels(as.factor(as.character(y$date))), each = n)
                        hour <- rep(rep(c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12",
                                          "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"), each = n / 24), days)
                        minutes <- rep(c("00", "05", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55"), 24 * days)
                        y2$time <- paste(hour, ":", minutes, ":", "00", sep = "")

                        for(i in 1:dim(y2)[1]) {

                                y2$id[i] <- paste(substr(y2$date[i], start = 3, stop = 4),
                                                  substr(y2$date[i], start = 6, stop = 7),
                                                  substr(y2$date[i], start = 9, stop = 10),
                                                  hour[i], minutes[i], sep = "")

                        }

                        id <- setdiff(y2$id, y$id)

                        y3 <- data.frame()
                        for(i in 1:length(id)) {

                                position <- which(y2$id == id[i])
                                y3 <- rbind(y3, y2[position,])


                        }
                        y <- rbind(y, y3)

                        y <- y[order(y$id), ]
                        row.names(y) <- c(1:dim(y)[1])

                        y <- y[,-dim(y)[2]]

                }

        } else if(ts == 15) {

                if(type == "normal") {

                        y <- y[,-dim(y)[2]]

                } else if(type == "round") {

                        y$hour <- substr(y$time, start = 1, stop = 2)
                        y$minutes <- substr(y$time, start = 4, stop = 5)

                        for(i in 1:dim(y)[1]) {

                                if(y$minutes[i] >= "00" && y$minutes[i] < "08") {

                                        y$minutes[i] <- "00"

                                } else if(y$minutes[i] >= "08" && y$minutes[i] < "23") {

                                        y$minutes[i] <- "15"

                                } else if(y$minutes[i] >= "23" && y$minutes[i] < "38") {

                                        y$minutes[i] <- "30"

                                } else if(y$minutes[i] >= "38" && y$minutes[i] < "53") {

                                        y$minutes[i] <- "45"

                                } else if(y$minutes[i] >= "53") {

                                        y$minutes[i] <- "00"
                                        y$hour[i] <- as.character(as.numeric(y$hour[i]) + 1)

                                        if(y$hour[i] == "24") {

                                                y$hour[i] <- "23"
                                                y$minutes[i] <- "45"

                                        }

                                }

                                if(nchar(as.character(y$hour[i])) == 1) {

                                        y$hour[i] <- paste("0", as.character(y$hour[i]), sep = "")

                                }

                                if(nchar(as.character(y$minutes[i])) == 1) {

                                        y$minutes[i] <- paste("0", as.character(y$minutes[i]), sep = "")

                                }

                        }

                        y$time <- paste(y$hour, ":", y$minutes, ":", "00", sep = "")

                        for(i in 1:dim(y)[1]) {

                                y$id[i] <- paste(substr(y$date[i], start = 3, stop = 4),
                                                 substr(y$date[i], start = 6, stop = 7),
                                                 substr(y$date[i], start = 9, stop = 10),
                                                 substr(y$time[i], start = 1, stop = 2),
                                                 substr(y$time[i], start = 4, stop = 5), sep = "")

                        }

                        y <- y[order(y$id), ]
                        row.names(y) <- c(1:dim(y)[1])

                        id <- levels(as.factor(as.character(y$id)))

                        y3 <- data.frame()
                        for(i in 1:length(id)) {

                                position <- NULL
                                position <- which(id[i] == y$id)

                                if(length(position) == 1) {

                                        y3 <- rbind(y3, y[position,])

                                } else if(length(position) > 1) {

                                        y2 <- y[position[1],]

                                        for(j in 2:length(position)) {

                                                for(k in 1:dim(y)[2]) {

                                                        if(is.na(y2[,k]) || y2[,k] == "" || y2[,k] == "0") {

                                                                y2[,k] <- y[position[j],k]

                                                        }

                                                }

                                        }

                                        y3 <- rbind(y3, y2)

                                }

                        }

                        y <- y3

                        y <- y[order(y$id), ]
                        row.names(y) <- c(1:dim(y)[1])

                        y <- y[,-c(dim(y)[2], dim(y)[2]-1, dim(y)[2]-2)]

                } else if(type == "complete") {

                        y$hour <- substr(y$time, start = 1, stop = 2)
                        y$minutes <- substr(y$time, start = 4, stop = 5)

                        for(i in 1:dim(y)[1]) {

                                if(y$minutes[i] >= "00" && y$minutes[i] < "08") {

                                        y$minutes[i] <- "00"

                                } else if(y$minutes[i] >= "08" && y$minutes[i] < "23") {

                                        y$minutes[i] <- "15"

                                } else if(y$minutes[i] >= "23" && y$minutes[i] < "38") {

                                        y$minutes[i] <- "30"

                                } else if(y$minutes[i] >= "38" && y$minutes[i] < "53") {

                                        y$minutes[i] <- "45"

                                } else if(y$minutes[i] >= "53") {

                                        y$minutes[i] <- "00"
                                        y$hour[i] <- as.character(as.numeric(y$hour[i]) + 1)

                                        if(y$hour[i] == "24") {

                                                y$hour[i] <- "23"
                                                y$minutes[i] <- "45"

                                        }

                                }

                                if(nchar(as.character(y$hour[i])) == 1) {

                                        y$hour[i] <- paste("0", as.character(y$hour[i]), sep = "")

                                }

                                if(nchar(as.character(y$minutes[i])) == 1) {

                                        y$minutes[i] <- paste("0", as.character(y$minutes[i]), sep = "")

                                }

                        }

                        y$time <- paste(y$hour, ":", y$minutes, ":", "00", sep = "")

                        for(i in 1:dim(y)[1]) {

                                y$id[i] <- paste(substr(y$date[i], start = 3, stop = 4),
                                                 substr(y$date[i], start = 6, stop = 7),
                                                 substr(y$date[i], start = 9, stop = 10),
                                                 substr(y$time[i], start = 1, stop = 2),
                                                 substr(y$time[i], start = 4, stop = 5), sep = "")

                        }

                        y <- y[order(y$id), ]
                        row.names(y) <- c(1:dim(y)[1])

                        id <- levels(as.factor(as.character(y$id)))

                        y3 <- data.frame()
                        for(i in 1:length(id)) {

                                position <- NULL
                                position <- which(id[i] == y$id)

                                if(length(position) == 1) {

                                        y3 <- rbind(y3, y[position,])

                                } else if(length(position) > 1) {

                                        y2 <- y[position[1],]

                                        for(j in 2:length(position)) {

                                                for(k in 1:dim(y)[2]) {

                                                        if(is.na(y2[,k]) || y2[,k] == "" || y2[,k] == "0") {

                                                                y2[,k] <- y[position[j],k]

                                                        }

                                                }

                                        }

                                        y3 <- rbind(y3, y2)

                                }

                        }
                        y <- y3

                        y <- y[order(y$id), ]
                        row.names(y) <- c(1:dim(y)[1])

                        y <- y[,-c(dim(y)[2], dim(y)[2]-1)]

                        days <- length(levels(as.factor(as.character(y$date))))
                        n <- 96
                        y2 <- data.frame(matrix(nrow = days * n, ncol = dim(y)[2]))
                        names(y2) <- names(y)

                        y2$date <- rep(levels(as.factor(as.character(y$date))), each = n)
                        hour <- rep(rep(c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12",
                                          "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"), each = n / 24), days)
                        minutes <- rep(c("00", "15", "30", "45"), 24 * days)
                        y2$time <- paste(hour, ":", minutes, ":", "00", sep = "")

                        for(i in 1:dim(y2)[1]) {

                                y2$id[i] <- paste(substr(y2$date[i], start = 3, stop = 4),
                                                  substr(y2$date[i], start = 6, stop = 7),
                                                  substr(y2$date[i], start = 9, stop = 10),
                                                  hour[i], minutes[i], sep = "")

                        }

                        id <- setdiff(y2$id, y$id)

                        y3 <- data.frame()
                        for(i in 1:length(id)) {

                                position <- which(y2$id == id[i])
                                y3 <- rbind(y3, y2[position,])


                        }
                        y <- rbind(y, y3)

                        y <- y[order(y$id), ]
                        row.names(y) <- c(1:dim(y)[1])

                        y <- y[,-dim(y)[2]]

                }

        }

        return(y)

}

