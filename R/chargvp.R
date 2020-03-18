
chargvp <- function(x) {


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


        y <- data.frame(matrix(nrow = 1, ncol = 8))
        names(y) <- c("Mean", "Std.deviation", "Glucose.time.<54", "Glucose.time.[54,70)",
                      "Glucose.time.[70,180]", "Glucose.time.[70,140]", "Glucose.time.>180", "Glucose.time.>250")

        target.up1 <- 140
        target.up2 <- 180
        target.up3 <- 250
        target.down1 <- 54
        target.down2 <- 70

        y$Mean <- round(mean(x$glucose, na.rm = T), digits = 2)
        y$Std.deviation <- round(sd(x$glucose, na.rm = T), digits = 2)
        y$'Glucose.time.<54' <- round(mean(pstrgvp(x, tdown = target.down1, tup = target.up2)$lpstr), digits = 2)
        y$'Glucose.time.[54,70)' <- round(mean(pstrgvp(x, tdown = target.down2, tup = target.up2)$lpstr), digits = 2) -
                round(mean(pstrgvp(x, tdown = target.down1, tup = target.up2)$lpstr), digits = 2)
        y$'Glucose.time.[70,180]' <- round(mean(pstrgvp(x, tdown = target.down2, tup = target.up2)$npstr), digits = 2)
        y$'Glucose.time.[70,140]' <- round(mean(pstrgvp(x, tdown = target.down2, tup = target.up1)$npstr), digits = 2)
        y$'Glucose.time.>180' <- round(mean(pstrgvp(x, tdown = target.down1, tup = target.up2)$hpstr), digits = 2)
        y$'Glucose.time.>250' <- round(mean(pstrgvp(x, tdown = target.down1, tup = target.up3)$hpstr), digits = 2)

        return(y)

}

