
fillgvp <- function(x, method = "linear", n = 4) {


        if(dim(x)[1] == 0) {

                stop("Dimension of data-set must be higher than 0.")

        }

        names <- c("glucose")
        names <- match(names, names(x))
        if(any(is.na(names))) {

                stop("Name of data-set must be glucose.")

        }

        if(all(is.na(x$glucose))) {

                stop("Variable glucose must be non-NA value.")

        }

        if(!is.numeric(x$glucose)) {

                stop("Variable glucose must be numeric.")

        }

        if(method != "linear" && method != "cubic") {

                stop("method must be linear or cubic.")

        }

        if(!is.numeric(n)) {

                stop("n must be numeric.")

        }

        if(n < 0) {

                stop("n must be positive values.")

        }


        position <- is.na(x$glucose)
        na.before <- dim(x[position,])[1]

        if(method == "linear") {

                x$glucose <- round(na.approx(x$glucose, maxgap = n, na.rm = F), digits = 0)

        } else if(method == "cubic") {

                x$glucose <- round(na.spline(x$glucose, maxgap = n, na.rm = F), digits = 0)

        }

        position <- is.na(x$glucose)
        na.after <- na.before - dim(x[position,])[1]

        message(paste(na.before, "NA glucose values in dataset before filling."))
        message(paste(na.after, "NA glucose values filled."))
        message(paste(dim(x[position,])[1], "NA glucose values in dataset after filling."))

        return(x)

}

