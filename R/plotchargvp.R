
plotchargvp <- function(x, text = FALSE) {


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


        y <- data.frame(matrix(nrow = 5, ncol = 3))
        names(y) <- c("x", "Measure", "Value")

        y$x <- "x1"
        y$Measure <- c("4-<54", "3-[54,70)", "2-[70,180]", "1-[180,250]", "0->250")

        z <-chargvp(x)
        y$Value <- c(z$'Glucose.time.<54', z$'Glucose.time.[54,70)', z$'Glucose.time.[70,180]',
                     z$'Glucose.time.>180'-z$'Glucose.time.>250', z$'Glucose.time.>250')

        colors <- c("#D53E4F", "#FB8072", "#B3DE69", "#FFFFB3", "#FFED6F")

        size.letter <- 10
        y.plot.min <- 0
        y.plot.max <- 101
        y.break <- 10

        y$Measure <- as.factor(as.character(y$Measure))
        levels(y$Measure) <- c(">250", "(180,250]", "[70,180]", "[54,70)", "<54")

        if(text == FALSE) {

                g <- ggplot(y, aes(fill = y$Measure, y = y$Value, x = x)) + theme_classic() +
                        geom_bar(position = "stack", stat = "identity") +
                        scale_y_continuous(name = "Percentage spent in target range [%]",
                                           breaks = seq(y.plot.min, y.plot.max, y.break),
                                           limits = c(y.plot.min, y.plot.max)) +
                        theme(axis.text.y=element_text(size = 15)) +
                        theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) +
                        theme(strip.text.y = element_text(size = 15)) +
                        theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
                        scale_fill_manual("Target [mg/dl]", values = c("<54" = colors[1], "[54,70)" = colors[2], "[70,180]" = colors[3], "(180,250]" = colors[4], ">250" = colors[5])) +
                        guides(color = guide_legend(override.aes = list(size=size.letter)))

        } else {

                g <- ggplot(y, aes(fill = y$Measure, y = y$Value, x = x)) + theme_classic() +
                        geom_bar(position = "stack", stat = "identity") +
                        geom_text(aes(label = paste(y$Value, "%")), color = "blue", position = "stack", vjust = 1) +
                        scale_y_continuous(name = "Percentage spent in target range [%]",
                                           breaks = seq(y.plot.min, y.plot.max, y.break),
                                           limits = c(y.plot.min, y.plot.max)) +
                        theme(axis.text.y=element_text(size = 15)) +
                        theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) +
                        theme(strip.text.y = element_text(size = 15)) +
                        theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
                        scale_fill_manual("Target [mg/dl]", values = c("<54" = colors[1], "[54,70)" = colors[2], "[70,180]" = colors[3], "(180,250]" = colors[4], ">250" = colors[5])) +
                        guides(color = guide_legend(override.aes = list(size=size.letter)))

        }



        suppressWarnings(plot(g))

}

