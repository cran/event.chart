# event.convert.r
# convert 2-column coded events to multiple event time for event.chart()
# input: a matrix or dataframe with at least 2 columns
#        by default, the first column contains the event time and
#                    the second column contains the k event codes (e.g. 1=dead, 0=censord)
# ouput: a matrix of k columns, each column contains the time of kth coded event
#        
event.convert <- function(data2, event.time = 1, event.code = 2)
{
        dim.d <- dim(data2)
        len.t <- length(event.time)
        if(len.t != length(event.code))
                stop("length of event.time and event.code must be the same")
        if(any(event.time > dim.d[2]))
                stop(paste("Column(s) in event.time cannot be greater than ", 
                        dim.d[2]))
        if(any(event.code > dim.d[2]))
                stop(paste("Column(s) in event.code cannot be greater than ", 
                        dim.d[2]))
        name.data <- names(data2)[event.time]
        if(is.null(name.data)) {
                name.data <- paste("V", event.time, sep = "")
        }
        n.level <- rep(NA, len.t)
        for(i in (1:len.t)) {
                n.level[i] <- length(table(data2[, event.code[i]]))
        }
        tot.col <- sum(n.level)
        data.out <- matrix(NA, dim.d[1], tot.col)
        name.col <- rep(NA, tot.col)
        n.col <- 1
        for(i in (1:len.t)) {
                tab.d <- table(data2[, event.code[i]])
                if(is.null(class(data2[, event.code[i]])))
                        level.value <- as.numeric(names(tab.d))
                else level.value <- names(tab.d)
                for(j in (1:length(tab.d))) {
                        data.out[, n.col] <- rep(NA, dim.d[1])
                        check <- data2[, event.code[i]] == level.value[j]
                        check[is.na(check)] <- F
                        data.out[, n.col][data2[, event.code[i]] == level.value[
                                j]] <- data2[, event.time[i]][check]
                        name.col[n.col] <- paste(name.data[i], ".", names(tab.d
                                )[j], sep = "")
                        n.col <- n.col + 1
                }
        }
        dimnames(data.out) <- list(1:dim.d[1], name.col)
        return(as.matrix(data.out))
}
