cluster <- function(df) {
    # standardize features
    # feature minus feature mean 
    # divided by feature range

    #V1  V2  V3  V4           V5
    #5.1 3.5 1.4 0.1     I.setosa
    means <- c(mean(df[,1]),mean(df[,2]),mean(df[,3]),mean(df[,4]))
    maxs <- c(max(df[,1]),max(df[,2]),max(df[,3]),max(df[,4]))
    mins <- c(min(df[,1]),min(df[,2]),min(df[,3]),min(df[,4]))
    
    for(i in 1:nrow(df)){
     df[i,1] <- (df[i,1]-means[1])/(maxs[1]-mins[1])
     df[i,2] <- (df[i,2]-means[2])/(maxs[2]-mins[2])
     df[i,3] <- (df[i,3]-means[3])/(maxs[3]-mins[3])
     df[i,4] <- (df[i,4]-means[4])/(maxs[4]-mins[4])
    }
    #df
    
    #use kmeans()
    #kmeans(x, centers, iter.max = 10, nstart = 1,
            #algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
            #      "MacQueen"), trace=FALSE)

    # test data inital centers are 1,6,11
    centers <- as.matrix(df[1,1:4])
    centers <- rbind(centers, as.matrix(df[51,1:4]))
    centers <- rbind(centers, as.matrix(df[101,1:4]))

    kmeans(df[,1:4],centers)
}