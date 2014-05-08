expandDirectory <- function(directory) {
    # check for tools to indicates relative or absolute path
    if ("R.utils" %in% rownames(installed.packages())) {
        # if not aboslute, get the absolute path    
        if (!R.utils::isAbsolutePath(directory)) {
            directory <- R.utils::getAbsolutePath(directory)             
        }  
        else {
            directory <- directory
        }
    }
    else {
        # no R.utils installed... grep
        if (Sys.info()['sysname']=="Windows") {
            # is windows
            if (grepl("^[A-Za-z]:",directory)) {
                # is absolute 
                directory <- directory
            } else {
                directory <- paste(getwd(),directory, sep="/")
            }
        }
        else {
            # not windows
            if (grepl("^/",directory)) {
                # is absolute   
                directory <- directory
            } else {
                directory <- paste(getwd(),directory, sep="/")
            }      
        }
    }
}