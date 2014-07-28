##' GI to IPI mapping
##'
##'
##' @title GI2IPI
##' @param gi gi number
##' @return gi-ipi data.frame
##' @export
##' @author Yu Guangchuang
GI2IPI <- function(gi) {
    ## use gi2ipi.xrefs.gz for mapping IPI
    ## GI to IPI cross-references (gi2ipi.xrefs.gz) file
    ## is a convenient tab-delineated file mapping from GI numbers to IPI entries.
    ## This file can be downloaded from the ftp://ftp.ebi.ac.uk/pub/databases/IPI/current/
    cat ("NCBI GI:IPI mapping file prepared for the following IPI releases:\n
chicken\t\t3.69\ncow\t\t3.61\nhuman\t\t3.75\nzebrafish\t3.74\narabidopsis\t3.73\nmouse\t\t3.75\nrat\t\t3.75\n")
    fname <- "gi2ipi"
    tryCatch(utils::data(list=fname, package="bitr"))
    GI2IPI <- get("gi2ipi")
    ## idx <- unlist(sapply(gi, function(x) which(GI2IPI[,1] == x)))
    ## ipi <- GI2IPI[idx,]

    gi <- data.frame(gi=gi)
    ipi <- merge(gi, GI2IPI, by.x="gi", by.y="gi")

    ipi <- ipi[, c("gi", "ipi")]
    ipi <- ipi[!is.na(ipi[,2]),]

    ## write.table(ipi, file="gi2ipi.csv", sep=",", row.names=FALSE)
    return(ipi)
}

