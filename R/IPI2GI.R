##' IPI 2 GI mapping
##'
##'
##' @title IPI2GI
##' @param ipi ipi number
##' @return ipi-gi data.fram
##' @export
##' @author Yu Guangchuang
IPI2GI <- function(ipi) {
    ## use gi2ipi.xrefs.gz for mapping IPI
    ## GI to IPI cross-references (gi2ipi.xrefs.gz) file
    ## is a convenient tab-delineated file mapping from GI numbers to IPI entries.
    ## This file can be downloaded from the ftp://ftp.ebi.ac.uk/pub/databases/IPI/current/
    cat ("NCBI GI:IPI mapping file prepared for the following IPI releases:\n
chicken\t\t3.69\ncow\t\t3.61\nhuman\t\t3.75\nzebrafish\t3.74\narabidopsis\t3.73\nmouse\t\t3.75\nrat\t\t3.75\n")
    fname <- "gi2ipi"
    tryCatch(utils::data(list=fname, package="yTools"))
    GI2IPI <- get("gi2ipi")

    ##	idx <- unlist(sapply(ipi, function(x) which(GI2IPI[,2] == x)))
    ##	gi <- GI2IPI[idx,]

    ## gi <- c()
    ## for (i in ipi) {
    ##     gi <- rbind(gi, subset(GI2IPI, subset = GI2IPI[,2] == i))
    ## }

    ipi <- data.frame(ipi=ipi)
    gi <- merge(ipi, GI2IPI, by.x="ipi", by.y="ipi")

    gi <- gi[, c("ipi", "gi")]
    gi <- gi[!is.na(gi[,2]),]
    ## write.table(gi, file="ipi2gi.csv", sep=",", row.names=FALSE)
    return(gi)
}

