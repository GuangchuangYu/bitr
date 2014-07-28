##' Convert IPI to Symbol_symbol
##'
##' Given a character vector of IPI, this function will convert it to
##' Symbol_symbol.
##'
##'
##' @param ipi IPI.
##' @param organism organism.
##' @param useBiomart using biomart or using annnotation package to convert ID
##' @param withoutVersion with or without IPI version
##' @return \item{Symbol}{Symbol_symbol.}
##' @importFrom biomaRt getBM
##' @importFrom biomaRt useMart
##' @importFrom plyr ldply
##' @importMethodsFrom AnnotationDbi mappedkeys
##' @export
##' @seealso \code{\link{IPI2EG}}
##' @keywords manip
##' @examples
##'
##' 	IPI2Symbol("IPI00894498")
##'
IPI2Symbol <- function(ipi, organism = "human", useBiomart=FALSE, withoutVersion=TRUE) {
    if (useBiomart == TRUE) {
        ##require(biomaRt)
        if (organism == "human") {
            wh_dataset = "hsapiens_gene_ensembl"
        } else if (organism == "mouse") {
            wh_dataset = "mmusculus_gene_ensembl"
        } else if (organism == "rat") {
            wh_dataset = "rnorvegicus_gene_ensembl"
        } else {
            stop ("Not supported yet...\n")
        }
        ensembl = useMart("ensembl", dataset=wh_dataset)
        if (organism == "human") {
            symbol <- getBM(attributes=c("ipi", "hgnc_symbol"), filters="ipi", values=ipi, mart=ensembl)
        } else if (organism == "mouse" ) {
            symbol <- getBM(attributes=c("ipi", "mgi_symbol"), filters="ipi", values=ipi, mart=ensembl)
        }
        result <- symbol[!is.na(symbol[,2]),]
        result <- symbol[symbol[,2] != "",]
    } else {
        if (organism == "human") {
            ##' @importFrom org.Hs.ipi.db org.Hs.ipiSYMBOL
            pkg <- "org.Hs.ipi.db"
            require(pkg, character.only=TRUE)
            x <- eval(parse(text="org.Hs.ipiSYMBOL"))
            mapped_proteins <- mappedkeys(x)
            xx <- AnnotationDbi::as.list(x[mapped_proteins])
            if (withoutVersion == TRUE) {
                names(xx) = removeVer(names(xx))
            } else {
            }
            symbol <- xx[ipi]
            ipi.symbol.df <- ldply(symbol)
            result <- ipi.symbol.df
        }
    }
    names(result) <- c("ipi", "symbol")
    return (result)
}
