##' Convert IPI to EG
##'
##' Given a character vector of IPI, this function will convert it to Entrez
##' gene IDs.
##'
##'
##' @param ipi IPI.
##' @param organism organism
##' @param useBiomart using biomart or using annnotation package to convert ID
##' @param withoutVersion with or without IPI version
##' @return \item{result}{IPI.Entrez dataframe}
##' @importFrom biomaRt useMart
##' @importFrom biomaRt getBM
##' @importMethodsFrom AnnotationDbi as.list
##' @importMethodsFrom AnnotationDbi mappedkeys
##' @export
##' @seealso \code{\link{Uniprot2EG}}
##' @keywords manip
##' @examples
##'
##' 	IPI2EG("IPI00470674")
##'
##' @author Guangchuang Yu \url{http://ygc.name}
IPI2EG <- function(ipi, organism = "human", useBiomart=FALSE, withoutVersion=TRUE) {
    ipi <- as.character(ipi)
    if(useBiomart==TRUE) {
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
        eg <- getBM(attributes=c("ipi", "entrezgene"), filters="ipi", values=ipi, mart=ensembl)
        result <- eg[!is.na(eg[,2]),]
        result <- eg[eg[,2] != "",]
    } else {
        if (organism == "human") {
            ##' @importFrom org.Hs.ipi.db org.Hs.ipiGENEID
            pkg <- "org.Hs.ipi.db"
            require(pkg, character.only=TRUE)
            x <- eval(parse(text="org.Hs.ipiGENEID"))
            mapped_proteins <- mappedkeys(x)
            xx <- AnnotationDbi::as.list(x[mapped_proteins])
            if (withoutVersion == TRUE) {
                names(xx) = removeVer(names(xx))
            } else {
            }
            eg <- xx[ipi]
            ipi.eg.df <- ldply(eg)
            names(ipi.eg.df) <- c("ipi", "entrezgene")
            result <- ipi.eg.df
        }
    }

    return (result)
}

