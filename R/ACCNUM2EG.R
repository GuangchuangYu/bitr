##' Convert ACCNUM to Entrez
##'
##' Given a character vector of ACCNUM, this function will convert it to Entrez
##' gene IDs.
##'
##' @title ACCNUM2EG
##' @param ACCNUM Accession numbers
##' @param organism organism, only human supported yet.
##' @return a data.frame of accession to eg mapping
##' @importMethodsFrom AnnotationDbi mget
##' @importFrom org.Hs.eg.db org.Hs.egACCNUM2EG
##' @export
##' @author Guangchuang Yu \url{http://ygc.name}
##' @keywords manip
##' @examples
##'
##' 	ACCNUM2EG("AA086285")
##'
ACCNUM2EG <- function(ACCNUM, organism= "human") {
    if (organism == "human") {
        MAP <- org.Hs.egACCNUM2EG
        ##		mapped_genes <- mappedkeys(x)
        ##		xx <- as.list(x[mapped_genes])
    } else {
        stop ("Not supported yet...\n")
    }
    ACCNUM <- unlist(ACCNUM)
    ##	EG = unlist(xx[ACCNUM])
    EG = unlist(sapply(ACCNUM, function(i) MAP[[i]]))
    EG <- data.frame(ACCNUM = names(EG), entrezgene=EG)
    return(EG)
}

