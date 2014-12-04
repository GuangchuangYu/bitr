##' convert ENSEMBL to ENSEMBLROT
##'
##' 
##' @title ensg2ensp 
##' @param ensg ENSEMBL ID
##' @param organism supported organism
##' @return data.frame
##' @importFrom org.Hs.eg.db org.Hs.eg.db
##' @importFrom AnnotationDbi select
##' @export
##' @author ygc
ensg2ensp <- function(ensg, organism="human") {
    if (organism == "human") {
        res <- suppressWarnings(select(org.Hs.eg.db, keys=ensg, keytype="ENSEMBL",
                      columns=c("ENSEMBL", "ENSEMBLPROT")))
        return(res)
    } else {
        stop("organism not supported yet...")
    }
}
