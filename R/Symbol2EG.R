##' Convert Symbol to Entrez
##'
##' Given a character vector of Symbol, this function will convert it to
##' Entrez IDs.
##'
##'
##' @param symbol Symbol.
##' @param organism organism.
##' @return \item{eg}{Entrez IDs.}
##' @importFrom biomaRt useMart
##' @importFrom biomaRt getBM
##' @seealso \code{\link{ACCNUM2EG}}
##' @keywords manip
##' @export
##' @examples
##'
##' 	Symbol2EG("GAPDH")
##'
##' @author Guangchuang Yu \url{http://ygc.name}
Symbol2EG <- function(symbol, organism = "human") {
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
        eg <- getBM(attributes=c("hgnc_symbol", "entrezgene"),
                    filters="hgnc_symbol",
                    values=symbol,
                    mart=ensembl)
    }
    if (organism == "mouse") {
        eg <- getBM(attributes=c("external_gene_id", "entrezgene"),
                    filters="external_gene_id",
                    values=symbol,
                    mart=ensembl)
    }
    result <- eg[!is.na(eg[,2]),]
    return (result)
}

