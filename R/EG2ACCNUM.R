##' Convert Entrez to ACCNUM
##'
##' Given a character vector of entrez gene ids, this function will convert it to Accession numbers.
##'
##' @title EGACCNUM
##' @param eg Entrez gene ids
##' @param organism organism, only human supported yet.
##' @param type one of "gene" or "protein"
##' @return a data.frame of eg to accession mapping
##' @importMethodsFrom AnnotationDbi mget
##' @importFrom org.Hs.eg.db org.Hs.egACCNUM
##' @importFrom plyr ldply
##' @export
##' @seealso \code{\link{ACCNUM2EG}}
##' @author Guangchuang Yu \url{http://ygc.name}
##' @keywords manip
##' @examples
##'
##' 	EG2ACCNUM("8061")
##'
EG2ACCNUM <- function(eg, organism="human", type="gene") {
    if (organism == "human") {
        MAP <- org.Hs.egACCNUM
    } else {
        stop ("Not supported yet...\n")
    }

    ACCNUM <- mget(eg, MAP, ifnotfound=NA)
    ACCNUM <- switch(type,
                     gene = lapply(ACCNUM, function(i) i[grep("^NM", i)]) ,
                     protein = lapply(ACCNUM, function(i) i[grep("^NP", i)])
                     )

    result <- ldply(ACCNUM)
    colnames(result) <- c("entrezgene", "ACCNUM")

    return(result)
}
