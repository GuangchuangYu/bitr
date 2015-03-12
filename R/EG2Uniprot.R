##' Convert Entrez to uniprot ID
##'
##' Given a character vector of Entrez IDs, this function will convert it to
##' Uniprot ID
##'
##'
##' @param eg Entrez IDs.
##' @param organism organism.
##' @return \item{result}{eg.uniprot dataframe.}
##' @importFrom org.Hs.eg.db org.Hs.egUNIPROT
##' @export
##' @keywords manip
##' @examples
##'
##' 	EG2Uniprot("9961")
##'
EG2Uniprot <- function(eg, organism = "human") {
    eg <- as.character(eg)
    if (organism == "human") {
        res <- suppressWarnings(select(org.Hs.eg.db, keys = eg,
                                       keytype = "ENTREZID", 
                                       columns=c("ENTREZID", "UNIPROT")))
    } else {
        stop("only human was supported in current version...")
    }
    
    return (res)
}

