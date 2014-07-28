##' mapping ID
##'
##' 
##' @title IDconverter
##' @param ID ID vector
##' @param fromType type of the input
##' @param toType type for converting
##' @param organism only human supported now
##' @return data.frame
##' @export
##' @author Guangchuang Yu \url{http://ygc.name}
IDconverter <- function(ID, fromType, toType, organism) {
    if (fromType == "entrezgene") {
        result <- switch(toType,
                         ACCNUM = EG2ACCNUM(ID,organism),
                         Symbol = EG2Symbol(ID, organism),
                         )
    } else if (fromType == "ACCNUM") {
        result <- switch(toType,
                         entrezgene = ACCNUM2EG(ID,organism),
                         )
    } else if (fromType == "Uniprot") {
        result <- switch(toType,
                         entrezgene = Uniprot2EG(ID,organism),
                         )
     } else if (fromType == "IPI") {
        result <- switch(toType,
                         entrezgene = IPI2EG(ID,organism),
                         HGNC = IPI2Symbol(ID, organism)
                         )
    }


    return(result)
}
