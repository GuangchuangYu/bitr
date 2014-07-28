##' Convert Entrez to Gene symbol
##'
##' Given a character vector of Entrez IDs, this function will convert it to
##' Gene symbols.
##'
##'
##' @param eg Entrez IDs.
##' @param organism organism.
##' @param useBiomart using Biomart to query or using annotation packages.
##' @return \item{result}{eg.symbol dataframe.}
##' @importFrom biomaRt getBM
##' @importFrom biomaRt useMart
##' @importFrom org.Hs.eg.db org.Hs.egSYMBOL
##' @export
##' @keywords manip
##' @examples
##'
##' 	EG2Symbol("9961")
##'
EG2Symbol <- function(eg, organism = "human", useBiomart=FALSE) {
    eg <- as.character(eg)
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
            symbol <- getBM(attributes=c("entrezgene", "hgnc_symbol"),
                            filters="entrezgene", values=eg, mart=ensembl)
        } else if (organism == "mouse" ) {
            symbol <- getBM(attributes=c("entrezgene", "mgi_symbol"),
                            filters="entrezgene", values=eg, mart=ensembl)
        }
        result <- symbol[!is.na(symbol[,2]),]
        result <- symbol[symbol[,2] != "",]
    } else {
        if (organism == "human") {
            x <- org.Hs.egSYMBOL
            mapped_proteins <- mappedkeys(x)
            xx <- AnnotationDbi::as.list(x[mapped_proteins])
            symbol <- xx[eg]
            eg.symbol.df <- ldply(symbol)
            names(eg.symbol.df) <- c("entrezgene", "hgnc_symbol")
            result <- eg.symbol.df
        }
    }
    return (result)
}

