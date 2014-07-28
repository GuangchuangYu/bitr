##' Query Description of genes
##'
##' Query Description of genes
##'
##'
##' @param id gene ids.
##' @param type id type.
##' @param organism supported organism.
##' @return \item{desc}{a dataframe of gene id and corresponding description.}
##' @importFrom biomaRt getBM
##' @importFrom biomaRt useMart
##' @export
##' @keywords manip
getDesc <- function(id, type, organism = "human") {
    type <- match.arg(type, c("entrezgene", "ipi", "hgnc_symbol"))
    wh_dataset <- switch(organism,
                         human="hsapiens_gene_ensembl",
                         mouse="mmusculus_gene_ensembl",
                         rat="rnorvegicus_gene_ensembl"
                         )
    ensembl = useMart("ensembl", dataset=wh_dataset)
    desc = getBM(attributes=c(type, "description"),
        filters=type,
        values=id,
        mart=ensembl)
    return (desc)
}
