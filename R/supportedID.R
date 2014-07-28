supportedID <- function() {
    print("convert NCBI accession number...")
    cat("ACCNUM2EG:\tAccession number\t=>  entrezgene\n")

    print("convert Entrezgene ID...")
    cat("EG2ACCNUM:\tEntrezgeneID\t\t=>  NCBI accession number\n")
    cat("EG2Symbol:\tEntrezgeneID\t\t=>  gene Symbol\n")

    print("convert IPI...")
    cat("IPI2EG:\t\tIPI\t\t\t=>  entrezgene\n")
    cat("IPI2Symbol:\tIPI\t\t\t=>  gene Symbol\n")
    
    print("convert Symbol...")
    cat("Symbol2EG:\tSymbol\t\t\t=>  entrezgene\n")

    print("convert Uniprot...")
    cat("Uniprot2EG:\tUniprot\t\t\t=>  entrezgene\n")
}
