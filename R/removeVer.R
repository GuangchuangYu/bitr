##' Remover Version
##'
##' Remover Version, such as IPI123123.1.
##'
##'
##' @param id Gene ID.
##' @return \item{id.nr}{Gene ID without version.}
##' @keywords manip
##' @export
##' @examples
##'
##' 	removeVer("IPI00470674.2")
##'
removeVer <- function(id) {
    id.nr <- unname(sapply(id, function(x) {unlist(strsplit(x, "\\."))[1]}))
    return (id.nr)
}
