##' Get Leading ID in a ID strings.
##'
##' Given a character string of several IDs, this function will split it and
##' return the first ID.
##'
##'
##' @param id ID.
##' @param split split the IDs
##' @return \item{lid}{Leading ID.}
##' @export
##' @seealso \code{\link{IPI2EG}}
##' @keywords manip
##' @examples
##'
##' 	getLeadingID("IPI00000684;IPI00607787;IPI00217816", split=";")
##'
getLeadingID <- function(id, split = ";") {
	lid <- c()
	for (i in 1:length(id)) {
		lid[i] = strsplit(id[i], split)[[1]][1]
	}
	return(lid)
}
