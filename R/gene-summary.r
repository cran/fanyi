#' query gene information from NCBI
#' 
#' This function query gene information (including gene name, description and summary) from NCBI Gene database
#' @title gene_summary 
#' @rdname gene-summary
#' @param entrez entrez gene IDs
#' @return A data frame containing the information
#' @author Guangchuang Yu 
#' @examples 
#' \dontrun{
#' gene_summary('1236')
#' }
#' @export
gene_summary <- function(entrez) {
    gene_summary_ncbi(entrez)
}

#' @importFrom yulab.utils get_cache_item
#' @importFrom yulab.utils update_cache_item
#' @importFrom yulab.utils get_cache_element
gene_summary_ncbi <- function(entrez) {
    .cache <- "NCBI"
    entrez <- as.character(entrez)
    ncbi <- get_cache_item(.cache)
    
    newitem <- entrez[!entrez %in% names(ncbi)]
    if (length(newitem) > 0) {
        x <- .get_entrez_summary(newitem)
        update_cache_item(.cache, x)
    }
    
    res <- get_cache_element(.cache, entrez)
    .extract_gene_summary(res)
}

#' @importFrom rentrez entrez_summary
.get_entrez_summary <- function(entrez) {
    x <- rentrez::entrez_summary(db='gene', id=entrez)
    if (length(entrez) == 1) {
        ncbi <- list()
        ncbi[[entrez]] <- x
    } else {
        ncbi <- x
    }    
    return(ncbi)
}

.extract_gene_summary <- function(summary) {
    if (inherits(summary, "esummary")) {
        res <- as.data.frame(summary[c("uid", "name", "description", "summary")])
        return(res)
    } 

    # 'esummary_list' object

    res <- lapply(summary, function(item) {
        .extract_gene_summary(item)
    }) |> do.call(rbind, args = _)

    rownames(res) <- res$name
    return(res)
}

#' @rdname search-gene
#' @export
symbol2entrez <- function(x, organism = "Homo sapiens") {
    res <- search_gene(x, organism)
    colnames(res)[1] <- "SYMBOL"
    return(res)
}

#' search genes and return corresponding entrez ids
#' 
#' This function query genes (e.g., symbols) from NCBI Gene database and return corresponding entrez gene IDs
#' @title search_gene
#' @rdname search-gene
#' @param x terms to search
#' @param organism correpsonding organism of the input terms
#' @return A data frame with TERM and ENTREZ columns
#' @author Guangchuang Yu 
#' @export
search_gene <- function(x, organism = "Homo sapiens") {
    query <- sprintf("%s[Gene] AND %s[Organism]", x, organism)
    entrez <- vapply(query, .search_gene, FUN.VALUE = character(1))
    res <- data.frame(TERM=x, ENTREZ=entrez)
    rownames(res) <- NULL
    return(res)
}

#' @importFrom rentrez entrez_search
.search_gene <- function(query, retmax = 1) {
    res <- tryCatch(entrez_search(db='gene', term=query, retmax=retmax),
                    error = function(e) NULL)
    if (is.null) return("")
    return(res$ids)
}
