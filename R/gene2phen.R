#' Gene to phenotype (gene2phen)
#'
#' Function to retrieve phenotypes associated with a given gene.
#'
#' @param gene Gene name (string). Example: "BRCA1".
#'
#' @return A data.frame with OMIM IDs and phenotype labels associated with the gene, ordered by phenotype label.
#'
#' @examples
#' \dontrun{
#' gene2phen("BRCA1")
#' }
#'
#' @export

gene2phen <- function(gene) {
  # Define the SPARQL endpoint
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  # Build the SPARQL query
  query <- sprintf(
    "
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX hgene: <http://rdf.biogateway.eu/gene/9606/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    SELECT DISTINCT ?omim_id ?phen_label
    WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/gene2phen> {
            hgene:%s obo:RO_0002331 ?omim_id .
            ?omim_id skos:prefLabel ?phen_label .
        }
    }
    ",
    gene
  )

  # Execute the query
  results <- SPARQL(endpoint_sparql, query)$results

  # Check if there are results
  if (nrow(results) == 0) {
    return("No data available for the introduced gene or you may have introduced an instance is not a gene. Check your data type with type_data function.")
  } else {
    # Process omim_id by removing the URI prefix
    results$omim_id = gsub("<|>", "", results$omim_id)
    results$omim_id = gsub("http://purl.bioontology.org/ontology/OMIM/", "omim:", results$omim_id)
    # Sort the results by phenotype label
    results = results[order(results$phen_label), ]
    row.names(results) = 1:nrow(results)
    return(results)
  }
}
