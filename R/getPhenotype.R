#' Get phenotype information (getPhenotype)
#'
#' Function to retrieve phenotype information based on a given phenotype label, OMIM ID, or MTHU identifier.
#'
#' @param phenotype Phenotype label (string), OMIM ID (6-digit string), or MTHU identifier (string). Example: "breast cancer" or "604704".
#'
#' @return A data.frame containing OMIM IDs and phenotype labels. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' getPhenotype("breast cancer")
#' }
#'
#' @export

getPhenotype <- function(phenotype) {
  # Define the SPARQL endpoint
  endpoint_sparql <- "https://2407.biogateway.eu/sparql"

  # Build the initial SPARQL query
  query <- sprintf(
    "
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    SELECT DISTINCT ?omim_id ?phen_label
    WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/omim> {
            { ?omim_id skos:prefLabel ?phen_label }
            UNION
            { ?omim_id skos:altLabel ?phen_label }
        }
        FILTER regex(?phen_label, '%s', 'i')
    }
    ",
    phenotype
  )

  # Execute the query
  results <- SPARQL(endpoint_sparql, query)$results

  # If there are results, process omim_id to remove the URI
  if (nrow(results) > 0) {
    results$omim_id <- gsub("http://purl.bioontology.org/ontology/OMIM/", "", results$omim_id)
    results$omim_id <- gsub("<|>", "", results$omim_id)
  }

  # Check if there are results
  if (nrow(results) == 0) {
    # Check if the phenotype is a 6-digit number or starts with "MTHU"
    if (nchar(phenotype) == 6 && !is.na(as.numeric(phenotype)) || grepl("^MTHU", phenotype)) {
      query_phen <- sprintf(
        "
        PREFIX omim: <http://purl.bioontology.org/ontology/OMIM/>
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
        SELECT DISTINCT ?phen_label
        WHERE {
            GRAPH <http://rdf.biogateway.eu/graph/omim> {
                omim:%s skos:prefLabel ?phen_label
            }
        }
        ",
        phenotype
      )

      # Execute the second query
      results <- SPARQL(endpoint_sparql, query_phen)$results
    } else {
      return("No data available for the introduced phenotype or you may have introduced an instance that is not a phenotype. Check your data type with type_data function.")
    }
  }

  # Sort the results by phen_label
  results_sorted <- results[order(results$phen_label), ]

  return(results_sorted)
}
