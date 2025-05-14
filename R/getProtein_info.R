#' Get protein information (getProtein_info)
#'
#' Function to obtain the main features of a specific protein, including ID, definition, evidence level, and optionally articles and alternative sources.
#'
#' @param protein Name of the protein (string). Example: "TBX5_HUMAN".
#' @param sources Include sources (boolean: TRUE or FALSE). Default: TRUE.
#'
#' @return A list containing unique protein alternative IDs, definition, evidence level, articles, and alternative sources. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' getProtein_info("TBX5_HUMAN", sources = TRUE)
#' }
#'
#' @export

getProtein_info <- function(protein, sources = T) {
  # Endpoint SPARQL
  sparql_endpoint <- "https://semantics.inf.um.es/biogateway"

  # Query building
  if (sources == T) {
    query <- sprintf(
      "
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX schema: <http://schema.org/>
    PREFIX sio: <http://semanticscience.org/resource/>
    SELECT DISTINCT ?protein_alt_id ?definition ?evidence_level ?articles ?alt_sources
    WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
            ?prot skos:prefLabel \"%s\" ;
                  skos:altLabel ?protein_alt_id ;
                  skos:definition ?definition ;
                  schema:evidenceLevel ?evidence_level .
            OPTIONAL { ?prot sio:SIO_000772 ?articles. }
            OPTIONAL { ?prot skos:closeMatch ?alt_sources. }
        }
    }
    ",
      protein
    )
  } else {
    query <- sprintf(
      "
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX schema: <http://schema.org/>
    PREFIX sio: <http://semanticscience.org/resource/>
    SELECT DISTINCT ?protein_alt_id ?definition ?evidence_level
    WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
            ?prot skos:prefLabel \"%s\" ;
                  skos:altLabel ?protein_alt_id ;
                  skos:definition ?definition ;
                  schema:evidenceLevel ?evidence_level .
        }
    }
    ",
      protein
    )
  }

  # Run query
  results <- SPARQL(sparql_endpoint, query)$results

  # Check results
  if (is.null(results) || nrow(results) == 0) {
    return("No data available for the introduced protein or you may have introduced an instance that is not a protein. Check your data type with type_data function.")
  } else {
    combined_result <- list(
      protein_alt_ids = unique(results$protein_alt_id),
      definition = unique(results$definition),
      evidence_level = unique(results$evidence_level),
      articles = unique(results$articlesīb),
      alt_sources = unique(results$alt_sources)
    )
    combined_result$articles = gsub("http://identifiers.org/pubmed/", "PMID:", combined_result$articles)
    combined_result$articles = gsub("<|>", "", combined_result$articles)

    combined_result$alt_sources = gsub("http://identifiers.org/.*/", "", combined_result$alt_sources)
    combined_result$alt_sources = gsub("<|>", "", combined_result$alt_sources)

    return(combined_result)
  }
}
