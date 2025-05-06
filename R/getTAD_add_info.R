#' Get additional TAD information (getTAD_add_info)
#'
#' Function to obtain additional data for a specific topologically associating domain (TAD), including evidence, databases, biological samples, and articles.
#'
#' @param tad Name of the TAD (string). Example: "tad/TADHS00000038004".
#'
#' @return A list containing unique values for evidence, databases, biological samples, and articles associated with the TAD. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' getTAD_add_info("tad/TADHS00000038004")
#' }
#'
#' @export

getTAD_add_info <- function(tad) {
  # SPARQL endpoint
  endpoint_sparql <- "https://2407.biogateway.eu/sparql"

  # Build SPARQL query
  query <- sprintf(
    "
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX sio: <http://semanticscience.org/resource/>
    PREFIX sch: <http://schema.org/>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT DISTINCT ?evidence ?database ?biological_samples ?articles
    WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/tad> {
            ?uri skos:prefLabel \"%s\" .
            ?tad_id rdf:type ?uri .
            OPTIONAL { ?tad_id obo:TXPO_0003500 ?biological_samples. }
            OPTIONAL { ?tad_id sch:evidenceOrigin ?evidence. }
            OPTIONAL { ?tad_id sio:SIO_000772 ?articles. }
            OPTIONAL { ?tad_id sio:SIO_000253 ?database. }
        }
    }
    ",
    tad
  )

  # Run query
  results <- SPARQL(endpoint_sparql, query)$results

  # Check for empty results
  if (nrow(results) == 0) {
    return("No data available for the introduced tad or you may have introduced an instance that is not a tad. Check your data type with type_data function.")
  }

  # Clean URIs
  if ("biological_samples" %in% names(results)) {
    results$biological_samples <- gsub("http://purl.obolibrary.org/obo/", "", results$biological_samples)
    results$biological_samples <- gsub("<|>", "", results$biological_samples)
  }

  if ("articles" %in% names(results)) {
    results$articles <- gsub("http://identifiers.org/pubmed/", "PMID:", results$articles)
    results$articles <- gsub("<|>", "", results$articles)
  }

  if ("evidence" %in% names(results)) {
    results$evidence <- gsub("<|>", "", results$evidence)
  }

  if ("database" %in% names(results)) {
    results$database <- gsub("<|>", "", results$database)
  }

  # Return results as a list with unique values
  combined_result <- list(
    evidence = unique(results$evidence),
    database = unique(results$database),
    biological_samples = unique(results$biological_samples),
    articles = unique(results$articles)
  )

  return(combined_result)
}
