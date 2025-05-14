#' Get additional CRM information (getCRM_add_info)
#'
#' Function to obtain additional data for a specific cis-regulatory module (CRM), including databases, alternative sources, biological samples, articles, and experimental evidence.
#'
#' @param crm Name of the CRM (string). Example: "crm/CRMHS00000005387".
#'
#' @return A list containing unique values for alternative sources, databases, biological samples, articles, and methods associated with the CRM. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' getCRM_add_info("crm/CRMHS00000005387")
#' }
#'
#' @export

getCRM_add_info <- function(crm) {
  # Endpoint SPARQL
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  # Query building
  query <- sprintf(
    "
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX dc: <http://purl.org/dc/terms/>
    PREFIX sio: <http://semanticscience.org/resource/>
    PREFIX schema: <http://schema.org/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT DISTINCT ?database ?alt_source ?biological_samples ?articles ?methods
    WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/crm> {
            ?uri_class skos:prefLabel \"%s\" .
            ?uri_inst rdf:type ?uri_class .
            OPTIONAL { ?uri_inst obo:TXPO_0003500 ?biological_samples. }
            OPTIONAL { ?uri_inst schema:evidenceOrigin ?alt_source. }
            OPTIONAL { ?uri_inst sio:SIO_000772 ?articles. }
            OPTIONAL { ?uri_inst sio:SIO_000253 ?uri_database.
		       ?uri_database skos:prefLabel ?database. }
	    OPTIONAL { ?uri_inst rdfs:isDefinedBy ?methods. }
        }
    }
    ",
    crm
  )

  # Run query
  results <- SPARQL(endpoint_sparql, query)$results

  # Check results
  if (nrow(results) == 0) {
    return("No data available for the introduced crm or you may have introduced an instance that is not a crm. Check your data type with type_data function.")
  }

  # Change URIs to IDs
  if ("alt_source" %in% names(results)) {
    results$alt_source = gsub("<|>", "", results$alt_source)
  }

  if ("biological_samples" %in% names(results)) {
    results$biological_samples <- gsub("http://purl.obolibrary.org/obo/", "", results$biological_samples)
    results$biological_samples <- gsub("<|>", "", results$biological_samples)
  }
  if ("articles" %in% names(results)) {
    results$articles <- gsub("http://identifiers.org/pubmed/", "PMID:", results$articles)
    results$articles <- gsub("<|>", "", results$articles)
  }
  if ("methods" %in% names(results)) {
    results$methods <- gsub("[http://purl.obolibrary.org/obo/]|[http://www.bioassayontology.org/bao#]|[http://www.ebi.ac.uk/efo/]", "", results$methods)
    results$methods <- gsub("", "", results$methods)
    results$methods <- gsub("<|>", "", results$methods)
  }

  # List of unique results
  combined_result <- list(
    alt_source = unique(results$alt_source),
    database = unique(results$database),
    biological_samples = unique(results$biological_samples),
    articles = unique(results$articles),
    methods = unique(results$methods)
  )

  return(combined_result)
}
