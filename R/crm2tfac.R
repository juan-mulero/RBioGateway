#' CRM to transcription factor (crm2tfac)
#'
#' Function to obtain the transcription factors associated with a CRM (Cis-Regulatory Module).
#'
#' @param crm CRM label (string). Example: "crm/CRMHS00000007832".
#'
#' @return A data.frame of transcription factors associated with the CRM and their sources, ordered by transcription factor name.
#'
#' @examples
#' \dontrun{
#' crm2tfac("crm/CRMHS00000007832")
#' }
#'
#' @export

crm2tfac <- function(crm) {
  # Define the SPARQL endpoint
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  # SPARQL prefixes used in the query
  prefixes <- "
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX rdfs: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX sio: <http://semanticscience.org/resource/>
    PREFIX sch: <http://schema.org/>
  "

  # SPARQL query to retrieve transcription factors linked to a CRM
  query <- sprintf(
    "
    %s
    SELECT DISTINCT ?tfac_name ?database ?articles ?biological_samples ?evidence
    WHERE {
      GRAPH <http://rdf.biogateway.eu/graph/crm> {
        ?crm skos:prefLabel \"%s\" .
      }
      GRAPH <http://rdf.biogateway.eu/graph/crm2tfac> {
        ?crm obo:RO_0002436 ?tfac .
        ?s rdfs:subject ?crm ;
           rdfs:predicate ?relation ;
           rdfs:object ?tfac .
        ?uri rdfs:type ?s .
        OPTIONAL { ?uri sio:SIO_000772 ?articles . }
        OPTIONAL { ?uri sio:SIO_000253 ?database . }
        OPTIONAL { ?uri obo:TXPO_0003500 ?biological_samples . }
        OPTIONAL { ?uri sch:evidenceOrigin ?evidence . }
      }
      GRAPH <http://rdf.biogateway.eu/graph/prot> {
        ?tfac skos:prefLabel ?tfac_name .
      }
    }
    ORDER BY ?tfac_name
    ", prefixes, crm
  )

  # Execute the SPARQL query
  results <- SPARQL(endpoint_sparql, query)$results

  # Handle no results
  if (nrow(results) == 0) {
    return("No data available for the introduced CRM or the instance is not recognized.")
  }

  # Simplify URIs
  results$articles <- gsub("<|>", "", results$articles)
  results$database <- gsub("<|>", "", results$database)
  results$articles <- gsub("http://identifiers.org/pubmed/", "PMID:", results$articles)
  results$biological_samples <- gsub("http://purl.obolibrary.org/obo/", "", results$biological_samples)
  results$biological_samples <- gsub("<|>", "", results$biological_samples)
  results$evidence <- gsub("<|>", "", results$evidence)
  results$evidence <- gsub("<|>", "", results$evidence)

  # Return data.frame sorted by transcription factor name
  results <- results[order(results$tfac_name), ]
  return(results)
}
