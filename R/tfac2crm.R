#' Transcription factor to CRMs (tfac2crm)
#'
#' Function to obtain cis-regulatory modules (CRMs) associated with a specific transcription factor, including their sources.
#'
#' @param tfac Transcription factor label (string). Example: "FOSL2_HUMAN".
#'
#' @return A data.frame containing CRM names, databases, articles, biological samples, and evidence, sorted by CRM name. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' tfac2crm("NKX31_HUMAN")
#' }
#'
#' @export

tfac2crm <- function(tfac) {
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

  # SPARQL query to retrieve CRMs regulated by the transcription factor
  query <- sprintf(
    "
    %s
    SELECT DISTINCT ?crm_name ?database ?articles ?biological_samples ?evidence
    WHERE {
      GRAPH <http://rdf.biogateway.eu/graph/prot> {
        ?tfac skos:prefLabel \"%s\" .
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
      GRAPH <http://rdf.biogateway.eu/graph/crm> {
        ?crm skos:prefLabel ?crm_name .
      }
    }
    ORDER BY ?crm_name
    ", prefixes, tfac
  )

  # Execute the SPARQL query
  results <- SPARQL(endpoint_sparql, query)$results

  # Handle no results
  if (nrow(results) == 0) {
    return("No data available for the introduced transcription factor or the instance is not recognized.")
  }

  # Clean and simplify URIs for readability
  results$articles <- gsub("<|>", "", results$articles)
  results$articles <- gsub("http://identifiers.org/pubmed/", "PMID:", results$articles)
  results$database <- gsub("<|>", "", results$database)
  results$biological_samples <- gsub("<|>", "", results$biological_samples)
  results$biological_samples <- gsub("http://purl.obolibrary.org/obo/", "", results$biological_samples)
  results$evidence <- gsub("<|>", "", results$evidence)

  # Return cleaned results as a sorted data.frame
  results <- results[order(results$crm_name), ]
  return(results)
}
