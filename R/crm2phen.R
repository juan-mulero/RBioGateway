#' CRM to phenotype (crm2phen)
#'
#' Function to obtain the phenotypes associated with a CRM (Cis-Regulatory Module).
#'
#' @param crm CRM label (string). Example: "crm/CRMHS00000007832".
#'
#' @return A data.frame of phenotypes associated with the CRM and their sources, ordered by phenotype ID.
#'
#' @examples
#' \dontrun{
#' crm2phen("crm/CRMHS00000005764")
#' }
#'
#' @export

crm2phen <- function(crm) {
  # Define the SPARQL endpoint
  endpoint_sparql <- "https://2407.biogateway.eu/sparql"

  # SPARQL prefixes used in the query
  prefixes <- "
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX rdfs: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX sio: <http://semanticscience.org/resource/>
  "

  # SPARQL query to retrieve phenotypes associated with a CRM, including evidence
  query <- sprintf(
    "
    %s
    SELECT DISTINCT ?phen_id ?database ?articles
    WHERE {
      GRAPH <http://rdf.biogateway.eu/graph/crm> {
        ?crm skos:prefLabel \"%s\" .
      }
      GRAPH <http://rdf.biogateway.eu/graph/crm2phen> {
        ?crm obo:RO_0002331 ?phen_id .
        ?uri rdfs:subject ?crm ;
             rdfs:object ?phen_id ;
             rdfs:predicate ?relation .
        ?instance rdf:type ?uri ;
                  sio:SIO_000772 ?articles ;
                  sio:SIO_000253 ?database .
      }
    }
    ORDER BY ?phen_id
    ", prefixes, crm
  )

  # Execute the SPARQL query
  results <- SPARQL(endpoint_sparql, query)$results

  # Handle empty results
  if (nrow(results) == 0) {
    return("No data available for the introduced CRM or the instance is not recognized as a CRM.")
  }

  # simplify URIs
  results$phen_id <- gsub("<|>", "", results$phen_id)
  results$phen_id <- gsub("http://purl.bioontology.org/ontology/OMIM/", "OMIM:", results$phen_id)
  results$phen_id <- gsub("http://purl.obolibrary.org/obo/DOID_", "DOID:", results$phen_id)
  results$phen_id <- gsub("https://id.nlm.nih.gov/mesh/", "MeSH:", results$phen_id)
  results$articles <- gsub("<|>", "", results$articles)
  results$articles <- gsub("http://identifiers.org/pubmed/", "PMID:", results$articles)
  results$database <- gsub("<|>", "", results$database)

  # Return results as a data.frame sorted by phenotype ID
  results <- results[order(results$phen_id), ]
  return(results)
}
