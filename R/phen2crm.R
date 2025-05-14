#' Phenotype to CRMs (phen2crm)
#'
#' Function to obtain cis-regulatory modules (CRMs) associated with a phenotype, identified by a phenotype label or OMIM ID.
#'
#' @param phenotype Phenotype label (string) or OMIM ID (string). Example: "schizophrenia" or "181500".
#'
#' @return A data.frame containing CRM names, OMIM IDs, databases, and articles associated with the phenotype, sorted by CRM name. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' phen2crm("schizophrenia")
#' }
#'
#' @export

phen2crm <- function(phenotype) {
  # Define the SPARQL endpoint
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  # SPARQL prefixes used in the query
  prefixes <- "
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX rdfs: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX sio: <http://semanticscience.org/resource/>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX omim: <http://purl.bioontology.org/ontology/OMIM/>
  "

  # SPARQL query to search phenotypes by label or altLabel
  query <- sprintf(
    "
    %s
    SELECT DISTINCT ?crm_name ?omim_id ?database ?articles
    WHERE {
      GRAPH <http://rdf.biogateway.eu/graph/omim> {
        {?omim_id skos:prefLabel ?label}
        UNION
        {?omim_id skos:altLabel ?label}
        FILTER regex(?label, '%s', 'i')
      }
      GRAPH <http://rdf.biogateway.eu/graph/crm2phen> {
        ?crm obo:RO_0002331 ?omim_id .
        ?s rdfs:subject ?crm ;
           rdfs:predicate ?relation ;
           rdfs:object ?phen .
        ?uri rdfs:type ?s .
        OPTIONAL { ?uri sio:SIO_000772 ?articles . }
        OPTIONAL { ?uri sio:SIO_000253 ?database . }
      }
      GRAPH <http://rdf.biogateway.eu/graph/crm> {
        ?crm skos:prefLabel ?crm_name .
      }
    }
    ORDER BY ?crm_name
    ", prefixes, phenotype
  )

  # Run query
  results <- SPARQL(endpoint_sparql, query)$results

  # If no results, check if input is a valid OMIM ID and run an alternative query
  if (nrow(results) == 0 && (nchar(phenotype) == 6 && !is.na(as.numeric(phenotype)) || grepl("^MTHU", phenotype))) {
    alt_query <- sprintf(
      "
      %s
      SELECT DISTINCT ?crm_name ?database ?articles
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/crm2phen> {
          ?crm obo:RO_0002331 omim:%s .
          ?s rdfs:subject ?crm ;
             rdfs:predicate ?relation ;
             rdfs:object ?phen .
          ?uri rdfs:type ?s .
          OPTIONAL { ?uri sio:SIO_000772 ?articles . }
          OPTIONAL { ?uri sio:SIO_000253 ?database . }
        }
        GRAPH <http://rdf.biogateway.eu/graph/crm> {
          ?crm skos:prefLabel ?crm_name .
        }
      }
      ORDER BY ?crm_name
      ", prefixes, phenotype
    )

    results <- SPARQL(endpoint_sparql, alt_query)$results
  }

  # Handle no results case
  if (nrow(results) == 0) {
    return("No data available for the introduced phenotype or the instance is not recognized.")
  }

  # Clean URI values for readability
  results$omim_id <- gsub("<|>", "", results$omim_id)
  results$omim_id <- gsub("http://purl.bioontology.org/ontology/OMIM/", "OMIM:", results$omim_id)
  results$articles <- gsub("<|>", "", results$articles)
  results$articles <- gsub("http://identifiers.org/pubmed/", "PMID:", results$articles)
  results$database <- gsub("<|>", "", results$database)

  # Return clean data.frame sorted by CRM name
  results <- results[order(results$crm_name), ]
  return(results)
}
