#' Protein to molecular functions (prot2mf)
#'
#' Function to obtain molecular functions associated with a specific protein, including their sources.
#'
#' @param protein Protein label or ID (string). Example: "BRCA1_HUMAN" or "P38398".
#'
#' @return A data.frame containing molecular function IDs, labels, relation labels, databases, and articles, sorted by molecular function label. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' prot2mf("BRCA1_HUMAN")
#' }
#'
#' @export

prot2mf <- function(protein) {
  # Endpoint SPARQL
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  # Define the prefixes and common parts of the query
  prefixes <-
    "
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX sio: <http://semanticscience.org/resource/>
    PREFIX oboowl: <http://www.geneontology.org/formats/oboInOwl#>
    "

  invariable_query <-
    "
    GRAPH <http://rdf.biogateway.eu/graph/prot2mf> {
      ?prot obo:RO_0002327 ?mf .
      ?uri_relation rdf:subject ?prot ;
           rdf:object ?mf ;
           skos:prefLabel ?relation_label .
      ?instance rdf:type ?uri_relation ;
           sio:SIO_000772 ?articles ;
           sio:SIO_000253 ?database .
    }
    GRAPH <http://rdf.biogateway.eu/graph/go> {
      ?mf rdfs:label ?mf_label ;
          oboowl:id ?mf_id .
    }
    "

  # Query building with prefLabel
  query <- sprintf(
    "
    %s
    SELECT DISTINCT ?mf_id ?mf_label ?relation_label ?database ?articles
    WHERE {
      GRAPH <http://rdf.biogateway.eu/graph/prot> {
        ?prot skos:prefLabel \"%s\" .
      }
      %s
    }
    ORDER BY ?mf_label
    ", prefixes, protein, invariable_query
  )

  # Run query
  results <- SPARQL(endpoint_sparql, query)$results

  if (nrow(results) == 0) {
    # Alternative query with altLabel
    query <- sprintf(
      "
      %s
      SELECT DISTINCT ?mf_id ?mf_label ?relation_label ?database ?articles
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?prot skos:altLabel \"%s\" .
        }
        %s
      }
      ORDER BY ?mf_label
      ", prefixes, protein, invariable_query
    )

    # Run alternative query
    results <- SPARQL(endpoint_sparql, query)$results
  }

  # Check if results are empty
  if (nrow(results) == 0) {
    return("No data available for the introduced protein or you may have introduced an instance that is not a protein.")
  } else {
    # Simplify URIs for articles and database
    results$articles <- gsub("<|>", "", results$articles)
    results$articles <- gsub("http://identifiers.org/pubmed/", "PMID:", results$articles)
    results$database <- gsub("<http://identifiers.org/goa/>", "GOA", results$database)
    return(results)
  }
}
