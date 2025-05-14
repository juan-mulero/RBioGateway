#' Protein to interacting proteins (prot2prot)
#'
#' Function to obtain proteins that interact with a specific protein, including their sources and interaction details.
#'
#' @param protein Protein label or ID (string). Example: "BRCA1_HUMAN" or "P38398".
#'
#' @return A data.frame containing interacting protein labels, relation labels, databases, articles, evidence levels, and interaction details, sorted by protein label. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' prot2prot("BRCA1_HUMAN")
#' }
#'
#' @export

prot2prot <- function(protein) {
  # Endpoint SPARQL
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  # Define the prefixes and common parts of the query
  prefixes <-
    "
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX sch: <http://schema.org/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX sio: <http://semanticscience.org/resource/>
    PREFIX oboowl: <http://www.geneontology.org/formats/oboInOwl#>
    "

  query <- sprintf(
    "
    %s
    SELECT DISTINCT ?prot_label ?relation_label ?database ?articles ?evidence_level ?interaction_details
    WHERE {
      GRAPH <http://rdf.biogateway.eu/graph/prot> {
        ?prot skos:prefLabel \"%s\" .
      }
      GRAPH <http://rdf.biogateway.eu/graph/prot2prot> {
        ?prot obo:RO_0002436 ?prot2 .
        ?uri_relation rdf:subject ?prot ;
             rdf:object ?prot2 ;
             skos:prefLabel ?relation_label .
        ?instance rdf:type ?uri_relation ;
             sio:SIO_000772 ?articles ;
             sio:SIO_000253 ?database ;
             sch:evidenceLevel ?evidence_level ;
             obo:BFO_0000050 ?interaction_details .
      }
      GRAPH <http://rdf.biogateway.eu/graph/prot> {
        ?prot2 skos:prefLabel ?prot_label .
      }
    }
    ", prefixes, protein
  )

  # Run query
  results <- SPARQL(endpoint_sparql, query)$results

  if (nrow(results) == 0) {
    # Alternative query with altLabel
    query_alt_label <- sprintf(
      "
      %s
      SELECT DISTINCT ?prot_label ?relation_label ?database ?articles ?evidence_level ?interaction_details
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?prot skos:altLabel \"%s\" .
        }
        GRAPH <http://rdf.biogateway.eu/graph/prot2prot> {
          ?prot obo:RO_0002436 ?prot2 .
          ?uri_relation rdf:subject ?prot ;
               rdf:object ?prot2 ;
               skos:prefLabel ?relation_label .
          ?instance rdf:type ?uri_relation ;
               sio:SIO_000772 ?articles ;
               sio:SIO_000253 ?database ;
               sch:evidenceLevel ?evidence_level ;
               obo:BFO_0000050 ?interaction_details .
        }
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?prot2 skos:prefLabel ?prot_label .
        }
      }
      ", prefixes, protein
    )

    results <- SPARQL(endpoint_sparql, query_alt_label)$results
  }

  # Check if results are empty
  if (nrow(results) == 0) {
    return("No data available for the introduced protein or you may have introduced an instance that is not a protein.")
  } else {
    # Simplify URIs for articles and database
    results$articles <- gsub("<|>", "", results$articles)
    results$articles <- gsub("http://identifiers.org/pubmed/", "PMID:", results$articles)
    results$database <- gsub("<http://identifiers.org/intact>", "IntAct", results$database)
    results$interaction_details <- gsub("<|>", "", results$interaction_details)
    results$interaction_details <- gsub("http://identifiers.org/intact/", "intact:", results$interaction_details)
    return(results)
  }
}
