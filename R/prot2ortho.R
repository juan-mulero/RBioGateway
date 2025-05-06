#' Protein to orthologous proteins (prot2ortho)
#'
#' Function to obtain orthologous proteins associated with a specific protein, including their taxa.
#'
#' @param protein Protein label or ID (string). Example: "BRCA1_HUMAN" or "P38398".
#'
#' @return A data.frame containing orthologous protein labels, orthology relation labels, and taxon names, sorted by protein label. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' prot2ortho("BRCA1_HUMAN")
#' }
#'
#' @export

prot2ortho <- function(protein) {
  # Endpoint SPARQL
  endpoint_sparql <- "https://2407.biogateway.eu/sparql"

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
    SELECT DISTINCT ?prot_label ?orthology_relation_label ?taxon
    WHERE {
      GRAPH <http://rdf.biogateway.eu/graph/prot> {
        ?prot skos:prefLabel \"%s\" .
      }
      GRAPH <http://rdf.biogateway.eu/graph/ortho> {
        ?uri rdf:object ?prot ;
             rdf:subject ?ortho_prot ;
             skos:prefLabel ?orthology_relation_label .
      }
      GRAPH <http://rdf.biogateway.eu/graph/prot> {
        ?ortho_prot skos:prefLabel ?prot_label ;
               obo:RO_0002162 ?ncbi_taxon .
      }
      GRAPH <http://rdf.biogateway.eu/graph/taxon> {
        ?ncbi_taxon rdfs:label ?taxon .
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
      SELECT DISTINCT ?prot_label ?orthology_relation_label ?taxon
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?prot skos:altLabel \"%s\" .
        }
        GRAPH <http://rdf.biogateway.eu/graph/ortho> {
          ?uri rdf:object ?prot ;
               rdf:subject ?ortho_prot ;
               skos:prefLabel ?orthology_relation_label .
        }
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?ortho_prot skos:prefLabel ?prot_label ;
                 obo:RO_0002162 ?ncbi_taxon .
        }
        GRAPH <http://rdf.biogateway.eu/graph/taxon> {
          ?ncbi_taxon rdfs:label ?taxon .
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
    return(results)
  }
}
