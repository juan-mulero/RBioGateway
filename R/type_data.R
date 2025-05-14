#' Identify entity type (type_data)
#'
#' Function to identify the type of a given entity.
#'
#' @param instance Entity label or ID (string). Example: "TP53" or "INSR_HUMAN".
#'
#' @return A character vector containing the type(s) of the entity. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' type_data("TP53")
#' }
#'
#' @export

type_data <- function(instance) {
  # Endpoint SPARQL
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  query <- sprintf( #Search by preferred label (prefLabel)
    "
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    SELECT DISTINCT ?label_type
    WHERE {
        GRAPH ?graph {
            ?uri skos:prefLabel \"%s\" ;
            rdfs:subClassOf ?type .
            ?type skos:prefLabel ?label_type
        }
    }
    ",
    instance
  )

  results <- SPARQL(endpoint_sparql, query)$results$label_type

  if (length(results) == 0) { #Search by alternative label (altLabel)
    query_alt_label <- sprintf(
      "
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      SELECT DISTINCT ?label_type
      WHERE {
          GRAPH ?graph {
              ?uri skos:altLabel \"%s\" ;
              rdfs:subClassOf ?type .
              ?type skos:prefLabel ?label_type
          }
      }
      ",
      instance
    )
    results <- SPARQL(endpoint_sparql, query_alt_label)$results$label_type
  }

  if (length(results) == 0) {
    query_go <- sprintf( #Seach by hasOBONamespace
      "
      PREFIX obo: <http://www.geneontology.org/formats/oboInOwl#>
      SELECT DISTINCT ?label_type
      WHERE {
          GRAPH ?graph {
              ?uri obo:id \"%s\" ;
              obo:hasOBONamespace ?label_type .
          }
      }
      ",
      instance
    )
    results <- SPARQL(endpoint_sparql, query_go)$results$label_type
  }

  if (length(results) > 0) {
    return(results)
  } else {
    return("No data available for this instance, or format not covered")
  }
}
