#' Protein to gene (prot2gene)
#'
#' Function to retrieve the gene encoding a specific protein.
#'
#' @param protein Protein label or ID (string). Example: "BRCA1_HUMAN" or "P38398".
#'
#' @return A data.frame containing gene IDs for the gene encoding the protein. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' prot2gene("BRCA1_HUMAN")
#' }
#'
#' @export

prot2gene <- function(protein) {
  # Define the SPARQL endpoint
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  # Build the initial SPARQL query
  query <- sprintf(
    "
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX sio: <http://semanticscience.org/resource/>
    SELECT DISTINCT ?gene_id
    WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
            ?prot skos:prefLabel '%s' .
        }
        GRAPH <http://rdf.biogateway.eu/graph/gene> {
            ?gen sio:SIO_010078 ?prot ;
                 skos:prefLabel ?gene_id.
        }
    }
    ",
    protein
  )

  # Execute the initial query
  results <- SPARQL(endpoint_sparql, query)$results

  # If no results, try with alternative label
  if (nrow(results) == 0) {
    query_alt_label <- sprintf(
      "
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX obo: <http://purl.obolibrary.org/obo/>
      PREFIX sio: <http://semanticscience.org/resource/>
      SELECT DISTINCT ?gene_id
      WHERE {
          GRAPH <http://rdf.biogateway.eu/graph/prot> {
              ?prot skos:altLabel '%s' .
          }
          GRAPH <http://rdf.biogateway.eu/graph/gene> {
              ?gen sio:SIO_010078 ?prot ;
                   skos:prefLabel ?gene_id.
          }
      }
      ",
      protein
    )
    # Execute the alternative query
    results <- SPARQL(endpoint_sparql, query_alt_label)$results
  }

  # Check if there are results
  if (nrow(results) == 0) {
    return("No data available for the introduced protein or you may have introduced an instance that is not a protein. Check your data type with type_data function.")
  } else {
    return(results)
  }
}
