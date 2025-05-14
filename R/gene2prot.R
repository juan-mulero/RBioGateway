#' Gene to proteins (gene2prot)
#'
#' Function to retrieve proteins associated with a given gene, optionally filtered by taxon.
#'
#' @param gene Gene name (string). Example: "BRCA1".
#' @param taxon Taxon name (string) or taxon ID (string or integer), or NULL if not specified. Example: "Homo sapiens" or 9606. Default: NULL.
#'
#' @return A vector of protein names associated with the gene.
#'
#' @examples
#' \dontrun{
#' gene2prot("TOX3", taxon = "Homo sapiens")
#' }
#'
#' @export

gene2prot <- function(gene, taxon = NULL) {
  # Define the SPARQL endpoint
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  # Case when taxon is NULL
  if (is.null(taxon)) {
    query <- sprintf(
      "
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX sio: <http://semanticscience.org/resource/>
      SELECT DISTINCT ?prot ?prot_name
      WHERE {
          GRAPH <http://rdf.biogateway.eu/graph/gene> {
              ?gene skos:prefLabel '%s';
                    sio:SIO_010078 ?prot .
          }
          GRAPH <http://rdf.biogateway.eu/graph/prot> {
              ?prot skos:prefLabel ?prot_name .
          }
      }
      ",
      gene
    )
    # Execute the query
    results <- SPARQL(endpoint_sparql, query)$results$prot_name
    # Check if there are results
    if (length(results) == 0) {
      return("No data available for the introduced gene. Check that the gene id is correct or if you have introduced the taxon correctly.")
    } else {
      # Sort the results by protein name
      results = sort(results)
      return(results)
    }
  } else {
    # Case when taxon is provided as a numeric value
    if (!is.na(suppressWarnings(as.numeric(taxon)))) {
      query <- sprintf(
        "
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
        PREFIX gene: <http://rdf.biogateway.eu/gene/%s/>
        PREFIX sio: <http://semanticscience.org/resource/>
        SELECT DISTINCT ?prot ?prot_name
        WHERE {
            GRAPH <http://rdf.biogateway.eu/graph/gene> {
                gene:%s sio:SIO_010078 ?prot .
            }
            GRAPH <http://rdf.biogateway.eu/graph/prot> {
                ?prot skos:prefLabel ?prot_name .
            }
        }
        ",
        taxon, gene
      )
      # Execute the query
      results <- SPARQL(endpoint_sparql, query)$results$prot_name
      # Check if there are results
      if (length(results) == 0) {
        return("No data available for the introduced gene. Check that the gene id is correct or if you have introduced the taxon correctly.")
      } else {
        return(sort(results))
      }
    } else {
      # Case when taxon is provided as a name
      query <- sprintf(
        "
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
        PREFIX sio: <http://semanticscience.org/resource/>
        PREFIX obo: <http://purl.obolibrary.org/obo/>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT DISTINCT ?prot ?prot_name
        WHERE {
            GRAPH <http://rdf.biogateway.eu/graph/gene> {
              ?gene skos:prefLabel '%s';
                    sio:SIO_010078 ?prot ;
                    obo:RO_0002162 ?taxon .
            }
            GRAPH <http://rdf.biogateway.eu/graph/taxon> {
              ?taxon rdfs:label '%s' .
            }
            GRAPH <http://rdf.biogateway.eu/graph/prot> {
                ?prot skos:prefLabel ?prot_name .
            }
        }
        ",
        gene, taxon
      )
      # Execute the query
      results <- SPARQL(endpoint_sparql, query)$results$prot_name
      # Check if there are results
      if (length(results) == 0) {
        return("No data available for the introduced gene. Check that the gene id is correct or if you have introduced the taxon correctly.")
      } else {
        return(sort(results))
      }
    }
  }
}
