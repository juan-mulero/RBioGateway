#' Phenotype to genes (phen2gene)
#'
#' Function to retrieve genes associated with a given phenotype, identified by a phenotype label or OMIM ID.
#'
#' @param phenotype Phenotype label (string) or OMIM ID (string). Example: "lung cancer".
#'
#' @return A data.frame containing gene names associated with the phenotype, sorted by gene name. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' phen2gene("lung cancer")
#' }
#'
#' @export

phen2gene <- function(phenotype) {
  # Endpoint SPARQL
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  # Query building for general phenotype search
  query <- sprintf(
    "
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX sio: <http://semanticscience.org/resource/>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    SELECT DISTINCT ?gene_name
    WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/omim> {
            {?omim_id skos:prefLabel ?label}
            UNION
            {?omim_id skos:altLabel ?label}
        }
        FILTER regex(?label, \"%s\", \"i\")
        GRAPH <http://rdf.biogateway.eu/graph/gene2phen> {
            ?gene obo:RO_0002331 ?omim_id .
        }
        GRAPH <http://rdf.biogateway.eu/graph/gene> {
            ?gene sio:SIO_010078 ?prot ;
                  skos:prefLabel ?gene_name.
        }
    }
    ",
    phenotype
  )

  # Run query
  results <- SPARQL(endpoint_sparql, query)$results

  # Check if results are empty
  if (nrow(results) == 0) {
    # Check if the phenotype is a valid OMIM identifier
    if (grepl("^MTHU|^[0-9]{6}$", phenotype)) {
      query_phen <- sprintf(
        "
        PREFIX omim: <http://purl.bioontology.org/ontology/OMIM/>
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
        PREFIX obo: <http://purl.obolibrary.org/obo/>
        SELECT DISTINCT ?gene_name
        WHERE {
            GRAPH  <http://rdf.biogateway.eu/graph/gene2phen> {
                ?gene obo:RO_0002331 omim:%s .
            }
            GRAPH <http://rdf.biogateway.eu/graph/gene> {
                ?gene skos:prefLabel ?gene_name.
            }
        }
        ",
        phenotype
      )
      results <- SPARQL(endpoint_sparql, query_phen)$results
    }
  }

  # Check if we have results
  if (nrow(results) != 0) {
    # Sort results by gene name
    results_sorted <- results[order(results$gene_name), ]
    return(results_sorted)
  } else {
    return("No data available for the introduced phenotype or you may have introduced an instance that is not a phenotype. Check your data type with type_data function.")
  }
}
