#' Get TADs by coordinates (getTADs_by_coord)
#'
#' Function to retrieve topologically associating domains (TADs) located within a specified range of genomic coordinates.
#'
#' @param chromosome Chromosome name (string). Example: "chr-13".
#' @param start Start position (integer).
#' @param end End position (integer).
#'
#' @return A data.frame containing TAD IDs, start, and end positions for TADs within the specified coordinates. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' getTADs_by_coord("chr-13", 34120000, 35840000)
#' }
#'
#' @export

getTADs_by_coord <- function(chromosome, start, end) {
  # Define the SPARQL endpoint
  endpoint_sparql <- "https://2407.biogateway.eu/sparql"

  # Translate the chromosome name
  chromosome_ncbi <- translate_chr(chromosome)

  # Build the SPARQL query
  query <- sprintf(
    "
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX nuccore: <https://www.ncbi.nlm.nih.gov/nuccore/>
    SELECT DISTINCT ?tad_id ?start ?end
    WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/tad> {
            ?tad obo:GENO_0000894 ?start ;
                  skos:prefLabel ?tad_id ;
                  obo:GENO_0000895 ?end ;
                  obo:BFO_0000050 nuccore:%s ;
                  obo:RO_0002162 ?taxon .
            # Filter by the specified chromosome and coordinate range
            FILTER (?start >= %s && ?end <= %s)
        }
    }
    ",
    chromosome_ncbi, start, end
  )

  # Execute the SPARQL query
  results <- SPARQL(endpoint_sparql, query)$results

  # Check if there are any results
  if (nrow(results) == 0) {
    return("No data available for the introduced genomic coordinates.")
  } else {
    # Sort the results by TAD id
    results_sorted <- results[order(results$tad_id), ]

    return(results_sorted)
  }
}
