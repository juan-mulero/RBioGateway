#' Get genes by coordinates (getGenes_by_coord)
#'
#' Function to retrieve genes located within a specified range of genomic coordinates, optionally filtered by strand orientation.
#'
#' @param chr Chromosome name (string). Example: "chr-1".
#' @param start Start position (integer).
#' @param end End position (integer).
#' @param strand Strand orientation (string), or NULL if not specified. Example: "ForwardStrandPosition". Default: NULL.
#'
#' @return A data.frame containing gene names, start, end, and strand for genes within the specified coordinates. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' getGenes_by_coord("chr-1", 52565276, 58596412, "ForwardStrandPosition")
#' }
#'
#' @export

getGenes_by_coord <- function(chr, start, end, strand = NULL) {
  # Define the SPARQL endpoint
  endpoint_sparql <- "https://2407.biogateway.eu/sparql"

  # Translate chromosome name to NCBI ID
  chr_ncbi <- translate_chr(chr)

  # Case when strand is NULL
  if (is.null(strand)) {
    query <- sprintf(
      "
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX obo: <http://purl.obolibrary.org/obo/>
      PREFIX nuccore: <https://www.ncbi.nlm.nih.gov/nuccore/>
      SELECT DISTINCT ?gene_name ?start ?end ?strand
      WHERE {
          GRAPH <http://rdf.biogateway.eu/graph/gene> {
              ?gene obo:GENO_0000894 ?start ;
                    skos:prefLabel ?gene_name ;
                    obo:GENO_0000895 ?end ;
                    obo:BFO_0000050 nuccore:%s ;
                    obo:GENO_0000906 ?strand ;
                    obo:RO_0002162 ?taxon .
              # Filter by the specified chromosome and coordinate range
              FILTER (xsd:integer(?start) >= %s && xsd:integer(?end) <= %s)
          }
      }
      ",
      chr_ncbi, start, end
    )

    # Execute the SPARQL query
    results <- SPARQL(endpoint_sparql, query)$results

    # Process the strand field by removing the URI prefix
    if (nrow(results) > 0) {
      results$strand <- gsub("http://biohackathon.org/resource/faldo#", "", results$strand)
    }
  }
  # Case when strand is specified
  else {
    query <- sprintf(
      "
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX obo: <http://purl.obolibrary.org/obo/>
      PREFIX nuccore: <https://www.ncbi.nlm.nih.gov/nuccore/>
      PREFIX strand: <http://biohackathon.org/resource/faldo#>
      SELECT DISTINCT ?gene_name ?start ?end
      WHERE {
          GRAPH <http://rdf.biogateway.eu/graph/gene> {
              ?gene obo:GENO_0000894 ?start ;
                    skos:prefLabel ?gene_name ;
                    obo:GENO_0000895 ?end ;
                    obo:BFO_0000050 nuccore:%s ;
                    obo:GENO_0000906 strand:%s ;
                    obo:RO_0002162 ?taxon .
              # Filter by the specified chromosome and coordinate range
              FILTER (xsd:integer(?start) >= %s && xsd:integer(?end) <= %s)
          }
      }
      ",
      chr_ncbi, strand, start, end
    )

    # Execute the SPARQL query
    results <- SPARQL(endpoint_sparql, query)$results
  }

  # Check if there are any results
  if (nrow(results) == 0) {
    return("No data available for the introduced genomic coordinates.")
  } else {
    # Sort the results by gene name
    results_sorted <- results[order(results$gene_name), ]
    return(results_sorted)
  }
}
