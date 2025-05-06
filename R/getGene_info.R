#' Get gene information (getGene_info)
#'
#' Function to obtain the main data for a specific gene, including coordinates, definition, and alternative sources, filtered by taxon.
#'
#' @param gene Gene symbol (string). Example: "Brca1".
#' @param taxon Taxon name (string) or taxon ID (string or integer). Example: "Mus musculus" or 10090.
#'
#' @return A list containing the start, end, strand, chromosome, assembly, definition, and alternative gene sources for the gene. If no data is available, a list with an error message is returned.
#'
#' @examples
#' \dontrun{
#' getGene_info("Brca1", "Mus musculus")
#' }
#'
#' @export

getGene_info <- function(gene, taxon) {
  # SPARQL endpoint
  endpoint_sparql <- "https://2407.biogateway.eu/sparql"

  # Check if taxon is numeric or a name
  if (suppressWarnings(!is.na(as.numeric(taxon)))) {
  } else {
    # Query to get taxon ID from its name
    query_tax <- sprintf(
      "
      PREFIX tax: <http://purl.obolibrary.org/obo/>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      SELECT DISTINCT ?taxon
      WHERE {
          GRAPH <http://rdf.biogateway.eu/graph/taxon> {
              ?taxon rdfs:label '%s'.
          }
      }
      ",
      taxon
    )

    tax_result <- SPARQL(endpoint_sparql, query_tax)$results

    # If no taxon is found, return an error message
    if (nrow(tax_result) == 0) {
      return(list(error = "No data available for the introduced taxon."))
    }

    # Extract numeric taxon ID from URI
    tax_uri <- tax_result$taxon[1]
    num_taxon <- gsub(".*_|>", "", tax_uri)
  }

  # Main query to retrieve gene information
  query <- sprintf(
    "
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX gene: <http://rdf.biogateway.eu/gene/%s/>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX dc: <http://purl.org/dc/terms/>
    SELECT DISTINCT ?start ?end ?strand_label ?chr_label ?assembly_label ?definition ?alt_gene_sources
    WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/gene> {
            gene:%s  obo:GENO_0000894 ?start ;
                    obo:GENO_0000895 ?end ;
                    obo:BFO_0000050 ?chr ;
                    obo:GENO_0000906 ?strand ;
                    skos:definition ?definition ;
                    skos:closeMatch ?alt_gene_sources ;
                    dc:hasVersion ?assembly .
            ?chr skos:prefLabel ?chr_label .
            ?strand skos:prefLabel ?strand_label .
            ?assembly skos:prefLabel ?assembly_label .
        }
    }
    ",
    num_taxon, gene
  )

  results <- SPARQL(endpoint_sparql, query)$results

  # If no results are found, return an error message
  if (nrow(results) == 0) {
    return(list(error = "No data available for the introduced gene."))
  }

  # Process alternative gene sources as a list
  alt_gene_sources <- unique(results$alt_gene_sources[!is.na(results$alt_gene_sources)])
  alt_gene_sources <- gsub("^<|>$", "", alt_gene_sources) # Remove < at start and > at end

  # Return results as a list
  return(list(
    start = results$start[1],
    end = results$end[1],
    strand = results$strand_label[1],
    chromosome = results$chr_label[1],
    assembly = results$assembly_label[1],
    definition = results$definition[1],
    alt_gene_sources = alt_gene_sources
  ))
}
