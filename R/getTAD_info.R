#' Get TAD information (getTAD_info)
#'
#' Function to obtain the main data for a specific topologically associating domain (TAD), including coordinates, taxon, and definition.
#'
#' @param tad Name of the TAD (string). Example: "tad/TADHS00000038004".
#'
#' @return A data.frame containing the chromosome, start, end, assembly, taxon, and definition of the TAD. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' getTAD_info("tad/TADHS00000038004")
#' }
#'
#' @export

getTAD_info <- function(tad) {
  # Endpoint SPARQL
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  # Query
  query <- sprintf(
    "
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX dc: <http://purl.org/dc/terms/>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT DISTINCT ?chr ?start ?end ?assembly ?taxon ?definition
    WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/tad> {
            ?tad_id skos:prefLabel \"%s\" ;
                    obo:GENO_0000894 ?start ;
                    obo:GENO_0000895 ?end ;
                    obo:BFO_0000050 ?URI_chr ;
                    obo:RO_0002162 ?URI_taxon ;
                    skos:definition ?definition ;
                    dc:hasVersion ?URI_assembly .

	          ?URI_chr skos:prefLabel ?chr .
	          ?URI_assembly skos:prefLabel ?assembly .
       }
        ?URI_taxon rdfs:label ?taxon .
    }
    ",
    tad
  )

  # Run query
  results <- SPARQL(endpoint_sparql, query)$results

  # Check results
  if (nrow(results) == 0) {
    return("No data available for the introduced tad or you may have introduced an instance that is not a tad. Check your data type with type_data function.")
  }

  return(results)
}
