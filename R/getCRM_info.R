#' Get CRM information (getCRM_info)
#'
#' Function to obtain the main data for a specific cis-regulatory module (CRM), including coordinates, taxon, and definition.
#'
#' @param crm Name of the CRM (string). Example: "crm/CRMHS00000005387".
#'
#' @return A data.frame containing the start, end, chromosome, assembly, taxon, and definition of the CRM. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' getCRM_info("crm/CRMHS00000005387")
#' }
#'
#' @export

getCRM_info <- function(crm) {
  # Endpoint SPARQL
  endpoint_sparql <- "https://2407.biogateway.eu/sparql"

  # Query building
  query <- sprintf(
    "
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX dc: <http://purl.org/dc/terms/>
    PREFIX sio: <http://semanticscience.org/resource/>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT DISTINCT ?start ?end ?chr ?assembly ?taxon ?definition
    WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/crm> {
            ?enh_id skos:prefLabel \"%s\" ;
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
    crm
  )

  # Run query
  results <- SPARQL(endpoint_sparql, query)$results

  # Check results
  if (nrow(results) == 0) {
    return("No data available for the introduced crm or you may have introduced an instance that is not a crm. Check your data type with type_data function.")
  } else {
    return(results)
  }
}


