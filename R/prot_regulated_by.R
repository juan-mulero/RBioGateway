#' Proteins regulating a protein (prot_regulated_by)
#'
#' Function to obtain proteins that regulate a specific protein of interest, filtered by regulation type.
#'
#' @param protein Protein label or ID (string). Example: "BRCA1_HUMAN" or "P38398".
#' @param regulation_type Type of regulation (string): "positive", "negative", or "all". Default: "all".
#'
#' @return A data.frame containing regulating protein labels, definitions, regulatory relation labels, databases, articles, and evidence levels, sorted by protein label. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' prot_regulated_by("BRCA1_HUMAN", regulation_type = "negative")
#' }
#'
#' @export

prot_regulated_by <- function(protein, regulation_type = "all") {
  # Endpoint
  endpoint_sparql <- "https://2407.biogateway.eu/sparql"

  # Prefixes
  prefixes <- "
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX sch: <http://schema.org/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX sio: <http://semanticscience.org/resource/>
    PREFIX oboowl: <http://www.geneontology.org/formats/oboInOwl#>
  "

  # Query building with prefLabel
  query <- sprintf(
    "
    %s
    SELECT DISTINCT ?prot_label ?definition ?regulatory_relation_label ?database ?articles ?evidence_level
    WHERE {
      GRAPH <http://rdf.biogateway.eu/graph/prot> {
        ?prot skos:prefLabel \"%s\" .
      }
      GRAPH <http://rdf.biogateway.eu/graph/reg2targ> {
        ?uri_relation rdf:object ?prot ;
             rdf:subject ?prot2 ;
             skos:definition ?definition ;
             skos:prefLabel ?regulatory_relation_label .
        ?instance rdf:type ?uri_relation ;
             sio:SIO_000772 ?articles ;
             sio:SIO_000253 ?database ;
             sch:evidenceLevel ?evidence_level .
      }
      GRAPH <http://rdf.biogateway.eu/graph/prot> {
        ?prot2 skos:prefLabel ?prot_label .
      }
    }
    ", prefixes, protein
  )

  # Run query
  results <- SPARQL(endpoint_sparql, query)$results

  # If no results, try with altLabel
  if (nrow(results) == 0) {
    query_alt_label <- sprintf(
      "
      %s
      SELECT DISTINCT ?prot_label ?definition ?regulatory_relation_label ?database ?articles ?evidence_level
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?prot skos:altLabel \"%s\" .
        }
        GRAPH <http://rdf.biogateway.eu/graph/reg2targ> {
          ?uri_relation rdf:object ?prot ;
               rdf:subject ?prot2 ;
               skos:definition ?definition ;
               skos:prefLabel ?regulatory_relation_label .
          ?instance rdf:type ?uri_relation ;
               sio:SIO_000772 ?articles ;
               sio:SIO_000253 ?database ;
               sch:evidenceLevel ?evidence_level .
        }
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?prot2 skos:prefLabel ?prot_label .
        }
      }
      ", prefixes, protein
    )

    # Run query with altLabel
    results <- SPARQL(endpoint_sparql, query_alt_label)$results
  }

  # Check if results are empty
  if (nrow(results) == 0) {
    return("No data available for the introduced protein or you may have introduced an instance that is not a protein. Check your data type with type_data function.")
  } else {
    # Simplify URIs
    results$articles <- gsub("<|>", "", results$articles)
    results$articles <- gsub("http://identifiers.org/pubmed/", "PMID:", results$articles)
    results$database <- gsub("<http://signor.uniroma2.it>", "Signor", results$database)

    # Filter by regulation type
    if (regulation_type == "positive") {
      results <- results[grepl("positive regulation", results$definition, ignore.case = TRUE), ]
      if (nrow(results) == 0) {
        return("No data available on positive regulation of the introduced gene. Use the ‘all’ option to search for regulatory information without specifying the type.")
      }
    } else if (regulation_type == "negative") {
      results <- results[grepl("negative regulation", results$definition, ignore.case = TRUE), ]
      if (nrow(results) == 0) {
        return("No data available on negative regulation of the introduced gene. Use the ‘all’ option to search for regulatory information without specifying the type.")
      }
    }

    # Sort results by protein label
    results = results[order(results$prot_label), ]
    row.names(results) = 1:nrow(results)
    return(results)
  }
}
