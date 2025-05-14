#' Gene to transcription factors (gene2tfac)
#'
#' Function to obtain the transcription factors regulating a gene, optionally filtered by regulation type.
#'
#' @param gene Gene label (string). Example: "BRCA1".
#' @param regulation_type Type of regulation (string). Options: "positive", "negative", or "all". Default: "all".
#'
#' @return A data.frame or list of data.frames containing transcription factors regulating the gene, their sources, evidence level, and definition. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' gene2tfac("BRCA1", regulation_type = "negative")
#' }
#'
#' @export

gene2tfac <- function(gene, regulation_type = "all") {
  # Define the SPARQL endpoint
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  # Prefixes
  prefixes <- "
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX rdfs: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX sio: <http://semanticscience.org/resource/>
    PREFIX sch: <http://schema.org/>
  "

  # Helper function to clean URIs
  clean_results <- function(df) {
    if (!"articles" %in% colnames(df)) df$articles <- NA
    if (!"database" %in% colnames(df)) df$database <- NA
    df$articles <- gsub("<|>", "", df$articles)
    df$articles <- gsub("http://identifiers.org/pubmed/", "PMID:", df$articles)
    df$database <- gsub("<|>", "", df$database)
    df$database <- gsub("<http://identifiers.org/goa/>", "GOA", df$database)
    return(df)
  }

  # Base query builder for any regulation type
  build_query <- function(predicate_uri) {
    sprintf(
      "
      %s
      SELECT DISTINCT ?tfac_name ?database ?articles ?evidence_level ?definition
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/gene> {
          ?gene skos:prefLabel \"%s\" .
        }
        GRAPH <http://rdf.biogateway.eu/graph/tfac2gene> {
          ?tfac %s ?gene .
          ?s rdfs:subject ?tfac ;
             rdfs:predicate ?relation ;
             rdfs:object ?gene ;
             skos:definition ?definition .
          ?uri rdfs:type ?s .
          OPTIONAL { ?uri sio:SIO_000772 ?articles . }
          OPTIONAL { ?uri sio:SIO_000253 ?database . }
          OPTIONAL { ?uri sch:evidenceLevel ?evidence_level . }
        }
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?tfac skos:prefLabel ?tfac_name .
        }
      }
      ", prefixes, gene, predicate_uri
    )
  }

  # Run SPARQL queries
  positive_results <- SPARQL(endpoint_sparql, build_query("obo:RO_0002429"))$results
  negative_results <- SPARQL(endpoint_sparql, build_query("obo:RO_0002430"))$results

  # Filter by regulation type if specified
  if (regulation_type == "positive") {
    positive_results <- positive_results[grepl("positive regulation", positive_results$definition), ]
    if (nrow(positive_results) == 0) {
      return("No data available on positive regulation of the introduced gene.")
    }
    return(clean_results(positive_results))
  }

  if (regulation_type == "negative") {
    negative_results <- negative_results[grepl("negative regulation", negative_results$definition), ]
    if (nrow(negative_results) == 0) {
      return("No data available on negative regulation of the introduced gene.")
    }
    return(clean_results(negative_results))
  }

  # If both are empty and regulation_type == "all", run general query
  if (nrow(positive_results) == 0 && nrow(negative_results) == 0 && regulation_type == "all") {
    general_query <- sprintf(
      "
      %s
      SELECT DISTINCT ?tfac_name ?database ?articles ?evidence_level ?definition
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/gene> {
          ?gene skos:prefLabel \"%s\" .
        }
        GRAPH <http://rdf.biogateway.eu/graph/tfac2gene> {
          ?s rdfs:object ?gene ;
             rdfs:subject ?tfac ;
             rdfs:predicate ?relation ;
             skos:definition ?definition .
          ?uri rdfs:type ?s .
          OPTIONAL { ?uri sio:SIO_000772 ?articles . }
          OPTIONAL { ?uri sio:SIO_000253 ?database . }
          OPTIONAL { ?uri sch:evidenceLevel ?evidence_level . }
        }
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?tfac skos:prefLabel ?tfac_name .
        }
      }
      ", prefixes, gene
    )

    general_results <- SPARQL(endpoint_sparql, general_query)$results

    if (nrow(general_results) == 0) {
      return("No data available for the introduced gene.")
    }

    return(clean_results(general_results))
  }

  # Combine both regulation types into a list of data.frames
  list(
    positive_regulation = clean_results(positive_results),
    negative_regulation = clean_results(negative_results)
  )
}
