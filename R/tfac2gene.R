#' Transcription factor to genes (tfac2gene)
#'
#' Function to obtain genes regulated by a specific transcription factor, filtered by regulation type.
#'
#' @param tfac Transcription factor label (string). Example: "NKX31_HUMAN".
#' @param regulation_type Type of regulation (string): "positive", "negative", or "all". Default: "all".
#'
#' @return A data.frame containing gene names, databases, articles, evidence levels, and definitions for the specified regulation type, sorted by gene name. If regulation_type is "all", a named list with data.frames for positive and negative regulation is returned. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' tfac2gene("NKX31_HUMAN", regulation_type = "positive")
#' }
#'
#' @export

tfac2gene <- function(tfac, regulation_type = "all") {
  # Define the SPARQL endpoint
  endpoint_sparql <- "https://2407.biogateway.eu/sparql"

  # SPARQL prefixes used in all queries
  prefixes <- "
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX rdfs: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX sio: <http://semanticscience.org/resource/>
    PREFIX sch: <http://schema.org/>
  "

  # Helper to clean and prepare result data.frames
  clean_results <- function(df) {
    if (!"articles" %in% colnames(df)) df$articles <- NA
    if (!"database" %in% colnames(df)) df$database <- NA
    if (!"evidence_level" %in% colnames(df)) df$evidence_level <- NA
    df$articles <- gsub("<|>", "", df$articles)
    df$articles <- gsub("http://identifiers.org/pubmed/", "PMID:", df$articles)
    df$database <- gsub("<|>", "", df$database)
    df$database <- gsub("<http://identifiers.org/goa/>", "GOA", df$database)
    df[order(df$gene_name), ]
  }

  # Query builder
  build_query <- function(predicate_uri) {
    sprintf(
      "
      %s
      SELECT DISTINCT ?gene_name ?database ?articles ?evidence_level ?definition
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?tfac skos:prefLabel \"%s\" .
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
        GRAPH <http://rdf.biogateway.eu/graph/gene> {
          ?gene skos:prefLabel ?gene_name .
        }
      }
      ", prefixes, tfac, predicate_uri
    )
  }

  # Run both queries
  positive_results <- SPARQL(endpoint_sparql, build_query("obo:RO_0002429"))$results
  negative_results <- SPARQL(endpoint_sparql, build_query("obo:RO_0002430"))$results

  # Filter if specified
  if (regulation_type == "positive") {
    positive_results <- positive_results[grepl("positive regulation", positive_results$definition), ]
    if (nrow(positive_results) == 0) {
      return("No data available on positive regulation of the introduced transcription factor.")
    }
    return(clean_results(positive_results))
  }

  if (regulation_type == "negative") {
    negative_results <- negative_results[grepl("negative regulation", negative_results$definition), ]
    if (nrow(negative_results) == 0) {
      return("No data available on negative regulation of the introduced transcription factor.")
    }
    return(clean_results(negative_results))
  }

  # If no results and regulation_type == "all", use fallback general query
  if (nrow(positive_results) == 0 && nrow(negative_results) == 0 && regulation_type == "all") {
    general_query <- sprintf(
      "
      %s
      SELECT DISTINCT ?gene_name ?database ?articles ?evidence_level ?definition
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?tfac skos:prefLabel \"%s\" .
        }
        GRAPH <http://rdf.biogateway.eu/graph/tfac2gene> {
          ?s rdfs:subject ?tfac ;
             rdfs:object ?gene ;
             rdfs:predicate ?relation ;
             skos:definition ?definition .
          ?uri rdfs:type ?s .
          OPTIONAL { ?uri sio:SIO_000772 ?articles . }
          OPTIONAL { ?uri sio:SIO_000253 ?database . }
          OPTIONAL { ?uri sch:evidenceLevel ?evidence_level . }
        }
        GRAPH <http://rdf.biogateway.eu/graph/gene> {
          ?gene skos:prefLabel ?gene_name .
        }
      }
      ", prefixes, tfac
    )

    general_results <- SPARQL(endpoint_sparql, general_query)$results
    if (nrow(general_results) == 0) {
      return("No data available for the introduced transcription factor.")
    }
    return(clean_results(general_results))
  }

  # Return both results (positive and negative) as named list of data.frames
  list(
    positive_regulation = clean_results(positive_results),
    negative_regulation = clean_results(negative_results)
  )
}
