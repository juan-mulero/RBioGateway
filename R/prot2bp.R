#' Protein to biological processes (prot2bp)
#'
#' Function to obtain biological processes associated with a specific protein, optionally filtered by taxon and including sources.
#'
#' @param protein Protein label or ID (string). Example: "TBX5_HUMAN" or "Q99593".
#' @param taxon Taxon name (string) or taxon ID (string or integer). Example: "Homo sapiens" or 9606. Default: "Homo sapiens".
#' @param sources Include sources (boolean: TRUE or FALSE). Default: FALSE.
#'
#' @return A data.frame containing biological process labels and IDs, and optionally relation labels, databases, and articles if sources is TRUE, sorted by process label. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' prot2bp("TBX5_HUMAN", taxon = "Homo sapiens", sources = FALSE)
#' }
#'
#' @export

prot2bp <- function(protein, taxon = "Homo sapiens", sources = F){
  #Endpoint
  endpoint_sparql = "https://2407.biogateway.eu/sparql"

  #Check sources and invariable parts of the queries
  select = ifelse(sources, "?bp ?bp_id ?relation_label ?database ?articles", "?bp ?bp_id")
  prefixes =
    "
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIx rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX oboowl: <http://www.geneontology.org/formats/oboInOwl#>
    PREFIX sio: <http://semanticscience.org/resource/>
    PREFIX taxon: <http://purl.obolibrary.org/obo/NCBITaxon_>
    "

  invariable_query =
    "
    GRAPH <http://rdf.biogateway.eu/graph/prot2bp> {
      ?prot obo:RO_0002331 ?uri_bp .
      ?uri_relation rdf:subject ?prot ;
        rdf:object ?uri_bp ;
        skos:prefLabel ?relation_label .
      ?instance rdf:type ?uri_relation ;
        sio:SIO_000772 ?articles ;
        sio:SIO_000253 ?database .
    }
    GRAPH <http://rdf.biogateway.eu/graph/go> {
      ?uri_bp rdfs:label ?bp ;
        oboowl:id ?bp_id .
    }
    "

  #Check if "taxon" is the name or the id
  if (is.character(taxon)){
    #Query building with prefLabel
    query = sprintf(
      "
      %s
      SELECT DISTINCT %s
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?prot skos:prefLabel \"%s\" ;
            obo:RO_0002162 ?uri_taxon .
        }
        ?uri_taxon rdfs:label \"%s\" .

        %s
      }
      ", prefixes, select, protein, taxon, invariable_query
    )

    # Run query
    results <- SPARQL(endpoint_sparql, query)$results

    if (nrow(results) < 1){
      #Query building with altLabel
      query = sprintf(
        "
      %s
      SELECT DISTINCT %s
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?prot skos:altLabel \"%s\" ;
            obo:RO_0002162 ?uri_taxon .
        }
        ?uri_taxon rdfs:label \"%s\" .

        %s
      }
      ", prefixes, select, protein, taxon, invariable_query
      )

      # Run query
      results <- SPARQL(endpoint_sparql, query)$results
    }

  } else if (is.numeric(taxon)) { #taxon as ID. Example: 9606
    #Query building with prefLabel
    query = sprintf(
      "
      %s
      SELECT DISTINCT %s
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?prot skos:prefLabel \"%s\" ;
            obo:RO_0002162 taxon:%s .
        }

        %s
      }
      ", prefixes, select, protein, taxon, invariable_query
    )

    #Run query
    results <- SPARQL(endpoint_sparql, query)$results

    if (nrow(results) < 1){
      #Query building with altLabel
      query = sprintf(
        "
      %s
      SELECT DISTINCT %s
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?prot skos:altLabel \"%s\" ;
            obo:RO_0002162 taxon:%s .
        }

        %s
      }
      ", prefixes, select, protein, taxon, invariable_query
      )
    }

    #Run query
    results <- SPARQL(endpoint_sparql, query)$results

  } else {return("Incorrect Taxon format")}

  if (nrow(results) > 0){
    if (sources == T){
      #URI simplification
      results$articles = gsub("<|>", "", results$articles)
      results$articles = gsub("http://identifiers.org/pubmed/", "PMID:", results$articles)
      results$database = gsub("<http://identifiers.org/goa/>", "GOA", results$database)
      return(results)
    } else { #There are not sources, only the bp
      return(results)
    }
  } else {
    return("No data, or incorrect protein/taxon provided")
  }
}
