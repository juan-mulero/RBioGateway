#' Molecular function to proteins (mf2prot)
#'
#' Function to obtain the proteins associated with a molecular function, optionally filtered by taxon and including sources.
#'
#' @param molecular_function Molecular function label or ID (string). Example: "protein kinase activity".
#' @param taxon Taxon name (string) or taxon ID (string or integer), or NULL if not specified. Example: "Homo sapiens" or 9606. Default: NULL.
#' @param sources Include sources (boolean: TRUE or FALSE). Default: FALSE.
#'
#' @return A data.frame of proteins associated with the molecular function and their sources if sources is TRUE, otherwise a vector of unique protein names. If no data is available, a message is returned.
#'
#' @examples
#' \dontrun{
#' mf2prot("protein kinase activity", "Homo sapiens", sources = TRUE)
#' }
#'
#' @export

mf2prot <- function(molecular_function, taxon = NULL, sources = FALSE) {
  # Endpoint
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  # Check sources and invariable parts of the queries
  select <- ifelse(sources,
                   "?protein_name ?relation_label ?database ?articles", "?protein_name")
  prefixes <- "
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX sio: <http://semanticscience.org/resource/>
    PREFIX oboowl: <http://www.geneontology.org/formats/oboInOwl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX taxon: <http://purl.obolibrary.org/obo/NCBITaxon_>
  "

  invariable_query <- "
    GRAPH <http://rdf.biogateway.eu/graph/prot2mf> {
      ?uri_relation rdf:subject ?prot ;
        rdf:predicate ?relation ;
        rdf:object ?uri_mf ;
        skos:prefLabel ?relation_label .
      ?instance rdf:type ?uri_relation ;
        sio:SIO_000772 ?articles ;
        sio:SIO_000253 ?database .
    }
    GRAPH <http://rdf.biogateway.eu/graph/prot> {
      ?prot skos:prefLabel ?protein_name .
    }
  "

  # Handle taxon
  if (is.null(taxon)) {
    # Query building with prefLabel (no taxon)
    query <- sprintf(
      "
      %s
      SELECT DISTINCT %s
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/go> {
          ?uri_mf rdfs:label ?mf_label .
          FILTER regex(?mf_label, '%s', 'i')
        }
        %s
      }
      ORDER BY ?protein_name
      ", prefixes, select, molecular_function, invariable_query
    )

    # Run query
    results <- SPARQL(endpoint_sparql, query)$results

    # If no results, try with GO ID
    if (nrow(results) < 1) {
      query <- sprintf(
        "
        %s
        SELECT DISTINCT %s
        WHERE {
          GRAPH <http://rdf.biogateway.eu/graph/go> {
            ?uri_mf oboowl:id '%s' .
          }
          %s
        }
        ORDER BY ?protein_name
        ", prefixes, select, molecular_function, invariable_query
      )

      results <- SPARQL(endpoint_sparql, query)$results
    }

  } else if (is.character(taxon)) {
    # Query building with prefLabel and taxon name
    query <- sprintf(
      "
      %s
      SELECT DISTINCT %s
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/go> {
          ?uri_mf rdfs:label ?mf_label .
          FILTER regex(?mf_label, '%s', 'i')
        }
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?prot obo:RO_0002162 ?uri_taxon .
        }
        GRAPH <http://rdf.biogateway.eu/graph/taxon> {
          ?uri_taxon rdfs:label '%s' .
        }
        %s
      }
      ORDER BY ?protein_name
      ", prefixes, select, molecular_function, taxon, invariable_query
    )

    results <- SPARQL(endpoint_sparql, query)$results

    # If no results, try with GO ID
    if (nrow(results) < 1) {
      query <- sprintf(
        "
        %s
        SELECT DISTINCT %s
        WHERE {
          GRAPH <http://rdf.biogateway.eu/graph/go> {
            ?uri_mf oboowl:id '%s' .
          }
          GRAPH <http://rdf.biogateway.eu/graph/prot> {
            ?prot obo:RO_0002162 ?uri_taxon .
          }
          GRAPH <http://rdf.biogateway.eu/graph/taxon> {
            ?uri_taxon rdfs:label '%s' .
          }
          %s
        }
        ORDER BY ?protein_name
        ", prefixes, select, molecular_function, taxon, invariable_query
      )

      results <- SPARQL(endpoint_sparql, query)$results
    }

  } else if (is.numeric(taxon)) {
    # Query building with prefLabel and taxon ID
    query <- sprintf(
      "
      %s
      SELECT DISTINCT %s
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/go> {
          ?uri_mf rdfs:label ?mf_label .
          FILTER regex(?mf_label, '%s', 'i')
        }
        GRAPH <http://rdf.biogateway.eu/graph/prot> {
          ?prot obo:RO_0002162 taxon:%s .
        }
        %s
      }
      ORDER BY ?protein_name
      ", prefixes, select, molecular_function, taxon, invariable_query
    )

    results <- SPARQL(endpoint_sparql, query)$results

    # If no results, try with GO ID
    if (nrow(results) < 1) {
      query <- sprintf(
        "
        %s
        SELECT DISTINCT %s
        WHERE {
          GRAPH <http://rdf.biogateway.eu/graph/go> {
            ?uri_mf oboowl:id '%s' .
          }
          GRAPH <http://rdf.biogateway.eu/graph/prot> {
            ?prot obo:RO_0002162 taxon:%s .
          }
          %s
        }
        ORDER BY ?protein_name
        ", prefixes, select, molecular_function, taxon, invariable_query
      )

      results <- SPARQL(endpoint_sparql, query)$results
    }

  } else {
    return("Incorrect Taxon format")
  }

  # Process results
  if (nrow(results) > 0) {
    if (sources) {
      # URI simplification
      if ("articles" %in% colnames(results)) {
        results$articles <- gsub("<|>", "", results$articles)
        results$articles <- gsub("http://identifiers.org/pubmed/", "PMID:", results$articles)
      }
      if ("database" %in% colnames(results)) {
        results$database <- gsub("<http://identifiers.org/goa/>", "GOA", results$database)
      }
      return(results)
    } else {

      return(unique(results$protein_name))
    }
  } else {
    return("No data available for the introduced molecular function. Check that the molecular function ID or taxon is correct.")
  }
}
