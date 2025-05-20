##### Example 3: Amyotrophic Lateral Sclerosis (ALS)

### Step 1: Retrieve phenotype information about ALS
# --------------------------------------------------
als_info <- getPhenotype("Amyotrophic lateral sclerosis")
print(als_info)
unique_als_info <- unique(als_info$phen_label)
print(unique_als_info)

### Step 2: Retrieve ALS-associated genes and their protein products
# --------------------------------------------
als_genes <- phen2gene("Amyotrophic lateral sclerosis")
print(als_genes)
#  Extract all gene names as a clean character vector
als_gene_list <- as.character(unlist(als_genes))
print(als_gene_list)
# Loop through each gene and retrieve associated proteins
for (gene in als_gene_list) {
  cat("\n--- Proteins derived from gene:", gene, "---\n")
  gene_prots <- gene2prot(gene, taxon = "Homo sapiens")
  print(gene_prots)
}

### Step 3: Retrieve transcription factors associated with ALS-related genes
# --------------------------------------------------------------------------

# 3.1 Retrieve transcription factors that regulate positively ALS-related genes
for (gene in als_gene_list) {
  cat("\n--- Transcription factors derived from gene:", gene, "---\n")
  
  # Fetch transcription factors (TFs) with positive regulation for the current gene
  tfacs_gene_info <- gene2tfac(gene, regulation_type = "positive")
  
  # Check if the result is NULL (no TFs found or API error)
  if (is.null(tfacs_gene_info)) {
    cat("No transcription factors found for", gene, "\n")
    next  # Skip to next gene in the loop
  }
  
  if (is.data.frame(tfacs_gene_info) && "tfac_name" %in% names(tfacs_gene_info)) {
    unique_tfacs <- unique(tfacs_gene_info$tfac_name)
    print(unique_tfacs)
  }
  
  else if (is.atomic(tfacs_gene_info)) {
    print(unique(tfacs_gene_info))
  }

  else {
    cat("Unexpected output format for gene", gene, "Available fields:", 
        paste(names(tfacs_gene_info), collapse = ", "), "\n")
  }
}

# 3.2 Retrieve transcription factors that regulate negatively ALS-related genes
for (gene in als_gene_list) {
  cat("\n--- Transcription factors derived from gene:", gene, "---\n")
  
  # Fetch transcription factors (TFs) with positive regulation for the current gene
  tfacs_gene_info <- gene2tfac(gene, regulation_type = "negative")
  
  # Check if the result is NULL (no TFs found or API error)
  if (is.null(tfacs_gene_info)) {
    cat("No transcription factors found for", gene, "\n")
    next  # Skip to next gene in the loop
  }
  
  if (is.data.frame(tfacs_gene_info) && "tfac_name" %in% names(tfacs_gene_info)) {
    unique_tfacs <- unique(tfacs_gene_info$tfac_name)
    print(unique_tfacs)
  }
  
  else if (is.atomic(tfacs_gene_info)) {
    print(unique(tfacs_gene_info))
  }
  
  else {
    cat("Unexpected output format for gene", gene, "Available fields:", 
        paste(names(tfacs_gene_info), collapse = ", "), "\n")
  }
}

