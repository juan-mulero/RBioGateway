##### Example 2: Phenylketonuria (PKU)

### Step 1: Retrieve phenotype information about PKU
# --------------------------------------------------
pku_info <- getPhenotype("Phenylketonuria")
print(pku_info)


### Step 2: Retrieve genes associated with Phenylketonuria
# ---------------------------------------------------------
pku_genes <- phen2gene("Phenylketonuria")
print(pku_genes)


### Step 3: Retrieve general information about the PAH gene
# ----------------------------------------------------------
gene_info <- getGene_info("PAH", taxon = "Homo sapiens")
print(gene_info)


### Step 4: Retrieve cis-regulatory modules (CRMs) and transcription factors
# --------------------------------------------------------------------------

# 4.1 Get CRMs associated with the PAH gene
crms_PKU <- gene2crm("PAH")
print(crms_PKU)

# Extract unique CRM names
unique_crms <- unique(crms_PKU$crm_name)
print(unique_crms)

# Count how many unique CRMs are associated
num_unique_crms <- length(unique_crms)
message("There are ", num_unique_crms, " unique CRMs associated with PAH.")

# 4.2 Get transcription factors related with the PAH gene

# 4.2.1 Retrieve transcription factors that positively regulate PAH
tfacs_pos_PKU <- gene2tfac("PAH", regulation_type = "positive")
print(tfacs_pos_PKU)
# Count how many transcription factors regulates positively PAH
unique_tfacs_pos <- unique(tfacs_pos_PKU$tfac_name)
num_pos_tfacs <- length(unique_tfacs_pos)
message("There are ", num_pos_tfacs, " transcription factors positively regulating PAH.")
print(unique_tfacs_pos)

# 4.2.2 Retrieve transcription factors that negatively regulate PAH
tfacs_neg_PKU <- gene2tfac("PAH", regulation_type = "negative")
print(tfacs_neg_PKU)


### Step 6: Explore biological context of the protein encoded by the gene
# -----------------------------------------------------------------------

# 6.1 Retrieve proteins derived from the PAH gene
gene_proteins <- gene2prot("PAH", taxon = "Homo sapiens")
print(gene_proteins)

# Select the canonical isoform (PH4H_HUMAN)

# 6.2 Retrieve molecular functions associated with the protein
protein_mf <- prot2mf("PH4H_HUMAN")
print(protein_mf)

# 6.3 Retrieve biological processes involving the protein
protein_bp <- prot2bp("PH4H_HUMAN")
print(protein_bp)