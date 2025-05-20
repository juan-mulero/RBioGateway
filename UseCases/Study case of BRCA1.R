##### Example 1: Study case of gene BRCA1

### Step 1: Get core information for the BRCA1 gene
# -------------------------------------------------
brca1_info <- getGene_info("BRCA1", taxon = "Homo sapiens")
print(brca1_info)


### Step 2: Retrieve phenotypes associated with BRCA1
# ---------------------------------------------------
phenotypes_brca1 <- gene2phen("BRCA1")
print(phenotypes_brca1)

## What proteins derive from BRCA1 transcripts?
brca1_prots <- gene2prot("BRCA1", taxon = "Homo sapiens")
print(brca1_prots)
# Number of proteins (isoforms)
num_isoforms <- length(brca1_prots)
message("There are ", num_isoforms, " isoforms derive form BRCA1 transcripts.")


### Step 3: Cellular compartments, molecular functions and biological processes for BRCA1_HUMAN
# ---------------------------------------------------------------------------------------------

# 3.1.1 Retrieve cellular compartments where BRCA1_HUMAN is localized
brca1_cc <- prot2cc("BRCA1_HUMAN")
print(brca1_cc)

# 3.1.2 Extract and display the list of compartment labels
unique_compartments <- unique(brca1_cc$cc_label)
compartments <- list("Cellular compartments" = unique_compartments)
print(compartments)

# 3.1.3 Count how many compartments are reported
num_compartments <- length(unique_compartments)
message("There are ", num_compartments, " cellular compartments where BRCA1 is present.")

# 3.2.1 Retrieve molecular functions annotated for BRCA1_HUMAN
brca1_mf <- prot2mf("BRCA1_HUMAN")
print(brca1_mf)

# 3.2.2 Extract and display the list of unique molecular function labels
unique_mf <- unique(brca1_mf$mf_label)
molecular_functions <- list("Molecular functions" = unique_mf)
print(molecular_functions)

# 3.2.3 Count how many unique molecular functions are reported
num_mf <- length(unique_mf)
message("There are ", num_mf, " molecular functions associated with BRCA1.")

# 3.3.1 Retrieve biological processes involving BRCA1_HUMAN
brca1_bp <- prot2bp("BRCA1_HUMAN")
print(brca1_bp)

# 3.3.2 Extract and display the list of unique biological process labels
unique_bp <- unique(brca1_bp$bp)
biological_processes <- list("Biological processes" = unique_bp)
print(biological_processes)

# 3.3.3 Count how many unique biological processes are reported
num_bp <- length(unique_bp)
message("There are ", num_bp, " biological processes involving BRCA1.")


### Step 4: Protein interactions and evolutionary conservation of BRCA1_HUMAN
# ----------------------------------------------------------------------------

# 4.1 Protein–protein interactions
# Which proteins physically or functionally interact with BRCA1_HUMAN?

brca1_ppi <- prot2prot("BRCA1_HUMAN")
print(brca1_ppi)

# How many unique interactions are reported?
unique_interactors <- unique(brca1_ppi$prot_label)
num_interactors <- length(unique_interactors)
message("BRCA1_HUMAN interacts with ", num_interactors, " different proteins.")
print(unique_interactors)

# 4.2 Orthologous proteins in other species
# Which orthologs exist for BRCA1_HUMAN in model organisms?

brca1_ortho <- prot2ortho("BRCA1_HUMAN")
print(brca1_ortho)

# Number of orthologs found
num_ortho <- nrow(brca1_ortho)
message("A total of ", num_ortho, " orthologous proteins were found for BRCA1_HUMAN.")

# What species are represented?
species_list <- unique(brca1_ortho$taxon)
print(list("Species with orthologs" = species_list))

