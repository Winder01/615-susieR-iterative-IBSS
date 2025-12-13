library(data.table)

# --- 1. SETUP PATHS ---
sumstats_path <- "E:/OneDrive/UMich Master/2025 Fall/BIOS615/Project/susiedata4catherine/susiedata4catherine/sumstats/E4_LIPOPROT.1-54839974-55239974.tsv.gz"
ld_path       <- "E:/OneDrive/UMich Master/2025 Fall/BIOS615/Project/susiedata4catherine/susiedata4catherine/ld/finngen_r12_ld.1-54839974-55239974.tsv.gz"

cat("========================================================\n")
cat("DIAGNOSTIC SCRIPT: FINNGEN ID MATCHER\n")
cat("========================================================\n\n")

# --- 2. LOAD & CLEAN SUMSTATS ---
cat("1. Loading Summary Statistics...\n")
if (!file.exists(sumstats_path)) stop(paste("File not found:", sumstats_path))

# Read first 500 rows
ss <- fread(sumstats_path, nrows = 500)

# CLEAN NAMES: Handle "#chrom"
old_names_ss <- names(ss)
names(ss) <- tolower(names(ss))
names(ss)[names(ss) == "#chrom"] <- "chrom" # Fix the hash issue

cat("   -> Original Cols:", paste(head(old_names_ss, 5), collapse=", "), "...\n")
cat("   -> Cleaned Cols: ", paste(head(names(ss), 5), collapse=", "), "...\n")

if (!"chrom" %in% names(ss)) stop("Column 'chrom' (or #chrom) not found in SumStats.")

# --- 3. CONSTRUCT IDs IN SUMSTATS ---
cat("\n2. Constructing IDs in SumStats...\n")

# Ensure character type for pasting
ss$chrom <- as.character(ss$chrom)
ss$pos   <- as.character(ss$pos)
ss$ref   <- as.character(ss$ref)
ss$alt   <- as.character(ss$alt)

# Version A: Raw (1_12345_A_G)
# Note: We strip 'chr' from chrom just in case it exists, to ensure a clean raw start
ss$chrom_clean <- gsub("^chr", "", ss$chrom, ignore.case = TRUE) 
ss$match_id_raw <- paste(ss$chrom_clean, ss$pos, ss$ref, ss$alt, sep = "_")

# Version B: With Prefix (chr1_12345_A_G)
ss$match_id_chr <- paste0("chr", ss$match_id_raw)

cat("   -> Sample Raw ID:  ", ss$match_id_raw[1], "\n")
cat("   -> Sample Chr ID:  ", ss$match_id_chr[1], "\n")


# --- 4. LOAD & CLEAN LD MATRIX ---
cat("\n3. Loading LD Matrix...\n")
if (!file.exists(ld_path)) stop(paste("File not found:", ld_path))

ld <- fread(ld_path, nrows = 500)

# CLEAN NAMES: Handle "#chr" if present, though we mostly need variant1
names(ld) <- tolower(names(ld))
names(ld)[names(ld) == "#chr"] <- "chr" 

cat("   -> LD Columns: ", paste(head(names(ld), 5), collapse=", "), "...\n")

if (!"variant1" %in% names(ld)) {
  cat("[!] WARNING: 'variant1' column not found. Checking first column...\n")
  ld_id_col <- ld[[1]] # Fallback to first column
} else {
  ld_id_col <- ld$variant1
}

cat("   -> Sample LD ID:   ", ld_id_col[1], "\n")


# --- 5. INTERSECTION CHECK ---
cat("\n4. CHECKING INTERSECTIONS\n")

matches_raw <- intersect(ss$match_id_raw, ld_id_col)
matches_chr <- intersect(ss$match_id_chr, ld_id_col)

count_raw <- length(matches_raw)
count_chr <- length(matches_chr)

cat("   ------------------------------------------------\n")
cat("   Matches using RAW format (e.g. 1_123_A_G):   ", count_raw, "\n")
cat("   Matches using CHR format (e.g. chr1_123_A_G):", count_chr, "\n")
cat("   ------------------------------------------------\n")

# --- 6. RECOMMENDATION ---
if (count_chr > count_raw) {
  cat("\n[CONCLUSION] The LD file uses 'chr' prefixes.\n")
  cat("             Action: Use paste0('chr', ...) in your App.\n")
} else if (count_raw > 0) {
  cat("\n[CONCLUSION] The LD file does NOT use 'chr' prefixes.\n")
  cat("             Action: Use raw paste(...) in your App.\n")
} else {
  cat("\n[CONCLUSION] NO MATCHES. \n")
  cat("             Check if the chromosome numbers match (e.g., 23 vs X).\n")
  cat("             Check if positions are from the same genome build (hg19 vs hg38).\n")
}
