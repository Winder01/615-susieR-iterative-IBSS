# GenAI Tutorial: Building the SuSiE Iterative Visualizer

**Project:** Interactive Visualization of SuSiE Fine-Mapping

**Author:** Catherine Yan

**Tools:** RShiny, Plotly, Google Gemini 3 & Gemini CLI

---

## 💻 Quick Navigation

* [Phase 1: App Structure establishment (UI & Iterative algorithm)](#phase-1-app-structure-establishment-ui--iterative-algorithm)
* [Phase 2: Simulating Genetic data](#phase-2-simulating-genetic-data)
* [Phase 3: The Challenge - Integrating Real-World Data (FinnGen)](#phase-3-the-challenge---integrating-real-world-data-finngen)
* [Phase 4: Performance Optimization](#phase-4-performance-optimization)
* [Phase 5: Final Polish & UI Refinement](#phase-5-final-polish--ui-refinement)
* [Conclusion & Best Practices](#conclusion--best-practices)
* [Possible Trouble shooting](#possible-trouble-shooting)

---


## Introduction
This tutorial documents the development process of an RShiny application designed to visualize the Iterative Bayesian Stepwise Selection (IBSS) process of the SuSiE algorithm.

Using Generative AI (Google Gemini CLI & Gemini 3), I transitioned from a basic structure to a performance-optimized tool capable of handling large-scale genomic data (FinnGen). The development process highlights how GenAI handles boilerplate code, diagnoses statistical errors, and optimizes algorithms.

---

## Phase 1: App Structure establishment (UI & Iterative algorithm)
**Goal:** Establish the app framework with basic UI, parameters, and plots setups. 

Instead of writing the UI skeleton manually, I prompted the AI to generate a sidebarLayout with a specific focus on the "Iteration Slider".

### Key Prompt (Step 1)
> **Goal:** I need to rebuild my SuSiE RShiny App. Please write the `app.R` code to set up the **Framework** and **Iterative Logic**.
>
> **1. Libraries:** `shiny`, `plotly`, `DT`, `susieR`, `data.table`, `matrixStats`, `reshape2`.
>
> **2. UI Layout (Follow strictly):** Use `fluidPage` with `sidebarLayout`.
> * **Sidebar (Width 3):**
>     * **Data Config:** Header "1. Data Configuration", Radio buttons (Summary/Individual), SelectInput (Simulated Data), SelectInput (Scenario).
>     * **Execution:** Header "2. Execution", ActionButton "Run Analysis" (primary class).
>     * **Viz Control:** A gray `div` containing `sliderInput` ("iteration_step", animate=TRUE) and "Next Step" button.
> * **MainPanel (Width 9):**
>     * **Top Config Bar:** A gray `div` (`#e9ecef`) with input fields: `L` (10), `Max Iter` (100), `Lambda` (0.1), `Coverage` (0.95), `Prior Var` (0.1).
>     * **Results:** `tabsetPanel` with tabs: "PIP Plot", "ELBO Convergence", "Credible Sets", "Data Preview".
>
> **3. Server Logic:**
> * **Reactive Values:** Store `susie_fit`, `trace`, and `total_steps`.
> * **Run Analysis:** Generate dummy data (N=100, P=50), run `susie(..., track_fit=TRUE)`, and update the slider range.
>
> **4. Visualization (Crucial):**
> * **PIP Plot:** Use `plotly`. Must be a **Scatter Plot** (`mode='markers'`), not a bar plot.
> * **Style:** X-axis = Variable Index, Y-axis = PIP. Points colored gray by default, colored differently if in a Credible Set. Size scales with PIP.
> * **Dynamics:** Plot data from the *specific iteration* selected by the slider.

**Outcome:** I established the app's core framework by implementing a clean `sidebarLayout` and enabling SuSiE's `track_fit = TRUE` logic to capture the model's iteration history. This allows users to "replay" the algorithm's convergence step-by-step via an animated slider that updates the PIP scatter plot in real-time.

![step1](https://github.com/user-attachments/assets/0b8e91d3-d3af-469d-8d13-d2c187499cef)
**Figure 1:** The AI-generated framework successfully animating the IBSS iterations.

---

## Phase 2: Simulating Genetic data
**Goal:** Verify the visualization using toy example data. 

I needed specific genetic scenarios (e.g., Perfect Collinearity) to test if the visualization colors correctly identified Credible Sets.

### Key Prompt (Step 2)
> The UI framework is good, but I want to clean it up and ensure the plot colors work.
>
> **1. Create Simulation Script (`simulation_v3.R`):**
> Write a separate file with these 4 functions (returning `X`, `y`, `true_coef`, `p`, `causal_indices`):
> * `simulate_perfect_collinearity`: $N=500, P=1000$. Two causal variants. Copy col 200 to 400, 800 to 600.
> * `simulate_moderate_ld`: LD Blocks size 10, $r=0.6$.
> * `simulate_multiple_ld_blocks`: Blocks with $r=0.99, 0.6, 0.3$.
> * `simulate_tricky_scenario`: One block (size 50, $r=0.95$) with 3 weak signals.
>
> **2. Update `app.R` UI (Clean Look):**
> * Add `source("simulation_v3.R")` at the top.
> * **Remove Gray Backgrounds:** Remove the `style = "background: ..."` arguments from the "Visualization Control" and "Analysis Parameters" divs to match a clean white design.
>
> **3. Fix Visualization Colors (Force Colors):**
> * **Logic:** Create a `cs_label` column. If not in a CS, set to "No CS". Convert to **Factor** with "No CS" as the first level.
> * **Plotly Call:** Define a color vector: `c("lightgray", "red", "blue", "green", "orange", "purple", "cyan", "magenta")`.
> * **Mapping:** Inside `plot_ly`, set `color = ~cs_label` and `colors = my_colors`.
> * **Markers:** Scale size with PIP: `marker = list(size = ~pip * 15 + 6, opacity = 0.8)`.

**Outcome:**
I implemented four specific genetic simulation scenarios to rigorously test the model's behavior under different linkage disequilibrium patterns. Simultaneously, I refined the visualization logic to enforce a strict color mapping scheme, ensuring that Credible Sets are visually distinct from background noise.

![step2](https://github.com/user-attachments/assets/b00d5bf0-d31f-426b-9166-0c1231a0b023)

**Figure 2.** Visualizing simulated data in 'multiple ld blocks' cases

---

## Phase 3: The Challenge - Integrating Real-World Data (FinnGen)
**Problem:** When I attempted to load real Summary Statistics from FinnGen, the app crashed or showed "No Overlap". 

**Diagnosis:** The ID formats were mismatched (rsID vs chr_pos).

### Step 3.1: AI-Assisted Diagnosis
Instead of guessing, I asked the AI to write a diagnostic script to "look" at the data structure.


#### Key Prompt (Step 3.1)
> I want to integrate **FinnGen** data, but the Variant IDs in the Summary Statistics file do not match the format used in the LD Matrix file.
>
> Please write a standalone R script named **`debug_finngen_ids.R`** to help me diagnose this. The script should:
>
> **1. Load Libraries:** Use `data.table`.
> **2. Read Files:** Read a Summary Statistics file (placeholder `sumstats.gz`) and an LD Matrix file (`ld.gz`).
> **3. Inspect SumStats:**
> * Print column names.
> * Construct a "Trial ID" using columns `chrom`, `pos`, `ref`, `alt` (paste with underscores).
> * Print the first 5 "Trial IDs".
> **4. Inspect LD Matrix:**
> * Print the first 5 values from the `variant1` column.
> **5. Check Overlap:**
> * Calculate the intersection between SumStats IDs and LD Matrix IDs.
> * Print the number of matching variants.
>
> This script will help me see if there is a prefix mismatch (e.g., "chr1" vs "1") so I can tell you how to fix it in the main App.

**Outcome:**
Running this script confirmed that the Summary Statistics lacked the "chr" prefix required to match the IDs in the FinnGen LD matrix, pinpointing the exact cause of the data integration failure.

<img width="730" height="315" alt="step3 1" src="https://github.com/user-attachments/assets/eb4e28c8-d344-408e-a86e-deba8d6a8846" />

**Figure 3.** Suggestion on aligning LD matrix and summary stats made by GenAI


### Step 3.2: The Fix & Robust Regularization

After confirming the ID mismatch, I encountered a second issue: susie_rss failed to converge due to the "Out-of-sample LD" problem (mismatch between reference panel and study sample). 

I prompted the AI to implement a robust pipeline:

1) ID Repair: Automatically prepend "chr" to match the LD matrix.
2) Regularization: Add a Lambda parameter to modify the LD matrix diagonal ($R = (1-\lambda)R + \lambda I$).

Note: As there was some memory loss, especially on UI settings, during conversation, I firstly ask the GenAI to rewrite on the previous requirements.

#### Key Prompt (Step 3.2)
> The previous update caused a regression in the UI and a critical error in the analysis. I need you to rewrite the code to strictly follow these requirements:
>
> **1. UI Restoration (Top Priority):**
> * Use the `sidebarLayout` structure.
> * **Main Panel:** Restore the "Configuration Bar" (gray background) with parameters: `L`, `Max Iter`, `Lambda`, `Coverage`, `Prior Var`.
> * **Crucial Addition:** Add a `conditionalPanel` for FinnGen inputs: `Sample Size (N)` (default 520210) and `Top N Filter`.
>
> **2. Robust Server Logic (FinnGen):**
> * **Fix IDs:** The LD file uses `chr1_...` format. You **MUST** construct the SumStats ID as: `match_id = paste0("chr", chrom, "_", pos, "_", ref, "_", alt)`.
> * **Strict Intersection:** Filter both datasets to keep only overlapping IDs.
> * **Run SuSiE:**
>     * **CRITICAL FIX:** Pass `n = input$finngen_n` to `susie_rss`.
>     * **Regularization:** Apply `R = (1-lambda)*R + lambda*I`.
>     * Use `estimate_residual_variance = FALSE` and `check_prior = FALSE`.

**Outcome:**
I successfully restored the custom UI layout and integrated the robust ID matching logic derived from the diagnostic phase. Crucially, I resolved the convergence errors by explicitly passing the sample size ($N$) to the model and applying matrix regularization to handle the mismatched LD reference panel.

---

## Phase 4: Performance Optimization 
**The Problem:** The app was extremely slow when processing large genomic regions (5000+ SNPs). 

**The Solution:** I realized the code was building a massive matrix before filtering. I instructed the AI to invert the logic: Filter First, Build Matrix Second. I Also asked AI to employ `Rfast` as suggested by `SuSiE` tutorial. 

### Key Prompt (Step 4)
> The app runs quite slow for FinnGen datasets.
>
> **1. Optimize Algorithm (Speed Up):**
> * **Early Filtering (Crucial):**
>     1. Intersect IDs and Calculate Z-scores.
>     2. **IMMEDIATELY** sort SumStats by `abs(Z)` and keep only the top `input$top_n` rows. Discard the rest.
>     3. **Subset LD:** Only *after* filtering SumStats, subset the LD dataframe to match these top N variants.
>     4. **Dcast:** Run `dcast` on the small subset to prevent memory issues.
>
> **2. Enforce Rfast:**
> * Add `library(Rfast)` at the top to speed up matrix operations.
>
> **3. Robustness:**
> * Ensure `susie_rss` is called with `estimate_residual_variance = FALSE`, `check_prior = FALSE`, and `n = input$finngen_n`.

**Outcome:**
I resolved the column naming crash by implementing explicit name mapping instead of relying on fragile index positions. Furthermore, I re-architected the data processing flow to "Filter First," which drastically reduced memory usage and processing time by creating the correlation matrix only for the top N significant variants rather than the entire dataset.

![step4](https://github.com/user-attachments/assets/e9cf8edc-89b6-4e82-a583-787ec5837b32)

**Figure 4.** Matched & Speeded output for E4_LIPOPROT.1-54839974-55239974 in FinnGen. 

---

## Phase 5: Final Polish & UI Refinement
**Goal:** Align the user interface with professional standards.

I asked the AI to reorganize the layout, moving parameters to a top configuration bar and refining the plot axes to show physical chromosomal positions.

### Key Prompt (Step 5)
> The app is working well, but I need to polish the details to match my final design specifications.
>
> **1. Update Default Parameters:**
> * Change default **Lambda** to **0.1** (recommended baseline for FinnGen).
>
> **2. Improve PIP Plot (X-Axis):**
> * **Logic:** If position information exists, the X-axis MUST show **Chromosomal Position (Mb)** (scaled by `1e6`). Fall back to "Variable Index" only if missing.
> * Update label to "Chromosome Position (Mb)".
>
> **3. Format Credible Sets Table:**
> * Format output as a clean `DT::datatable`.
> * **Columns:** `CS`, `Variant`, `Pos`, `PIP` (4 decimals), `Beta`, `P-val`, `Z`.
> * **Logic:** Iterate through `fit$sets$cs`, extract rows, and combine into a single clean data frame.

**Outcome:**
I applied the final polish to the application by setting scientifically appropriate default parameters and converting abstract variable indices into physical chromosomal coordinates (Mb) for the plots. Additionally, I transformed the raw statistical output into a formatted, interactive table, making the fine-mapping results immediately interpretable for researchers.

<p align="center">
  <img src="https://github.com/user-attachments/assets/14105d6f-8dfa-4b72-a0c1-7fe8d3d3daaf" 
       width="950" 
       style="height: auto; max-width: 100%;"
       alt="Final SuSiE IBSS result: PIP plot - High Resolution">
  <br>
  <em>Figure 5. Final SuSiE IBSS result: PIP plot</em>
</p>

<p align="center">
  <img src="https://github.com/user-attachments/assets/8fb274f2-29c9-4341-b4b4-3f5b8310b0bc" 
       width="950" 
       style="height: auto; max-width: 100%;"
       alt="Final SuSiE IBSS result: Credible Sets Table - High Resolution">
  <br>
  <em>Figure 6. Final SuSiE IBSS result: Credible Sets Table</em>
</p>

<p align="center">
  <img src="https://github.com/user-attachments/assets/7f8dd15c-f9da-4c44-9202-9b9cc54b1a31" 
       width="700" 
       style="height: auto; max-width: 100%;"
       alt="Final SuSiE IBSS result: Data Preview and Analysis Log - High Resolution">
  <br>
  <em>Figure 7. Final SuSiE IBSS result: Data Preview and Log</em>
</p>

---

## Conclusion & Best Practices
Through this project, I learned that GenAI is not the Creator but a powerful accelerator that requires human guidance. It initially failed to handle the FinnGen ID mismatch and large data adaption, but once guided by clear diagnostic steps, it implemented complex solutions instantly.

Based on my development experience, here are the key takeaways for successfully using GenAI in bioinformatics coding:

* **Precision Prompting is Key:**
    * Use **bullet points** to break down requirements are helpful.
    * Explicitly separate **UI instructions** (layout order, styling, specific widgets) from **Server logic**.
    * Ambiguous prompts lead to regressions; be specific about *what* changes and *where*.

* **Strategic Debugging:**
    * Do not simply paste error logs. Hypothesize the source (e.g., algorithmic bottleneck vs. memory limit) in your prompt.
    * **"Clean Context" Strategy:** If a bug is complex, open a fresh AI chat session to brainstorm solutions. This prevents the main development thread from becoming confused by failed attempts. (NOTE: One could always try different AI! They may give different perspectives)

* **The "Human-in-the-Loop" Necessity:**
    * If the AI gets stuck in an error loop (especially with data formatting), stop generation.
    * Manually inspect **intermediate values** (e.g., `print(head(data))`).
    * *Example:* In this project, the AI could not "guess" the FinnGen ID mismatch (`chr1_` vs `1_`). I had to manually diagnose the data structure first, then instruct the AI on the specific string manipulation required.

---

## Possible Trouble shooting
During the development with GenAI, you might encounter specific errors. Here is a log of common issues, their root causes, and example prompts used to fix them.

### 1. Error: "Non-numeric argument to binary operator"
**Symptom:** The app crashes when trying to visualize the Iteration History.
**Reason:** The helper function `susie_get_cs()` attempts to calculate Credible Sets dynamically but often fails on historical trace objects because they lack full purity statistics or sample size data required for the calculation.
**Solution:** Do not rely on the helper function for historical steps. Manually extract the `alpha` matrix and calculate PIPs directly.

#### Example Prompt
> "The app crashes when sliding the iteration bar with 'non-numeric argument'.
> **Fix:** In the `renderPlotly` section, add a **Safety Wrapper**.
> * Check if `fit$sets$cs` exists.
> * If it fails, manually calculate PIP = `1 - apply(1 - fit$alpha, 2, prod)`.
> * Do not call `susie_get_cs()` on the trace object."

### 2. Plot Colors Not Displaying / Random Colors
**Symptom:** Points in the Credible Sets are not colored correctly (e.g., all gray), or the legend shows weird variable names.
**Reason:** Plotly's default mapping (`color = ~variable`) often fails in RShiny when the grouping factor (Credible Sets) changes dynamically every iteration. The AI struggles to map factor levels consistently in reactive contexts.
**Solution:** Bypass Plotly's automatic mapping. Force the AI to write a "Direct Color Assignment" loop using a fixed palette.

#### Example Prompt
> "The plot colors are inconsistent. Please use **Direct Color Mapping**:
> 1. Create a vector of hex codes: `colors_pal <- c('#1f77b4', '#ff7f0e', ...)`
> 2. Create a default color vector `cols <- rep('lightgray', length(pip))`
> 3. Loop through `final_fit$sets$cs`:
>    * `idx <- final_fit$sets$cs[[i]]`
>    * `cols[idx] <- colors_pal[i]`
> 4. **Crucial:** In `plot_ly`, use `marker = list(color = cols)` directly. Do NOT use `color = ~variable`."

### 3. Error: "object 'v1' not found" (during dcast)
**Symptom:** The FinnGen upload fails specifically when building the matrix, with an error about missing columns.
**Reason:** This is a "Fragile Code" issue. The AI often tries to rename columns by numeric index (e.g., `names(df)[2] <- "v2"`). If the input file format don't have the same column names, the renaming targets the wrong column, and the subsequent `dcast` function fails.
**Solution:** Force the AI to use **Explicit Name Matching**.

#### Example Prompt
> "I am getting `object 'v1' not found`.
> **Fix:** Do NOT rename columns by index (e.g., `names(ld)[2]`).
> Instead, identify the columns explicitly:
> * Find the column named 'variant1' or 'v1'.
> * Find the column named 'variant2' or 'v2'.
> * Use `data.table::setnames` to rename them explicitly to 'v1', 'v2', and 'r' **before** running `dcast`."

### 4. Warning: "IBSS algorithm did not converge" (Result is NA/Inf)
**Symptom:** The analysis runs but results in empty plots or errors saying "Infinite value".
**Reason:** In `susie_rss` (Summary Statistics mode), the model **must** know the Sample Size ($N$) to infer the residual variance correctly. Without it, or if the LD matrix mismatches the population (Out-of-sample LD), the mathematical optimization breaks.
**Solution:** Explicitly pass $N$ and apply Matrix Regularization ($\lambda$) to the LD matrix.

#### Example Prompt
> "The model fails to converge.
> **1. Critical Fix:** You MUST pass `n = input$finngen_n` to the `susie_rss` function.
> **2. Robustness:** Add Regularization to the matrix: `R = (1-lambda)*R + lambda*I`.
> **3. Settings:** Set `estimate_residual_variance = FALSE` to prevent optimization crashes."

### 5. "No Overlap" / Zero Variants Found
**Symptom:** You upload valid FinnGen files, but the app says "0 variants matched".
**Reason:** **ID Format Mismatch**. One file uses `chr1_12345_A_G` and the other uses `1_12345_A_G` (missing the 'chr' prefix).
**Solution:** Refer to Step 3 for more details. Ask the AI to write a **Diagnostic Script** first to inspect the strings side-by-side.

#### Example Prompt
> "The IDs are not matching.
> Please write a standalone script `debug_ids.R`:
> 1. Load the first 100 rows of both files.
> 2. Print the `variant_id` column from the LD file.
> 3. Construct the ID from the SumStats file (`chrom_pos_ref_alt`).
> 4. Print them side-by-side so I can check for prefix mismatches (e.g., 'chr')."



