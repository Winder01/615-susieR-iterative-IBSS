# GenAI Tutorial: Building the SuSiE Iterative Visualizer
**Project:** Interactive Visualization of SuSiE Fine-Mapping

**Author:** Yuhui Yan

**Tools:** RShiny, Plotly, Google Gemini 3 (LLM)

## Introduction
This tutorial documents the development process of an RShiny application designed to visualize the Iterative Bayesian Stepwise Selection (IBSS) process of the SuSiE algorithm.

Using Generative AI (Google Gemini CLI & Gemini 3), I transitioned from a basic structure to a performance-optimized tool capable of handling large-scale genomic data (FinnGen). The development process highlights how GenAI handles boilerplate code, diagnoses statistical errors, and optimizes algorithms.


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

## Phase 3: The Challenge - Integrating Real-World Data (FinnGen)
**The Problem:** When I attempted to load real Summary Statistics from FinnGen, the app crashed or showed "No Overlap". 

The Diagnosis: The ID formats were mismatched (rsID vs chr_pos).

Step 3.1: AI-Assisted Diagnosis
Instead of guessing, I asked the AI to write a diagnostic script to "look" at the data structure.



## Possible Trouble shooting
### Non-numeric argument to binary operator
**Reason:** Possibly happens because credible sets cannot be calculated iteractively through “trace” in susieR. Specifically, susie_get_cs() attempts to calculate Credible Sets on the fly. If the trace object is missing components (like the sample size n or the data X needed for purity checks), the internal math fails with that "non-numeric" error. 

**Solution:** Add "Safety Wrapper" to handle the trace objects, manually assin the class and calculating PIPs directly if the helper fails.



