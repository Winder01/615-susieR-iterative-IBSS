# SuSiE Iterative Visualization Project

## Project Description
A Shiny application to visualize the iterative process of the SuSiE fine-mapping algorithm.
(BIOSTATS 615 course project: Interactive Visualization and Iterative Exploration of susieR package.)

## 🔗 Links
* **Live Web Application**: [Click here to open App](https://cyyyyy.shinyapps.io/publish/)
* **GenAI Tutorial**: [Read my GenAI Experience here](https://https://winder01.github.io./615-susieR-iterative-IBSS/GenAI_Tutorial/GenAI_Tutorial.html)

## 📂 Repository Structure

This repository contains the complete R Shiny application and all associated development documentation, logs, and utility scripts.

### Core Application Files
* **`app.R`**: The main R Shiny application file. Note that this file requires `simulation_v3.R` to be in the root directory to handle simulation scenarios.
* **`simulation_v3.R`**: Contains the four specific genetic simulation functions used to test algorithm performance and visualization.

### Data and Utilities
* **`data/`**: Directory containing example datasets, including FinnGen summary statistics and LD matrices.
    * **FinnGen Data Usage Example:**
        1.  Open the App. Under "Data Source", select "Upload FinnGen".
        2.  **Upload SumStats:** Use the example file: `data/FinnGen Data Sample/sumstats/E4_LIPOPROT.1-54839974-55239974.tsv.gz`.
        3.  **Upload LD Matrix:** Use the example file: `data/FinnGen Data Sample/ld/finngen_r12_ld.1-54839974-55239974.tsv.gz`.
        4.  (Optional) Adjust parameters (e.g., Lambda, Top N Filter).
        5.  Click **Run Analysis**.
* **`scripts/`**: Directory for utility scripts used during data processing.
    * **`create_example_datasets_v3.R`**: Used to generate data for input testing.
    * **`MatchID.R`**: A diagnostic script used to check and debug format and matching issues between the LD matrix and summary statistics files.

### Development and Documentation (GenAI Tutorial)
* **`GenAI_Tutorial/`**: Directory containing the documentation of the GenAI-assisted development process.
    * **`GenAI_Tutorial.md`**: The main tutorial file. Follow the steps within this file to replicate the application development, including debugging tips and optimization strategies.
    * **`GenAI_Tutorial_log.md`**: The detailed log file used to capture interactions and decisions made during the tutorial app generation.
    * **`GenAI_Tutorial/figure and GIF`**: Contains all figures, screenshots, and animated GIFs referenced in the main tutorial document.
    * **`GenAI_Tutorial/Tutorial versions`**: Contains archived versions of the application code corresponding to different development stages.
