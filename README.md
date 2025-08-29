# SustainabilityAnalytics_FS25_Team3

## 🌍 Project Goal
This project studies the impact of glacier melt on Swiss water resources...

## 📂 Folder Structure
- **`SustainabilityAnalytics_FS25_Team3.Rproj`**: RStudio project file 
  - Opens the project with the correct working directory set to the repo root  
  - Makes relative paths in R scripts and RMarkdown files work consistently  
  - Integrates with Git for version control inside RStudio  

- **`data/raw`**: Original, unprocessed data  

- **`data/processed`**: Cleaned, merged, and transformed data  

- **`R/`**: Modular R scripts containing functions to reuse across analyses  

- **`analysis/`**: Step-by-step analysis RMarkdown files  

- **`reports/`**: Final report  

- **`reports/figures`**: Figures to include in the final report

## ⚙️ How to Start Working

### 1. Clone the Repository
```bash
git clone https://github.com/ValerieLuethi/SustainabilityAnalytics_FS25_Team3.git
cd SustainabilityAnalytics_FS25_Team3
```
### 2. Set Up the R Environment with `renv`

We use `renv` to ensure all team members have the **same package versions** and that the project is reproducible.

After cloning the repository:
1. Open the R project file `SustainabilityAnalytics_FS25_Team3.Rproj` in RStudio.
2. Restore the project environment by running:
```bash
install.packages("renv") # if not already installed
renv::restore()
```
### 3. Adding new packages

Install the package in RStudio in proj.: 
```bash
install.package("package_name")
```
then update the lockfile to record the new package:
```bash
renv::snapshot()
```
commit the updated files to Git (do not commit renv/activate.R):
```bash
git add renv.lock renv/settings.json
git commit -m "Add package_name to project environment"
git push origin main
```
whenever you pull changes from Git that include renv.lock use:
```bash
renv::restore()
```






