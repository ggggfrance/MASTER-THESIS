library(ellmer)
library(tidyverse)
library(glue)
library(knitr)

dataset_dior <- read_csv("Dior_dataset_cleaned.csv")
data_preview <- head(dataset_dior, 5)
preview_md <- kable(data_preview, format = "markdown")

# Define the ANOVA results 
anova_results <- "
ANOVA Results:
- F-value: 12.69
- p-value: < 0.001
- Eta-squared (η²): 0.0687 (Effect size)
- Degrees of freedom (df): 1 (between groups), N-2 (within groups)
"

# System prompt 
system_prompt <- "You are a Senior Data Analyst. 
Your task is to rigorously interpret statistical outputs and translate them into business recommendations, 
avoiding speculation not supported by the data."

# User prompt
user_prompt <- glue("
Context: We are analyzing the relationship between the purchase channel (Online vs. In-Store) and the importance of staff assistance for Dior customers. 
Customers who purchase in-store have a higher sample mean regarding the importance of assistance.

Here is a preview (first 5 rows) of the dataset used:
{preview_md}

We ran an ANOVA test with the following raw results:
{anova_results}

Based EXCLUSIVELY on the provided statistical results, concisely answer these three points:
1. Statistical inference: Do we reject or fail to reject the null hypothesis?
2. Magnitude: How do you operationally interpret the η² value (0.0687)?
3. Business Insight: What are the implications for Dior regarding resource allocation for assistance between online and physical store customers?
")

# Monte Carlo Simulation Setup
n_runs <- 100
simulation_results <- vector("list", n_runs)

for (i in 1:n_runs) {
  
  tryCatch({
    chat <- chat_google_gemini(model = "gemini-3.7-flash", system_prompt = system_prompt)
    
    response <- chat$chat(user_prompt)
    
    simulation_results[[i]] <- tibble(
      run = i,
      timestamp = Sys.time(),
      prompt_sent = user_prompt,
      llm_output = response
    )
    
    cat("Run", i, "completed successfully.\n")
    
    Sys.sleep(1.5)
    
  }, error = function(e) {
    cat("Error at run", i, ":", conditionMessage(e), "\n")
    simulation_results[[i]] <- tibble(
      run = i,
      timestamp = Sys.time(),
      prompt_sent = user_prompt,
      llm_output = paste("API ERROR:", conditionMessage(e))
    )
  })
}

df_results <- bind_rows(simulation_results)

write_csv(df_results, "Task2_MonteCarlo_ANOVA_Results.csv")