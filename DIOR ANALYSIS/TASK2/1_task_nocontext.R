library(ellmer)
library(tidyverse)
library(glue)

anova_results <- "
ANOVA Results:
- F-value: 12.69
- p-value: < 0.001
- Eta-squared (η²): 0.0687 (Effect size)
- Degrees of freedom (df): 1 (between groups), N-2 (within groups)
"
# System prompt identical to the previous one to maintain control over variables
system_prompt <- "You are a Senior Data Analyst. 
Your task is to rigorously interpret statistical outputs and translate them into business recommendations, 
avoiding speculation not supported by the data."

# USER PROMPT WITHOUT CONTEXT AND WITHOUT HINTS ABOUT THE MEANS
user_prompt_no_context <- glue("
We ran an ANOVA test on a Dior customer dataset to analyze the relationship between the purchase channel (Online vs. In-Store) and the importance of staff assistance.

Here are the raw statistical results:
{anova_results}

Based EXCLUSIVELY on the provided statistical results, concisely answer these three points:
1. Statistical inference: Do we reject or fail to reject the null hypothesis?
2. Magnitude: How do you operationally interpret the η² value (0.0687)?
3. Business Insight: What are the implications for Dior regarding resource allocation for assistance between online and physical store customers?
")

# Monte Carlo Simulation Setup 
n_runs <- 100
simulation_nocontext <- vector("list", n_runs)

for (i in 1:n_runs) {
  tryCatch({
    chat <- chat_google_gemini(model = "gemini-3.7-flash", system_prompt = system_prompt)
    response <- chat$chat(user_prompt_no_context)
    
    simulation_nocontext[[i]] <- tibble(
      run = i,
      timestamp = Sys.time(),
      prompt_sent = user_prompt_no_context,
      llm_output = response
    )
    
    cat("Run", i, "completed.\n")
    Sys.sleep(1.5)
    
  }, error = function(e) {
    cat("Error at run", i, ":", conditionMessage(e), "\n")
    simulation_nocontext[[i]] <- tibble(
      run = i,
      timestamp = Sys.time(),
      prompt_sent = user_prompt_no_context,
      llm_output = paste("API ERROR:", conditionMessage(e))
    )
  })
}

df_nocontext <- bind_rows(simulation_nocontext)
write_csv(df_nocontext, "Task2_NoContext_Results.csv")