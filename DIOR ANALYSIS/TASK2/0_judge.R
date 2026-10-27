library(ellmer)
library(tidyverse)
library(glue)
library(jsonlite) 

# Load the results from the previous simulation
df_results <- read_csv("Task2_MonteCarlo_ANOVA_Results.csv")

# Define the Judge Prompt 
judge_system_prompt <- "
You are an analytical evaluator. Your task is to evaluate the accuracy of a response provided by another LLM against a predefined Ground Truth.
You must reply EXCLUSIVELY with a valid JSON object, without any additional text or markdown formatting (no backticks ```json ... ```).

The JSON must have exactly this structure:
{
  \"q1_reject_h0\": true/false, 
  \"q2_magnitude_correct\": true/false,
  \"q3_business_insight_correct\": true/false,
  \"q3_inversion_error\": true/false
}
"

# Setup the evaluation loop
n_rows <- nrow(df_results)
evaluations_list <- vector("list", n_rows)

for (i in 1:n_rows) {
  
  llm_response <- df_results$llm_output[i]
  
  # User Prompt for the judge containing the Ground Truth
  judge_user_prompt <- glue("
  Evaluate the following RESPONSE based on this GROUND TRUTH:
  1. Inference: It must explicitly state to reject the null hypothesis.
  2. Magnitude: It must interpret the effect (η² = 0.0687) as small-to-medium (or at least not as 'large' or 'null').
  3. Business Insight: It must suggest allocating more resources/priority to IN-STORE assistance compared to online.
  4. Inversion Error: Flag as 'true' if the response incorrectly advises increasing assistance for ONLINE.

  RESPONSE TO EVALUATE:
  \"{llm_response}\"
  ")
  
  tryCatch({
    judge_chat <- chat_google_gemini(model = "gemini-3.7-flash", system_prompt = judge_system_prompt)
    
    # Execute the API call
    raw_response <- judge_chat$chat(judge_user_prompt)
    
    # Clean up any markdown backticks if the LLM inserts them despite instructions
    clean_json <- str_remove_all(raw_response, "^```json\\n|^```\\n|```$")
    
    # Parse the JSON into a data.frame (1 row)
    parsed_json <- fromJSON(clean_json) %>% as_tibble()
    
    # Add the original run ID for the subsequent join
    parsed_json$run <- df_results$run[i]
    
    evaluations_list[[i]] <- parsed_json
    
    Sys.sleep(1.5)
    
  }, error = function(e) {
    evaluations_list[[i]] <- tibble(
      q1_reject_h0 = NA,
      q2_magnitude_correct = NA,
      q3_business_insight_correct = NA,
      q3_inversion_error = NA,
      run = df_results$run[i]
    )
  })
}

df_evaluations <- bind_rows(evaluations_list)
df_final <- df_results %>%
  left_join(df_evaluations, by = "run")

final_metrics <- df_final %>%
  summarise(
    inference_accuracy = mean(q1_reject_h0, na.rm = TRUE),
    magnitude_accuracy = mean(q2_magnitude_correct, na.rm = TRUE),
    business_accuracy = mean(q3_business_insight_correct, na.rm = TRUE),
    inversion_error_rate = mean(q3_inversion_error, na.rm = TRUE),
    successfully_evaluated_responses = sum(!is.na(q1_reject_h0))
  )

write_csv(df_final, "Task2_LLM_Judge_Evaluated.csv")
print("Aggregate Metrics:")
print(final_metrics)
