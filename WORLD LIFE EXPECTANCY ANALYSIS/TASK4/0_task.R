library(ellmer)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)

llm_generator <- chat_google_gemini(
  model = "gemini-3.7-flash", 
  system_prompt = "You are an expert data scientist in statistics. Answer questions concisely and accurately.",
  api_args = list(generationConfig = list(temperature = 0.7))
)
summary_text <- "
Call:
lm(formula = log_Life.expectancy ~ Population.ages.15.64 * Unemployment + 
    Poverty + GDP.per.capita + Access.to.electricity + Incidence.of.tuberculosis, 
    data = data)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.192599 -0.030031  0.008529  0.032530  0.124610 

Coefficients:
                                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)                         4.1342040  0.0267261 154.688  < 2e-16 ***
Population.ages.15.64               0.0026728  0.0157700   0.169 0.865620    
Unemployment                       -0.0038974  0.0010966  -3.554 0.000494 ***
Poverty                            -0.0012778  0.0003337  -3.829 0.000182 ***
GDP.per.capita                      0.0384436  0.0053107   7.239 1.60e-11 ***
Access.to.electricity               0.0011018  0.0002553   4.316 2.72e-05 ***
Incidence.of.tuberculosis          -0.0001299  0.0000329  -3.949 0.000116 ***
Population.ages.15.64:Unemployment  0.0019088  0.0014678   1.300 0.195255    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.05025 on 166 degrees of freedom
Multiple R-squared:  0.8036,	Adjusted R-squared:  0.7954 
F-statistic: 97.05 on 7 and 166 DF,  p-value: < 2.2e-16
"

# PROMPT DESIGN
prompt_task4 <- sprintf("
Here is the multiple linear regression output of my final model (best_model):
%s
Based EXCLUSIVELY on this output, answer the following requests from the professor:
1. How good does the model fit the data? Find and comment suitable measures of goodness of fit.
2. Accurately interpret the numerical impact of the GDP.per.capita variable coefficient on life expectancy, 
knowing that GDP.per.capita in the dataset is already on a logarithmic scale.
", summary_text)

n_simulations <- 100

results_task4 <- map_dfr(1:n_simulations, function(i) {
  
  response <- llm_generator$chat(prompt_task4)
  
  tibble(
    sim_id = i,
    llm_response = response
  )
}, .progress = TRUE)

# JUDGE
llm_judge <- chat_google_gemini(
  model = "gemini-3.7-flash",
  api_args = list(generationConfig = list(temperature = 0.0)) 
)
score_interpretation <- function(response_text) {
  judge_prompt <- sprintf("
  You are an automatic scorer. Evaluate the following response from a statistics student.
  Response to evaluate:
  \"%s\"
  Verify the following Ground Truth Criteria:
  1. R-squared: Did they correctly mention that R2 is approximately 0.8036 (or 80.36%%)?
  2. Log-Log Math: Did they interpret the GDP.per.capita coefficient correctly by stating that a 1%% increase in GDP.per.capita 
  corresponds to an approximate 0.0384%% (or 0.038%%) increase in life expectancy? (WARNING FOR ERRORS: if they say 3.84%%, 0.38%%, 
  or talk about a '1 unit' increase instead of a '1%%' increase, it is a fatal error).
  Answer EXCLUSIVELY in JSON format with the following structure, without anything else:
  {
    \"r2_correct\": true/false,
    \"log_log_correct\": true/false,
    \"error_reasoning\": \"brief explanation of the mathematical error if present, otherwise 'No error'\"
  }
  ", response_text)
  eval_raw <- llm_judge$chat(judge_prompt)
  clean_json <- str_remove_all(eval_raw, "```json|```") %>% trimws()
  
  tryCatch({
    jsonlite::fromJSON(clean_json)
  }, error = function(e) {
    list(r2_correct = NA, log_log_correct = NA, error_reasoning = "Error parsing JSON")
  })
}

scored_results <- results_task4 %>%
  mutate(
    evaluation = map(llm_response, score_interpretation)
  ) %>%
  unnest_wider(evaluation)

summary_metrics <- scored_results %>%
  summarise(
    accuracy_r2 = mean(r2_correct, na.rm = TRUE),
    accuracy_log_log = mean(log_log_correct, na.rm = TRUE),
    total_perfect_responses = sum(r2_correct == TRUE & log_log_correct == TRUE, na.rm = TRUE)
  )

print("Monte Carlo Results (100 runs):")
print(summary_metrics)

error_analysis <- scored_results %>%
  filter(log_log_correct == FALSE) %>%
  count(error_reasoning, sort = TRUE)

print("Common errors in log-log interpretation:")
print(error_analysis)

saveRDS(scored_results, "MonteCarlo_Results_Task4.rds")
write.csv2(scored_results, "MonteCarlo_Results_Task4.csv", row.names = FALSE)

saveRDS(summary_metrics, "Summary_Metrics_Task4.rds")
write.csv2(summary_metrics, "Summary_Metrics_Task4.csv", row.names = FALSE)

saveRDS(error_analysis, "Summary_Error_Task4.rds")
write.csv2(error_analysis, "Summary_Error_Task4.csv", row.names = FALSE)
