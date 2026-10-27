library(leaps)
library(dplyr)
library(jsonlite)
library(ellmer)

df <- read.csv2("Dataset_World_Life_Expectancy.csv", stringsAsFactors = FALSE)

df_clean <- df %>%
  # Exclude non-numeric identifier columns
  select(-Country.Name, -Country.Code) %>%
  # Apply structural data transformations
  mutate(
    Life.expectancy = log(Life.expectancy),
    GDP.per.capita = log(GDP.per.capita),
    Population.ages = ifelse(Population.ages >= mean(Population.ages, na.rm = TRUE), 1, 0),
    Continent = as.factor(Continent)
  )

# BEST SUBSET SELECTION 
regfit_full <- regsubsets(Life.expectancy ~ ., data = df_clean, nvmax = 15)
reg_summary <- summary(regfit_full)

n <- nrow(df_clean)
p <- 1:length(reg_summary$bic)
rss <- reg_summary$rss

bic <- reg_summary$bic
cp <- reg_summary$cp
adjr2 <- reg_summary$adjr2
aic <- n * log(rss / n) + 2 * p

metrics_df <- data.frame(
  Model_Size = p,
  AIC = round(aic, 2),
  BIC = round(bic, 2),
  Cp = round(cp, 2),
  Adj_R2 = round(adjr2, 4)
)

dynamic_models_summary <- paste(capture.output(print(metrics_df, row.names = FALSE)), collapse = "\n")

# PROMPT DESIGN 
# System Prompt: Theoretical cues (e.g., parsimony principle, explicit BIC vs AIC guidance) are removed
system_prompt <- "You are a student in Data Analytics completing a statistical learning assignment. 
Your task is to evaluate the results of a Best Subset Selection to fulfill the professor's requirements.
You must select the final model size evaluating the penalization differences. 
Return your response in JSON format with the following keys: 
- 'selected_model_size' (integer)
- 'reasoning' (string, explain your choice based strictly on the provided metrics and the professor's instructions)."
# User Prompt: Injects visual description of conflicting criteria (AIC/Cp min at 13 vs BIC/Plateau at 6)
user_prompt <- paste0(
  "Here are the assignment instructions provided by the professor:\n",
  "4. Consider all the available covariates (and potential interactions) to explain your response and assume a linear or a 
  generalized linear regression model. Perform a subset selection to explore all the possible models.\n",
  "5. What is the best overall model obtained according to different criteria (e.g. AIC, BIC, adjusted R2, Mallow's Cp)? 
    Show some plots to provide evidence for your answer.'\n\n",
  "To address these points, a Best Subset Selection was performed to predict the log-transformed Quality of Life (Life.expectancy).\n",
  "The results are summarized in plots, where each criteria is plotted against the number of predictors. ",
  "Both AIC and Mallow’s Cp reached their minimum values with models including 13 predictors. ",
  "In contrast, BIC identified the optimal model at 6 predictors. ",
  "Visually, all the plots show a clear “plateau” starting around the sixth predictor.\n\n",
  "Metrics per number of predictors (Model_Size) are provided below. Note: Cross-validation error is omitted in this output step, 
  so please base your final decision on the metrics below and the visual descriptions provided:\n\n",
  dynamic_models_summary,
  "\n\nAnalyze this data and select the best model size fulfilling the professor's instruction.")

# MONTE CARLO SIMULATION AND ANALYSIS

n_simulations <- 100
ground_truth_size <- 6 
results_list <- vector("list", n_simulations)
for (i in 1:n_simulations) {
  # Configure client with high temperature to induce logical variation
  chat <- chat_google_gemini(
    model = "gemini-3.7-flash", 
    system_prompt = system_prompt,
    api_args = list(
      generationConfig = list(
        temperature = 1.2
      )
    )
  )
  
  iter_result <- tryCatch({
    response <- chat$chat(user_prompt)
    clean_response <- gsub("```json\n|```", "", response)
    llm_output <- fromJSON(clean_response)
    
    data.frame(
      iteration = i,
      selected_model_size = llm_output$selected_model_size,
      size_match = llm_output$selected_model_size == ground_truth_size,
      overfitting_flag = llm_output$selected_model_size > 7,
      reasoning = llm_output$reasoning,
      status = "Success",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      iteration = i,
      selected_model_size = NA,
      size_match = NA,
      overfitting_flag = NA,
      reasoning = NA,
      status = paste("Error:", e$message),
      stringsAsFactors = FALSE
    )
  })
  
  results_list[[i]] <- iter_result
  Sys.sleep(1) 
}

montecarlo_results <- bind_rows(results_list)

summary_stats <- montecarlo_results %>%
  filter(status == "Success") %>%
  summarise(
    total_successful = n(),
    exact_match_rate = mean(size_match, na.rm = TRUE),
    overfitting_rate = mean(overfitting_flag, na.rm = TRUE),
    mean_model_size = mean(selected_model_size, na.rm = TRUE),
    sd_model_size = sd(selected_model_size, na.rm = TRUE)
  )

print(summary_stats)

saveRDS(montecarlo_results, "MonteCarlo_Results_ModelSelection_StressTest.rds")
write.csv2(montecarlo_results, "MonteCarlo_Results_ModelSelection_StressTest.csv", row.names = FALSE)
