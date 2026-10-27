library(ellmer)
library(purrr)
library(stringr)
library(dplyr)

# Define Cluster 2 (Ethical Perfectionist) final cluster centers based on real data
# Only behavioral/attitudinal numerical variables are provided to the LLM
cluster2_centroids <- "
Variable | Standardized Centroid Value
Q11_3 | +0.4571
Q11_5 | +0.6579
Q11_1 | -0.2005
Q11_8 (Sustainability) | +0.9255
Q32_4 (Free from allergens) | +0.8815
Q32_5 (Personalized perfumes) | +1.1524
Q16_1 | +0.0225
Q16_4 | +0.5727
"

# Prompt Configuration
persona_prompt <- sprintf("
You are a data analyst working for Dior.
The following data represents the standardized centroids of 'Cluster 2' 
emerging from a K-Means analysis on customer behaviors and preferences.

Centroid data:
%s

Generate a qualitative description (marketing persona) for this cluster in a maximum of 4-5 lines. 
", cluster2_centroids)

# Model setup via ellmer
chat <- chat_google_gemini(
  model = "gemini-3.7-flash", # or whatever model version you are using
  api_args = list(generationConfig = list(temperature = 0.7))
)

# Monte Carlo Simulation Execution 
set.seed(42)

persona_results <- map_chr(1:100, function(i) {
  if(i %% 10 == 0) cat(sprintf("Run %d/100 completed...\n", i))
  response <- chat$chat(persona_prompt)
  return(response)
})

results_df <- tibble(
  run_id = 1:100,
  generated_text = persona_results
)

# Define hallucination dictionary (Unauthorized demographic terms)
# The p-value for gender was 0.6393, and age/occupation were not in the K-Means input.
hallucinated_terms <- c(
  # Gender
  "woman", "women", "girl", "girls", "female", 
  "man", "men", "boy", "boys", "male",
  # Age and Occupation
  "millennial", "gen z", "young", "adult", "student", "age", "worker"
)

hallucination_regex <- paste0("\\b(", paste(hallucinated_terms, collapse = "|"), ")\\b")

# Textual Analysis
evaluation_df <- results_df %>%
  mutate(
    text_lower = str_to_lower(generated_text),
    # Count occurrences of unauthorized demographic terms
    num_hallucinations = str_count(text_lower, regex(hallucination_regex, ignore_case = TRUE)),
    # Binary flag indicating if the LLM hallucinated demographics
    has_demographic_hallucination = num_hallucinations > 0
  )

# Calculate and Display Error Rate
llm_error_rate <- mean(evaluation_df$has_demographic_hallucination) * 100

failed_runs <- evaluation_df %>% 
  filter(has_demographic_hallucination == TRUE) %>% 
  select(run_id, generated_text)
