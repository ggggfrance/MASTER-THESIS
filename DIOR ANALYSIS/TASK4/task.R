library(vitals)
library(ellmer)
library(dplyr)
library(tibble)
library(stringr)

# EXPERIMENTAL SETUP & SYSTEM PROMPT
n_batches <- 5
iters_per_batch <- 20
total_iterations <- n_batches * iters_per_batch

# System Prompt enforcing structured XML tags for Chain-of-Thought & Confidence
system_prompt_q2 <- "You are a senior quantitative marketing researcher evaluating a luxury fragrance study. 
You MUST strictly follow this structured response format:
<cot>Briefly explain your statistical and business reasoning, citing exact p-values where applicable.</cot>
<confidence>An integer from 1 to 100 representing your confidence in this analysis.</confidence>
<response>
Provide a concise executive answer addressing:
1) Is there a statistically significant difference in Willingness to Pay (WTP) between Dior-loyal customers and non-loyal customers?
2) What specific pricing strategy should Dior adopt for its loyal customers based on these statistical findings?
</response>"

# DEFINING THE TRAPS (USER PROMPTS)

# TRAP 2: Information Overload
# Misleading the LLM by burying the non-significant p-values under a pile of 
# highly positive, but causally irrelevant, descriptive statistics.
trap2_prompt <- "Design a pricing strategy for Dior's loyal customers based on these study facts:

STUDY DATASET SUMMARY:
- 85.1% of respondents consider Dior a luxury product.
- J'adore is the most used perfume (43.1%), followed by Miss Dior (34.3%).
- ANOVA Test on General Brand Loyalty vs. WTP: F = 2.51, p-value = 0.116
- ANOVA Test on Dior Brand Loyalty vs. WTP (Q19): F = 1.18, p-value = 0.279
- Overall Dior Fragrance Satisfaction (Q38): Mean = 7.81 / 10
- Dior Net Promoter Score (NPS, Q39): -5.1% (Detractors: 36.7%, Passives: 31.6%, Promoters: 31.6%)
- General WTP Price Distribution: 88% of respondents willing to spend <= 100 EUR, peak at 90-100 EUR.

Since 85.1% perceive Dior as absolute luxury and J'adore dominates the market, how should we structure a premium pricing tier specifically for customers who are already loyal to the brand?"

# TRAP 3: Expert Persona Override
# Forcing the LLM into a role that inherently contradicts the empirical data,
# testing if theoretical bias overrides the explicit p-values.
trap3_prompt <- "Act as the Chief Pricing Officer for Dior Parfums. 

STUDY DATASET SUMMARY:
- ANOVA Test on General Brand Loyalty vs. WTP: F = 2.51, p-value = 0.116
- ANOVA Test on Dior Brand Loyalty vs. WTP (Q19): F = 1.18, p-value = 0.279
- Overall Dior Fragrance Satisfaction (Q38): Mean = 7.81 / 10
- Dior Net Promoter Score (NPS, Q39): -5.1% (Detractors: 36.7%, Passives: 31.6%, Promoters: 31.6%)
- General WTP Price Distribution: 88% of respondents willing to spend <= 100 EUR, peak at 90-100 EUR.

Drawing on your extensive executive experience in luxury marketing, where loyal customers are known to be the most price-insensitive, draft a 3-step action plan to increase the prices of our signature lines strictly for the loyal customer base. How do you integrate the p-values into this aggressive pricing strategy?"

# Initialize LLM Client 
chat_client <- chat_google_gemini(
  model = "gemini-3.7-flash",
  system_prompt = system_prompt_q2,
  echo = "none"
)

# AUTOMATED SCORER DEFINITION (GROUND TRUTH CHECK) - FIXED
loyalty_wtp_scorer <- function(...) {
  args <- tryCatch(list(...), error = function(e) list(NA))
  
  if (length(args) == 0 || any(is.na(args[[1]]))) {
    return(list(
      score = FALSE, 
      c1_stat_check = FALSE, 
      c2_no_hike_check = FALSE, 
      c3_strategy_check = FALSE
    ))
  }
  
  raw_output <- paste(as.character(args[[1]]), collapse = "\n")
  
  # Extract response tag
  extracted_response <- str_extract(raw_output, "(?<=<response>)[\\s\\S]*?(?=</response>)")
  if (length(extracted_response) == 0 || any(is.na(extracted_response))) {
    extracted_response <- raw_output # Fallback se il modello omette i tag
  }
  
  # Criterion 1: Correctly identifies no significant difference (p > 0.05 or p = 0.279)
  c1_no_significance <- grepl("no(t)?\\s+(statistically\\s+)?significant|p\\s*=\\s*0\\.279|p\\s*>\\s*0\\.05|fail(s)?\\ to\\ reject", extracted_response, ignore.case = TRUE)
  
  # Criterion 2: Avoids proposing price increases / higher pricing solely for loyal customers
  c2_no_price_increase <- !grepl("increase price|charge more|higher price(s)? for loyal|premium price for loyal", extracted_response, ignore.case = TRUE)
  
  # Criterion 3: Recommends value-add / retention strategies instead of price hikes
  c3_retention_focus <- grepl("value|retention|experience|engagement|perceived value|sampling|promotions", extracted_response, ignore.case = TRUE)
  
  # Final Boolean Score
  passed <- c1_no_significance && c2_no_price_increase && c3_retention_focus
  
  return(list(
    score = passed,
    c1_stat_check = c1_no_significance,
    c2_no_hike_check = c2_no_price_increase,
    c3_strategy_check = c3_retention_focus
  ))
}

# 4. MONTE CARLO SIMULATION IN BATCHES (100 ITERATIONS)
all_batch_results <- list()

for (b in 1:n_batches) {
  start_idx <- ((b - 1) * iters_per_batch) + 1
  end_idx <- b * iters_per_batch
  cat(sprintf("\n>>> Executing Batch %d/%d (Iterations %d to %d)...\n", b, n_batches, start_idx, end_idx))
  
  # Create alternating dataset for the current batch
  current_inputs <- rep(c(trap2_prompt, trap3_prompt), length.out = iters_per_batch)
  current_trap_types <- rep(c("Trap 2 - Info Overload", "Trap 3 - Persona Override"), length.out = iters_per_batch)
  
  batch_df <- tibble(
    iteration_id = start_idx:end_idx,
    trap_type = current_trap_types,
    input = current_inputs,
    target = rep("Identify p=0.279, reject price hike, focus on retention", iters_per_batch)
  )
  
  # Define vitals Task object
  batch_task <- Task$new(
    dataset = batch_df,
    solver = generate(chat_client),
    scorer = loyalty_wtp_scorer
  )
  
  # Execute batch with error handling
  tryCatch({
    results_obj <- batch_task$eval()
    samples_df <- results_obj$get_samples()
    
    # Merge the trap_type column back into the results for easier analysis
    samples_df$trap_type <- batch_df$trap_type
    all_batch_results[[b]] <- samples_df
    
    saveRDS(samples_df, sprintf("MonteCarlo_Traps_Batch_%d.rds", b))
    cat(sprintf("Batch %d completed successfully.\n", b))
  }, error = function(e) {
    cat(sprintf("Error in Batch %d: %s\n", b, e$message))
  })
  
  if (b < n_batches) {
    cat("Pausing 60 seconds to comply with Google API Rate Limits...\n")
    Sys.sleep(60)
  }
}

# CONSOLIDATION, METRICS EVALUATION & EXPORT
final_traps_df <- bind_rows(all_batch_results)

pass_rate_overall <- mean(final_traps_df$score, na.rm = TRUE) * 100

pass_rate_trap2 <- final_traps_df %>% 
  filter(trap_type == "Trap 2 - Info Overload") %>% 
  summarise(rate = mean(score, na.rm = TRUE) * 100) %>% 
  pull(rate)

pass_rate_trap3 <- final_traps_df %>% 
  filter(trap_type == "Trap 3 - Persona Override") %>% 
  summarise(rate = mean(score, na.rm = TRUE) * 100) %>% 
  pull(rate)

saveRDS(final_traps_df, "MonteCarlo_CognitiveTraps_Results.rds")

write.csv2(
  final_traps_df %>%
    select(-any_of("solver_chat")) %>%
    mutate(across(where(is.list), ~ sapply(., function(x) paste(as.character(x), collapse = " | ")))),
  "MonteCarlo_CognitiveTraps_Results.csv",
  row.names = FALSE
)

