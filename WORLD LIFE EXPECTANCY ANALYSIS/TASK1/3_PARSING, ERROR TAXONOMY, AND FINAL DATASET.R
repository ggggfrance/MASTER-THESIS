library(dplyr)
library(stringr)
library(ellmer)
library(purrr)

raw_results_cot <- readRDS("MonteCarlo_CoT_Raw_Results.rds")

# Parse the structured output into distinct columns
parsed_dataset <- raw_results_cot %>%
  mutate(
    Task = "Pre-processing",
    Target_Methodology = target,
    Pass_Rate_Strict = score,
    Raw_Output = result,
    # Extract Chain of Thought
    Chain_of_Thought = str_extract(Raw_Output, "(?<=<cot>)[\\s\\S]*?(?=</cot>)"),
    # Extract and clean Confidence Score
    Confidence_Score = as.numeric(str_extract(str_extract(Raw_Output, "(?<=<confidence>)[\\s\\S]*?(?=</confidence>)"), "\\d+")),
    # Extract final R Code
    Model_Code_Snippet = str_extract(Raw_Output, "(?<=<code>)[\\s\\S]*?(?=</code>)")
  ) %>%
  # Handle potential parsing failures
  mutate(
    Chain_of_Thought = if_else(is.na(Chain_of_Thought), "Parsing Failed", trimws(Chain_of_Thought)),
    Model_Code_Snippet = if_else(is.na(Model_Code_Snippet), "Parsing Failed", trimws(Model_Code_Snippet))
  )

# Configure the LLM-as-a-Judge for Error Taxonomy
judge_prompt <- "You are a data science professor evaluating a student's R code. 
Evaluate the provided dplyr code. 
Assign an Error_Category strictly from: ['Syntax Error', 'Methodological Divergence', 'Perfect Execution']. 
Assign a Judge_Score strictly from 1 to 5 (5 being perfect reasoning, even if it omits strict log transformations on bounded variables).
Return ONLY this exact format:
Category: [Your Category]
Score: [Your Score]"

evaluate_taxonomy <- function(code_snippet, iter_id) {
  if (code_snippet == "Parsing Failed") {
    return(tibble(Error_Category = "Formatting Failure", LLM_Judge_Score = 0))
  }
  
  temp_chat <- chat_google_gemini(model = "gemini-3.7-flash", system_prompt = judge_prompt, echo = "none")
  response <- temp_chat$chat(paste("Evaluate this code:\n", code_snippet))
  
  category <- str_extract(response, "(?<=Category: ).*")
  score <- as.numeric(str_extract(response, "(?<=Score: )\\d"))
  
  cat(sprintf("Evaluated Taxonomy Iteration %d: %s\n", iter_id, category))
  return(tibble(Error_Category = category, LLM_Judge_Score = score))
}

# Taxonomy Evaluation
taxonomy_results <- map2_dfr(parsed_dataset$Model_Code_Snippet, parsed_dataset$id, evaluate_taxonomy)

# Assemble the Definitive Dataset
final_thesis_dataset <- parsed_dataset %>%
  bind_cols(taxonomy_results) %>%
  select(
    Iteration_ID = id, 
    Task, 
    Target_Methodology, 
    Chain_of_Thought,
    Confidence_Score,
    Model_Code_Snippet, 
    Pass_Rate_Strict, 
    Error_Category, 
    LLM_Judge_Score
  )

saveRDS(final_thesis_dataset, "Definitive_Dataset_CoT.rds")
write.csv2(final_thesis_dataset, "Definitive_Dataset_CoT.csv", row.names = FALSE)

