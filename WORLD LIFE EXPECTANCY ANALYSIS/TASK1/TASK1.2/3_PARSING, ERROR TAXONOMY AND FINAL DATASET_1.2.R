library(dplyr)
library(stringr)
library(ellmer)
library(purrr)

raw_results_viz_cot <- readRDS("MonteCarlo_CoT_Raw_Results_Task1_2.rds")

# Tag parsing 
parsed_dataset_viz <- raw_results_viz_cot %>%
  mutate(
    Task = "Data Visualization",
    Target_Methodology = "geom_histogram, ggpairs, geom_boxplot, geom_bar",
    Pass_Rate_Strict = score,
    Raw_Output = result,
    Chain_of_Thought = str_extract(Raw_Output, "(?<=<cot>)[\\s\\S]*?(?=</cot>)"),
    Confidence_Score = as.numeric(str_extract(str_extract(Raw_Output, "(?<=<confidence>)[\\s\\S]*?(?=</confidence>)"), "\\d+")),
    Model_Code_Snippet = str_extract(Raw_Output, "(?<=<code>)[\\s\\S]*?(?=</code>)")
  ) %>%
  mutate(
    Chain_of_Thought = if_else(is.na(Chain_of_Thought), "Parsing Failed", trimws(Chain_of_Thought)),
    Model_Code_Snippet = if_else(is.na(Model_Code_Snippet), "Parsing Failed", trimws(Model_Code_Snippet))
  )

# Judge setup 
judge_prompt_viz <- "You are a data science professor evaluating a student's R ggplot2 code for exploratory data analysis. 
Evaluate the provided visual analysis approach. 
Assign an Error_Category strictly from: ['Syntax Error', 'Under-specification Output', 'Perfect Execution']. 
(Use 'Under-specification Output' if the code is valid and explores the data well, 
but might have chosen different plots than a rigid 4-plot template).
Assign a Judge_Score strictly from 1 to 5 (5 being perfect exploratory visual reasoning).
Return ONLY this exact format:
Category: [Your Category]
Score: [Your Score]"
evaluate_viz_taxonomy <- function(code_snippet, iter_id) {
  if (code_snippet == "Parsing Failed") {
    return(tibble(Error_Category = "Formatting Failure", LLM_Judge_Score = 0))
  }
  temp_chat <- chat_google_gemini(model = "gemini-3.7-flash", system_prompt = judge_prompt_viz, echo = "none")
  response <- temp_chat$chat(paste("Evaluate this dataviz code:\n", code_snippet))
  category <- str_extract(response, "(?<=Category: ).*")
  score <- as.numeric(str_extract(response, "(?<=Score: )\\d"))
  return(tibble(Error_Category = category, LLM_Judge_Score = score))
}
taxonomy_results_viz <- map2_dfr(parsed_dataset_viz$Model_Code_Snippet, parsed_dataset_viz$id, evaluate_viz_taxonomy)
final_thesis_dataset_viz <- parsed_dataset_viz %>%
  bind_cols(taxonomy_results_viz) %>%
  select(Iteration_ID = id, Task, Target_Methodology, Chain_of_Thought, Confidence_Score, Model_Code_Snippet, 
          Pass_Rate_Strict, Error_Category, LLM_Judge_Score)

saveRDS(final_thesis_dataset_viz, "Definitive_Dataset_CoT_Task1_2.rds")
write.csv2(final_thesis_dataset_viz, "Definitive_Dataset_CoT_Task1_2.csv", row.names = FALSE)
