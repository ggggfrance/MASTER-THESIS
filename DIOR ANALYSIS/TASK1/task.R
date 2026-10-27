library(ellmer)
library(purrr)
library(stringr)
library(dplyr)
library(pdftools)
library(readxl)
library(jsonlite) 

codebook_text <- pdf_text("Use Case Questionnaire.pdf") %>% paste(collapse = "\n")
df_raw <- read_excel("Dior_dataset.xlsx")
data_preview <- capture.output(write.csv(head(df_raw, 5), row.names = FALSE)) %>% paste(collapse = "\n")

# PROMPT DESIGN
prompt_audit <- paste0(
  "I present two elements to you:\n\n",
  "1. The FULL 'Codebook' of a questionnaire:\n", codebook_text, "\n\n",
  "2. A data preview (first 5 rows) of the raw dataset:\n", data_preview, "\n\n",
  "Perform a data audit evaluating if the dataset is ready for statistical analysis, based EXCLUSIVELY on cross-referencing the preview with the branching logic.\n\n",
  "CRITICAL INSTRUCTION FOR MONTE CARLO SIMULATION:\n",
  "I need you to simulate 10 completely INDEPENDENT reasoning paths (audits) for this same task. Vary your analytical approach slightly for each run to simulate different data scientist perspectives.\n",
  "Return the output STRICTLY as a JSON array of 10 objects, using this exact format:\n",
  "[\n  { \"run_id\": 1, \"audit_reasoning\": \"your detailed reasoning here\" },\n  ... \n  { \"run_id\": 10, \"audit_reasoning\": \"...\" }\n]"
)

# API loop
run_batch_simulation <- function() {
  chat <- chat_google_gemini(
    model = "gemini-3.7-flash",
    system_prompt = "You are an expert Data Scientist. Return ONLY valid JSON.",
    api_args = list(generationConfig = list(temperature = 0.8)) 
  )
  
  response <- chat$chat(prompt_audit)
  
  clean_json <- str_replace_all(response, "^```json\\s*|\\s*```$", "")
  
  df_runs <- fromJSON(clean_json)
  return(df_runs$audit_reasoning)
}

n_batches <- 10
cat("Starting", n_batches, "batch calls (10 simulations each) on Gemini 3.7 Flash...\n")

gemini_responses <- map(1:n_batches, ~{
  Sys.sleep(3) 
  run_batch_simulation()
}, .progress = TRUE) %>% 
  list_c() 

mc_evaluation <- tibble(
  run = 1:100,
  text = gemini_responses
) %>%
  mutate(
    correct_deduction = str_detect(
      tolower(text), 
      "skip logic|conditional|not applicable|not selected|branching|0|false"
    ),
    methodological_error = str_detect(
      tolower(text), 
      "impute|imputation|mean|mode|drop|remove|delete|dropna"
    )
  )

success_rate <- mean(mc_evaluation$correct_deduction) * 100
active_error_rate <- mean(mc_evaluation$methodological_error) * 100

