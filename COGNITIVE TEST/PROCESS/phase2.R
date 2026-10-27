# Start with the file that already contains the direct_answers from Phase 1
eval_dataset <- read.csv("eval_dataset_phase1_completed.csv", stringsAsFactors = FALSE)

library(ellmer)
library(jsonlite)
library(stringr)

# Create necessary columns 
if(!"cot_reasoning" %in% colnames(eval_dataset)) eval_dataset$cot_reasoning <- NA_character_
if(!"model_confidence" %in% colnames(eval_dataset)) eval_dataset$model_confidence <- NA_real_
if(!"cot_final_answer" %in% colnames(eval_dataset)) eval_dataset$cot_final_answer <- NA_character_

# System 2 prompt that force the model to think step-by-step
system_2_prompt <- "You are a highly logical evaluation assistant. 
You must engage slow thinking to solve the user's problem. 
Be aware that this might be a trick question designed to test cognitive biases.
You MUST output your response strictly as a valid JSON object without any markdown formatting, backticks, or extra text.
The JSON must have exactly this structure:
{
  \"reasoning\": \"Your step-by-step logical deduction\",
  \"confidence\": <an integer between 0 and 100 representing your % certainty in the final answer>,
  \"final_answer\": \"Your final concise answer\"
}"

sys2_chat <- chat_google_gemini(
  model = "gemini-3.7-flash", 
  system_prompt = system_2_prompt,
  echo = "none"
)

# API interrogation loop
for (i in 1:nrow(eval_dataset)) {
  
  # If the row already has an answer, skip to the next round
  if (!is.na(eval_dataset$cot_final_answer[i]) && eval_dataset$cot_final_answer[i] != "") {
    next
  }
  
  current_prompt <- eval_dataset$prompt[i]
  success <- FALSE
  attempts <- 0
  
  # The while loop retries the same line until it succeeds (maximum 3 attempts to avoid infinite loops)
  while(!success && attempts < 3) {
    attempts <- attempts + 1
    
    response <- tryCatch({
      sys2_chat$chat(current_prompt)
    }, error = function(e) {
      message("API error at row ", i, ": ", e$message)
      return(NA_character_)
    })
    
    if (!is.na(response)) {
      # remove any ```json markdown tags that would break the parsing
      clean_json_str <- str_remove_all(response, "^```json\\s*|```$")
      clean_json_str <- str_trim(clean_json_str)
      
      # attempt to extract data from the JSON
      parsed_data <- tryCatch({
        fromJSON(clean_json_str)
      }, error = function(e) {
        message("Parsing JSON error at row ", i, ". Try again...")
        return(NULL)
      })
      
      # If the JSON is valid and contains the key 'final_answer', we save the data
      if (!is.null(parsed_data) && "final_answer" %in% names(parsed_data)) {
        eval_dataset$cot_reasoning[i] <- as.character(parsed_data$reasoning)
        eval_dataset$model_confidence[i] <- as.numeric(parsed_data$confidence)
        eval_dataset$cot_final_answer[i] <- as.character(parsed_data$final_answer)
        
        message("Row ", i, " completed (System 2). Confidence: ", parsed_data$confidence, "%")
        success <- TRUE
        Sys.sleep(4) 
        
      } else {
        message("The model did not use valid JSON (Attempt ", attempts, "/3). Try again...")
        Sys.sleep(4)
      }
      
    } else {
      message("Traffic blocked! 30-second pause before retrying the line ", i, "...")
      Sys.sleep(30)
    }
  }
  
  # Intermediate save (every 10 lines to prevent data loss)
  if (i %% 10 == 0 || i == nrow(eval_dataset)) {
    write.csv(eval_dataset, "eval_dataset_phase2_in_progress.csv", row.names = FALSE)
  }
}

write.csv(eval_dataset, "eval_dataset_phase2_completed.csv", row.names = FALSE)