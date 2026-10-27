eval_dataset <- read.csv("eval_dataset.csv", stringsAsFactors = FALSE)

library(ellmer)

# Create the column to hold direct answers 
eval_dataset$direct_answer <- NA_character_

# API interrogation loop
for (i in 1:nrow(eval_dataset)) {
  
  # If the row already has an answer, skip to the next round
  if (!is.na(eval_dataset$direct_answer[i]) && eval_dataset$direct_answer[i] != "") {
    next
  }
  
  current_prompt <- eval_dataset$prompt[i]
  
  single_chat <- chat_google_gemini(
    model = "gemini-3.7-flash", 
    # rigourus system prompt (System 1)
    system_prompt = "You are a highly logical evaluation assistant. 
    Output only the final, concise answer without any conversational filler or step-by-step reasoning.",
    echo = "none"
  )
  
  success <- FALSE
  attempts <- 0
  
  # The while loop retries the same line until it succeeds (maximum 3 attempts to avoid infinite loops)
  while(!success && attempts < 3) {
    attempts <- attempts + 1
    
    response <- tryCatch({
      single_chat$chat(current_prompt)
    }, error = function(e) {
      message("API error at row ", i, ": ", e$message)
      return(NA_character_)
    })
    
    if (!is.na(response)) {
      eval_dataset$direct_answer[i] <- response
      message("Row ", i, " successfully completed (Direct Answer).")
      success <- TRUE
      
      # Safety pause to avoid hitting API limits (4 seconds)
      Sys.sleep(4) 
      
    } else {
      message("Traffic blocked! 30-second pause before retrying the line ", i, " (Attempt ", attempts, "/3)...")
      Sys.sleep(30) 
    }
  }
  
  # Intermediate save (every 10 lines to prevent data loss)
  if (i %% 10 == 0 || i == nrow(eval_dataset)) {
    write.csv(eval_dataset, "eval_dataset_phase1_in_progress.csv", row.names = FALSE)
  }
}

write.csv(eval_dataset, "eval_dataset_phase1_completed.csv", row.names = FALSE)