eval_dataset <- read.csv("eval_dataset_phase2_completed.csv", stringsAsFactors = FALSE)

# Create two score columns for the test comparison between the two diffrent systems
if(!"score_system1" %in% colnames(eval_dataset)) eval_dataset$score_system1 <- NA_character_
if(!"score_system2" %in% colnames(eval_dataset)) eval_dataset$score_system2 <- NA_character_

library(ellmer)
library(dplyr)
library(stringr)

# Judge prompt that evaluates system 1 answer vs system 2 answer
judge_sys_prompt <- "You are an expert, objective evaluation judge. 
You will be provided with a Question, the Expected Correct Answer, and a Model's Response. 
Your task is to determine if the Model's Response is semantically correct based on the Expected Answer.
Ignore typos, extra formatting, or conversational filler in the Model's Response.
Output STRICTLY '1' if the Model's Response is correct or means the same thing.
Output STRICTLY '0' if the Model's Response is incorrect or contradicts the Expected Answer.
Do not output any other characters."

judge_chat <- chat_google_gemini(
  model = "gemini-3.7-flash", 
  system_prompt = judge_sys_prompt,
  echo = "none"
)

# API interrogation loop
for (i in 1:nrow(eval_dataset)) {
  
  # A. JUDGE SYSTEM 1 (Direct Answer)
  if (is.na(eval_dataset$score_system1[i]) || eval_dataset$score_system1[i] == "") {
    
    prompt_sys1 <- sprintf(
      "Question: %s\nExpected Answer: %s\nModel Response: %s",
      eval_dataset$prompt[i],
      eval_dataset$expected_answer[i],
      eval_dataset$direct_answer[i]
    )
    
    success <- FALSE
    attempts <- 0
    
    while(!success && attempts < 3) {
      attempts <- attempts + 1
      
      response <- tryCatch({
        judge_chat$chat(prompt_sys1)
      }, error = function(e) {
        message("API Error (Judge Sys1) row ", i, ": ", e$message)
        return(NA_character_)
      })
      
      if (!is.na(response)) {
        clean_score <- str_extract(response, "[0-1]") 
        eval_dataset$score_system1[i] <- clean_score
        success <- TRUE
        Sys.sleep(4) 
      } else {
        message("Traffic blocked! 30s pause for Judge Sys1... (Attempt ", attempts, "/3)")
        Sys.sleep(30)
      }
    }
  }

  # B. JUDGE SYSTEM 2 (CoT Final Answer)
  if (is.na(eval_dataset$score_system2[i]) || eval_dataset$score_system2[i] == "") {
    
    prompt_sys2 <- sprintf(
      "Question: %s\nExpected Answer: %s\nModel Response: %s",
      eval_dataset$prompt[i],
      eval_dataset$expected_answer[i],
      eval_dataset$cot_final_answer[i]
    )
    
    success <- FALSE
    attempts <- 0
    
    while(!success && attempts < 3) {
      attempts <- attempts + 1
      
      response <- tryCatch({
        judge_chat$chat(prompt_sys2)
      }, error = function(e) {
        message("API Error (Judge Sys2) row ", i, ": ", e$message)
        return(NA_character_)
      })
      
      if (!is.na(response)) {
        clean_score <- str_extract(response, "[0-1]") 
        eval_dataset$score_system2[i] <- clean_score
        success <- TRUE
        Sys.sleep(4) 
      } else {
        message("Traffic blocked! 30s pause for Judge Sys2... (Attempt ", attempts, "/3)")
        Sys.sleep(30)
      }
    }
  }
  
  # Console feedback with both scores
  message(sprintf("Row %d completed. Sys1 Score: %s | Sys2 Score: %s", 
                  i, eval_dataset$score_system1[i], eval_dataset$score_system2[i]))
  
  # Intermediate backup every 10 rows
  if (i %% 10 == 0 || i == nrow(eval_dataset)) {
    write.csv(eval_dataset, "eval_dataset_phase3_in_progress.csv", row.names = FALSE)
  }
}

write.csv(eval_dataset, "eval_dataset_final_scored.csv", row.names = FALSE)