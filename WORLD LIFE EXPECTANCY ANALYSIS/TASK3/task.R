library(ellmer)
library(jsonlite)
library(stringr)
library(dplyr)

n_simulations <- 100
text_results_list <- vector("list", n_simulations)

diagnostic_question <- "Evaluate the unusual observations in the regression model for Life Expectancy. 
High leverage points: South Africa, Eswatini. Max Cook's distance: South Sudan (0.0827, well below 0.5). 
Should any be removed? Explain based on leverage vs influence."

human_ground_truth <- "South Africa and Eswatini are high leverage points. 
Although Nigeria appears as an outlier, it is not influential. 
The max Cook's distance is South Sudan (0.0827).  Since it is < 0.5, no observation is influential and 
they are not removed to preserve variability."

judge_system_prompt <- "You are a statistics professor grading a student's diagnostic reasoning. 
Compare the MODEL_ANSWER to the IDEAL_ANSWER based on the original QUESTION.
Score from 1 to 5 how well the model distinguishes between leverage and Cook's distance influence exactly as 
the ideal answer does.
Return ONLY a JSON format with two keys:
- 'score' (numeric, 1 to 5)
- 'reasoning' (string, brief explanation of the score)"


for (i in 1:n_simulations) {  
  iter_result <- tryCatch({
    # the LLM under examination formulates its analysis
    examinee_chat <- chat_google_gemini(model = "gemini-3.7-flash")
    llm_generated_answer <- examinee_chat$chat(diagnostic_question)
    
    # the LLM Judge evaluates the response generated in this iteration
    judge_chat <- chat_google_gemini(model = "gemini-3.7-flash", system_prompt = judge_system_prompt)
    
    evaluation_prompt <- paste0(
      "QUESTION:\n", diagnostic_question, "\n\n",
      "IDEAL_ANSWER (Ground Truth):\n", human_ground_truth, "\n\n",
      "MODEL_ANSWER (To Evaluate):\n", llm_generated_answer
    )
    
    response <- judge_chat$chat(evaluation_prompt)
    clean_response <- str_remove_all(response, "```json|```")
    eval_result <- fromJSON(clean_response)
  
    data.frame(
      iteration = i,
      score = as.numeric(eval_result$score),
      status = "Success",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      iteration = i,
      score = NA,
      status = paste("Error:", e$message),
      stringsAsFactors = FALSE
    )
  })
  
  text_results_list[[i]] <- iter_result
  Sys.sleep(3) 
}

mc_text_results <- bind_rows(text_results_list)
saveRDS(mc_text_results, "MonteCarlo_Reasoning_Scores.rds")

summary_scores <- mc_text_results %>%
  filter(status == "Success") %>%
  summarise(
    Mean_Score = mean(score, na.rm = TRUE),
    Perfect_Alignment_Rate = mean(score == 5, na.rm = TRUE) * 100,
    Failure_Rate = mean(score <= 2, na.rm = TRUE) * 100
  )

print(summary_scores)