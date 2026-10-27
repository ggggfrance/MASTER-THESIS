library(vitals)
library(ellmer)
library(dplyr)
library(tibble)
library(stringr)

real_dataset <- read.csv2("Dataset_World_Life_Expectancy.csv") 

data_metadata <- paste(
  "Here is the structural and statistical summary of the dataset:\n",
  "--- STRUCTURE ---\n",
  paste(capture.output(str(real_dataset)), collapse = "\n"),
  "\n--- SUMMARY STATISTICS ---\n",
  paste(capture.output(summary(real_dataset)), collapse = "\n"),
  sep = ""
)
n_batches <- 10
iters_per_batch <- 10
all_results_cot <- list()
cot_system_prompt <- "You are an expert data analyst. You MUST strictly follow this response format:
<cot>Briefly explain your statistical reasoning for choosing these specific plots based on the data structure</cot>
<confidence>An integer from 1 to 100 representing your confidence</confidence>
<code>Pure R code using ggplot2 and GGally</code>"
chat_client_cot <- chat_google_gemini(
  model = "gemini-3.7-flash", 
  system_prompt = cot_system_prompt,
  echo = "none"
)

for (b in 1:n_batches) {
  batch_dataset_cot <- tibble(
    iteration_id = (((b-1)*iters_per_batch) + 1):(b*iters_per_batch),
    input = rep(
      paste(
        "Use suitable representation to graph the data.",
        "Return only and exclusively the pure R code using ggplot2 (and GGally if needed). 
        Do not add any analytical comments, explanations, or # comments inside the code.",
        data_metadata
      ),
      iters_per_batch
    ),
    target = rep("geom_histogram, ggpairs, geom_boxplot, geom_bar", iters_per_batch))
  batch_task_cot <- Task$new(
    dataset = batch_dataset_cot,
    solver = generate(chat_client_cot),
    scorer = function(target, output, ...) {
      if (missing(output) || is.null(output)) {
        return(list(score = FALSE, result = "Error: API Timeout or Missing Output"))
      }
      raw_output <- paste(as.character(output), collapse = "\n")
      if (length(raw_output) == 0 || is.na(raw_output) || raw_output == "NA") {
        return(list(score = FALSE, result = "Error: NA Output"))
      }
      extracted_code <- str_extract(raw_output, "(?<=<code>)[\\s\\S]*?(?=</code>)")
      if (length(extracted_code) == 0 || is.na(extracted_code[1])) {
        return(list(score = FALSE, result = raw_output))
      }
      objectives <- c("geom_histogram|geom_density", "ggpairs|pairs", "geom_boxplot", "geom_bar")
      passed <- all(sapply(objectives, function(obj) grepl(obj, extracted_code[1])))
      return(list(score = passed, result = raw_output))
    })
  
  
  tryCatch({
    batch_results <- batch_task_cot$eval()
    batch_df <- batch_results$get_samples()
    all_results_cot[[b]] <- batch_df
    saveRDS(batch_df, sprintf("MonteCarlo_CoT_Batch_%d.rds", b))
  }, error = function(e) {
  })
  if (b < n_batches) {
    Sys.sleep(60)
  }
}

raw_results_viz_cot <- bind_rows(all_results_cot)
saveRDS(raw_results_viz_cot, "MonteCarlo_CoT_Raw_Results_Task1_2.rds")