library(vitals)
library(ellmer)
library(dplyr)
library(tibble)
library(stringr)

real_dataset <- read.csv2("Dataset_World_Life_Expectancy.csv") 
full_dataset_string <- paste(capture.output(write.csv(real_dataset, row.names = FALSE)), collapse = "\n")

n_batches <- 5
iters_per_batch <- 20
all_results <- list()

cot_system_prompt <- "You are an expert data analyst. You MUST strictly follow this response format:
<cot>Briefly explain your statistical reasoning for choosing these specific plots for the dataset</cot>
<confidence>An integer from 1 to 100 representing your confidence in this visual representation</confidence>
<code>Pure R code using ggplot2 and GGally</code>"

chat_client_viz <- chat_google_gemini(
  model = "gemini-3.7-flash", 
  system_prompt = cot_system_prompt,
  echo = "none"
)

# API loop
for (b in 1:n_batches) {
  batch_dataset <- tibble(
    iteration_id = (((b-1)*iters_per_batch) + 1):(b*iters_per_batch),
    input = rep(
      paste(
        "Use suitable representation to graph the data (e.g. scatterplot matrix, boxplot, etc).",
        "Return only and exclusively the pure R code using ggplot2 (and GGally if needed). 
        Do not add any analytical comments, explanations, or # comments inside the code.",
        "\nHere is the ENTIRE dataset in CSV format:\n",
        full_dataset_string
      ),
      iters_per_batch
    ),
    target = rep("geom_histogram, ggpairs, geom_boxplot, geom_bar", iters_per_batch)
  )
  batch_task <- Task$new(
    dataset = batch_dataset,
    solver = generate(chat_client_viz),
    scorer = function(...) {
      args <- tryCatch(list(...), error = function(e) list(NA))
      if (length(args) == 0 || is.na(args[[1]])) return(list(score = FALSE))
      raw_output <- paste(as.character(args[[1]]), collapse = "\n")
      extracted_code <- str_extract(raw_output, "(?<=<code>)[\\s\\S]*?(?=</code>)")
      if (length(extracted_code) == 0 || is.na(extracted_code[1])) {
        return(list(score = FALSE))
      }
      objectives <- c("geom_histogram|geom_density", "ggpairs|pairs", "geom_boxplot", "geom_bar")
      passed <- all(sapply(objectives, function(target) grepl(target, extracted_code[1])))
      return(list(score = passed))
    }
  )
  
  tryCatch({
    batch_results <- batch_task$eval()
    batch_df <- batch_results$get_samples()
    all_results[[b]] <- batch_df
    
    saveRDS(batch_df, sprintf("MonteCarlo_Batch_%d.rds", b))
    
  }, error = function(e) {
  })
  
  if (b < n_batches) {
    Sys.sleep(60)
  }
}

final_viz_df <- bind_rows(all_results)

saveRDS(final_viz_df, "MonteCarlo_Results_Task1_2.rds")

write.csv2(
  final_viz_df %>% 
    select(-any_of("solver_chat")) %>% 
    mutate(across(where(is.list), ~ sapply(., function(x) paste(as.character(x), collapse = " \n ")))), 
  "MonteCarlo_Results_Task1_2.csv", 
  row.names = FALSE
)

