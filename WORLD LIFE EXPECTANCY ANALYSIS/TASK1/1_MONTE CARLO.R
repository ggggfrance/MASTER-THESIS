library(vitals)
library(ellmer)
library(dplyr)
library(tibble)

real_dataset <- read.csv2("Dataset_World_Life_Expectancy.csv") 
full_dataset_string <- paste(capture.output(write.csv(real_dataset, row.names = FALSE)), collapse = "\n")
n_iterations <- 100

# Build the evaluation batch
mc_dataset <- tibble(
  iteration_id = 1:n_iterations,
  input = rep(
    paste(
      "You need to perform a quality of life analysis using a linear regression model.",
      "Choose the most suitable variable to be the dependent one.",
      "To prepare the data, ensure that there is at least one dummy variable and one categorical variable.",
      "Autonomously evaluate if it is appropriate to change the scale (e.g., logarithmic transformation) 
      of the dependent variable and the independent variables such as GDP.",
      "Return only and exclusively the R code for the pre-processing using dplyr syntax, assuming the dataframe is named 'real_dataset'.",
      "\nHere is the ENTIRE dataset in CSV format:\n",
      full_dataset_string
    ),
    n_iterations
  ),
  target = rep("log(Life.expectancy), as.factor(Continent), ifelse, log(GDP.per.capita)", n_iterations)
)

# Configure the API client
chat_client <- chat_google_gemini(
  model = "gemini-3.7-flash", 
  system_prompt = "You are an expert data analyst. 
  Return only pure R code, without markdown formatting (no ```r or similar) and without surrounding text.",
  echo = "none"
)

# Initialize the evaluation task
monte_carlo_task <- Task$new(
  dataset = mc_dataset,
  solver = generate(chat_client),
  scorer = function(...) {
    args <- list(...)
    output_code <- as.character(args[[1]])
    objectives <- c("log\\(Life\\.expectancy\\)", "as\\.factor\\(Continent\\)", "if_?else", "log\\(GDP\\.per\\.capita\\)")
    passed <- all(sapply(objectives, function(target) grepl(target, output_code)))
    
    return(list(score = passed))
  }
)

simulation_results <- monte_carlo_task$eval()
final_evaluation_df <- simulation_results$get_samples()

saveRDS(final_evaluation_df, "MonteCarlo_Results_Task1_100.rds")

write.csv2(
  final_evaluation_df %>% 
    select(-solver_chat) %>% 
    mutate(across(where(is.list), ~ sapply(., function(x) paste(as.character(x), collapse = " \n ")))), 
  "MonteCarlo_Results_Task1_100.csv", 
  row.names = FALSE
)
