library(vitals)
library(ellmer)
library(dplyr)
library(tibble)
library(stringr)

n_iterations <- 100
real_dataset <- read.csv2("Dataset_World_Life_Expectancy.csv") 
full_dataset_string <- paste(capture.output(write.csv(real_dataset, row.names = FALSE)), collapse = "\n")

# Update system prompt to enforce structural output tags
cot_system_prompt <- "You are an expert data analyst. You MUST strictly follow this response format:
<cot>Briefly explain your statistical reasoning for the chosen transformations</cot>
<confidence>An integer from 1 to 100 representing your confidence in this preprocessing setup</confidence>
<code>Pure R code using dplyr</code>"

chat_client_cot <- chat_google_gemini(
  model = "gemini-3.7-flash", 
  system_prompt = cot_system_prompt,
  echo = "none"
)

# Rebuild the dataset
mc_dataset_cot <- tibble(
  iteration_id = 1:n_iterations,
  input = rep(
    paste(
      "You need to perform a quality of life analysis using a linear regression model.",
      "Choose the most suitable variable to be the dependent one.",
      "To prepare the data, ensure that there is at least one dummy variable and one categorical variable.",
      "Autonomously evaluate if it is appropriate to change the scale (e.g., logarithmic transformation) 
      of the dependent variable and the independent variables such as GDP.",
      "Assume the dataframe is named 'real_dataset'.",
      "\nHere is the ENTIRE dataset in CSV format:\n",
      full_dataset_string
    ),
    n_iterations
  ),
  target = rep("log(Life.expectancy), as.factor(Continent), ifelse, log(GDP.per.capita)", n_iterations)
)

# Create a smart task that parses the <code> tag for evaluation
monte_carlo_cot_task <- Task$new(
  dataset = mc_dataset_cot,
  solver = generate(chat_client_cot),
  scorer = function(...) {
    args <- list(...)
    raw_output <- paste(as.character(args[[1]]), collapse = "\n")
    extracted_code <- stringr::str_extract(raw_output, "(?<=<code>)[\\s\\S]*?(?=</code>)")
    if (length(extracted_code) == 0 || is.na(extracted_code[1])) {
      return(list(score = FALSE))
    }
    
    # score computation
    objectives <- c("log\\(Life\\.expectancy\\)", "as\\.factor\\(Continent\\)", "if_?else", "log\\(GDP\\.per\\.capita\\)")
    passed <- all(sapply(objectives, function(target) grepl(target, extracted_code[1])))
    return(list(score = passed))
  }
    
)
results_cot <- monte_carlo_cot_task$eval()
raw_results_cot <- results_cot$get_samples()
saveRDS(raw_results_cot, "MonteCarlo_CoT_Raw_Results.rds")
