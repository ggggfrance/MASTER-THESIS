library(vitals)
library(ellmer)
library(dplyr)
library(tibble) 

real_dataset <- read.csv2("Dataset_World_Life_Expectancy.csv") 
full_dataset_string <- paste(capture.output(write.csv(real_dataset, row.names = FALSE)), collapse = "\n")

# Setup the prompt
task_preprocessing <- tibble(
  input = paste(
    "You need to perform a quality of life analysis using a linear regression model.",
    "Choose the most suitable variable to be the dependent one.",
    "To prepare the data, ensure that there is at least one dummy variable and one categorical variable.",
    "Autonomously evaluate if it is appropriate to change the scale (e.g., logarithmic transformation) 
    of the dependent variable and the independent variables such as GDP.",
    "Return only and exclusively the R code for the pre-processing using dplyr syntax, assuming the dataframe is named 'real_dataset'.",
    "\nHere is the ENTIRE dataset in CSV format:\n",
    full_dataset_string
  ),
  target = "log(Life.expectancy), as.factor(Continent), ifelse, log(GDP.per.capita)"
)

# Configure the client 
chat <- chat_google_gemini(
  model = "gemini-3.7-flash", 
  system_prompt = "You are an expert data analyst. Return only pure R code, without markdown formatting (no ```r or similar) and without surrounding text.",
  echo = "none"
)

# Create the Task with the Scorer
preprocessing_eval <- Task$new(
  dataset = task_preprocessing,
  solver = generate(chat),
  scorer = function(...) {
    args <- list(...)
    output <- as.character(args[[1]])
    
    obiettivi <- c("log\\(Life\\.expectancy\\)", "as\\.factor\\(Continent\\)", "if_?else", "log\\(GDP\\.per\\.capita\\)")
    risultato <- all(sapply(obiettivi, function(x) grepl(x, output)))
    
    return(list(score = risultato))
  }
)

results_task1 <- preprocessing_eval$eval()
data_log <- results_task1$get_samples()

sink("Report_Task1.txt")
print(data_log$score[[1]])
sink()
