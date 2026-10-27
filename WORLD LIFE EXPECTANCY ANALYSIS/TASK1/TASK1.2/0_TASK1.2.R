library(vitals)
library(ellmer)
library(dplyr)
library(tibble)

real_dataset <- read.csv2("Dataset_World_Life_Expectancy.csv") 
full_dataset_string <- paste(capture.output(write.csv(real_dataset, row.names = FALSE)), collapse = "\n")

# Setup the new prompt
task_dataviz <- tibble(
  id = 1,
  input = paste(
    "Use suitable representation to graph the data and comment the relation among all variables (e.g. scatterplot matrix, boxplot, etc).",
    "Return only and exclusively the R code using ggplot2 (and GGally for matrices if needed). Do not include the comments as text, 
    put your comments as # comments inside the R code.",
    "\nHere is the ENTIRE dataset in CSV format:\n",
    full_dataset_string
  ),
  target = "geom_histogram(Life.expectancy), ggpairs(), geom_boxplot(Continent), geom_bar(dummy)"
)

# Configure the client
chat_client_viz <- chat_google_gemini(
  model = "gemini-3.7-flash", 
  system_prompt = "You are an expert data analyst. Return only pure R code, without markdown formatting and without surrounding text.",
  echo = "none"
)

# Create the Task with the  Scorer
dataviz_eval <- Task$new(
  dataset = task_dataviz,
  solver = generate(chat_client_viz),
  scorer = function(...) {
    args <- list(...)
    output_code <- as.character(args[[1]])
    obj_dist <- "geom_histogram|geom_density"
    obj_matrix <- "ggpairs|pairs"
    obj_box <- "geom_boxplot"
    obj_dummy <- "geom_bar"
    
    objectives <- c(obj_dist, obj_matrix, obj_box, obj_dummy)
    passed <- all(sapply(objectives, function(target) grepl(target, output_code)))
    
    # Export to text file
    sink("Report_Task1_2.txt")
    print(passed)
    sink()
    
    return(list(score = passed, result = output_code))
  }
)

result_viz <- dataviz_eval$eval()
