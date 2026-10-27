library(flextable)
library(officer)

task3_metrics <- data.frame(
  Metric = c("1. Perfect Alignment Rate", "2. Failure Rate", "3. Mean Score"),
  Value = c("100%", "0%", "5")
)

white_border <- fp_border(color = "white", width = 1.5)

task3_flex <- flextable(task3_metrics) %>%
  add_header_lines("Table: LLM Diagnostic Reasoning Stability Metrics") %>%
  border_remove() %>% 
  align(align = "center", part = "all") %>%
  
  bg(part = "all", bg = "#FFFFFF") %>% 
  
  bg(i = 2, part = "header", bg = "#D0D0D0") %>%       
  bg(i = 2, part = "body", bg = "#F2F2F2") %>%         
  
  color(i = 1, part = "header", color = "#1F497D") %>% 
  bold(i = 1, part = "header") %>%
  fontsize(i = 1, part = "header", size = 13) %>%
  
  bold(i = 2, part = "header") %>%
  
  border_inner_h(border = white_border, part = "all") %>%
  border_inner_v(border = white_border, part = "all") %>%
  
  width(j = 1, width = 3.5) %>%
  width(j = 2, width = 1.2)