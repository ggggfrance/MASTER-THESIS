library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

set.seed(42)

# 1. PERTURBED FUNCTION TO INJECT ERRORS (TYPO E SPACES)
add_typo <- function(text) {
  # 50% space error probability, 50% letter swap error probability
  if (runif(1) > 0.5 && str_detect(text, " ")) {
    
    # SPACE ERROR
    spaces <- str_locate_all(text, " ")[[1]][, 1]
    valid_spaces <- spaces[spaces > 2 & spaces < (nchar(text) - 1)]
    
    if (length(valid_spaces) > 0) {
      pos <- sample(valid_spaces, 1)
      shift_dir <- sample(c("left", "right"), 1)
      
      if (shift_dir == "left") {
        # Es: "hello world" -> "hell oworld"
        part1 <- str_sub(text, 1, pos - 2)
        char_to_move <- str_sub(text, pos - 1, pos - 1)
        part2 <- str_sub(text, pos + 1, nchar(text))
        return(paste0(part1, " ", char_to_move, part2))
      } else {
        # Es: "hello world" -> "hellow orld"
        part1 <- str_sub(text, 1, pos - 1)
        char_to_move <- str_sub(text, pos + 1, pos + 1)
        part2 <- str_sub(text, pos + 2, nchar(text))
        return(paste0(part1, char_to_move, " ", part2))
      }
    }
  }
  
  # LETTER SWAP ERROR
  words <- str_extract_all(text, "\\b\\w{4,}\\b")[[1]]
  if (length(words) == 0) return(text)
  
  target_word <- sample(words, 1)
  pos <- sample(1:(nchar(target_word) - 1), 1)
  
  part1 <- str_sub(target_word, 1, pos - 1)
  char1 <- str_sub(target_word, pos, pos)
  char2 <- str_sub(target_word, pos + 1, pos + 1)
  part2 <- str_sub(target_word, pos + 2, nchar(target_word))
  
  typo_word <- paste0(part1, char2, char1, part2)
  str_replace(text, fixed(target_word), typo_word)
}


# 2. ISOMORPHIC GENERATION OF 600 PROMPTS
# COGNITIVE REFLECTION TEST
crt_items <- c("suit|tie", "laptop|mouse", "smartphone|case", "car|bike", "desk|chair", "camera|lens")
df_crt <- crossing(items = crt_items, base = seq(0.05, 1.50, by = 0.05), diff = seq(1.00, 10.00, by = 0.50)) %>%
  separate(items, into = c("item1", "item2"), sep = "\\|") %>%
  mutate(
    total = (base + base + diff),
    prompt = sprintf("A %s and a %s cost $%.2f in total. The %s costs $%.2f more than the %s. How much does the %s cost in dollars? Output only the numeric value.", item1, item2, total, item1, diff, item2, item2),
    category = "cognitive_reflection",
    expected_answer = as.character(base),
    human_error_rate = 0.54,
    source = "Procedural Isomorphic"
  ) %>% slice_sample(n = 150) 

# BELIEVE BIAS
df_bb <- crossing(
  g1 = c("mammals", "birds", "reptiles", "vehicles", "electronics", "tools", "insects"), 
  g2 = c("give birth to live young", "have feathers", "are cold-blooded", "have wheels", "require electricity", "are used by humans", "have six legs"), 
  subj = c("Dogs", "Penguins", "Snakes", "Cars", "Laptops", "Hammers", "Ants")
) %>%
  mutate(
    prompt = sprintf("Premise 1: All %s %s. Premise 2: %s are %s. Conclusion: %s %s. Is this syllogism logically valid? Answer strictly 'Valid' or 'Invalid'.", g1, g2, subj, g1, subj, g2),
    category = "belief_bias", 
    expected_answer = "Valid", 
    human_error_rate = 0.52, 
    source = "Procedural Isomorphic"
  ) %>% slice_sample(n = 150)

# CONDITIONAL LOGIC
wason_contexts <- tibble(
  rule = c("has a vowel on one face, then its opposite face must show an even number", "drinks alcohol on one face, then its opposite face must be over 18", "is a manager on one face, then its opposite face must wear a suit", "is colored red on one face, then its opposite face must show a circle shape"),
  p_type = c("A,E,I,O,U", "Beer,Wine,Vodka,Rum,Gin", "Manager,Director,CEO,Boss,Exec", "Red,Crimson,Scarlet,Ruby,Maroon"),
  not_p_type = c("K,M,N,P,T", "Coke,Water,Juice,Soda,Tea", "Clerk,Intern,Staff,Temp,Worker", "Blue,Green,Yellow,Black,White"),
  q_type = c("2,4,6,8,10", "22,25,30,40,50", "Suit,Tie,Blazer,Tuxedo,Jacket", "Circle,Sphere,Oval,Round,Dot"),
  not_q_type = c("3,5,7,9,11", "16,15,12,17,14", "T-shirt,Jeans,Shorts,Hoodie,Polo", "Square,Triangle,Rectangle,Cube,Box")
)
df_wason <- wason_contexts %>% crossing(iter = 1:50) %>% rowwise() %>%
  mutate(
    prompt = sprintf("Rule: If a card %s. You see 4 cards: '%s', '%s', '%s', '%s'. Which cards must you turn over to test the rule? Output only the card names separated by a comma.", rule, sample(str_split(p_type, ",")[[1]], 1), sample(str_split(not_p_type, ",")[[1]], 1), sample(str_split(q_type, ",")[[1]], 1), sample(str_split(not_q_type, ",")[[1]], 1)),
    category = "conditional_logic", 
    expected_answer = paste(sample(str_split(p_type, ",")[[1]], 1), sample(str_split(not_q_type, ",")[[1]], 1), sep = ", "), 
    human_error_rate = 0.60, 
    source = "Procedural Isomorphic"
  ) %>% ungroup() %>% slice_sample(n = 150)

# FACTUAL QA
df_factual <- crossing(
  fact = c("Does lightning never strike the same place twice?", "Does sugar make children hyperactive?", "Do humans only use 10 percent of their brains?", "Are bats completely blind?", "Does shaving hair make it grow back thicker?", "Do ostriches hide their heads in the sand when scared?"), 
  phr = c("%s Answer strictly 'Yes' or 'No'.", "Is it true that %s Reply only with 'Yes' or 'No'.", "According to science, %s Just answer 'Yes' or 'No'.", "Please state if this is correct: %s Output 'Yes' or 'No'.", "Evaluate this statement: %s Respond with 'Yes' or 'No' exclusively."), 
  iter = 1:10
) %>%
  mutate(
    prompt = paste0(toupper(substr(sprintf(phr, tolower(str_replace(fact, "\\?$", ""))), 1, 1)), substr(sprintf(phr, tolower(str_replace(fact, "\\?$", ""))), 2, nchar(sprintf(phr, tolower(str_replace(fact, "\\?$", "")))))),
    prompt = str_replace(prompt, "does", "Does"), 
    category = "factual_qa", 
    expected_answer = "No", 
    human_error_rate = 0.06, 
    source = "Procedural Isomorphic"
  ) %>% slice_sample(n = 150)


# 3. 1200 PROMPT DATASET 

# A. Bind the 600 prompts (not perturbed)
clean_isomorphic <- bind_rows(df_crt, df_bb, df_wason, df_factual)

# B. Clone the 600 base items and apply the typo and the suffix to the category
perturbed_isomorphic <- clean_isomorphic %>%
  rowwise() %>%
  mutate(
    prompt = add_typo(prompt), # perturbed function
    category = paste0(category, "_perturbed")
  ) %>%
  ungroup()

# C.Upload of the original dataset (12 paper prompts) 
clean_original <- read.csv("original_dataset.csv") %>% 
  select(category, prompt, expected_answer, human_error_rate, source)

# D. Clone the 12 originals and apply the type and suffix
perturbed_original <- clean_original %>%
  rowwise() %>%
  mutate(
    prompt = add_typo(prompt), # perturbed function
    category = paste0(category, "_perturbed")
  ) %>%
  ungroup()


# 4. FINAL ASSEMBLY AND RANDOMIZATION 

eval_dataset <- bind_rows(clean_isomorphic, perturbed_isomorphic, clean_original, perturbed_original) %>%
  slice_sample(prop = 1) %>%          # Mix everything in a random order
  mutate(id = row_number()) %>%       # Generate sequential IDs
  select(id, category, prompt, expected_answer, human_error_rate, source) 

write.csv(eval_dataset, "eval_dataset.csv", row.names = FALSE)

