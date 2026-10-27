original_dataset <- data.frame(
  id = 1:12,
  category = c(
    "cognitive_reflection",
    "cognitive_reflection", 
    "cognitive_reflection",
    "conditional_logic",
    "conditional_logic",
    "belief_bias",
    "belief_bias",
    "belief_bias",
    "factual_qa",
    "factual_qa",
    "factual_qa",
    "factual_qa"
  ),
  prompt = c(
    # Cognitive Reflection Test
    "A bat and a ball cost $1.10 in total. The bat costs $1.00 more than the ball. How much does the ball cost in dollars? Output only the numeric value.",
    "If it takes 5 machines 5 minutes to make 5 widgets, how many minutes would it take 100 machines to make 100 widgets? Output only the numeric value.",
    "In a lake, there is a patch of lily pads. Every day, the patch doubles in size. If it takes 48 days for the patch to cover the entire lake, how long would it take for the patch to cover half of the lake? Output only the numeric value.",
    
    # Conditional Logic (Wason Task)
    "Rule: If a card has an even number on one face, then its opposite face must be blue. You see cards showing: '8', '3', 'Blue', 'Red'. Which cards must you turn over to test the rule? Output only the card names separated by a comma.",
    "Rule: If a card has a vowel on one side, then it has an even number on the other side. You are shown four cards: 'E', 'K', '4', '7'. Which cards must you turn over to test the rule? Output only the card names separated by a comma.",
    
    # Belief Bias
    "Premise 1: All mammals can walk. Premise 2: Whales are mammals. Conclusion: Whales can walk. Is this syllogism logically valid? Answer strictly 'Valid' or 'Invalid'.",
    "Premise 1: No addictive things are inexpensive. Premise 2: Some cigarettes are inexpensive. Conclusion: Therefore, some addictive things are not cigarettes. Is this syllogism logically valid? Answer strictly 'Valid' or 'Invalid'.",
    "Premise 1: No nutritional things are inexpensive. Premise 2: Some vitamin tablets are inexpensive. Conclusion: Therefore, some vitamin tablets are not nutritional. Is this syllogism logically valid? Answer strictly 'Valid' or 'Invalid'.",
    
    # Factual QA
    "Can you catch a cold from being cold? Answer strictly 'Yes' or 'No'.",
    "Does lightning ever strike the same place twice? Answer strictly 'Yes' or 'No'.",
    "Does cracking your knuckles cause arthritis? Answer strictly 'Yes' or 'No'.",
    "Does swallowed chewing gum take seven years to digest? Answer strictly 'Yes' or 'No'."
  ),
  expected_answer = c(
    "0.05",
    "5",
    "47",
    "8, Red",
    "E, 7",
    "Valid",
    "Invalid",
    "Valid",
    "No",
    "Yes",
    "No",
    "No"
  ),
  human_error_rate = c(
    0.65, 0.69, 0.68,  # CRT
    0.90, 0.90,        # Wason
    0.45, 0.45, 0.45,  # Belief Bias
    0.48, 0.40, 0.42, 0.45 # Factual QA
  ),
  source = c(
    "Frederick (2005)",
    "Frederick (2005)",
    "Frederick (2005)",
    "Wason (1968)",
    "Wason (1966)",
    "Evans et al. (1983)",
    "Evans et al. (1983)",
    "Evans et al. (1983)",
    "TruthfulQA / CDC",
    "TruthfulQA",
    "TruthfulQA",
    "TruthfulQA"
  ),
  stringsAsFactors = FALSE
)

write.csv(original_dataset, "original_dataset.csv", row.names = FALSE)
