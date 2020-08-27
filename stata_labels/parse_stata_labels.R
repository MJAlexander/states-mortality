library(tidyverse)

# example coded data (fake, only 10 rows, only a few variables)
d <- read_csv("d.csv")

# read in the raw text
labels_raw <- read_file("gss_labels.txt")

# get into a tibble with a column for the variable name and the label definitions
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% # each def ends in ;
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% # split on the first space 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>% 
  select(variable_name, cases) %>% 
  drop_na()

# now split up the different parts of the label definition. 
# This is a vector where every second cell is the number code
label_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}")) 

## want to join the cases together in a string with case_when syntax
## here's a functioon that will do that

add_cw_text <- function(x,
                        y # variable name
                        ){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
  
}

# tibble with variable name and a string of the case_when statement linking codes to labels
cw_statements <- label_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)

# now we can apply these to the coded data by parsing out evaluating the case_when string
d %>% 
  mutate_at(.vars = vars(brthcan:marstat), 
            .funs = funs(labeled = eval(parse(text = cw_statements %>% 
                                      filter(variable_name==deparse(substitute(.))) %>% 
                                      select(cw_statement) %>% 
                                      pull())))) 

