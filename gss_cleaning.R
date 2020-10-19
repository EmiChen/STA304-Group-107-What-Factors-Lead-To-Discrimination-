#### Workspace set-up ####
library(janitor)
library(tidyverse)

# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("AAIRofKs.csv")
dict <- read_lines("gss_dict.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("gss_labels.txt")


#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))
 
# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables
gss <- raw_data %>% 
  select(CASEID,
         agegr10,
         discrim,
         cbu_150,
         dur_110,
         drr_110,
         esc1_01,
         srh_115,
         whw_120,
         isl_100
         ) %>% 
  mutate_at(vars(agegr10:isl_100), .funs = funs(ifelse(.>=996, NA, .))) %>% 
  mutate_at(.vars = vars(agegr10:srh_115),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))

# Fix the names
gss <- gss %>% 
  clean_names() %>% 
  rename(if_cyber_bullying = cbu_150,
         if_school_attandence = esc1_01,
         if_discrim = discrim,
         if_mental_healthy = srh_115,
         if_drugs = dur_110,
         if_drink = drr_110,
         age = agegr10,
         number_relative_friends = isl_100,
         working_hour_weekly = whw_120,
         ) 

#### Clean up ####
    
gss <- gss %>% 
  mutate_at(vars(if_cyber_bullying:working_hour_weekly), 
            .funs = funs(ifelse(.=="Valid skip"|.=="Refusal"|.=="Not stated"|.=="Don't know", "NA", .))) 

gss <- gss %>% 
  mutate_at(vars(if_cyber_bullying), .funs = funs(case_when(
    .=="Yes"~1,
    .=="No"~0,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(if_discrim), .funs = funs(case_when(
    .=="Yes"~1,
    .=="No"~0,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(if_drink), .funs = funs(case_when(
    .=="Every day"~1,
    .=="4-6 times a week"~2,
    .=="2-3 times a week"~3,
    .=="Once a week"~4,
    .=="Once or twice in the past month"~5,
    .=="Not in the past month"~6,
    .=="Never drinks"~7,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(if_school_attandence), .funs = funs(case_when(
    .=="Yes"~1,
    .=="No"~0,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(if_drugs), .funs = funs(case_when(
    .=="Yes"~1,
    .=="No"~0,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(if_mental_healthy), .funs = funs(case_when(
    .=="Excellent"~1,
    .=="Very good"~2,
    .=="Good"~3,
    .=="Fair"~4,
    .=="Poor"~5,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(age), .funs = funs(case_when(
    .=="15 to 24"~1,
    .=="25 to 34"~2,
    .=="35 to 44"~3, 
    .=="45 to 54"~4,
    .=="55 to 64"~5,
    .=="65 to 74"~6,
    .=="75 years and older"~7,
    .=="NA"~as.numeric(NA)
  )))

write_csv(gss, "gss.csv")
