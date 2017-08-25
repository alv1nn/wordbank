library(tidyverse)
library(feather)

### Phonetic Distance
phons <- read_csv("assocs/phonoData.csv")
phon_dict <- read_csv("assocs/phonoDict.csv")

items <- get_item_data()

fix_phons <- phons %>%
  select(-X1) %>%
  group_by(language) %>%
  filter(W2 > W1) %>%
  left_join(phon_dict, by = c("W1" = "definition_phono", "language")) %>%
  select(-W1) %>%
  rename(W1 = definition_wb) %>%
  left_join(phon_dict, by = c("W2" = "definition_phono", "language")) %>%
  select(-W2) %>%
  rename(W2 = definition_wb) %>%
  arrange(language,W1,W2,PhonoDist) %>%
  distinct(language, W1, W2, PhonoDist) 

write_feather(fix_phons, "assocs/phons.feather")

### McRae norms
assocs <- read_csv("assocs/assoc_mat.csv") %>%
  gather(out_node, McRae_all, -in_node)

p_assocs <- read_csv("assocs/p_assoc_mat.csv") %>%
  gather(out_node, McRae_p, -in_node)

c_assocs <- read_csv("assocs/c_assoc_mat.csv") %>%
  gather(out_node, McRae_c, -in_node)

mcrae <- left_join(assocs, c_assocs) %>%
  left_join(p_assocs) %>%
  filter(in_node != out_node) %>%
  rename(W1 = in_node, W2 = out_node) 

eng_items <- items %>%
  filter(language == "English (American)") %>%
  select(definition, uni_lemma) %>%
  distinct(uni_lemma, .keep_all = TRUE) %>%
  filter(!is.na(uni_lemma))

recode <- function(word){
  if(word == "chicken") "chicken (animal)"
  else if(word == "fish") "fish (animal)"
  else if(word == "orange") "orange (food)"
  else if(word == "dress") "dress (object)"
  else if(word == "church") "church*"
  else word
}

mcrae <- mcrae %>%
  rowwise() %>%
  mutate(W1 = recode(W1), W2 = recode(W2))

left_join(mcrae, eng_items, by = c("W1" = "definition")) %>%
  rename(U1 = uni_lemma) %>%
  left_join(eng_items, by = c("W2" = "definition")) %>%
  rename(U2 = uni_lemma) %>%
  select(U1, U2, McRae_all, McRae_c, McRae_p) %>%
  write_feather("assocs/mcrae_uni.feather")


## W2V
w2v <- read_csv("assocs/word2vec.csv")

w2v_dict <- read_csv("assocs/vocab_wb.csv") %>%
  select(words_wb, words)

w2v_out <- w2v %>%
  left_join(w2v_dict, by = c("word1" = "words")) %>%
  rename(W1 = words_wb) %>%
  select(-word1) %>%
  left_join(w2v_dict, by = c("word2" = "words")) %>%
  rename(W2 = words_wb) %>%
  select(-word2) %>%
  mutate(language = "English (American)") %>%
  select(language, W1, W2, CosSim)

write_feather(w2v_out, "assocs/w2v.feather")
  

