rm (list = ls())
require(tidyverse)

total_dr_files <- list()
gender_dr_files <- list()

for (q in  c(0.75, 0.85, 0.95)){
  # q <- 0.85
  data_path <- paste0("data/csv/MA-DR/", q)
  print(data_path)
  all_files <- dir(data_path, pattern = "*.csv") %>% as.data.frame() %>% 
    rename("fnames" = ".") # %>% filter(!str_detect(fnames, "75p_diseases"))
  
  total_pop_files <- all_files %>% filter(!str_detect(fnames, "75p_diseases") & !str_detect(fnames, "male")
  & !str_detect(fnames, "female"))
  
  total_pop_files <- all_files %>% filter(!str_detect(fnames, "75p_diseases") & !str_detect(fnames, "male")
                                          & !str_detect(fnames, "female"))
  
  gender_pop_files <- all_files %>% filter(!fnames %in% total_pop_files$fnames) %>% filter(!str_detect(fnames, "75p_diseases"))
  
  total_dr_files[[as.character(q)]] <- data_frame(filename = total_pop_files$fnames) %>% 
    mutate(file_contents = map(filename, ~ read_csv(file.path(data_path, .)))
    ) %>% mutate(filename = tools::file_path_sans_ext(filename), quantile = q) %>% unnest(cols = file_contents)
  
  gender_dr_files[[as.character(q)]] <- data_frame(filename = gender_pop_files$fnames) %>% 
    mutate(file_contents = map(filename, ~ read_csv(file.path(data_path, .)))
    ) %>% mutate(filename = tools::file_path_sans_ext(filename), quantile = q) %>% unnest(cols = file_contents)
  
}

write_csv(total_dr_files %>% purrr::map_dfr(rbind), "data/csv/MA-DR/combined_tables.csv")

write_csv(gender_dr_files %>% purrr::map_dfr(rbind), "data/csv/MA-DR/combined_tables_by_gender.csv")

