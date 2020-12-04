library(tidyverse)

input <- readLines("04/input.txt")
input

## append "" to final line to make it a complete case
input[length(input) + 1] <- ""

## collapse all to single row
input.list <- list()
tmp <- ""
for(i in 1:length(input)){
  ## stop and save when reach line break
  if(input[i] == ""){
    input.list[[length(input.list) + 1]] <- tmp
    tmp <- ""
  }
  
  ## if not a new person, append to existing line
  tmp <- ifelse(tmp == "",
                ## first entry
                input[i],
                ## subsequent entry
                paste(tmp, input[i]))
}
input.list

## build single row within each list object then bind together
out <- map(input.list, 
           ~ read.table(text=gsub(" ", "\n", .x), sep=":", comment.char="$") %>% 
             tidyr::pivot_wider(names_from = "V1", values_from = "V2") %>% 
             mutate_all(as.character)) %>% 
  ## imputes NAs for any missing
  ## leverage this to easily filter out rows missing obs
  bind_rows()

head(out)

#### PART ONE SOLUTION
out %>% 
  ## cid optional
  select(-cid) %>%
  drop_na() %>%
  nrow()
  
#### PART TWO SOLUTION
out %>% 
  ## cid still optional
  select(-cid) %>% 
  ## add data validation
  ## replace invalid with NA, then filter out
  mutate(
    byr = ifelse(byr >= 1920 & byr <= 2002, byr, NA_real_),
    iyr = ifelse(iyr >= 2010 & iyr <= 2020, iyr, NA_real_),
    eyr = ifelse(eyr >= 2020 & eyr <= 2030, eyr, NA_real_),
    hgt = case_when(
      str_detect(hgt, "cm") & 
        str_extract(hgt, "[:digit:]+") >= 150 & 
        str_extract(hgt, "[:digit:]+") <= 193 ~ hgt,
      str_detect(hgt, "in") & 
        str_extract(hgt, "[:digit:]+") >= 59 & 
        str_extract(hgt, "[:digit:]+") <= 76 ~ hgt,
      TRUE ~ NA_character_
    ), 
    hcl = ifelse(
      str_detect(hcl,"^#[0-9a-f]{6}$"), 
      hcl, NA_character_
    ),
    ecl = ifelse(
      ecl %in% c("amb","blu","brn","gry","grn","hzl","oth"),
      ecl, NA_character_
    ),
    pid = ifelse(
      str_detect(pid, "^[:digit:]{9}$"),
      pid, NA_character_
    )
  ) %>% 
  drop_na() %>% 
  nrow()


