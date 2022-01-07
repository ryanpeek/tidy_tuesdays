# ayso

library(tidyverse)
library(googlesheets4)
library(janitor)

# Googlesheets ------------------------------------------------------------


# auth
options(gargle_oauth_cache = here::here(".secrets"))

# auth for sheets
gs4_auth(email = "ryan.a.peek@gmail.com", cache = here::here(".secrets"),
         scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")




# Sheet -------------------------------------------------------------------

# plates
gs_id <- "1Sfr0xioFeWcX7JPAZVBke5Y52-ZjNEwoa8B2Noq95kY"
wb_rap <- gs4_get(gs_id)

(player_info <- googlesheets4::sheet_names(wb_rap)) # list sheets in wb


# players
players <- read_sheet(wb_rap, sheet = 1) %>% clean_names() %>% 
  rename(first_name = 9, last_name=10, pos1 = 14, pos2 = 15) %>% 
  mutate(bib = unlist(bib),
         pos1 = unlist(pos1),
         pos2 = unlist(as.character(pos2)),
         parent_guardian_phone_number = unlist(as.character(parent_guardian_phone_number)),
         ayso_rank = unlist(as.character(ayso_rank)))
str(players)
names(players)

# tryout_eval
eval <- read_sheet(wb_rap, sheet = 2) %>% clean_names()


# Select Data -------------------------------------------------------------

players_sel <- players %>% 
  select(first_name:last_name, bib:color_x_y, pos1,pos2, ayso_rank) %>% 
  mutate(pos1 = case_when(
    grepl("central midfielder", pos1, ignore.case = TRUE) ~ "CM",
    grepl("media|midfielder|mid-fielder|midfield|offense|mid field", pos1, ignore.case = TRUE) ~ "M",
    grepl("forward|striker", pos1, ignore.case = TRUE) ~ "Striker",
    grepl("defensa|Defense|Defender|back", pos1, ignore.case = TRUE) ~ "D",
    grepl("Goalie|Keeper|Goalkeeper", pos1, ignore.case = TRUE) ~ "GK",
    TRUE ~ "Undecided"
  ),
  pos2 = case_when(
    grepl("central midfielder", pos2, ignore.case = TRUE) ~ "CM",
    grepl("media|midfielder|mid-fielder|midfield|offense|mid field", pos2, ignore.case = TRUE) ~ "M",
    grepl("forward|striker", pos2, ignore.case = TRUE) ~ "Striker",
    grepl("defensa|Defense|Defender|back", pos2, ignore.case = TRUE) ~ "D",
    grepl("Goalie|Keeper|Goalkeeper", pos2, ignore.case = TRUE) ~ "GK",
    TRUE ~ "Undecided"
  ))

write_csv(players_sel, "~/Downloads/ayso_united_sel.csv")


