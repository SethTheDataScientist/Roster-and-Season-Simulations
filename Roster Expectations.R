##REQUIRES NFL WAR SETUP

#RosterTest <- nflreadr::load_participation(seasons = c(2021))

Roster22 <-  load_rosters(2022)


setwd("C:/Users/sethl/OneDrive/Excel Files/Football/Draft/WAR")

Draft_WAR_add <- read_xlsx("NFL WAR Draft Chart.xlsm")

NFLWARDraftData <- read_xlsx("NFL WAR Data.xlsx")

draftpicks <- load_draft_picks(2022) %>% 
  left_join(Draft_WAR_add, by = c("pick" = "Pick Number"))


setwd("C:/Users/sethl/OneDrive/Important Stuff/R/R files/NFL")

setwd("C:/Users/sethl/OneDrive/Excel Files/Football/Data Csv/Misc")


CoachingChanges1999 <- read_xlsx("NFL Coaching Changes 2011-2020.xlsm")

setwd("C:/Users/sethl/OneDrive/Important Stuff/R/R files/NFL")

# Updating Analytical Win Loss Record ------------------------------------------------


WinLossUpdating <- pbpCurrent %>% 
  filter(!is.na(posteam), posteam != "",
         posteam != "*CLE*") %>% 
  mutate(Conference = case_when(posteam == home_team & HomeDivision == "NFCW" ~ "NFC",
                                posteam == home_team & HomeDivision == "NFCE" ~ "NFC",
                                posteam == home_team & HomeDivision == "NFCN" ~ "NFC",
                                posteam == home_team & HomeDivision == "NFCS" ~ "NFC",
                                posteam == away_team & AwayDivision == "NFCW" ~ "NFC",
                                posteam == away_team & AwayDivision == "NFCE" ~ "NFC",
                                posteam == away_team & AwayDivision == "NFCN" ~ "NFC",
                                posteam == away_team & AwayDivision == "NFCS" ~ "NFC",
                                T ~ "AFC" ))%>% 
  group_by(season, game_id) %>% 
  mutate(MaxLead = max(score_differential_post, na.rm = T),
         FinalScore = tail(score_differential_post, 1)) %>% 
  group_by(season, posteam, game_id) %>%  
  mutate(OffWPcutoff2 = if_else(median(wp[game_half=="Half2"], na.rm = T) >= 0.9 |
                                  mean(wp[game_half=="Half2"], na.rm = T) >= 0.9,
                                1, 0),
         OffWPcutoff3 = if_else(median(wp[game_half=="Half2"], na.rm = T) <= 0.15 |
                                  mean(wp[game_half=="Half2"], na.rm = T) <= 0.15 ,
                                1, 0)) %>% 
  group_by(season, defteam, game_id) %>%  
  mutate(DefWPcutoff2 = if_else(median(wp[game_half=="Half2"], na.rm = T) <= 0.1 |
                                  mean(wp[game_half=="Half2"], na.rm = T) <= 0.1,
                                1, 0),
         DefWPcutoff3 = if_else(median(wp[game_half=="Half2"], na.rm = T) >= 0.85 |
                                  mean(wp[game_half=="Half2"], na.rm = T) >= 0.85 ,
                                1, 0)) %>% 
  slice_tail(n = 1) %>%
  group_by(season, week, posteam) %>% 
  summarise(PosteamHome = case_when(posteam == home_team ~ 1,
                                    posteam == away_team ~ 0,
                                    T ~ 0),
            season_type = head(season_type, 1),
            Conference = head(Conference, 1),
            defteam = head(defteam, 1),
            Counts = n(),
            Wins = sum(PosteamWin, na.rm = T),
            Losses = sum(if_else(PosteamWin == 0, 1, 0), na.rm = T),
            PosteamAna16WPwins = sum(case_when(PosteamWin == 1 & abs(FinalScore) > 8 ~ 0.875,
                                               OffWPcutoff2 == 1 ~ 0.875,
                                               PosteamWin == 1 & MaxLead > 16 ~ 0.75,
                                               OffWPcutoff3 == 1 ~ -0.125,
                                               PosteamWin == 0 & abs(FinalScore) > 8 ~ -0.125,
                                               PosteamWin == 1 & abs(FinalScore) <= 8 ~ 0.425,
                                               PosteamWin == 0 & abs(FinalScore) <= 8 ~ 0.55)),
            DefteamAna16WPwins = sum(case_when(PosteamWin == 0 & abs(FinalScore) > 8 ~ 0.875,
                                               DefWPcutoff2 == 1 ~ 0.875,
                                               PosteamWin == 0 & MaxLead > 16 ~ 0.75,
                                               DefWPcutoff3 == 1 ~ -0.125,
                                               PosteamWin == 1 & abs(FinalScore) > 8 ~ -0.125,
                                               PosteamWin == 0 & abs(FinalScore) <= 8 ~ 0.425,
                                               PosteamWin == 1 & abs(FinalScore) <= 8 ~ 0.55))) %>% 
  group_by(season, posteam)  %>% 
  arrange((week)) %>% 
  mutate(PosteamValue = cumsum(PosteamAna16WPwins)) %>% 
  group_by(season, defteam) %>% 
  arrange((week)) %>% 
  mutate(DefteamValue = cumsum(DefteamAna16WPwins)) %>% 
  group_by(season, week) %>% 
  mutate(PRPosValue = percent_rank(PosteamValue),
         PRDefValue = percent_rank(DefteamValue)) %>% 
  group_by(season, posteam) %>% 
  mutate(PosAdjustment = case_when(week < 4 ~ PosteamAna16WPwins * 0.5,
                                   T ~ PosteamAna16WPwins * PRDefValue),
         PosteamADJValue = cumsum(PosAdjustment)*2,
         DefAdjustment = case_when(week < 4 ~ DefteamAna16WPwins * 0.5,
                                   T ~ DefteamAna16WPwins * PRPosValue),
         DefteamADJValue = cumsum(DefAdjustment)*2,
         Games = n(),
         Slope = round(tail(PosteamADJValue, 1)/n(),2),
         Wins = cumsum(Wins),
         Season = as.double(paste0(substr(season, 3,4)))) %>%
  group_by(season, posteam) %>% 
  slice_tail(n = 1) 



# Expected WAR Coachinhg -------------------------------------------------


RosterFill <- Roster22 %>% 
  filter(season == 2022 & !is.na(team), status == "Active", !is.na(pff_id)) %>% 
  select(season, team, full_name, first_name, last_name, pff_id, position) %>% 
  mutate(Season = as.double(paste0(substr(season, 2, 4))),
         rosterpos = position,
         pff_id = as.double(pff_id),
         QBname = paste0(substr(first_name, 1, 1),".",last_name)) %>% 
  select(!position) 

WARnewFill <- NFLWARDraftData %>% 
  full_join(RosterFill, by = c("season", "team_name" = "team",
                               "player_id" = "pff_id", "player" = "full_name")) %>% 
  mutate(position = case_when(is.na(position) == 1 ~ rosterpos,
                              T ~ position))

DraftAddFill <- draftpicks %>% 
  group_by(season, team) %>% 
  summarise(DraftPickWAR = sum(`Regressed Mean 4-Year WAR`, na.rm = T)/4)


CoachWinsOverExpected <- WinLossUpdating %>% 
  left_join(CoachingChanges1999, by = c("season", "posteam" = "Team")) %>% 
  select(season, posteam,  HC, PosteamADJValue) %>% 
  left_join(QBWARJoin, by = c("season", "posteam" = "team_name")) %>% 
  left_join(WRWARJoin, by = c("season", "posteam" = "team_name")) %>% 
  left_join(RBWARJoin, by = c("season", "posteam" = "team_name")) %>% 
  left_join(OLWARJoin, by = c("season", "posteam" = "team_name")) %>% 
  left_join(DefWARJoin, by = c("season", "posteam" = "team_name")) %>% 
  group_by(season, posteam) %>% 
  slice_head(n = 1) %>% 
  group_by() %>% 
  mutate(RosterWAR = RecTeamWAR + RBTeamWAR + OLTeamWAR + DefTeamWAR) %>% 
  select(season, posteam, HC, player.x, PosteamADJValue, RosterWAR, WAR.x,
         RecTeamWAR, RBTeamWAR, OLTeamWAR, DefTeamWAR) %>% 
  mutate(CWOE = PosteamADJValue - RosterWAR - WAR.x) %>%   
  group_by() %>% 
  mutate(TotalAvg = mean(CWOE, na.rm = T),
         TotalMed = median(CWOE, na.rm = T)) %>%
group_by(HC) %>%
  arrange(desc(season)) %>% 
  slice_head(n = 5) %>% 
  summarise(lastTeam = head(posteam, 1),
            lastSeason = head(season, 1),
            Seasons = n(),
            AvgCWOE = mean(CWOE, na.rm = T),
            MedCWOE = median(CWOE, na.rm = T),
            TotalAvg = head(TotalAvg, 1),
            TotalMed = head(TotalMed, 1))

CWOE <-  CoachingChanges1999 %>% 
  filter(season == 2022) %>% 
  left_join(CoachWinsOverExpected, by = c("HC")) %>% 
  mutate(TotalAvg = mean(TotalAvg, na.rm = T)/2,
         TotalMed = mean(TotalMed, na.rm = T)/2,
         AvgCWOE = case_when(is.na(AvgCWOE) == 1 ~ TotalAvg*0.75,
                             T ~ AvgCWOE)/2,
         MedCWOE = case_when(is.na(MedCWOE) == 1 ~ TotalMed*0.75,
                             T ~ MedCWOE)/2)


QBTierWAR <- QBtiers %>% 
  mutate(season = case_when(QBname == "D.Watson" & team_name == "HOU" &
                              season == 2020 ~ 2021,
                            T ~ season),
         team_name = case_when(QBname == "B.Mayfield" ~ "CAR",
                               QBname == "D.Watson" ~ "CLE",
                               QBname == "M.Trubisky" ~ "PIT",
                               QBname == "C.Wentz" ~ "WAS",
                               QBname == "M.Ryan" ~ "IND",
                               QBname == "R.Wilson" ~ "DEN",
                               QBname == "D.Lock" ~ "SEA",
                               QBname == "M.Mariota" ~ "ATL",
                               T ~ team_name)) %>% 
  group_by(season, team_name, QBname) %>% 
  summarise(PRFloor = PRFloor,
            PRCeiling = PRCeiling,
            Value = 3.814*PRFloor + 0.509*PRCeiling,
            FullValue = 0.820 * PRFloor - 0.066 * PRCeiling +
              3.78*PREPA - 0.019 * PRWar) %>% 
  filter(QBname == "D.Watson" | QBname == "L.Jackson" |
           QBname == "J.Burrow" | QBname == "M.Trubisky" |
           QBname == "J.Allen" | QBname == "Z.Wilson" |
           QBname == "M.Jones" | QBname == "T.Tagovailoa" |
           QBname == "R.Tannehill" | QBname == "M.Ryan" |
           QBname == "T.Lawrence" | QBname == "D.Mills" |
           QBname == "P.Mahomes" | QBname == "J.Herbert" |
           QBname == "D.Carr" | QBname == "R.Wilson" |
           QBname == "A.Rodgers" | QBname == "J.Fields" |
           QBname == "K.Cousins" | QBname == "J.Goff" |
           QBname == "D.Prescott" | QBname == "C.Wentz" |
           QBname == "D.Jones" | QBname == "J.Hurts" |
           QBname == "T.Brady" | QBname == "M.Mariota" |
           QBname == "J.Winston" | QBname == "B.Mayfield" |
           QBname == "K.Murray" | QBname == "M.Stafford" |
           QBname == "D.Lock" | QBname == "T.Lance" ) %>%
  group_by(QBname) %>% 
  arrange(desc(Value)) %>% 
  arrange(desc(season)) %>% 
  slice_head(n= 1) %>% 
  group_by() %>% 
  select(!season) %>% 
  mutate(season = 2022)

ExWARteamall1GroupFillTEST <- WARnewFill %>% 
  mutate(Warsnap = WAR/Snaps,
         position = case_when(is.na(position) ~ rosterpos,
                              T ~ position)) %>% 
  group_by(player_id)  %>% 
  arrange(desc(season)) %>% 
  mutate(TotalSeasons = n(),
         Year = TotalSeasons - (seq(1, n(), 1)-1),
         ExpectWarsnap = (lead(Warsnap, n = 1)+ 
                            lead(Warsnap, n = 2))/2,
         ExpectWarsnap = case_when(is.na(ExpectWarsnap) == 1 & lead(Warsnap, n = 1) <= 0 ~ 
                                     (lead(Warsnap, n = 1)*-0.25),
                                   is.na(ExpectWarsnap) == 1 & lead(Warsnap, n = 1) > 0 ~ 
                                     (lead(Warsnap, n = 1)*0.75),
                                   T ~ ExpectWarsnap),
         ExpectWarsnap = case_when(is.na(ExpectWarsnap) == 1 ~ Warsnap,
                                   T ~ ExpectWarsnap),
         ExpectedSnaps = mean(Snaps[season >= 2019], na.rm = T),
         ExpectedWAR = ExpectWarsnap * ExpectedSnaps,
         ExpectedWAR = case_when(player_id == 40485 ~ ExpectedWAR/3,
                                 T ~ ExpectedWAR),
         ExpectWar = ExpectWarsnap * Snaps,
         WARDiff = WAR - ExpectWar,
         MaxQBWAR = max(Warsnap[position == "QB" & season >= 2018],
                        na.rm = T)*ExpectedSnaps) %>% 
  arrange(desc(position)) %>% 
  mutate(position = head(position, 1)) %>% 
  arrange(desc(ExpectedWAR)) %>% 
  distinct() %>% 
  left_join(PosDevelopmentYEAR, by = c("position", "Year")) %>% 
  left_join(PosDevelopmentVALUE, by = c("position")) %>% 
  mutate(Development = case_when(Year <= 5 & 
                                   ExpectedWAR <= ExpectedWARPos & 
                                   ExpectedWAR <= 0 &
                                   ExpectedWAR <= ExpectedWARPosPR0.3 ~ 
                                   ExpectedWARPosPR0.3,
                                 Year <= 5 & 
                                   ExpectedWAR <= ExpectedWARPos &
                                   ExpectedWAR > 0 &
                                   ExpectedWAR <= ExpectedWARPosPR0.5 ~ 
                                   ExpectedWARPosPR0.5,
                                 Year <= 5 & 
                                   ExpectedWAR <= ExpectedWARPos &
                                   ExpectedWAR > ExpectedWARPosPR0.3 &
                                   ExpectedWAR <= ExpectedWARPosPR0.5 ~ 
                                   ExpectedWARPosPR0.5,
                                 Year <= 5 & Year > 2 &
                                   ExpectedWAR <= ExpectedWARPos & 
                                   ExpectedWAR > ExpectedWARPosPR0.5 &
                                   ExpectedWAR <= ExpectedWARPosPR0.7~ 
                                   ExpectedWARPosPR0.7,
                                 T ~ 0),
         ExpectedWAR = case_when(Development >= ExpectedWARPos &
                                   Development != 0 ~ ExpectedWARPos,
                                 T ~ ExpectedWAR)) %>% 
  group_by(season, team_name)  %>% 
  left_join(QBTierWAR, by = c("season", "team_name"))   %>% 
  distinct() %>% 
  summarise(Season = as.double(paste0(substr(season,3,4))),
            ExpectWAR = sum(ExpectedWAR[position != "QB" & ExpectedSnaps >= 250], na.rm =T) +
              max(ExpectedWAR[position == "QB"], na.rm =T),
            ValueExpectWAR =  sum(ExpectedWAR[position != "QB" & ExpectedSnaps >= 250], na.rm =T) +
              Value[1],
            FullValueExpectWAR =  sum(ExpectedWAR[position != "QB" & ExpectedSnaps >= 250], na.rm =T) +
              FullValue[1],
            MaxExpectWAR = sum(ExpectedWAR[position != "QB" & ExpectedSnaps >= 250], na.rm =T) +
              max(MaxQBWAR[position == "QB"], na.rm =T), 
            NonQBEWAR = sum(ExpectedWAR[position != "QB" & ExpectedSnaps >= 250], na.rm =T),
            QBEWAR = case_when(max(ExpectedWAR[position == "QB"], na.rm =T) == -Inf ~ 
                                 sum(ExpectedWAR[position == "QB"], na.rm =T),
                               T ~ max(ExpectedWAR[position == "QB"], na.rm =T)),
            QBValueExpectation = Value[1],
            QBFullValueExpectation = FullValue[1],
            TopEWAR = max(ExpectedWAR, na.rm = T),
            TopEWARPlayer = head(player, 1),
            TopEWARPos = head(position, 1)
  )  %>% 
  filter(season == 2022) %>% 
  slice_head(n = 1) %>% 
  left_join(DraftAddFill, by  = c("season", "team_name" = "team")) %>% 
  distinct() %>%  
  group_by() %>% 
  mutate(Conference = case_when(team_name == "ARI" ~ "NFC",
                                team_name == "SF" ~ "NFC",
                                team_name == "LA" ~ "NFC",
                                team_name == "SEA" ~ "NFC",
                                team_name == "MIN" ~ "NFC",
                                team_name == "CHI" ~ "NFC",
                                team_name == "GB" ~ "NFC",
                                team_name == "DET" ~ "NFC",
                                team_name == "DAL" ~ "NFC",
                                team_name == "PHI" ~ "NFC",
                                team_name == "WAS" ~ "NFC",
                                team_name == "NYG" ~ "NFC",
                                team_name == "TB" ~ "NFC",
                                team_name == "CAR" ~ "NFC",
                                team_name == "NO" ~ "NFC",
                                team_name == "ATL" ~ "NFC",
                                T  ~ "AFC")) %>% 
  left_join(CWOE, by = c("season", "team_name" = "Team")) %>% 
  mutate(FinalValueExpect = ValueExpectWAR + AvgCWOE + DraftPickWAR,
         FinalFullValueExpect = FullValueExpectWAR + AvgCWOE + DraftPickWAR,
         MaxFinalExpect = MaxExpectWAR + MedCWOE + DraftPickWAR) %>% 
  select(Season, season, Conference, team_name, HC, OC,
         ExpectWAR, ValueExpectWAR, FullValueExpectWAR,
         MaxExpectWAR, NonQBEWAR, QBEWAR,
         QBValueExpectation, QBFullValueExpectation, TopEWAR,
         TopEWARPlayer, TopEWARPos,
         DraftPickWAR, AvgCWOE, MedCWOE, FinalValueExpect,
         FinalFullValueExpect, MaxFinalExpect
         ) 

write_xlsx(ExWARteamall1, path = "EXWar team Final With updated QBs.xlsx")



ggplot(ExWARteamall1, aes(x = FinalExpect, y = MaxFinalExpect))+
  facet_wrap(~Conference)+
  geom_point(aes(color = case_when(FinalExpect >= 5 & MaxFinalExpect >= 5 ~ "blue",
                                   FinalExpect >= 5 & MaxFinalExpect < 5 ~ "green",
                                   FinalExpect < 5 & MaxFinalExpect >= 5 ~ "orange",
                                   FinalExpect < 5 & MaxFinalExpect <5 ~ "red"),
                 fill = case_when(FinalExpect >= 5 & MaxFinalExpect >= 5 ~ "blue",
                                  FinalExpect >= 5 & MaxFinalExpect < 5 ~ "green",
                                  FinalExpect < 5 & MaxFinalExpect >= 5 ~ "orange",
                                  FinalExpect < 5 & MaxFinalExpect <5 ~ "red")),
             shape = case_when(ExWARteamall1$MaxFinalExpect  >= 5 ~ 24,
                               ExWARteamall1$MaxFinalExpect  < 5 ~ 21,
                               T ~ 1),
             size = 3)+
  ggrepel::geom_text_repel(aes(label = team_name), size = 4, box.padding = 0.2,
                           force = 5, max.overlaps = Inf,
                           min.segment.length = 0)+
  geom_vline(xintercept = 5, linetype = "dashed")+
  geom_hline(yintercept = 5, linetype = "dashed")+
  scale_colour_identity(aesthetics = c("fill", "color"))+
  theme_reach()+
  labs(title = "Final Expected Roster WAR vs Max QB Expected Roster WAR",
       subtitle = "Triangle represents teams with Total Roster EWAR >= 5,
        Same Roster Expectations, but take Max QB WAR as new Expectation",
       x = "Final Expected Roster WAR",
       y = "Max QB Expected Roster WAR")
  ggsave("2021 Expected WAR .png", height =8, width = 13, units = "in", dpi = 350)




ggplot(ExWARteamall1, aes(x = NonQBEWAR, y = FinalExpect))+
  geom_point(aes(color = case_when(NonQBEWAR >= 1.9 & (MaxExpectWAR - NonQBEWAR) >= 2 ~ "blue",
                                   NonQBEWAR >= 1.9 & (MaxExpectWAR - NonQBEWAR) < 2 ~ "green",
                                   NonQBEWAR < 1.9 & (MaxExpectWAR - NonQBEWAR) >= 2 ~ "orange",
                                   NonQBEWAR < 1.9 & (MaxExpectWAR - NonQBEWAR) <2 ~ "red"),
                 fill = case_when(NonQBEWAR >= 1.9 & (MaxExpectWAR - NonQBEWAR) >= 2 ~ "blue",
                                  NonQBEWAR >= 1.9 & (MaxExpectWAR - NonQBEWAR) < 2 ~ "green",
                                  NonQBEWAR < 1.9 & (MaxExpectWAR - NonQBEWAR) >= 2 ~ "orange",
                                  NonQBEWAR < 1.9 & (MaxExpectWAR - NonQBEWAR) <2 ~ "red")),
             shape = case_when(ExWARteamall1$FinalExpect  >= 5 ~ 24,
                               ExWARteamall1$FinalExpect  < 5 ~ 21,
                               T ~ 1),
             size = 3)+
  geom_hline(yintercept = 5, linetype = "dashed")+
  scale_colour_identity(aesthetics = c("fill", "color"))+
  theme_reach()+
  labs(title = "Season Success vs Final Expected WAR",
       subtitle = "Triangle represents teams with Total Roster EWAR >= 5,
           Color is Ratio of Max QB to Roster WAR, Blue > Green > Yellow > Red,
           Success is Wins (REG+POST) with additions related to Playoff Success",
       x = "Season Success",
       y = "Final Expected Roster WAR")
  ggsave("Roster vs Wins .png", height =8, width = 13, units = "in", dpi = 350)


# Positional Development Curve Adjustments --------------------------------

  PosDevelopmentVALUE <- WARnewFill %>% 
    mutate(Warsnap = WAR/Snaps,
           position = case_when(is.na(position) ~ rosterpos,
                                T ~ position)) %>% 
    group_by(player_id)  %>% 
    arrange(desc(season)) %>% 
    mutate(TotalSeasons = n(),
           Year = TotalSeasons - (seq(1, n(), 1)-1),
            ExpectWarsnap = (lead(Warsnap, n = 1)+ 
                              lead(Warsnap, n = 2))/2,
           ExpectWarsnap = case_when(is.na(ExpectWarsnap) == 1 & lead(Warsnap, n = 1) <= 0 ~ 
                                       (lead(Warsnap, n = 1)*-0.25),
                                     is.na(ExpectWarsnap) == 1 & lead(Warsnap, n = 1) > 0 ~ 
                                       (lead(Warsnap, n = 1)*0.75),
                                     T ~ ExpectWarsnap),
           ExpectWarsnap = case_when(is.na(ExpectWarsnap) == 1 ~ Warsnap,
                                     T ~ ExpectWarsnap),
           ExpectedSnaps = mean(Snaps[season >= 2019], na.rm = T),
           ExpectedWAR = ExpectWarsnap * ExpectedSnaps,
           ExpectedWAR = case_when(player_id == 40485 ~ ExpectedWAR/3,
                                   T ~ ExpectedWAR),
           ExpectWar = ExpectWarsnap * Snaps,
           WARDiff = WAR - ExpectWar,
           MaxQBWAR = max(Warsnap[position == "QB" & season >= 2018],
                          na.rm = T)*ExpectedSnaps) %>% 
    arrange(desc(position)) %>% 
    mutate(position = head(position, 1)) %>% 
    arrange(desc(ExpectedWAR)) %>% 
    distinct() %>% 
    group_by(position) %>% 
    summarise(OutputWAR = ExpectedWAR,
              ExpectedWARPosPR = percent_rank(ExpectedWAR)) %>% 
    filter(!is.na(ExpectedWARPosPR))%>% 
    distinct() %>% 
    summarise(ExpectedWARPosPR0.3 = OutputWAR[ExpectedWARPosPR == min(Closest(ExpectedWARPosPR, 0.3, na.rm = T))],
           ExpectedWARPosPR0.5 = OutputWAR[ExpectedWARPosPR == min(Closest(ExpectedWARPosPR, 0.5, na.rm = T))],
           ExpectedWARPosPR0.7 = OutputWAR[ExpectedWARPosPR == min(Closest(ExpectedWARPosPR, 0.7, na.rm = T))]) 
    filter(ExpectedWARPosPR == ExpectedWARPosPR0.3 |
             ExpectedWARPosPR == ExpectedWARPosPR0.5 |
             ExpectedWARPosPR == ExpectedWARPosPR0.7 )
    group_by(position, Year) %>% 
    summarise(ExpectedWARPos = mean(ExpectedWAR, na.rm = T)) 
    
    
    PosDevelopmentTEST <- WARnewFill %>% 
      mutate(Warsnap = WAR/Snaps,
             position = case_when(is.na(position) ~ rosterpos,
                                  T ~ position)) %>% 
      group_by(player_id)  %>% 
      arrange(desc(season)) %>% 
      mutate(TotalSeasons = n(),
             Year = TotalSeasons - (seq(1, n(), 1)-1),
             ExpectWarsnap = (lead(Warsnap, n = 1)+ 
                                lead(Warsnap, n = 2))/2,
             ExpectWarsnap = case_when(is.na(ExpectWarsnap) == 1 & lead(Warsnap, n = 1) <= 0 ~ 
                                         (lead(Warsnap, n = 1)*-0.25),
                                       is.na(ExpectWarsnap) == 1 & lead(Warsnap, n = 1) > 0 ~ 
                                         (lead(Warsnap, n = 1)*0.75),
                                       T ~ ExpectWarsnap),
             ExpectWarsnap = case_when(is.na(ExpectWarsnap) == 1 ~ Warsnap,
                                       T ~ ExpectWarsnap),
             ExpectedSnaps = mean(Snaps[season >= 2019], na.rm = T),
             ExpectedWAR = ExpectWarsnap * ExpectedSnaps,
             ExpectedWAR = case_when(player_id == 40485 ~ ExpectedWAR/3,
                                     T ~ ExpectedWAR),
             ExpectWar = ExpectWarsnap * Snaps,
             WARDiff = WAR - ExpectWar,
             MaxQBWAR = max(Warsnap[position == "QB" & season >= 2018],
                            na.rm = T)*ExpectedSnaps) %>% 
      arrange(desc(position)) %>% 
      mutate(position = head(position, 1)) %>% 
      arrange(desc(ExpectedWAR)) %>% 
      distinct() %>% 
      left_join(PosDevelopmentYEAR, by = c("position", "Year")) %>% 
      left_join(PosDevelopmentVALUE, by = c("position")) %>% 
      mutate(Development = case_when(Year <= 5 & 
                              ExpectedWAR <= ExpectedWARPos & 
                                ExpectedWAR <= 0 &
                                ExpectedWAR <= ExpectedWARPosPR0.3 ~ 
                                ExpectedWARPosPR0.3,
                              Year <= 5 & 
                                ExpectedWAR <= ExpectedWARPos &
                                ExpectedWAR > 0 &
                                ExpectedWAR <= ExpectedWARPosPR0.5 ~ 
                                ExpectedWARPosPR0.5,
                              Year <= 5 & 
                                ExpectedWAR <= ExpectedWARPos &
                                ExpectedWAR > ExpectedWARPosPR0.3 &
                                ExpectedWAR <= ExpectedWARPosPR0.5 ~ 
                                ExpectedWARPosPR0.5,
                              Year <= 5 & Year > 2 &
                                ExpectedWAR <= ExpectedWARPos & 
                                ExpectedWAR > ExpectedWARPosPR0.5 &
                                ExpectedWAR <= ExpectedWARPosPR0.7~ 
                                ExpectedWARPosPR0.7,
                              T ~ 0),
             ExpectedWAR = case_when(Development >= ExpectedWARPos &
                                        Development != 0 ~ ExpectedWARPos,
                                     T ~ Development))
    
    

# Pos group Values --------------------------------------------------------

  
  ExWARteamall1GroupFill <- WARnewFill %>% 
    mutate(Warsnap = WAR/Snaps,
           position = case_when(is.na(position) ~ rosterpos,
                                T ~ position)) %>% 
    group_by(player_id)  %>% 
    arrange(desc(season)) %>% 
    mutate(ExpectWarsnap = (lead(Warsnap, n = 1)+ 
                              lead(Warsnap, n = 2))/2,
           ExpectWarsnap = case_when(is.na(ExpectWarsnap) == 1 & lead(Warsnap, n = 1) <= 0 ~ 
                                       (lead(Warsnap, n = 1)*-0.25),
                                     is.na(ExpectWarsnap) == 1 & lead(Warsnap, n = 1) > 0 ~ 
                                       (lead(Warsnap, n = 1)*0.75),
                                     T ~ ExpectWarsnap),
           ExpectWarsnap = case_when(is.na(ExpectWarsnap) == 1 ~ Warsnap,
                                     T ~ ExpectWarsnap),
           ExpectedSnaps = mean(Snaps[season >= 2019], na.rm = T),
           ExpectedWAR = ExpectWarsnap * ExpectedSnaps,
           ExpectedWAR = case_when(player_id == 40485 ~ ExpectedWAR/3,
                                   T ~ ExpectedWAR),
           ExpectWar = ExpectWarsnap * Snaps,
           WARDiff = WAR - ExpectWar,
           MaxQBWAR = max(Warsnap[position == "QB" & season >= 2018],
                          na.rm = T)*ExpectedSnaps) %>% 
    arrange(desc(position)) %>% 
    mutate(position = head(position, 1)) %>% 
    arrange(desc(ExpectedWAR)) %>% 
    group_by(season, team_name)  %>%  
    distinct()   %>% 
    filter(season >= 2021) %>% 
    mutate(Posgroup = case_when(position == "QB" ~ "QB",
                                position == "OT" |
                                  position == "G" |
                                  position == "C" ~ "OL",
                                position == "WR" | 
                                  position == "TE" |
                                  position == "HB" ~ "Skill",
                                position == "CB" |
                                  position == "S" | 
                                  position == "DB" ~ "DB",
                                position == "LB" ~ "LB",
                                position == "DI" |
                                  position == "ED" |
                                  position == "DE" | 
                                  position == "DT" ~ "DL")) %>% 
    group_by(season, team_name, Posgroup) %>% 
    summarise(ExpectedWAR = sum(ExpectedWAR, na.rm = T),
              ActualWAR = sum(WAR, na.rm = T),
              Diff = ActualWAR - ExpectedWAR) %>% 
    group_by(season, Posgroup) %>% 
    arrange((ExpectedWAR)) %>% 
    mutate(Rank = (n()+1) - rank(ExpectedWAR)) %>% 
    group_by(season, team_name) %>% 
    mutate(MyValue = sum(ExpectedWAR[Posgroup == "DB" |
                                       Posgroup == "Skill" 
                                     & season == 2022], na.rm = T))
  
  