EliteRosters <- IndivWARData %>% 
  group_by(player_id) %>% 
  arrange(desc(season)) %>% 
  slice_head(n = 3) %>% 
  mutate(RecentAvg = mean(WARTile)) %>% 
  filter(season == 2022) %>% 
  group_by(team_name) %>% 
  mutate(Above80 = if_else(RecentAvg >= 0.8, 1, 0),
         Above90 = if_else(RecentAvg >= 0.9, 1, 0),
         Above95 = if_else(RecentAvg >= 0.95, 1, 0),
         EliteQB = if_else(Above80 == 1 & position == "QB", 1, 0)) %>% 
  arrange(desc(RecentAvg)) %>% 
  summarise(Above80 = sum(Above80),
            Above90 = sum(Above90),
            Above95 = sum(Above95),
            EliteQB = sum(EliteQB),
            TopPlayer = head(player, 1))


Adot <- pbp2022 %>% 
  filter(season == 2022, posteam == "BUF", pass == 1, !is.na(air_yards), down != 4) %>% 
  group_by(week) %>% 
  summarise(adot = mean(air_yards),
            togo = mean(ydstogo)) 


ggplot(Adot, aes(x = week, y = adot))+
  geom_point(aes(color = "red"))+
  scale_color_identity(aesthetics = c("color", "fill"))


AdotVSToGo <- pbp2022 %>% 
  filter(season == 2022, posteam == "CIN", pass == 1, !is.na(air_yards), down != 4) %>% 
  group_by(week, down) %>% 
  summarise(adot = mean(air_yards),
            togo = mean(ydstogo)) %>% 
  group_by(down) %>% 
  mutate(Avgtogo = mean(togo))

ggplot(AdotVSToGo, aes(x = week, y = togo))+
  geom_point(aes(color = "blue"))+
  geom_point(aes(y = adot, color = "red"))+
  facet_wrap(~down)+
  geom_hline(aes(yintercept = Avgtogo))+
  scale_color_identity(aesthetics = c("color", "fill"))
