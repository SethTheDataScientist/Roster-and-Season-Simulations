
# Simulation of 21 season -------------------------------------------------

elo_model <- function(teams, games, week_num, ...) {
  
  # round out (away from zero)
  # this way the simulator never simulates a tie
  # the simulator will still allow ties to be simulated if you want
  # ... but not on playoff games
  round_out <- function(x) {
    x[!is.na(x) & x < 0] <- floor(x[!is.na(x) & x < 0])
    x[!is.na(x) & x > 0] <- ceiling(x[!is.na(x) & x > 0])
    return(x)
  }
  
  # we're going to store elo as a new columns in the teams data
  # it won't start off there of course, so we need to determine it
  # from our arguments
  if (!("elo" %in% colnames(teams))) {
    args <- list(...)
    if ("elo" %in% names(args)) {
      # pull the elo info from custom arguments
      teams <- teams %>%
        dplyr::inner_join(args$elo %>% dplyr::select(team, elo), by = c("team" = "team"))
    } else {
      # error with a friendly error message if no elo data is passed in
      stop("Pass in a tibble `elo` as an argument to `simulate_nfl()`")
    }
  }
  
  # isolate the ratings data by sim and by team only
  # we will want to join to the games data later and don't want excess columns
  ratings <- teams %>% dplyr::select(sim, team, elo)
  
  # simulate game outcomes
  games <- games %>%
    # add in the away team's elo to the game data
    # note we join on both `sim` and the team
    # always join on `sim` to make sure each sim cares about only its data
    dplyr::inner_join(ratings, by = c("sim" = "sim", "away_team" = "team")) %>%
    dplyr::rename(away_elo = elo) %>%
    # repeat for the home team as well
    dplyr::inner_join(ratings, by = c("sim" = "sim", "home_team" = "team")) %>%
    dplyr::rename(home_elo = elo) %>%
    dplyr::mutate(
      # calculate the elo difference
      elo_diff = home_elo - away_elo,
      # we also can calculate the estimate (mean points home team wins by)
      estimate = elo_diff,
      result = dplyr::case_when(
        # !!! ALWAYS DO THIS NEXT LINE IN YOUR `result` CHANGES !!!
        # you have to make sure you're only changing unfinished games in current week
        # if you don't do this, it will usually error out on a friendly error message
        is.na(result) & week == week_num ~ 
          as.integer(round_out(rnorm(n(), estimate, 13))),
        # if not this week or known result, leave as-is
        TRUE ~ as.integer(result)
      ),
      # simplify to 1 = win, 0 = loss, 0.5 = tie to help calculate elo shift
      outcome = dplyr::case_when(
        is.na(result) ~ NA_real_,
        result > 0 & location == "Home" ~ 0.61,
        result > 0 & location != "Home" ~ 0.42,
        result < 0 & location == "Home" ~ 0.60,
        result < 0 & location != "Home" ~ 0.11,
        result == 0 & location == "Home" ~ 0.62,
        result == 0 & location != "Home" ~ 0.37,
        TRUE ~ 0.5
      )
    ) %>%
    # we don't want these columns in `games` any more
    # remove any columns you don't need when you're done
    # otherwise the next week they'll get joined as `col.x` and `col.y`
    # which will almost certainly break your script
    dplyr::select(
      -away_elo, -home_elo, -elo_diff, -estimate,
      -outcome
    )
  
  
  # return the updated teams and games information
  # note that `teams` will now have an updated `elo` column which will
  # be used for the next week's games
  # note that starting `elo` values are the same per-team... 
  # ... but after that will differ per sim depending on that sim's results
  return(list(teams = teams, games = games))
}





initial_elo <- tibble::tibble(
  team = unique(nflseedR::divisions$team)
)

initial_elo <- initial_elo %>% 
  left_join(WinLossUpdating, by = c("team" = "posteam")) %>% 
  mutate(elo = case_when(is.na(PosteamADJValue) == 1 ~ 0.5,
                         T ~ PosteamADJValue)) %>% 
  select(!PosteamADJValue)

test <- simulate_nfl(
  nfl_season = 2021,
  process_games = elo_model,
  elo = initial_elo)

write_xlsx(test, path = "2021 Sim Playoffs .xlsx")

