get_my_local_db_access_SDP2 <- function(){
  
  if (!nzchar(Sys.getenv("BQ_SA_JSON"))) {
    stop("Missing required environment variable: BQ_SA_JSON")
  }
  
  connection_info <- config::get(
    value = "dataconnection",  
    config = Sys.getenv("R_CONFIG_ACTIVE", "default"), 
    file = Sys.getenv("R_CONFIG_FILE", "Empire_bq_db_connection.yml"), 
    use_parent = TRUE
  )
  
  # Decode JSON and write to temp file
  json_string <- rawToChar(jsonlite::base64_dec(Sys.getenv("BQ_SA_JSON")))
  tmp <- tempfile(fileext = ".json")
  writeLines(json_string, tmp)
  
  # Auth first, THEN clean up
  bigrquery::bq_auth(path = tmp)
  unlink(tmp)
  
  # create BigQuery connection  
  tryCatch({
    con <- dbConnect(
      bigrquery::bigquery(),
      project = connection_info$project_id,
      dataset = connection_info$dataset,
      billing = connection_info$billing
    )
    return(con)
    # error handling 
  }, error = function(e) {
    message(paste("Connection error:", e$message))
    message("Connection info: ", paste(names(connection_info), unlist(connection_info), sep = "=", collapse = ", "))
    stop(e)
  })
  
}

get_loan_player_stats <- function(bq_con, rbny_loan_players, date_from, date_to){
  
  # Get match summary information: minutes, goals, assists.
  player_match_summary <- dbGetQuery(bq_con,
                                     glue_sql(
                                       "SELECT
	t1.team_id_statsbomb,
	t1.match_id_statsbomb,
	t1.player_id_statsbomb,
	t1.metric_name,
	t1.metric_value,
	t2.match_date,
	t3.team_name AS team,
	COALESCE(t4.player_nickname, t4.player_name) as player
FROM
	`40_intermediate_statsbomb.int_player_match_stats` t1
	LEFT JOIN `40_intermediate_statsbomb.int_matches` t2 ON t1.match_id_statsbomb = t2.match_id_statsbomb
	LEFT JOIN `40_intermediate_statsbomb.int_teams` t3 ON t1.team_id_statsbomb = t3.team_id_statsbomb
	LEFT JOIN `40_intermediate_statsbomb.int_players` t4 ON t1.player_id_statsbomb = t4.player_id_statsbomb
WHERE
	t1.player_id_statsbomb IN ({player_ids_needed*})
	AND t2.match_date BETWEEN {date_from_needed} AND {date_to_needed}
	AND t1.metric_name IN ('minutes', 'goals', 'assists')",
  player_ids_needed = rbny_loan_players$player_id_statsbomb,
  date_from_needed = date_from,
  date_to_needed = date_to,
  .con = bq_con
  )) 
  
  # Filter to player-level match stats during their loan period 
  player_match_summary_loan_period <- player_match_summary %>% 
    left_join(rbny_loan_players %>% 
                select(player_id_statsbomb, team_id_statsbomb, loan_team_joined_date),
              by = c("player_id_statsbomb", "team_id_statsbomb")) %>% 
    filter(match_date >= loan_team_joined_date)
  
  # Get lineup information.
  player_team_lineups <- dbGetQuery(bq_con,
                               glue_sql(
                                 "SELECT
	t1.team_id_statsbomb,
	t5.team_name as team,
	t1.match_id_statsbomb,
	t3.match_date,
	t1.player_id_statsbomb,
	COALESCE(t4.player_nickname, t4.player_name) as player,
	t2.start_reason,
	t2.end_reason
FROM
  `40_intermediate_statsbomb.int_lineups` t1
	LEFT JOIN `40_intermediate_statsbomb.int_player_positions` t2 ON (t1.match_id_statsbomb = t2.match_id_statsbomb AND t1.player_id_statsbomb = t2.player_id_statsbomb AND t1.team_id_statsbomb = t2.team_id_statsbomb)
	LEFT JOIN `40_intermediate_statsbomb.int_matches` t3 ON t1.match_id_statsbomb = t3.match_id_statsbomb
	LEFT JOIN `40_intermediate_statsbomb.int_players` t4 ON t1.player_id_statsbomb = t4.player_id_statsbomb
	LEFT JOIN `40_intermediate_statsbomb.int_teams` t5 ON t1.team_id_statsbomb = t5.team_id_statsbomb
WHERE
  t1.team_id_statsbomb IN ({team_ids_needed*})
	AND t3.match_date BETWEEN {date_from_needed} AND {date_to_needed}",
  team_ids_needed = rbny_loan_players$team_id_statsbomb,
  date_from_needed = date_from,
  date_to_needed = date_to,
  .con = bq_con
  )) 
  
  # Filter to team lineup information during player loan period 
  player_team_lineups_loan_period <- player_team_lineups %>% 
    left_join(rbny_loan_players %>% 
                select(team_id_statsbomb, loan_team_joined_date),
              by = "team_id_statsbomb") %>% 
    filter(match_date >= loan_team_joined_date)
  
  # Get last, next matches for each team.
  player_team_schedules <- dbGetQuery(bq_con,
                                     glue_sql(
                                       "WITH team_matches AS (
  SELECT
    t1.match_id_statsbomb,
    t1.match_date,
    t1.home_team_id_statsbomb AS team_id_statsbomb,
    t2.team_name,
    CONCAT(FORMAT_DATE('%b %d', t1.match_date), ' vs. ', t3.team_name) AS match_label
  FROM `40_intermediate_statsbomb.int_matches` t1
  LEFT JOIN `40_intermediate_statsbomb.int_teams` t2 ON t1.home_team_id_statsbomb = t2.team_id_statsbomb
  LEFT JOIN `40_intermediate_statsbomb.int_teams` t3 ON t1.away_team_id_statsbomb = t3.team_id_statsbomb
  WHERE t1.home_team_id_statsbomb IN ({team_ids_needed*})
    AND t1.match_date >= {date_from_needed}

  UNION ALL

  SELECT
    t1.match_id_statsbomb,
    t1.match_date,
    t1.away_team_id_statsbomb AS team_id_statsbomb,
    t3.team_name,
    CONCAT(FORMAT_DATE('%b %d', t1.match_date), ' @ ', t2.team_name) AS match_label
  FROM `40_intermediate_statsbomb.int_matches` t1
  LEFT JOIN `40_intermediate_statsbomb.int_teams` t2 ON t1.home_team_id_statsbomb = t2.team_id_statsbomb
  LEFT JOIN `40_intermediate_statsbomb.int_teams` t3 ON t1.away_team_id_statsbomb = t3.team_id_statsbomb
  WHERE t1.away_team_id_statsbomb IN ({team_ids_needed*})
    AND t1.match_date >= {date_from_needed}
),

team_bounds AS (
  SELECT
    team_id_statsbomb,
    ARRAY_AGG(IF(match_date <= CURRENT_DATE(), match_id_statsbomb, NULL) IGNORE NULLS ORDER BY match_date DESC LIMIT 1)[SAFE_OFFSET(0)] AS last_match_id_statsbomb,
    MAX(IF(match_date <= CURRENT_DATE(), match_date, NULL)) AS last_match_date,
    ARRAY_AGG(IF(match_date > CURRENT_DATE(), match_id_statsbomb, NULL) IGNORE NULLS ORDER BY match_date ASC LIMIT 1)[SAFE_OFFSET(0)] AS next_match_id_statsbomb,
    MIN(IF(match_date > CURRENT_DATE(), match_date, NULL)) AS next_match_date
  FROM team_matches
  GROUP BY team_id_statsbomb
)

SELECT
  b.team_id_statsbomb,
  MAX(m.team_name)                                            AS team,
  b.last_match_id_statsbomb,
  b.last_match_date,
  MAX(IF(m.match_date = b.last_match_date, m.match_label, NULL)) AS last_match_label,
  b.next_match_id_statsbomb,
  b.next_match_date,
  MAX(IF(m.match_date = b.next_match_date, m.match_label, NULL)) AS next_match_label
FROM team_bounds b
LEFT JOIN team_matches m ON b.team_id_statsbomb = m.team_id_statsbomb
GROUP BY b.team_id_statsbomb, b.last_match_date, b.last_match_id_statsbomb, b.next_match_date, b.next_match_id_statsbomb",
                                     team_ids_needed = rbny_loan_players$team_id_statsbomb,
                                     date_from_needed = date_from,
                                     date_to_needed = date_to,
                                     .con = bq_con))

  # Make sure last match date falls during player loan period 
  player_team_schedules_loan_period <- player_team_schedules %>% 
    left_join(rbny_loan_players %>% 
                select(team_id_statsbomb, loan_team_joined_date),
              by = "team_id_statsbomb") %>% 
    filter(last_match_date >= loan_team_joined_date)
  
  # Team-level games played during the player loan period.
  team_games <- player_team_lineups_loan_period %>%
    group_by(team_id_statsbomb) %>%
    summarise(team_games_played = n_distinct(match_id_statsbomb))
  
  # Player appearances/availability
  player_lineup_info <- player_team_lineups_loan_period %>%
    filter(player_id_statsbomb %in% rbny_loan_players$player_id_statsbomb) %>%
    group_by(player, player_id_statsbomb, team, team_id_statsbomb, match_id_statsbomb) %>%
    summarise(
      squad_made = 1,
      start      = as.integer(any(start_reason == "Starting XI", na.rm = TRUE)),
      appearance = as.integer(!all(is.na(start_reason))),
      full_90    = as.integer(
        any(start_reason == "Starting XI", na.rm = TRUE) &
          any(end_reason == "Final Whistle", na.rm = TRUE)
      ),
      .groups    = "drop"
    )
  
  # Compute summary stats for each player.
  player_summary_wide <- player_lineup_info %>%
    left_join(player_match_summary_loan_period, by = c("team_id_statsbomb", "team", "match_id_statsbomb", "player_id_statsbomb", "player")) %>%
    left_join(player_team_schedules_loan_period, by = c("team_id_statsbomb", "team")) %>% 
    group_by(player, player_id_statsbomb, team, team_id_statsbomb) %>%
    summarise(
      # Season totals
      total_squads_made = length(unique((match_id_statsbomb[squad_made == 1]))),
      total_appearances = length(unique((match_id_statsbomb[appearance == 1]))),
      total_starts = length(unique((match_id_statsbomb[start == 1]))),
      total_full_90s = length(unique((match_id_statsbomb[full_90 == 1]))),
      total_minutes = round(sum(metric_value[metric_name == "minutes"], na.rm = T)),
      total_goals = sum(metric_value[metric_name == "goals"], na.rm = T),
      total_assists = sum(metric_value[metric_name == "assists"], na.rm = T),
      
      # Last match info
      squad_made_last_match = coalesce(squad_made[match_id_statsbomb == last_match_id_statsbomb][1], 0),
      appearance_last_match = coalesce(appearance[match_id_statsbomb == last_match_id_statsbomb][1], 0),
      start_last_match = coalesce(start[match_id_statsbomb == last_match_id_statsbomb][1], 0),
      full_90_last_match = coalesce(full_90[match_id_statsbomb == last_match_id_statsbomb][1], 0),
      minutes_last_match = coalesce( round(metric_value[metric_name == "minutes" & match_id_statsbomb == last_match_id_statsbomb][1]), 0),
      goals_last_match = coalesce(metric_value[metric_name == "goals" & match_id_statsbomb == last_match_id_statsbomb][1], 0),
      assists_last_match = coalesce(metric_value[metric_name == "assists" & match_id_statsbomb == last_match_id_statsbomb][1], 0),
      
      # Match information
      last_match = unique(last_match_label),
      next_match = unique(next_match_label),
      
      .groups = "drop"
    ) %>% 
    left_join(team_games, by = "team_id_statsbomb") %>%
    mutate(
      squad_made_pct = round(total_squads_made / team_games_played * 100),
      appearance_pct = round(total_appearances / team_games_played * 100),
      start_pct = round(total_starts / team_games_played * 100),
      pct_90s = round(total_full_90s / team_games_played * 100)
    ) %>%
    ungroup()
  
  return(player_summary_wide)
  
}

get_loan_player_stats_scoutastic <- function(bq_con, rbny_loan_players, date_from, date_to){
  
  # Get match summary information: minutes, goals, assists.
  player_match_summary <- dbGetQuery(bq_con,
                                     glue_sql(
                                       "SELECT
  	t1.team_id_scoutastic,
  	t1.match_id_scoutastic,
  	t3.competition_name as competition,
  	t3.competition_level_definition as competition_tier,
  	t1.player_id_scoutastic,
  	concat(t1.first_name, ' ', t1.last_name) AS player,
  	t1.in_lineup,
  	t1.minutes_played,
  	t1.goals,
  	t1.assists,
  	safe_cast(t1.match_date_time AS DATE) AS match_date,
  	t2.team_name as team
  FROM
  	`40_intermediate_scoutastic.int_match_lineups` t1
  	JOIN `40_intermediate_scoutastic.int_teams` t2 ON t1.team_id_scoutastic = t2.team_id_scoutastic
  	JOIN `40_intermediate_scoutastic.int_competitions` t3 ON t1.competition_id_scoutastic = t3.competition_id_scoutastic
  WHERE
  	player_id_scoutastic IN ({player_ids_needed*})
  	AND t1.match_date_time BETWEEN {date_from_needed} AND {date_to_needed}",
                                       player_ids_needed = rbny_loan_players$player_id_scoutastic,
                                       date_from_needed = date_from,
                                       date_to_needed = date_to,
                                       .con = bq_con
                                     ))
  
  # Filter to player-level match stats during their loan period 
  player_match_summary_loan_period <- player_match_summary %>% 
    left_join(rbny_loan_players %>% 
                select(player_id_scoutastic, team_id_scoutastic, loan_team_joined_date),
              by = c("player_id_scoutastic", "team_id_scoutastic")) %>% 
    filter(match_date >= loan_team_joined_date)
  
  # Get lineup information.
  player_team_lineups <- dbGetQuery(bq_con,glue_sql(
    "SELECT
  	t1.team_id_scoutastic,
  	t1.match_id_scoutastic,
  	t3.competition_name as competition,
  	t3.competition_level_definition as competition_tier,
  	t1.player_id_scoutastic,
  	concat(t1.first_name, ' ', t1.last_name) AS player,
  	t1.in_lineup,
  	t1.minutes_played,
  	safe_cast(t1.match_date_time AS DATE) AS match_date,
  	t2.team_name as team,
  	t2.team_id_transfermarkt,
  	t4.home_team_id_transfermarkt,
  	t4.home_team_name,
  	t4.away_team_id_transfermarkt,
  	t4.away_team_name,
  	t6.event_type
  FROM
  	`40_intermediate_scoutastic.int_match_lineups` t1
  	JOIN `40_intermediate_scoutastic.int_teams` t2 ON t1.team_id_scoutastic = t2.team_id_scoutastic
  	JOIN `40_intermediate_scoutastic.int_competitions` t3 ON t1.competition_id_scoutastic = t3.competition_id_scoutastic
  	JOIN `40_intermediate_scoutastic.int_matches` t4 ON t1.match_id_scoutastic = t4.match_id_scoutastic
  	JOIN `10_meta_sdp_mappings.players_mapping_global` t5 on t1.player_id_scoutastic = t5.player_id_scoutastic
  	LEFT JOIN `40_intermediate_scoutastic.int_match_events` t6 on (t1.match_id_scoutastic = t6.match_id_scoutastic and t5.player_id_transfermarkt = t6.player_id_transfermarkt)
  WHERE
  	t1.team_id_scoutastic IN ({team_ids_needed*})
  	AND t1.match_date_time BETWEEN {date_from_needed} AND {date_to_needed}",
    team_ids_needed = rbny_loan_players$team_id_scoutastic,
    date_from_needed = date_from,
    date_to_needed = date_to,
    .con = bq_con
  )) 
  
  # Filter to team lineup information during player loan period 
  player_team_lineups_loan_period <- player_team_lineups %>% 
    left_join(rbny_loan_players %>% 
                select(team_id_scoutastic, loan_team_joined_date),
              by = "team_id_scoutastic") %>% 
    filter(match_date >= loan_team_joined_date)
  
  # Team schedule information
  player_team_schedules <- dbGetQuery(bq_con, glue_sql(
    "
  WITH base AS (
    SELECT
      match_id_scoutastic,
      SAFE_CAST(match_date_time AS DATE) AS match_date,
      home_team_id_transfermarkt,
      away_team_id_transfermarkt
    FROM `40_intermediate_scoutastic.int_matches`
    WHERE SAFE_CAST(match_date_time AS DATE) >= { date_from_needed }
  ),
  team_matches AS (
    SELECT
      b.match_id_scoutastic,
      b.match_date,
      b.home_team_id_transfermarkt AS team_id_transfermarkt,
      t2.team_name,
      CONCAT(FORMAT_DATE('%b %d', b.match_date), ' vs. ', t3.team_name) AS match_label
    FROM base b
    LEFT JOIN `40_intermediate_scoutastic.int_teams` t2 ON b.home_team_id_transfermarkt = t2.team_id_transfermarkt
    LEFT JOIN `40_intermediate_scoutastic.int_teams` t3 ON b.away_team_id_transfermarkt = t3.team_id_transfermarkt
    WHERE b.home_team_id_transfermarkt IN ({ team_ids_needed * })

    UNION ALL

    SELECT
      b.match_id_scoutastic,
      b.match_date,
      b.away_team_id_transfermarkt AS team_id_transfermarkt,
      t3.team_name,
      CONCAT(FORMAT_DATE('%b %d', b.match_date), ' @ ', t2.team_name) AS match_label
    FROM base b
    LEFT JOIN `40_intermediate_scoutastic.int_teams` t2 ON b.home_team_id_transfermarkt = t2.team_id_transfermarkt
    LEFT JOIN `40_intermediate_scoutastic.int_teams` t3 ON b.away_team_id_transfermarkt = t3.team_id_transfermarkt
    WHERE b.away_team_id_transfermarkt IN ({ team_ids_needed * })
  ),
team_bounds AS (
  SELECT
    team_id_transfermarkt,
    ARRAY_AGG(IF(match_date <= CURRENT_DATE(), match_id_scoutastic, NULL) IGNORE NULLS ORDER BY match_date DESC LIMIT 1)[SAFE_OFFSET(0)] AS last_match_id_scoutastic,
    MAX(IF(match_date <= CURRENT_DATE(), match_date, NULL)) AS last_match_date,
    ARRAY_AGG(IF(match_date > CURRENT_DATE(), match_id_scoutastic, NULL) IGNORE NULLS ORDER BY match_date ASC LIMIT 1)[SAFE_OFFSET(0)] AS next_match_id_scoutastic,
    MIN(IF(match_date > CURRENT_DATE(), match_date, NULL)) AS next_match_date
  FROM team_matches
  GROUP BY team_id_transfermarkt
)
  SELECT
    b.team_id_transfermarkt,
    MAX(m.team_name) AS team,
    b.last_match_id_scoutastic,
    b.last_match_date,
    MAX(IF(m.match_date = b.last_match_date, m.match_label, NULL)) AS last_match_label,
    b.next_match_id_scoutastic,
    b.next_match_date,
    MAX(IF(m.match_date = b.next_match_date, m.match_label, NULL)) AS next_match_label
  FROM team_bounds b
  LEFT JOIN team_matches m ON b.team_id_transfermarkt = m.team_id_transfermarkt
  GROUP BY b.team_id_transfermarkt, b.last_match_date, b.last_match_id_scoutastic, b.next_match_date, b.next_match_id_scoutastic
  ",
    team_ids_needed = rbny_loan_players$team_id_transfermarkt,
    date_from_needed = date_from,
    .con = bq_con
  ))

  # Make sure last match date falls during player loan period 
  player_team_schedules_loan_period <- player_team_schedules %>% 
    left_join(rbny_loan_players %>% 
                select(team_id_transfermarkt, loan_team_joined_date),
              by = "team_id_transfermarkt") %>% 
    filter(last_match_date >= loan_team_joined_date)
  
  # Team-level games played.
  team_games <- player_team_lineups_loan_period %>%
    group_by(team_id_scoutastic, team_id_transfermarkt) %>%
    summarise(team_games_played = n_distinct(match_id_scoutastic))
  
  # Player appearances/availability
  player_lineup_info <- player_team_lineups_loan_period %>%
    filter(player_id_scoutastic %in% rbny_loan_players$player_id_scoutastic) %>%
    group_by(player_id_scoutastic, team_id_scoutastic, team_id_transfermarkt, match_id_scoutastic) %>%
    summarise(
      squad_made = 1,
      start      = as.integer(any(in_lineup)),
      appearance = as.integer(max(minutes_played, na.rm = TRUE) > 0 | start == 1),
      full_90 = as.integer(any(in_lineup) & !any(event_type == "substituteOut", na.rm = TRUE)),
      .groups    = "drop"
    )
  
  
  # Compute summary stats for each player.
  player_summary_wide <- player_lineup_info %>%
    left_join(player_match_summary_loan_period, by = c("team_id_scoutastic", "match_id_scoutastic", "player_id_scoutastic")) %>%
    left_join(player_team_schedules_loan_period %>% 
                select(-team), by = c("team_id_transfermarkt")) %>% 
    
    group_by(player_id_scoutastic, team, team_id_scoutastic) %>%
    summarise(
      # Season totals
      total_squads_made = length(unique(match_id_scoutastic[squad_made == 1])),
      total_appearances = length(unique(match_id_scoutastic[appearance == 1])),
      total_starts = length(unique(match_id_scoutastic[start == 1])),
      total_full_90s = length(unique(match_id_scoutastic[full_90 == 1])),
      
      total_minutes = round(sum(minutes_played, na.rm = TRUE)),
      total_goals = round(sum(goals, na.rm = TRUE)),
      total_assists = round(sum(assists, na.rm = TRUE)),
      
      # Last match info — "?" when there's no last match to reference at all
      last_match_id_val = unique(last_match_id_scoutastic)[1],
      
      squad_made_last_match = if (is.na(last_match_id_val)) "?" else as.character(coalesce(squad_made[match_id_scoutastic == last_match_id_val][1], 0)),
      appearance_last_match = if (is.na(last_match_id_val)) "?" else as.character(coalesce(appearance[match_id_scoutastic == last_match_id_val][1], 0)),
      start_last_match = if (is.na(last_match_id_val)) "?" else as.character(coalesce(start[match_id_scoutastic == last_match_id_val][1], 0)),
      full_90_last_match = if (is.na(last_match_id_val)) "?" else as.character(coalesce(full_90[match_id_scoutastic == last_match_id_val][1], 0)),
      
      minutes_last_match = if (is.na(last_match_id_val)) "?" else as.character(coalesce(minutes_played[match_id_scoutastic == last_match_id_val][1], 0)),
      goals_last_match = if (is.na(last_match_id_val)) "?" else as.character(coalesce(goals[match_id_scoutastic == last_match_id_val][1], 0)),
      assists_last_match = if (is.na(last_match_id_val)) "?" else as.character(coalesce(assists[match_id_scoutastic == last_match_id_val][1], 0)),
      
      # Match information
      last_match = unique(last_match_label),
      next_match = unique(next_match_label),
      
      .groups = "drop"
    ) %>% 
    select(-last_match_id_val) %>%
    left_join(team_games, by = "team_id_scoutastic") %>%
    mutate(
      squad_made_pct = round(total_squads_made / team_games_played * 100),
      appearance_pct = round(total_appearances / team_games_played * 100),
      start_pct = round(total_starts / team_games_played * 100),
      pct_90s = round(total_full_90s / team_games_played * 100)
    ) %>%
    ungroup() %>% 
    # Join player names
    left_join(dbGetQuery(bq_con, glue_sql(
      "SELECT
  	player_id_scoutastic,
	CONCAT(first_name, ' ', last_name) as player
FROM
	`40_intermediate_scoutastic.int_players`
WHERE
	player_id_scoutastic IN ({player_ids_needed*})",
      player_ids_needed = .$player_id_scoutastic,
      .con = bq_con
    )),
    by = "player_id_scoutastic")
  
  return(player_summary_wide)
  
}  

get_loan_player_injuries <- function(bq_con, rbny_loan_players){
  
  player_injuries <- dbGetQuery(bq_con, glue_sql(
    "SELECT
  	t1.player_id_scoutastic,
  	CONCAT(t2.first_name, ' ', t2.last_name) as player,
  	t1.injury,
  	t1.date_from,
  	t1.date_to
  FROM
  	`40_intermediate_scoutastic.int_player_injuries` t1
  	JOIN `40_intermediate_scoutastic.int_players` t2 on t1.player_id_scoutastic = t2.player_id_scoutastic
  WHERE
  	t1.player_id_scoutastic IN ({player_ids_needed*})",
    player_ids_needed = rbny_loan_players$player_id_scoutastic,
    .con = bq_con
  ))
  
  player_injuries_loan_period <- player_injuries %>% 
    left_join(rbny_loan_players %>% 
                select(player_id_scoutastic, loan_team_joined_date),
              by = "player_id_scoutastic") %>% 
    filter(date_from >= loan_team_joined_date,
           is.na(date_to))
  
  return(player_injuries_loan_period)
  
}

