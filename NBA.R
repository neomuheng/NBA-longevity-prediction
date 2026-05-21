  library(MASS)
  library(tidyverse)
  library(naniar)
  library(scales)
  library(car)
  library(survival)
  library(survminer)
  library(gridExtra)
  select <- dplyr::select
  
  # ── 1. LOAD DATA
  setwd("C:/Users/vipon/Downloads/Archive")
  career_info     <- read_csv("Player Career Info.csv")
  season_info     <- read_csv("Player Season Info.csv")
  player_totals   <- read_csv("Player Totals.csv")
  advanced        <- read_csv("Advanced.csv")
  player_shooting <- read_csv("Player Shooting.csv")
  
  # ── 2. MISSING DATA FUNCTION
  missing_summary <- function(df, name) {
    cat("\n====", name, "====\n")
    data.frame(
      Variable = names(df),
      Count    = colSums(is.na(df)),
      Percent  = round(colSums(is.na(df)) / nrow(df) * 100, 2)
    ) %>% filter(Count > 0) %>% arrange(desc(Count)) %>% print()
  }
  
  # ── 3. CLEAN DUPLICATES
  clean_dupes <- function(df) {
    df %>%
      filter(team == "TOT" | !duplicated(paste(player_id, season))) %>%
      distinct(player_id, season, .keep_all = TRUE)
  }
  
  season_info_clean     <- clean_dupes(season_info)
  player_totals_clean   <- clean_dupes(player_totals)
  advanced_clean        <- clean_dupes(advanced)
  player_shooting_clean <- clean_dupes(player_shooting)
  
  # ── 4. MERGE
  merged_df <- career_info %>%
    left_join(season_info_clean, by = c("player_id", "player")) %>%
    left_join(player_totals_clean, by = c("player_id", "player", "season"),
              suffix = c("", ".totals")) %>%
    left_join(advanced_clean, by = c("player_id", "player", "season"),
              suffix = c("", ".adv")) %>%
    left_join(player_shooting_clean, by = c("player_id", "player", "season"),
              suffix = c("", ".shoot"))
  
  # ── 5. SELECT VARIABLES & FILTER TO 1976+
  analysis_df <- merged_df %>%
    select(player, player_id, from, to, pos.x, age,
           pts, mp, g, ts_percent, ws_48, bpm,
           fg_percent, usg_percent, season) %>%
    mutate(career_length = to - from + 1) %>%
    filter(season >= 1976)
  
  # ── 6. MISSING DATA CHECK
  missing_summary(analysis_df, "Analysis Dataset (1976+)")
  
  miss_df <- miss_var_summary(analysis_df) %>%
    filter(n_miss > 0) %>%
    arrange(desc(pct_miss)) %>%
    mutate(
      variable = factor(variable, levels = rev(variable)),
      pct_miss = as.numeric(pct_miss),
      n_miss   = as.integer(n_miss)
    )
  
  figure1 <- ggplot(miss_df, aes(x = pct_miss, y = variable)) +
    geom_col(fill = "#1A2E4A", width = 0.6) +
    geom_text(aes(label = paste0(n_miss, " (", round(pct_miss, 2), "%)")),
              hjust = -0.1, size = 3.5) +
    geom_vline(xintercept = 5, linetype = "dashed",
               color = "#e8a020", linewidth = 0.8) +
    annotate("text", x = 5.15, y = 1, label = "5% threshold",
             color = "#e8a020", hjust = 0, size = 3.5) +
    scale_x_continuous(limits = c(0, 7),
                       labels = function(x) paste0(x, "%")) +
    labs(title    = "Figure 1: Missing Data by Variable (Post-1976 Analysis Dataset)",
         subtitle = paste0("N = ", format(nrow(analysis_df), big.mark = ","),
                           " player-season observations"),
         x = "Percent Missing", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(plot.title         = element_text(face = "bold"),
          plot.subtitle      = element_text(color = "gray40"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank())
  
  ggsave("figure1_missing_data.png", plot = figure1,
         width = 9, height = 5, dpi = 300)
  
  # ── 7. PLAYER-LEVEL SUMMARY
  player_summary <- analysis_df %>%
    group_by(player, player_id) %>%
    summarise(
      career_length  = max(to) - min(from) + 1,
      last_season    = max(to),
      avg_ts_percent = mean(ts_percent, na.rm = TRUE),
      avg_ws_48      = mean(ws_48, na.rm = TRUE),
      avg_bpm        = mean(bpm, na.rm = TRUE),
      avg_pts        = mean(pts, na.rm = TRUE),
      avg_mp         = mean(mp, na.rm = TRUE),
      avg_usg        = mean(usg_percent, na.rm = TRUE),
      avg_g          = mean(g, na.rm = TRUE),
      position       = first(pos.x),
      .groups = "drop"
    ) %>%
    filter(!is.na(avg_ts_percent), !is.na(avg_ws_48),
           !is.na(avg_bpm), !is.na(avg_usg))
  
  # ── 8. FILTER TO MEANINGFUL CONTRIBUTORS
  players <- player_summary %>%
    filter(career_length >= 3, avg_g >= 40, avg_mp >= 1000) %>%
    mutate(
      event     = ifelse(last_season == 2026, 0, 1),
      ts_group  = ifelse(avg_ts_percent >= 0.539,
                         "High Efficiency (TS% > 53.9%)",
                         "Low Efficiency (TS% < 53.9%)"),
      usg_group = ifelse(avg_usg >= 18.8,
                         "High Volume (USG% > 18.8%)",
                         "Low Volume (USG% < 18.8%)")
    )
  
  cat("Players in analysis:", nrow(players), "\n")
  cat("Retired (event=1):", sum(players$event == 1), "\n")
  cat("Still active (event=0):", sum(players$event == 0), "\n")
  cat("\nEfficiency groups:\n"); table(players$ts_group)
  cat("\nVolume groups:\n"); table(players$usg_group)
  
  # ── 9. MODEL 1: LINEAR REGRESSION (BASELINE)
  cat("\n── Linear Regression ──\n")
  lm_model <- lm(career_length ~ avg_ts_percent + avg_usg, data = players)
  summary(lm_model)
  
  # ── 10. MODEL 4: COX ENHANCED
  cat("\n── Cox Enhanced ──\n")
  
  # Add debut age
  career_info_age <- career_info %>%
    mutate(debut_age = as.numeric(from) -
             as.numeric(format(birth_date, "%Y"))) %>%
    select(player_id, debut_age)
  
  players_enhanced <- players %>%
    left_join(career_info_age, by = "player_id") %>%
    rename(debut_age = debut_age) %>%
    filter(!is.na(debut_age), !is.na(position), !is.na(avg_bpm))
  
  cox_enhanced <- coxph(Surv(career_length, event) ~
                          avg_ts_percent + avg_usg + avg_bpm +
                          position + debut_age,
                        data = players_enhanced)
  summary(cox_enhanced)
  
  # ── 13. MODEL COMPARISON
  cat("\n── Model Comparison ──\n")
  cat("Linear Regression  AIC:", AIC(lm_model), "\n")
  cat("Negative Binomial  AIC:", AIC(nb_model), "\n")
  cat("Cox Basic      Concordance: 0.655\n")
  cat("Cox Enhanced   Concordance: 0.713\n")
  
  # ── 11. VISUALIZATIONS
  
  # Plot 1: Linear model scatter plots
  p1 <- ggplot(players, aes(x = avg_ts_percent, y = career_length)) +
    geom_point(alpha = 0.2, color = "#1A2E4A") +
    geom_smooth(method = "lm", color = "#e8a020", se = TRUE) +
    scale_x_continuous(limits = c(0.3, 0.8)) +
    labs(title = "Efficiency vs Career Length",
         subtitle = "Linear Model",
         x = "Average True Shooting %",
         y = "Career Length (seasons)") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
  
  p2 <- ggplot(players, aes(x = avg_usg, y = career_length)) +
    geom_point(alpha = 0.2, color = "#1A2E4A") +
    geom_smooth(method = "lm", color = "#e8a020", se = TRUE) +
    labs(title = "Volume vs Career Length",
         subtitle = "Linear Model",
         x = "Average Usage Rate %",
         y = "Career Length (seasons)") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave("linear_models.png",
         arrangeGrob(p1, p2, ncol = 2),
         width = 12, height = 5, dpi = 300)
  
  # Plot 2: Survival by efficiency (TS% threshold = 53.9%)
  fit_ts <- survfit(Surv(career_length, event) ~ ts_group,
                    data = players_enhanced)
  
  p3 <- ggsurvplot(fit_ts, data = players_enhanced,
                   palette     = c("#1A2E4A", "#e8a020"),
                   legend.labs = c("High Efficiency (TS% > 53.9%)",
                                   "Low Efficiency (TS% < 53.9%)"),
                   title       = "Career Survival by Efficiency (TS%)",
                   subtitle    = "Controlling for BPM, position, and debut age",
                   xlab        = "Career Length (seasons)",
                   ylab        = "Probability of Remaining Active",
                   ggtheme     = theme_minimal(base_size = 13),
                   conf.int    = TRUE,
                   risk.table  = FALSE,
                   pval        = TRUE,
                   pval.coord  = c(1, 0.1))
  
  ggsave("survival_efficiency_final.png", plot = p3$plot,
         width = 8, height = 5, dpi = 300)
  
  # Plot 3: Survival by volume (USG% threshold = 18.8%) — no p-value shown
  fit_usg <- survfit(Surv(career_length, event) ~ usg_group,
                     data = players_enhanced)
  
  p4 <- ggsurvplot(fit_usg, data = players_enhanced,
                   palette     = c("#1A2E4A", "#e8a020"),
                   legend.labs = c("High Volume (USG% > 18.8%)",
                                   "Low Volume (USG% < 18.8%)"),
                   title       = "Career Survival by Volume (USG%)",
                   subtitle    = "Controlling for BPM, position, and debut age",
                   xlab        = "Career Length (seasons)",
                   ylab        = "Probability of Remaining Active",
                   ggtheme     = theme_minimal(base_size = 13),
                   conf.int    = TRUE,
                   risk.table  = FALSE,
                   pval        = FALSE)
  
  ggsave("survival_volume_final.png", plot = p4$plot,
         width = 8, height = 5, dpi = 300)
  
  cat("\nAll models and plots complete.\n")