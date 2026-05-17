# ══════════════════════════════════════════════════════════════════════════
# Neurosurgery Cognitive Bias – Education Follow-Up Analysis
# COMBINED: Stimulation_data + Education_data

# ── Load libraries ─────────────────────────────────────────────────────────
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(effectsize)   # cohens_d()
library(writexl)
library(patchwork)    # combine plots

# CONFIGURATION  ← edit these to match your actual column names

FILE_PATH   <- "/Users/laurenbishop/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Cognitive Bias in Neurosurgery/Results/Education data.xlsx"

# ── Sheet names ────────────────────────────────────────────────────────────
SHEET_STIMULATION <- "Stimulation_data"
SHEET_EDUCATION   <- "Education_data"

# ── Participant ID column (must exist in both sheets to join) ─────────────
PARTICIPANT_ID_COL <- "Participant"

# ── Group column (from Education sheet) ────────────────────────────────────
GROUP_COL         <- "Resistant or Susceptible"
RESISTANT_LABEL   <- "Resistant"
SUSCEPTIBLE_LABEL <- "Susceptible"

# ── Columns for analysis ───────────────────────────────────────────────────
# FROM EDUCATION SHEET:
BIAS_MITIGATION_COL       <- "This simulation helped me develop bias-mitigation skills"
REALISM_FOLLOWUP_COL      <- "The simulation adequately reflected real clinical pressures and decision-making challenges"
TEACHING_EFFECTIVENESS_COL <- "This simulation was more effective than traditional teaching methods for learning about cognitive bias"
CLINICAL_PRACTICE_COL     <- "Participation in this simulation improved my clinical practice"

# FROM STIMULATION SHEET:
# (Add column names from your Stimulation_data sheet - e.g., error rates, decision times, etc.)
# For now, we'll discover them dynamically
STIMULATION_ANALYSIS_COLS <- c(
  # Add specific columns you want to analyze from Stimulation_data
  # e.g., "Error_Rate", "Decision_Time", "Initial_Diagnosis", etc.
)

# ══════════════════════════════════════════════════════════════════════════
# COLOURS & THEME
# ══════════════════════════════════════════════════════════════════════════

GROUP_COLOURS <- c("Resistant" = "#2166AC", "Susceptible" = "#D6604D")

base_theme <- theme_classic(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    axis.title    = element_text(size = 10),
    legend.position = "none"
  )


# ══════════════════════════════════════════════════════════════════════════
# HELPER FUNCTIONS
# ══════════════════════════════════════════════════════════════════════════

# ── Load & inspect data ───────────────────────────────────────────────────
load_data <- function() {
  cat("\n── Loading Stimulation data ──────────────────────────────────\n")
  stim_df <- read_excel(FILE_PATH, sheet = SHEET_STIMULATION)
  cat(sprintf("Rows: %d\n\n", nrow(stim_df)))
  cat("Column names in Stimulation_data:\n")
  for (i in seq_along(names(stim_df))) {
    cat(sprintf("  [%2d]  %s\n", i, names(stim_df)[i]))
  }
  
  cat("\n── Loading Education data ───────────────────────────────────\n")
  edu_df <- read_excel(FILE_PATH, sheet = SHEET_EDUCATION)
  cat(sprintf("Rows: %d\n\n", nrow(edu_df)))
  cat("Column names in Education_data:\n")
  for (i in seq_along(names(edu_df))) {
    cat(sprintf("  [%2d]  %s\n", i, names(edu_df)[i]))
  }
  
  list(stimulation = stim_df, education = edu_df)
}


# ── Combine sheets on participant ID ───────────────────────────────────────
combine_data <- function(stim_df, edu_df) {
  cat("\n── Joining sheets on Participant ID ──────────────────────────\n")
  
  # Check if participant ID column exists
  if (!PARTICIPANT_ID_COL %in% names(stim_df)) {
    stop(sprintf("Column '%s' not found in Stimulation_data", PARTICIPANT_ID_COL))
  }
  if (!PARTICIPANT_ID_COL %in% names(edu_df)) {
    stop(sprintf("Column '%s' not found in Education_data", PARTICIPANT_ID_COL))
  }
  
  # Rename participant ID for clarity
  stim_df <- stim_df %>% rename(participant_id = all_of(PARTICIPANT_ID_COL))
  edu_df <- edu_df %>% rename(participant_id = all_of(PARTICIPANT_ID_COL))
  
  # Join on participant ID
  combined_df <- stim_df %>%
    inner_join(edu_df, by = "participant_id")
  
  cat(sprintf("Stimulation data: %d rows\n", nrow(stim_df)))
  cat(sprintf("Education data: %d rows\n", nrow(edu_df)))
  cat(sprintf("Combined (matched): %d rows\n\n", nrow(combined_df)))
  
  if (nrow(combined_df) == 0) {
    warning("⚠ No matching participant IDs found! Check column names.")
  }
  
  combined_df
}


# ── Split into groups ─────────────────────────────────────────────────────
split_groups <- function(df) {
  df[[GROUP_COL]] <- trimws(df[[GROUP_COL]])
  resistant   <- df %>% filter(.data[[GROUP_COL]] == RESISTANT_LABEL)
  susceptible <- df %>% filter(.data[[GROUP_COL]] == SUSCEPTIBLE_LABEL)
  cat(sprintf("Group sizes → Resistant: %d  |  Susceptible: %d\n\n",
              nrow(resistant), nrow(susceptible)))
  list(resistant = resistant, susceptible = susceptible)
}


# ── Welch t-test with Cohen's d ───────────────────────────────────────────
run_ttest <- function(resistant, susceptible, col) {
  a <- as.numeric(resistant[[col]]) %>% na.omit()
  b <- as.numeric(susceptible[[col]]) %>% na.omit()
  
  if (length(a) < 2 || length(b) < 2) {
    cat(sprintf("  ⚠  Not enough data for t-test on '%s'\n\n", col))
    return(NULL)
  }
  
  tt  <- t.test(a, b, var.equal = FALSE)          # Welch's
  lev <- var.test(a, b)                            # Levene proxy (F-test)
  d   <- cohens_d(a, b)$Cohens_d
  
  cat(sprintf("\n%s\n", strrep("─", 60)))
  cat(sprintf("  Column      : %s\n", col))
  cat(sprintf("  Resistant   → M=%.2f, SD=%.2f, n=%d\n", mean(a), sd(a), length(a)))
  cat(sprintf("  Susceptible → M=%.2f, SD=%.2f, n=%d\n", mean(b), sd(b), length(b)))
  cat(sprintf("  Levene F=%.3f, p=%.3f\n", lev$statistic, lev$p.value))
  cat(sprintf("  Welch's t=%.3f, df=%.1f, p=%.4f  |  Cohen's d=%.3f\n",
              tt$statistic, tt$parameter, tt$p.value, d))
  cat(ifelse(tt$p.value < 0.05,
             "  ✓ Significant (p < .05)\n",
             "  ✗ Not significant\n"))
  
  list(col = col,
       M_resistant = mean(a), SD_resistant = sd(a), n_resistant = length(a),
       M_susceptible = mean(b), SD_susceptible = sd(b), n_susceptible = length(b),
       t = tt$statistic, df = tt$parameter, p = tt$p.value, cohens_d = d)
}


# ── Pearson + Spearman correlation ────────────────────────────────────────
run_correlation <- function(df, col_x, col_y, label) {
  tmp <- df %>%
    select(all_of(c(col_x, col_y))) %>%
    mutate(
      x = as.numeric(.data[[col_x]]),
      y = as.numeric(.data[[col_y]])
    ) %>%
    select(x, y) %>%
    na.omit()
  
  if (nrow(tmp) < 3) {
    cat(sprintf("  ⚠  Not enough data for correlation: '%s' vs '%s'\n\n", col_x, col_y))
    return(NULL)
  }
  
  pr <- cor.test(tmp$x, tmp$y, method = "pearson")
  sr <- cor.test(tmp$x, tmp$y, method = "spearman", exact = FALSE)
  
  cat(sprintf("\n%s\n", strrep("─", 60)))
  cat(sprintf("  Correlation : %s\n", label))
  cat(sprintf("  n=%d\n", nrow(tmp)))
  cat(sprintf("  Pearson  r=%.3f, p=%.4f\n", pr$estimate, pr$p.value))
  cat(sprintf("  Spearman ρ=%.3f, p=%.4f\n", sr$estimate, sr$p.value))
  cat(ifelse(pr$p.value < 0.05,
             "  ✓ Significant (p < .05)\n",
             "  ✗ Not significant\n"))
  
  list(label = label, n = nrow(tmp),
       x = tmp$x, y = tmp$y,
       pearson_r = pr$estimate, pearson_p = pr$p.value,
       spearman_r = sr$estimate, spearman_p = sr$p.value)
}


# ══════════════════════════════════════════════════════════════════════════
# PLOTTING FUNCTIONS
# ══════════════════════════════════════════════════════════════════════════

# ── Violin + box plots for t-tests ────────────────────────────────────────
plot_ttest <- function(resistant, susceptible, col, result) {
  plot_df <- bind_rows(
    resistant %>%
      select(score = all_of(col)) %>%
      mutate(Group = RESISTANT_LABEL, score = as.numeric(score)),
    susceptible %>%
      select(score = all_of(col)) %>%
      mutate(Group = SUSCEPTIBLE_LABEL, score = as.numeric(score))
  ) %>% na.omit()
  
  if (nrow(plot_df) == 0) return(NULL)
  
  short_title <- ifelse(nchar(col) > 45, paste0(substr(col, 1, 42), "…"), col)
  p_label <- ifelse(!is.null(result),
                    sprintf("p = %.3f%s", result$p, ifelse(result$p < 0.05, " *", "")),
                    "")
  
  ggplot(plot_df, aes(x = Group, y = score, fill = Group)) +
    geom_violin(alpha = 0.45, trim = TRUE, colour = NA) +
    geom_boxplot(width = 0.18, outlier.shape = 21, outlier.size = 1.5,
                 colour = "grey30", fill = "white") +
    geom_jitter(width = 0.08, size = 1.2, alpha = 0.5, colour = "grey20") +
    scale_fill_manual(values = GROUP_COLOURS) +
    annotate("text", x = 1.5, y = max(plot_df$score, na.rm = TRUE),
             label = p_label, size = 3.5, colour = "darkred", fontface = "italic") +
    labs(title = short_title, x = NULL, y = "Score") +
    base_theme
}


# ── Scatter + regression for correlations ────────────────────────────────
plot_correlation <- function(corr, x_label, y_label, title) {
  plot_df <- data.frame(x = corr$x, y = corr$y)
  
  annot <- sprintf(
    "Pearson r = %.3f  (p = %.3f)\nSpearman ρ = %.3f  (p = %.3f)\nn = %d",
    corr$pearson_r, corr$pearson_p,
    corr$spearman_r, corr$spearman_p,
    corr$n
  )
  
  ggplot(plot_df, aes(x = x, y = y)) +
    geom_point(colour = "#2166AC", size = 2.5, alpha = 0.65,
               shape = 21, fill = "#2166AC", stroke = 0.3) +
    geom_smooth(method = "lm", se = TRUE, colour = "crimson",
                fill = "pink", alpha = 0.15, linewidth = 0.9) +
    annotate("label", x = -Inf, y = Inf, label = annot,
             hjust = -0.05, vjust = 1.1, size = 3.2,
             fill = "white", label.size = 0.3, colour = "grey20") +
    labs(title = title, x = x_label, y = y_label) +
    base_theme
}


# ══════════════════════════════════════════════════════════════════════════
# MAIN
# ══════════════════════════════════════════════════════════════════════════

cat(strrep("=", 70), "\n")
cat("  Neurosurgery Cognitive Bias – Education + Stimulation Analysis\n")
cat(strrep("=", 70), "\n")

# Load both sheets
data_list <- load_data()
stim_df <- data_list$stimulation
edu_df <- data_list$education

# Combine on participant ID
df <- combine_data(stim_df, edu_df)

# Split into groups
groups <- split_groups(df)
resistant   <- groups$resistant
susceptible <- groups$susceptible


# ── 1. T-TESTS (EDUCATION VARIABLES) ──────────────────────────────────────
cat("\n╔", strrep("═", 66), "╗\n", sep = "")
cat("║  1. EDUCATION OUTCOMES – INDEPENDENT-SAMPLES T-TESTS", strrep(" ", 11), "║\n", sep = "")
cat("╚", strrep("═", 66), "╝\n", sep = "")

education_test_cols <- c(
  BIAS_MITIGATION_COL,
  REALISM_FOLLOWUP_COL,
  TEACHING_EFFECTIVENESS_COL,
  CLINICAL_PRACTICE_COL
)

ttest_results <- lapply(education_test_cols, function(col) {
  if (col %in% names(df)) {
    run_ttest(resistant, susceptible, col)
  } else {
    cat(sprintf("  ⚠  Column not found: '%s'\n", col))
    NULL
  }
})
names(ttest_results) <- education_test_cols

# Build and save violin plots
violin_plots <- mapply(function(col, res) {
  if (!is.null(res) && col %in% names(df)) {
    plot_ttest(resistant, susceptible, col, res)
  } else {
    NULL
  }
}, education_test_cols, ttest_results, SIMPLIFY = FALSE)

violin_plots <- Filter(Negate(is.null), violin_plots)

if (length(violin_plots) > 0) {
  combined_violins <- wrap_plots(violin_plots, ncol = min(2, length(violin_plots)))
  ggsave("t_test_results_education.png", combined_violins,
         width = 5 * min(2, length(violin_plots)), height = 5, dpi = 150)
  cat("\n  → Saved: t_test_results_education.png\n")
}


# ── 2. CORRELATIONS: EDUCATION vs BIAS RESISTANCE ───────────────────────
cat("\n╔", strrep("═", 66), "╗\n", sep = "")
cat("║  2. EDUCATION vs BIAS RESISTANCE CORRELATIONS", strrep(" ", 19), "║\n", sep = "")
cat("╚", strrep("═", 66), "╝\n", sep = "")

# Encode group as numeric for correlation (1 = Resistant, 0 = Susceptible)
df <- df %>%
  mutate(group_numeric = as.integer(trimws(.data[[GROUP_COL]]) == RESISTANT_LABEL))

# Correlations between education variables and bias resistance
corr_results <- list()

if (BIAS_MITIGATION_COL %in% names(df)) {
  corr_results$bias_mitigation <- run_correlation(
    df,
    col_x = BIAS_MITIGATION_COL,
    col_y = "group_numeric",
    label = "Bias-mitigation skills → Bias Resistance"
  )
}

if (TEACHING_EFFECTIVENESS_COL %in% names(df)) {
  corr_results$teaching <- run_correlation(
    df,
    col_x = TEACHING_EFFECTIVENESS_COL,
    col_y = "group_numeric",
    label = "Teaching effectiveness → Bias Resistance"
  )
}

if (CLINICAL_PRACTICE_COL %in% names(df)) {
  corr_results$clinical_practice <- run_correlation(
    df,
    col_x = CLINICAL_PRACTICE_COL,
    col_y = "group_numeric",
    label = "Clinical practice improvement → Bias Resistance"
  )
}

# Plot correlations
corr_plots <- list()

if (!is.null(corr_results$bias_mitigation)) {
  corr_plots$bias_mitigation <- plot_correlation(
    corr_results$bias_mitigation,
    x_label = "Bias-mitigation skills score",
    y_label = "Bias Resistance (1=Resistant, 0=Susceptible)",
    title = "Do perceived bias-mitigation skills correlate\nwith bias resistance?"
  )
  ggsave("corr_bias_mitigation_vs_resistance.png", corr_plots$bias_mitigation,
         width = 5.5, height = 5, dpi = 150)
  cat("  → Saved: corr_bias_mitigation_vs_resistance.png\n")
}

if (!is.null(corr_results$teaching)) {
  corr_plots$teaching <- plot_correlation(
    corr_results$teaching,
    x_label = "Teaching effectiveness score",
    y_label = "Bias Resistance (1=Resistant, 0=Susceptible)",
    title = "Does teaching effectiveness correlate\nwith bias resistance?"
  )
  ggsave("corr_teaching_vs_resistance.png", corr_plots$teaching,
         width = 5.5, height = 5, dpi = 150)
  cat("  → Saved: corr_teaching_vs_resistance.png\n")
}

if (!is.null(corr_results$clinical_practice)) {
  corr_plots$clinical_practice <- plot_correlation(
    corr_results$clinical_practice,
    x_label = "Clinical practice improvement score",
    y_label = "Bias Resistance (1=Resistant, 0=Susceptible)",
    title = "Does clinical practice improvement correlate\nwith bias resistance?"
  )
  ggsave("corr_clinical_practice_vs_resistance.png", corr_plots$clinical_practice,
         width = 5.5, height = 5, dpi = 150)
  cat("  → Saved: corr_clinical_practice_vs_resistance.png\n")
}


# ── 3. SUMMARY TABLE ──────────────────────────────────────────────────────
cat("\n╔", strrep("═", 66), "╗\n", sep = "")
cat("║  3. SUMMARY TABLE", strrep(" ", 48), "║\n", sep = "")
cat("╚", strrep("═", 66), "╝\n", sep = "")

ttest_rows <- lapply(ttest_results, function(r) {
  if (is.null(r)) return(NULL)
  data.frame(
    Analysis = "Education t-test",
    Variable = substr(r$col, 1, 50),
    Statistic = sprintf("t = %.3f", r$t),
    p_value = sprintf("%.4f", r$p),
    Effect_size = sprintf("d = %.3f", r$cohens_d),
    Significant = ifelse(r$p < 0.05, "Yes", "No"),
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

corr_rows <- lapply(corr_results, function(x) {
  if (is.null(x)) return(NULL)
  data.frame(
    Analysis = "Correlation",
    Variable = x$label,
    Statistic = sprintf("r = %.3f", x$pearson_r),
    p_value = sprintf("%.4f", x$pearson_p),
    Effect_size = "—",
    Significant = ifelse(x$pearson_p < 0.05, "Yes", "No"),
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

summary_table <- bind_rows(ttest_rows, corr_rows)

if (nrow(summary_table) > 0) {
  print(summary_table, row.names = FALSE)
  write_xlsx(summary_table, "analysis_summary.xlsx")
  cat("\n  → Saved: analysis_summary.xlsx\n")
}


# ── 4. COMBINED DATA EXPORT ───────────────────────────────────────────────
cat("\n╔", strrep("═", 66), "╗\n", sep = "")
cat("║  4. COMBINED DATA EXPORT", strrep(" ", 40), "║\n", sep = "")
cat("╚", strrep("═", 66), "╝\n", sep = "")

# Export combined dataset for further analysis
write_xlsx(df, "combined_stimulation_education_data.xlsx")
cat("  → Saved: combined_stimulation_education_data.xlsx\n")
cat(sprintf("     (All %d matched participants with both datasets)\n\n", nrow(df)))

cat("Done! ✓\n\n")
