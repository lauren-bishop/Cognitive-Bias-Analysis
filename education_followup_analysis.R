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
PARTICIPANT_ID_COL_STIM <- "ppt_number"     # From Stimulation_data
PARTICIPANT_ID_COL_EDU  <- "Participant"    # From Education_data

# ── Group column (from Education sheet) ────────────────────────────────────
GROUP_COL         <- "Resistant or Susceptible"
RESISTANT_LABEL   <- "Resistant"
SUSCEPTIBLE_LABEL <- "Susceptible"

# ── STIMULATION VARIABLES ──────────────────────────────────────────────────
BIAS_SCORE_MRI <- "bias_score_mri_handover"
BIAS_SCORE_NOTES <- "bias_score_pt_notes"
BIAS_SCORE_CONFIRM <- "bias_score_pt_confirm"
BIAS_SCORE_CONSULT <- "bias_score_consult"
BIAS_SCORE_TOTAL <- "bias_score_total"

SURG_MENTAL <- "surg_mental"
SURG_PHYSICAL <- "surg_physical"
SURG_TEMPORAL <- "surg_temporal"
SURG_TASK <- "surg_task"
SURG_STRESS <- "surg_stress"
SURG_DISTRACTION <- "surg_distraction"
SURG_TOTAL <- "surg_total"

REALISM_EXPERIMENT <- "How realistic is the simulation?"
STRESS_DURING <- "How stressed did you feel during the procedure?"
CONFIDENCE_DECISION <- "How confident did you feel in your decision-making process?"
PRESSURE_REGISTRAR <- "Did you feel any pressure to agree with the outgoing neurosurgical registrar?"

# ── EDUCATION VARIABLES ────────────────────────────────────────────────────
BIAS_MITIGATION_COL       <- "This simulation helped me develop bias-mitigation skills"
REALISM_FOLLOWUP_COL      <- "The simulation adequately reflected real clinical pressures and decision-making challenges"
TEACHING_EFFECTIVENESS_COL <- "This simulation was more effective than traditional teaching methods for learning about cognitive bias"
CLINICAL_PRACTICE_COL     <- "Participation in this simulation improved my clinical practice"
KNOWLEDGE_COL             <- "I acquired new knowledge about cognitive biases in this simulation"

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
  cat(sprintf("Rows: %d\n", nrow(stim_df)))
  
  cat("\n── Loading Education data ───────────────────────────────────\n")
  edu_df <- read_excel(FILE_PATH, sheet = SHEET_EDUCATION)
  cat(sprintf("Rows: %d\n\n", nrow(edu_df)))
  
  list(stimulation = stim_df, education = edu_df)
}


# ── Combine sheets on participant ID ───────────────────────────────────────
combine_data <- function(stim_df, edu_df) {
  cat("── Joining sheets on Participant ID ──────────────────────────\n")
  
  # Standardize participant IDs (convert to numeric if needed)
  stim_df <- stim_df %>% 
    mutate(participant_id = as.numeric(.data[[PARTICIPANT_ID_COL_STIM]]))
  edu_df <- edu_df %>% 
    mutate(participant_id = as.numeric(.data[[PARTICIPANT_ID_COL_EDU]]))
  
  # Join on participant ID
  combined_df <- stim_df %>%
    inner_join(edu_df, by = "participant_id")
  
  cat(sprintf("Stimulation data: %d rows\n", nrow(stim_df)))
  cat(sprintf("Education data: %d rows\n", nrow(edu_df)))
  cat(sprintf("Combined (matched): %d rows\n\n", nrow(combined_df)))
  
  if (nrow(combined_df) == 0) {
    stop("⚠ No matching participant IDs found!")
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


# ── Check variance before t-test ───────────────────────────────────────────
has_variance <- function(resistant, susceptible, col) {
  a <- as.numeric(resistant[[col]]) %>% na.omit()
  b <- as.numeric(susceptible[[col]]) %>% na.omit()
  
  # Check for constant values
  if (length(unique(a)) <= 1 || length(unique(b)) <= 1) {
    return(FALSE)
  }
  return(TRUE)
}


# ── Welch t-test with Cohen's d ───────────────────────────────────────────
run_ttest <- function(resistant, susceptible, col) {
  a <- as.numeric(resistant[[col]]) %>% na.omit()
  b <- as.numeric(susceptible[[col]]) %>% na.omit()
  
  if (length(a) < 2 || length(b) < 2) {
    return(NULL)
  }
  
  # Check variance
  if (length(unique(a)) <= 1 || length(unique(b)) <= 1) {
    cat(sprintf("  ⚠  Constant values in: '%s'\n", col))
    return(NULL)
  }
  
  tryCatch({
    tt  <- t.test(a, b, var.equal = FALSE)
    lev <- var.test(a, b)
    d   <- cohens_d(a, b)$Cohens_d
    
    cat(sprintf("\n%s\n", strrep("─", 70)))
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
  }, error = function(e) {
    cat(sprintf("  ✗ Error testing '%s': %s\n", col, e$message))
    NULL
  })
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
    return(NULL)
  }
  
  tryCatch({
    pr <- cor.test(tmp$x, tmp$y, method = "pearson")
    sr <- cor.test(tmp$x, tmp$y, method = "spearman", exact = FALSE)
    
    cat(sprintf("\n%s\n", strrep("─", 70)))
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
  }, error = function(e) {
    cat(sprintf("  ✗ Error in correlation: %s\n", e$message))
    NULL
  })
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
    geom_smooth(method = "lm", se = TRUE, colour = "#D6604D",
                fill = "#D6604D", alpha = 0.15, linewidth = 0.9) +
    annotate("label", x = -Inf, y = Inf, label = annot,
             hjust = -0.05, vjust = 1.1, size = 3.2,
             fill = "white", colour = "grey20", label.padding = unit(0.3, "lines")) +
    labs(title = title, x = x_label, y = y_label) +
    base_theme
}


# ══════════════════════════════════════════════════════════════════════════
# MAIN
# ══════════════════════════════════════════════════════════════════════════

cat(strrep("=", 80), "\n")
cat("  Neurosurgery Cognitive Bias – Education + Stimulation Analysis\n")
cat(strrep("=", 80), "\n")

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


# ══════════════════════════════════════════════════════════════════════════
# 1. T-TESTS: STIMULATION PERFORMANCE
# ══════════════════════════════════════════════════════════════════════════
cat("\n╔", strrep("═", 76), "╗\n", sep = "")
cat("║  1. STIMULATION PERFORMANCE – T-TESTS (Resistant vs Susceptible)", strrep(" ", 9), "║\n", sep = "")
cat("╚", strrep("═", 76), "╝\n", sep = "")

stimulation_test_cols <- c(
  BIAS_SCORE_TOTAL,
  BIAS_SCORE_MRI,
  BIAS_SCORE_NOTES,
  BIAS_SCORE_CONFIRM,
  BIAS_SCORE_CONSULT,
  SURG_TOTAL,
  SURG_MENTAL,
  SURG_STRESS,
  REALISM_EXPERIMENT,
  STRESS_DURING,
  CONFIDENCE_DECISION
)

stim_ttest_results <- lapply(stimulation_test_cols, function(col) {
  if (col %in% names(df)) {
    run_ttest(resistant, susceptible, col)
  } else {
    NULL
  }
})
names(stim_ttest_results) <- stimulation_test_cols
stim_ttest_results <- Filter(Negate(is.null), stim_ttest_results)

# Build and save violin plots for stimulation
if (length(stim_ttest_results) > 0) {
  stim_violin_plots <- mapply(function(col, res) {
    if (col %in% names(df)) {
      plot_ttest(resistant, susceptible, col, res)
    } else {
      NULL
    }
  }, names(stim_ttest_results), stim_ttest_results, SIMPLIFY = FALSE)
  
  stim_violin_plots <- Filter(Negate(is.null), stim_violin_plots)
  
  if (length(stim_violin_plots) > 0) {
    combined_stim_violins <- wrap_plots(stim_violin_plots, ncol = 3)
    ggsave("t_test_results_stimulation.png", combined_stim_violins,
           width = 15, height = 12, dpi = 150)
    cat("\n  → Saved: t_test_results_stimulation.png\n")
  }
}


# ══════════════════════════════════════════════════════════════════════════
# 2. T-TESTS: EDUCATION OUTCOMES
# ══════════════════════════════════════════════════════════════════════════
cat("\n╔", strrep("═", 76), "╗\n", sep = "")
cat("║  2. EDUCATION OUTCOMES – T-TESTS (Resistant vs Susceptible)", strrep(" ", 12), "║\n", sep = "")
cat("╚", strrep("═", 76), "╝\n", sep = "")

education_test_cols <- c(
  BIAS_MITIGATION_COL,
  REALISM_FOLLOWUP_COL,
  TEACHING_EFFECTIVENESS_COL,
  CLINICAL_PRACTICE_COL,
  KNOWLEDGE_COL
)

edu_ttest_results <- lapply(education_test_cols, function(col) {
  if (col %in% names(df)) {
    run_ttest(resistant, susceptible, col)
  } else {
    NULL
  }
})
names(edu_ttest_results) <- education_test_cols
edu_ttest_results <- Filter(Negate(is.null), edu_ttest_results)

# Build and save violin plots for education
if (length(edu_ttest_results) > 0) {
  edu_violin_plots <- mapply(function(col, res) {
    if (col %in% names(df)) {
      plot_ttest(resistant, susceptible, col, res)
    } else {
      NULL
    }
  }, names(edu_ttest_results), edu_ttest_results, SIMPLIFY = FALSE)
  
  edu_violin_plots <- Filter(Negate(is.null), edu_violin_plots)
  
  if (length(edu_violin_plots) > 0) {
    combined_edu_violins <- wrap_plots(edu_violin_plots, ncol = 2)
    ggsave("t_test_results_education.png", combined_edu_violins,
           width = 10, height = 10, dpi = 150)
    cat("\n  → Saved: t_test_results_education.png\n")
  }
}


# ══════════════════════════════════════════════════════════════════════════
# 3. CORRELATIONS
# ══════════════════════════════════════════════════════════════════════════
cat("\n╔", strrep("═", 76), "╗\n", sep = "")
cat("║  3. BIAS SCORES vs EDUCATION OUTCOMES CORRELATIONS", strrep(" ", 23), "║\n", sep = "")
cat("╚", strrep("═", 76), "╝\n", sep = "")

corr_results <- list()

# 3a. Total bias score vs bias mitigation skills
if (BIAS_SCORE_TOTAL %in% names(df) && BIAS_MITIGATION_COL %in% names(df)) {
  corr_results$bias_total_vs_mitigation <- run_correlation(
    df,
    col_x = BIAS_SCORE_TOTAL,
    col_y = BIAS_MITIGATION_COL,
    label = "Total Bias Score → Bias-Mitigation Skills"
  )
}

# 3b. Surgical total vs clinical practice improvement
if (SURG_TOTAL %in% names(df) && CLINICAL_PRACTICE_COL %in% names(df)) {
  corr_results$surg_total_vs_practice <- run_correlation(
    df,
    col_x = SURG_TOTAL,
    col_y = CLINICAL_PRACTICE_COL,
    label = "Surgical Performance → Clinical Practice Improvement"
  )
}

# 3c. Realism (experiment) vs realism (follow-up)
if (REALISM_EXPERIMENT %in% names(df) && REALISM_FOLLOWUP_COL %in% names(df)) {
  corr_results$realism_consistency <- run_correlation(
    df,
    col_x = REALISM_EXPERIMENT,
    col_y = REALISM_FOLLOWUP_COL,
    label = "Realism Consistency: Experiment vs Follow-up"
  )
}

# 3d. Stress during procedure vs bias scores
if (STRESS_DURING %in% names(df) && BIAS_SCORE_TOTAL %in% names(df)) {
  corr_results$stress_vs_bias <- run_correlation(
    df,
    col_x = STRESS_DURING,
    col_y = BIAS_SCORE_TOTAL,
    label = "Stress During Procedure → Total Bias Score"
  )
}

# 3e. Confidence vs bias scores
if (CONFIDENCE_DECISION %in% names(df) && BIAS_SCORE_TOTAL %in% names(df)) {
  corr_results$confidence_vs_bias <- run_correlation(
    df,
    col_x = CONFIDENCE_DECISION,
    col_y = BIAS_SCORE_TOTAL,
    label = "Decision-Making Confidence → Total Bias Score"
  )
}

corr_results <- Filter(Negate(is.null), corr_results)

# Plot correlations
if (length(corr_results) > 0) {
  for (i in seq_along(corr_results)) {
    plot_name <- names(corr_results)[i]
    corr <- corr_results[[i]]
    
    tryCatch({
      if (plot_name == "bias_total_vs_mitigation") {
        p <- plot_correlation(corr, "Total Bias Score", "Bias-Mitigation Skills Rating",
                             "Does simulation bias performance predict\nperceived bias-mitigation learning?")
        ggsave("corr_bias_score_vs_mitigation.png", p, width = 5.5, height = 5, dpi = 150)
        cat("  → Saved: corr_bias_score_vs_mitigation.png\n")
      } else if (plot_name == "surg_total_vs_practice") {
        p <- plot_correlation(corr, "Surgical Performance Score", "Clinical Practice Improvement",
                             "Does surgical performance predict\nclinical practice improvement?")
        ggsave("corr_surgical_vs_practice.png", p, width = 5.5, height = 5, dpi = 150)
        cat("  → Saved: corr_surgical_vs_practice.png\n")
      } else if (plot_name == "realism_consistency") {
        p <- plot_correlation(corr, "Realism (Experiment)", "Realism (Follow-up Survey)",
                             "Did participants change their minds\nabout simulation realism?")
        ggsave("corr_realism_consistency.png", p, width = 5.5, height = 5, dpi = 150)
        cat("  → Saved: corr_realism_consistency.png\n")
      } else if (plot_name == "stress_vs_bias") {
        p <- plot_correlation(corr, "Stress During Procedure", "Total Bias Score",
                             "Does stress during simulation\npredict bias performance?")
        ggsave("corr_stress_vs_bias.png", p, width = 5.5, height = 5, dpi = 150)
        cat("  → Saved: corr_stress_vs_bias.png\n")
      } else if (plot_name == "confidence_vs_bias") {
        p <- plot_correlation(corr, "Decision-Making Confidence", "Total Bias Score",
                             "Does overconfidence predict worse\nbias performance?")
        ggsave("corr_confidence_vs_bias.png", p, width = 5.5, height = 5, dpi = 150)
        cat("  → Saved: corr_confidence_vs_bias.png\n")
      }
    }, error = function(e) {
      cat(sprintf("  ✗ Error plotting %s: %s\n", plot_name, e$message))
    })
  }
}


# ══════════════════════════════════════════════════════════════════════════
# 4. SUMMARY TABLE
# ══════════════════════════════════════════════════════════════════════════
cat("\n╔", strrep("═", 76), "╗\n", sep = "")
cat("║  4. STATISTICAL SUMMARY TABLE", strrep(" ", 45), "║\n", sep = "")
cat("╚", strrep("═", 76), "╝\n", sep = "")

# T-test rows
all_ttest_rows <- lapply(c(stim_ttest_results, edu_ttest_results), function(r) {
  if (is.null(r)) return(NULL)
  data.frame(
    Analysis = "t-test",
    Variable = substr(r$col, 1, 55),
    Statistic = sprintf("t = %.3f", r$t),
    p_value = sprintf("%.4f", r$p),
    Effect_size = sprintf("d = %.3f", r$cohens_d),
    Significant = ifelse(r$p < 0.05, "Yes", "No"),
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

# Correlation rows
corr_rows <- lapply(corr_results, function(x) {
  if (is.null(x)) return(NULL)
  data.frame(
    Analysis = "Correlation",
    Variable = substr(x$label, 1, 55),
    Statistic = sprintf("r = %.3f", x$pearson_r),
    p_value = sprintf("%.4f", x$pearson_p),
    Effect_size = "—",
    Significant = ifelse(x$pearson_p < 0.05, "Yes", "No"),
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

summary_table <- bind_rows(all_ttest_rows, corr_rows)

if (nrow(summary_table) > 0) {
  print(summary_table, row.names = FALSE)
  write_xlsx(summary_table, "analysis_summary.xlsx")
  cat("\n  → Saved: analysis_summary.xlsx\n")
}


# ══════════════════════════════════════════════════════════════════════════
# 5. COMBINED DATA EXPORT
# ══════════════════════════════════════════════════════════════════════════
cat("\n╔", strrep("═", 76), "╗\n", sep = "")
cat("║  5. COMBINED DATA EXPORT", strrep(" ", 49), "║\n", sep = "")
cat("╚", strrep("═", 76), "╝\n", sep = "")

write_xlsx(df, "combined_stimulation_education_data.xlsx")
cat("  → Saved: combined_stimulation_education_data.xlsx\n")
cat(sprintf("     (%d matched participants with complete datasets)\n\n", nrow(df)))

cat(strrep("=", 80), "\n")
cat("Done! ✓\n\n")
