# Libraries
library(shiny)
library(shinydashboard)
library(readr)

# Settings (server-safe: artefacts live under www)
ARTEFACTS_DIR <- "www/artefacts"

if (dir.exists(ARTEFACTS_DIR)) {
  shiny::addResourcePath("arts", ARTEFACTS_DIR)  # served at /arts/<filename>
} else {
  warning("Artefacts directory not found: ", ARTEFACTS_DIR)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x


# Notes helpers (analysis / shows / insight)
NOTES_PATH <- file.path(ARTEFACTS_DIR, "notes.csv")
safe_read_notes <- function() {
  if (file.exists(NOTES_PATH)) readr::read_csv(NOTES_PATH, show_col_types = FALSE)
  else if (file.exists("notes.csv")) readr::read_csv("notes.csv", show_col_types = FALSE)
  else NULL
}
notes <- safe_read_notes()
get_note <- function(file_basename) {
  if (is.null(notes)) return(NULL)
  row <- notes[basename(notes$file) == basename(file_basename), , drop = FALSE]
  if (nrow(row)) row[1,] else NULL
}

# Artefact helpers
list_arts <- function() {
  if (!dir.exists(ARTEFACTS_DIR)) return(character(0))
  list.files(ARTEFACTS_DIR, pattern = "\\.html$", full.names = TRUE)
}
pretty_name <- function(path) {
  nm <- basename(path)
  nm <- sub("\\.html$", "", nm)
  nm <- gsub("_", " ", nm)
  tools::toTitleCase(nm)
}
choices_for <- function(files) {
  if (!length(files)) return(setNames(character(0), character(0)))
  setNames(files, vapply(files, pretty_name, "", USE.NAMES = FALSE))
}
iframe_for <- function(full_path, height = "700px") {
  src_rel <- file.path("arts", basename(full_path))
  tags$iframe(src = src_rel, width = "100%", height = height, class = "artifact-frame")
}

# Discover files
ALL_FILES <- list_arts()
b_all <- tolower(basename(ALL_FILES))

# Exclude visuals you listed as not working (match on filename patterns)
# (NOTE: We intentionally *do not* exclude overlay_minute_we_ggplotly or overlay_minute_reports_plotly)
EXCLUDE_PATTERNS <- c(
  # EDA.Rmd — remove broken ones
  "average_cpu_usage_over_time",
  "cpu_usage_over_time",                 # Average CPU usage over time
  "eda_logs_over_time_by_level",         # Logs over time by log level
  "logs_over_time",                      # Log volume over time
  "eda_logs_by_hour",                    # Logs by hour of day
  "trace_events_over_time",              # Trace events over time
  "trace_volume_by_operation",           # Trace volume by operation
  "trce_volume_by_operation",            # (typo-safe)
  
  # Log cleaning & classification.Rmd
  "logs_by_hour_of_day",
  "logs_by_hour",
  "logs_over_time_by_log_level",
  "cpu_usage_vs_errors_over_time",
  "cpu_vs_errors_wally",
  "wally_113", "wally_117", "wally_122", "wally_123", "wally_124"
)

exclude_regex <- if (length(EXCLUDE_PATTERNS)) paste(EXCLUDE_PATTERNS, collapse = "|") else "^$"
keep_mask <- if (length(ALL_FILES)) !grepl(exclude_regex, b_all, perl = TRUE) else logical(0)
ALL_FILES <- ALL_FILES[keep_mask]
b <- tolower(basename(ALL_FILES))

# Data tab shows only tabular HTMLs with "table" in the filename
DATA_FILES   <- ALL_FILES[grepl("table", b)]
# Model files for the original Models tabs
MODEL_FILES  <- ALL_FILES[grepl("(^|_)roc|confusion|feature_importance|^rf_|pred_vs_actual", b, perl = TRUE)]
# EDA catch-all (for the original EDA tabs)
EDA_FILES    <- setdiff(ALL_FILES, union(DATA_FILES, MODEL_FILES))

# Helper to filter by multiple grep keys (safe even with empty keys)
match_keys <- function(files, keys) {
  if (!length(files) || !length(keys)) return(character(0))
  bb <- tolower(basename(files))
  mask_list <- lapply(keys, function(k) grepl(k, bb, perl = TRUE))
  keep <- Reduce(`|`, mask_list)
  files[keep]
}
files_for <- function(keys) {
  if (!length(keys)) return(character(0))
  match_keys(ALL_FILES, keys)
}

# EDA sub-groups (by filename keywords) — for ORIGINAL EDA
EDA_GROUPS <- list(
  time   = c("minute", "time_series", "over_time", "reports", "_we_",
             "overlay_minute_we_ggplotly", "overlay_minute_reports_plotly"),
  corr   = c("corr", "correlation", "heatmap"),
  hist   = c("^hist_", "distribution"),
  trace  = c("^trace_", "trace_events", "traces_"),
  ratio  = c("^ratio", "error_rate", "errors"),
  cpuerr = c("cpu_vs_errors"),
  ccf    = c("^ccf_")
)

# Model sub-groups (by filename keywords)
MODEL_GROUPS <- list(
  rf    = c("^rf_", "roc_rf", "feature_importance", "confusion"),
  rnn   = c("cnn", "lstm"),
  xf    = c("transformer", "attention"),
  tree  = c("xgboost", "gbm", "gradient_boost"),
  other = c("model", "pred_vs_actual")
)

# EDA notebook sections → filename patterns
EDA_SECTIONS <- list(
  eda_load_filter = c("table_.*head"),
  eda_logs        = c("log_level_distribution","http_status_distribution","eda_log_level_counts","eda_logs_"),
  eda_metrics     = c("cpu_usage_distribution","memory_usage_distribution","hist_cpu_user_avg","hist_mem_used_avg",
                      "metrics_correlation_heatmap","numeric_corr_heatmap"),
  eda_traces      = c("traces_top_operations","traces_project_counts"),
  # Include overlays, *including* the two you asked to restore:
  eda_overlays    = c("^overlay_", "overlay_minute_we_ggplotly", "overlay_minute_reports_plotly"),
  eda_ratio_ccf   = c("^ratio","ratio_","ccf_","error_rate"),
  eda_align_feats = c("corr_matrix_minute","corr_matrix_minute_labeled"),
  eda_labels      = c("table_anomalies","error_rate_by_program","error_rate_program_pid_heatmap","total_errors_per_host"),
  eda_rf_baseline = c("^rf_","feature_importance","confusion","roc_rf","pred_vs_actual")
)

# Cleaning notebook sections → filename patterns
CLEAN_SECTIONS <- list(
  clean_overview   = c("table_logs_head","missingness_heatmap"),
  level_status     = c("log_level_distribution","eda_log_level_counts","http_status_distribution"),
  temporal_levels  = c("eda_logs_over_time_by_level|eda_logs_by_hour"), # excluded by patterns anyway
  entities         = c("eda_logs_per_user_hist","eda_logs_per_program_by_level",
                       "total_errors_per_host","error_rate_by_program",
                       "error_rate_program_pid_heatmap",
                       "top_programs_by_errors_across_hosts","top_pids_by_error_rate"),
  cpu_error_hosts  = c("error_rate_vs_avg_cpu_program_host",
                       "host_cpu_vs_mem_bubble","memory_vs_error_highlight"),
  corr_missing     = c("missingness_heatmap","numeric_corr_heatmap","metrics_correlation_heatmap",
                       "corr_matrix_minute","corr_matrix_minute_labeled")
)

# Defaults: take up to 3 visuals per group
default_three <- function(x) if (!length(x)) character(0) else head(x, 6)

HEIGHT_CHOICES <- c("700px","850px","1000px","1200px","85vh","90vh","95vh")

# Narratives (title + purpose per tabName)
NARR <- list(
  data = list(
    title   = "Establishing the ground truth",
    purpose = paste0(
      "Verify schemas, keys, time zones, and basic ranges before modeling. ",
      "Tables originate from the OpenStack Telemetry dataset (logs, metrics, traces with injected faults) ",
      "and were assembled using helper Python scripts from the linked GitHub repo. ",
      "These tables are the shared reference for everything else."
    )
  ),
  eda_time = list(
    title="Temporal Structure",
    purpose="Compare sequences and windows (minute-level overlays and report windows) to spot bursts, quiet periods, and recovery."
  ),
  eda_corr = list(
    title="Correlations & Heatmaps",
    purpose="Quantify relationships among metrics/logs/traces; surface multicollinearity and co-movement."
  ),
  eda_hist = list(
    title="Distributions & Ranges",
    purpose="Inspect value ranges and tails to understand normal operating envelopes vs outliers."
  ),
  eda_trace = list(
    title="Trace Activity & Coverage",
    purpose="Identify dominant services/operations and their contribution to total load over time."
  ),
  eda_ratio = list(
    title="Ratios & Error Rates",
    purpose="Track trace-to-op ratios and error rates to assess observability health and reliability."
  ),
  eda_cpuerr = list(
    title="CPU vs Errors",
    purpose="Test resource-pressure hypotheses by host/program; highlight hotspots worth investigation."
  ),
  eda_ccf = list(
    title="Cross-Correlation",
    purpose="Explore lead–lag relationships (e.g., warnings vs coverage) for causal hints."
  ),
  model_rf = list(
    title="Random Forest",
    purpose="Strict-label baseline for anomaly detection; interpret via feature importance, ROC, and confusion."
  ),
  model_rnn = list(
    title="CNN/LSTM",
    purpose="Sequence-aware models for temporal patterns and bursts (if available among artefacts)."
  ),
  model_xf = list(
    title="Transformers",
    purpose="Attention-based models for cross-modal temporal relationships (if available among artefacts)."
  ),
  model_tree = list(
    title="Tree-based (XGB/GBM)",
    purpose="Strong tabular baselines; useful for feature-driven inference (if artefacts exist)."
  ),
  model_other = list(
    title="Other / Mixed",
    purpose="Any additional diagnostics (predictions vs actuals, hybrids, etc.)."
  ),
  # NOTEBOOK: EDA.Rmd
  eda_load_filter = list(
    title="Load & Filter (EDA.Rmd)",
    purpose="Confirm time-window alignment across logs/metrics/traces, and sanity-check counts before deeper analysis."
  ),
  eda_logs = list(
    title="Log Exploration (EDA.Rmd)",
    purpose="Understand log levels, HTTP statuses, and volume patterns to separate API access logs from system logs."
  ),
  eda_metrics = list(
    title="Metrics Exploration (EDA.Rmd)",
    purpose="Inspect CPU/memory distributions and correlations; identify whether workloads are CPU- or memory-bound."
  ),
  eda_traces = list(
    title="Trace Exploration (EDA.Rmd)",
    purpose="See service/operation mix and patterns; identify dominant operations driving activity."
  ),
  eda_overlays = list(
    title="Overlays (EDA.Rmd)",
    purpose="Overlay logs, metrics, and traces on a common timeline."
  ),
  eda_ratio_ccf = list(
    title="Ratios & Cross-Correlation (EDA.Rmd)",
    purpose="Check trace coverage health (trace/op ratio) and lead–lag relationships with warnings/errors."
  ),
  eda_align_feats = list(
    title="Alignment & Features (EDA.Rmd)",
    purpose="Aggregate per-window features and inspect inter-feature relationships to prepare ML inputs."
  ),
  eda_labels = list(
    title="Anomaly Labeling (EDA.Rmd)",
    purpose="Compare strict vs loose labels; ensure anomalies are informative and not trivially tied to a single feature."
  ),
  eda_rf_baseline = list(
    title="RF Baseline (EDA.Rmd)",
    purpose="Sanity-check modeling signal under strict labels; inspect ROC, confusion matrix, and importance."
  ),
  # NOTEBOOK: Cleaning
  clean_overview = list(
    title="Cleaning Overview (Log cleaning & classification.Rmd)",
    purpose="Summarize data quality, preview tables, and visualize missingness patterns to guide fixes."
  ),
  level_status = list(
    title="Log Levels & Status Codes (Log cleaning & classification.Rmd)",
    purpose="Establish the shape of log traffic across levels and HTTP outcomes to separate noise from signal."
  ),
  temporal_levels = list(
    title="Temporal by Level (Log cleaning & classification.Rmd)",
    purpose="(Problematic hour-of-day/over-time charts were removed per your request.)"
  ),
  entities = list(
    title="Users / Programs / Hosts (Log cleaning & classification.Rmd)",
    purpose="Find the noisiest actors and hot spots to prioritize remediation and instrumentation."
  ),
  cpu_error_hosts = list(
    title="CPU vs Errors by Host (Log cleaning & classification.Rmd)",
    purpose="Host-level pressure tests; removed Wally-specific over-time variants per your request."
  ),
  corr_missing = list(
    title="Correlations & Missingness (Log cleaning & classification.Rmd)",
    purpose="Quantify relationships and ensure gaps don't masquerade as normal behavior."
  )
)

story_box <- function(tab_name) {
  meta <- NARR[[tab_name]]
  if (is.null(meta)) return(NULL)
  box(width = 12, solidHeader = TRUE, title = meta$title,
      status = "primary",
      p(meta$purpose))
}

# Reusable "no visuals" box
no_visuals_box <- function() {
  box(width = 12, solidHeader = TRUE, title = "No visuals available",
      status = "warning",
      p("Please refer to the appropriate notebook for further information on the model."))
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "DXC AI Dashboard"),
  dashboardSidebar(
    sidebarMenu(id = "tabs", selected = "brief",
                menuItem("Brief", tabName = "brief", icon = icon("book-open")),
                
                # ORIGINAL sections
                menuItem("Data (Tables)", tabName = "data", icon = icon("table")),
                menuItem("EDA (Original)", icon = icon("chart-area"), startExpanded = TRUE,
                         menuSubItem("By Time/Sequence", tabName = "eda_time"),
                         menuSubItem("Correlations & Heatmaps", tabName = "eda_corr"),
                         menuSubItem("Distributions", tabName = "eda_hist"),
                         menuSubItem("Trace / Coverage / Events", tabName = "eda_trace"),
                         menuSubItem("Ratios & Error Rates", tabName = "eda_ratio"),
                         menuSubItem("CPU vs Errors", tabName = "eda_cpuerr"),
                         menuSubItem("Cross-Correlation (CCF)", tabName = "eda_ccf")
                ),
                menuItem("Models & Evaluation (Original)", icon = icon("microscope"), startExpanded = TRUE,
                         menuSubItem("Random Forest", tabName = "model_rf"),
                         menuSubItem("CNN / LSTM", tabName = "model_rnn"),
                         menuSubItem("Transformers", tabName = "model_xf"),
                         menuSubItem("Tree-based (XGB/GBM)", tabName = "model_tree"),
                         menuSubItem("Other / Mixed", tabName = "model_other")
                ),
                
                # NEW notebook-oriented sections (ADDED)
                menuItem("EDA.Rmd (by notebook)", icon = icon("book"), startExpanded = TRUE,
                         menuSubItem("Load & Filter",          tabName = "eda_load_filter"),
                         menuSubItem("Logs",                   tabName = "eda_logs"),
                         menuSubItem("Metrics",                tabName = "eda_metrics"),
                         menuSubItem("Traces",                 tabName = "eda_traces"),
                         menuSubItem("Overlays",               tabName = "eda_overlays"),
                         menuSubItem("Ratios & CCF",           tabName = "eda_ratio_ccf"),
                         menuSubItem("Alignment & Features",   tabName = "eda_align_feats"),
                         menuSubItem("Anomaly Labeling",       tabName = "eda_labels"),
                         menuSubItem("RF Baseline",            tabName = "eda_rf_baseline")
                ),
                menuItem("Log cleaning & classification.Rmd", icon = icon("broom"), startExpanded = TRUE,
                         menuSubItem("Cleaning Overview",             tabName = "clean_overview"),
                         menuSubItem("Levels & Status Codes",         tabName = "level_status"),
                         menuSubItem("Temporal by Level",             tabName = "temporal_levels"),
                         menuSubItem("Users/Programs/Hosts",          tabName = "entities"),
                         menuSubItem("CPU vs Errors by Host",         tabName = "cpu_error_hosts"),
                         menuSubItem("Correlations & Missingness",    tabName = "corr_missing")
                ),
                
                # Sidebar controls (left pane only)
                hr(),
                
                # Data controls
                conditionalPanel("input.tabs == 'data'",
                                 selectInput("data_files", "Select tables (multi):",
                                             choices  = choices_for(DATA_FILES),
                                             selected = default_three(DATA_FILES),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("data_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                
                # ORIGINAL EDA controls
                conditionalPanel("input.tabs == 'eda_time'",
                                 selectInput("eda_time_files", "Select visuals:",
                                             choices  = choices_for(match_keys(EDA_FILES, EDA_GROUPS$time)),
                                             selected = default_three(match_keys(EDA_FILES, EDA_GROUPS$time)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_time_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'eda_corr'",
                                 selectInput("eda_corr_files", "Select visuals:",
                                             choices  = choices_for(match_keys(EDA_FILES, EDA_GROUPS$corr)),
                                             selected = default_three(match_keys(EDA_FILES, EDA_GROUPS$corr)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_corr_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'eda_hist'",
                                 selectInput("eda_hist_files", "Select visuals:",
                                             choices  = choices_for(match_keys(EDA_FILES, EDA_GROUPS$hist)),
                                             selected = default_three(match_keys(EDA_FILES, EDA_GROUPS$hist)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_hist_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'eda_trace'",
                                 selectInput("eda_trace_files", "Select visuals:",
                                             choices  = choices_for(match_keys(EDA_FILES, EDA_GROUPS$trace)),
                                             selected = default_three(match_keys(EDA_FILES, EDA_GROUPS$trace)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_trace_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'eda_ratio'",
                                 selectInput("eda_ratio_files", "Select visuals:",
                                             choices  = choices_for(match_keys(EDA_FILES, EDA_GROUPS$ratio)),
                                             selected = default_three(match_keys(EDA_FILES, EDA_GROUPS$ratio)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_ratio_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'eda_cpuerr'",
                                 selectInput("eda_cpuerr_files", "Select visuals:",
                                             choices  = choices_for(match_keys(EDA_FILES, EDA_GROUPS$cpuerr)),
                                             selected = default_three(match_keys(EDA_FILES, EDA_GROUPS$cpuerr)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_cpuerr_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'eda_ccf'",
                                 selectInput("eda_ccf_files", "Select visuals:",
                                             choices  = choices_for(match_keys(EDA_FILES, EDA_GROUPS$ccf)),
                                             selected = default_three(match_keys(EDA_FILES, EDA_GROUPS$ccf)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_ccf_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                
                # ORIGINAL Model controls
                conditionalPanel("input.tabs == 'model_rf'",
                                 selectInput("model_rf_files", "Select diagnostics:",
                                             choices  = choices_for(match_keys(MODEL_FILES, MODEL_GROUPS$rf)),
                                             selected = default_three(match_keys(MODEL_FILES, MODEL_GROUPS$rf)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("model_rf_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'model_rnn'",
                                 selectInput("model_rnn_files", "Select diagnostics:",
                                             choices  = choices_for(match_keys(MODEL_FILES, MODEL_GROUPS$rnn)),
                                             selected = default_three(match_keys(MODEL_FILES, MODEL_GROUPS$rnn)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("model_rnn_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'model_xf'",
                                 selectInput("model_xf_files", "Select diagnostics:",
                                             choices  = choices_for(match_keys(MODEL_FILES, MODEL_GROUPS$xf)),
                                             selected = default_three(match_keys(MODEL_FILES, MODEL_GROUPS$xf)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("model_xf_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'model_tree'",
                                 selectInput("model_tree_files", "Select diagnostics:",
                                             choices  = choices_for(match_keys(MODEL_FILES, MODEL_GROUPS$tree)),
                                             selected = default_three(match_keys(MODEL_FILES, MODEL_GROUPS$tree)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("model_tree_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'model_other'",
                                 selectInput("model_other_files", "Select diagnostics:",
                                             choices  = choices_for(match_keys(MODEL_FILES, MODEL_GROUPS$other)),
                                             selected = default_three(match_keys(MODEL_FILES, MODEL_GROUPS$other)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("model_other_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                
                # Notebook-oriented selectors (after exclusions)
                conditionalPanel("input.tabs == 'eda_load_filter'",
                                 selectInput("eda_load_files", "Select visuals:",
                                             choices  = choices_for(files_for(EDA_SECTIONS$eda_load_filter)),
                                             selected = default_three(files_for(EDA_SECTIONS$eda_load_filter)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_load_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'eda_logs'",
                                 selectInput("eda_logs_files", "Select visuals:",
                                             choices  = choices_for(files_for(EDA_SECTIONS$eda_logs)),
                                             selected = default_three(files_for(EDA_SECTIONS$eda_logs)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_logs_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'eda_metrics'",
                                 selectInput("eda_metrics_files", "Select visuals:",
                                             choices  = choices_for(files_for(EDA_SECTIONS$eda_metrics)),
                                             selected = default_three(files_for(EDA_SECTIONS$eda_metrics)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_metrics_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'eda_traces'",
                                 selectInput("eda_traces_files", "Select visuals:",
                                             choices  = choices_for(files_for(EDA_SECTIONS$eda_traces)),
                                             selected = default_three(files_for(EDA_SECTIONS$eda_traces)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_traces_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'eda_overlays'",
                                 selectInput("eda_overlays_files", "Select visuals:",
                                             choices  = choices_for(files_for(EDA_SECTIONS$eda_overlays)),
                                             selected = default_three(files_for(EDA_SECTIONS$eda_overlays)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_overlays_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'eda_ratio_ccf'",
                                 selectInput("eda_ratio_files", "Select visuals:",
                                             choices  = choices_for(files_for(EDA_SECTIONS$eda_ratio_ccf)),
                                             selected = default_three(files_for(EDA_SECTIONS$eda_ratio_ccf)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_ratio_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'eda_align_feats'",
                                 selectInput("eda_align_files", "Select visuals:",
                                             choices  = choices_for(files_for(EDA_SECTIONS$eda_align_feats)),
                                             selected = default_three(files_for(EDA_SECTIONS$eda_align_feats)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_align_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'eda_labels'",
                                 selectInput("eda_labels_files", "Select visuals:",
                                             choices  = choices_for(files_for(EDA_SECTIONS$eda_labels)),
                                             selected = default_three(files_for(EDA_SECTIONS$eda_labels)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_labels_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'eda_rf_baseline'",
                                 selectInput("eda_rf_files", "Select visuals:",
                                             choices  = choices_for(files_for(EDA_SECTIONS$eda_rf_baseline)),
                                             selected = default_three(files_for(EDA_SECTIONS$eda_rf_baseline)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("eda_rf_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                
                conditionalPanel("input.tabs == 'clean_overview'",
                                 selectInput("clean_overview_files", "Select visuals:",
                                             choices  = choices_for(files_for(CLEAN_SECTIONS$clean_overview)),
                                             selected = default_three(files_for(CLEAN_SECTIONS$clean_overview)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("clean_overview_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'level_status'",
                                 selectInput("level_status_files", "Select visuals:",
                                             choices  = choices_for(files_for(CLEAN_SECTIONS$level_status)),
                                             selected = default_three(files_for(CLEAN_SECTIONS$level_status)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("level_status_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'temporal_levels'",
                                 selectInput("temporal_levels_files", "Select visuals:",
                                             choices  = choices_for(files_for(CLEAN_SECTIONS$temporal_levels)),
                                             selected = default_three(files_for(CLEAN_SECTIONS$temporal_levels)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("temporal_levels_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'entities'",
                                 selectInput("entities_files", "Select visuals:",
                                             choices  = choices_for(files_for(CLEAN_SECTIONS$entities)),
                                             selected = default_three(files_for(CLEAN_SECTIONS$entities)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("entities_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'cpu_error_hosts'",
                                 selectInput("cpu_error_hosts_files", "Select visuals:",
                                             choices  = choices_for(files_for(CLEAN_SECTIONS$cpu_error_hosts)),
                                             selected = default_three(files_for(CLEAN_SECTIONS$cpu_error_hosts)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("cpu_error_hosts_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                ),
                conditionalPanel("input.tabs == 'corr_missing'",
                                 selectInput("corr_missing_files", "Select visuals:",
                                             choices  = choices_for(files_for(CLEAN_SECTIONS$corr_missing)),
                                             selected = default_three(files_for(CLEAN_SECTIONS$corr_missing)),
                                             multiple = TRUE, width = "100%"),
                                 selectInput("corr_missing_h", "Frame Height:", HEIGHT_CHOICES, selected = "700px", width = "100%")
                )
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background: #fff; }
      .box { border-top: 3px solid #3c8dbc; }
      .box-body { padding: 8px 12px; }
      .artifact-frame { border: 0; }
      .note-block { margin-top: 8px; }
      .note-block h4 { margin-top: 0; font-size: 16px; }
      .brief-pre { white-space: pre-wrap; font-family: inherit; font-size: 14px; margin: 0; }
      /* Force proper tab behavior even if AdminLTE JS doesn't bind */
      .tab-pane { display: none !important; }
      .tab-pane.active { display: block !important; }
    "))),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('activate-tab', function(msg){
        var sel = '#shiny-tab-' + msg.id;
        $('.tab-pane').removeClass('active');
        $(sel).addClass('active');
        // also mark the corresponding menu item as active
        $('aside .sidebar-menu li').removeClass('active');
        $('aside .sidebar-menu a[data-value=\"' + msg.id + '\"]').closest('li').addClass('active');
      });
    ")),
    
    tabItems(
      
      # ---- Brief ----
      tabItem(tabName = "brief",
              fluidRow(
                box(width = 12, title = "DXC AI Team Assesment — Stakeholder Brief", solidHeader = TRUE,
                    tags$pre(class = "brief-pre",
                             "1) Executive Summary
This proof-of-concept demonstrates how AI-assisted analytics can convert fragmented operational data into clear, timely insight for faster incident diagnosis. Signals describing system health—application logs, infrastructure metrics, and distributed traces—are captured by different tools, time-stamped on different clocks, and owned by different teams. My approach unifies these signals, detects unusual behavior, and highlights meaningful correlations across systems. By experimenting with multiple models and robust techniques, I focus on understanding the data itself, producing a single interactive web dashboard that is both actionable for engineers and understandable to non-technical stakeholders.

2) IT, in Plain Language
Information Technology has four jobs that matter to the business:
• Manage information: store, organize, process, and back up data reliably.
• Secure information: ensure only authorized access, protect data in transit, and detect threats.
• Keep systems connected: ensure reliable communication between devices, applications, and cloud services.
• Keep systems running: maintain software, monitor performance, and ensure essential services remain available during incidents.

3) My Role and Why This Workflow Emphasises Data and Insight
As part of DXC’s AI function, this team prioritizes research-driven analytics over day-to-day infrastructure tasks. I focus on producing actionable insights by combining Python-based processing with satistical tools like R, ensuring the data pipeline reliably transforms raw data into interpretable intelligence.

4) The Problem in Business Terms
Enterprises generate multi-modal across servers, applications, and networks. Incidents often cross team and system boundaries, making root-cause analysis slow and error-prone. This fragmentation increases downtime, lost revenue, and unproductive engineer hours.

5) The Solution I Am Proposing
I will build an end-to-end pipeline and dashboard that tells a coherent story from three noisy sources of truth. The pipeline ingests logs, metrics, and traces; cleans and aligns timelines; and normalizes data into a common schema. I'll apply anomaly detection using multiple models and techniques—feature engineering, SMOTE/weighting, and alternative labeling strategies—to understand how different approaches capture system behavior. Correlations across systems highlight meaningful sequences of events, presented in a clear, actionable dashboard.

6) How the Workflow Works, End to End
• Data layer: Logs, metrics, and traces are parsed, standardized, and aligned to a common reference window.
• Analytics layer: Multiple anomaly detection models are applied, revealing complementary insights about unusual system behavior. Robust preprocessing and feature engineering mitigate overfitting and highlight the nature of the data itself. Cross-system correlations are surfaced to connect related events.
• Presentation layer: A web dashboard summarizes overall health and recent anomalies. Stakeholders can drill down to see timelines, explanations, and raw records, answering “what changed,” “where did it start,” and “what should we check next.”

7) Training Different Models and Managing Accuracy
Training different models with varying accuracy affects the workflow mainly in how we present reliability, interpretability, and business value:
• Some models perform better on certain signals or anomaly types; the dashboard prioritizes the most reliable detections (strict labels), while less accurate models provide exploratory insight or inform future improvements.
• Tree-based or attention models can show why anomalies were flagged, even if a more complex LSTM/CNN slightly outperforms them numerically. This builds stakeholder confidence in the insights.
• Lower-performing models justify starting simple and scaling sophistication only where it materially improves actionable insight.
• Multiple model outputs can be integrated, with emphasis on consensus anomalies or confidence scores to clarify which alerts are most reliable.

8) Exploring Multiple Models and Robust Approaches
During the workflow, I experimented with CNNs, LSTMs, transformers, and tree-based methods to understand how different architectures capture patterns across logs, metrics, and traces. Rather than relying on a single model, I applied feature engineering, class balancing (SMOTE, weighting), and alternative labeling strategies to mitigate overfitting and highlight how the nature of the data influences performance. This revealed that no one model captures all signals perfectly; a multi-pronged approach is essential for learning from complex, multi-modal data Moving forward, a recommended strategy focuses on cross-modal attention models combined with strict-label training, explainable embeddings, and selective feature augmentation to maximize reliability while maintaining interpretability.

9) Data, Security, and Governance
I will use reputable public datasets that mimic real-world telemetry, with clear documentation on origin & processing.

10) What I Will Deliver
A working web dashboard and code repository containing scripts for ingestion, cleaning, anomaly detection, correlation, and visualization. A README explains the problem, approach, governance, and how to review or run the demo.

11) How I Will Measure Success
Success is measured by speed and clarity in explaining known incidents. Non-technical stakeholders should summarize an incident in a sentence or two, and engineers should be able to verify or refine conclusions using underlying evidence.

12) A Right-Sized Scope for a Workflow
Batch data simulates realistic cadence updates for the dashboard. This focuses on storytelling and insight without deploying 24/7 streaming, while leaving a clear path for future enterprise integration.

13) Risks and How I Mitigate Them
• Data realism: Use publicly available, multi-modal datasets.
• Over-engineering: Start with interpretable models; add sophistication only when it improves insight.

14) The Datasets I Will Use
Begin with a cloud-system OpenStack dataset including logs, metrics, and traces with injected faults. Optionally extend to enterprise-like datasets with heterogeneous sources to demonstrate robustness in less standardized environments. You can find more information here: https://zenodo.org/records/3549604

15) A Walkthrough of the User Experience
Stakeholders see system health summaries, timelines of anomalies, plain-language interpretations, and links to raw records. Engineers can drill down to verify findings, while managers can export concise summaries.

16) Why This Matters to the Business
The workflow closes the gap between technical detail and business understanding. Leaders and engineers share a single view, enabling faster incident response, reduced downtime, and better-informed capacity planning.")
                )
              )
      ),
      
      # ---- Data ----
      tabItem(tabName = "data",
              fluidRow(story_box("data")),
              fluidRow(uiOutput("data_cards"))
      ),
      
      # ---- ORIGINAL EDA subtabs ----
      tabItem(tabName = "eda_time",  fluidRow(story_box("eda_time")),  fluidRow(uiOutput("eda_time_cards"))),
      tabItem(tabName = "eda_corr",  fluidRow(story_box("eda_corr")),  fluidRow(uiOutput("eda_corr_cards"))),
      tabItem(tabName = "eda_hist",  fluidRow(story_box("eda_hist")),  fluidRow(uiOutput("eda_hist_cards"))),
      tabItem(tabName = "eda_trace", fluidRow(story_box("eda_trace")), fluidRow(uiOutput("eda_trace_cards"))),
      tabItem(tabName = "eda_ratio", fluidRow(story_box("eda_ratio")), fluidRow(uiOutput("eda_ratio_cards"))),
      tabItem(tabName = "eda_cpuerr",fluidRow(story_box("eda_cpuerr")),fluidRow(uiOutput("eda_cpuerr_cards"))),
      tabItem(tabName = "eda_ccf",   fluidRow(story_box("eda_ccf")),   fluidRow(uiOutput("eda_ccf_cards"))),
      
      # ---- Models subtabs ----
      tabItem(tabName = "model_rf",   fluidRow(story_box("model_rf")),   fluidRow(uiOutput("model_rf_cards"))),
      tabItem(tabName = "model_rnn",  fluidRow(story_box("model_rnn")),  fluidRow(uiOutput("model_rnn_cards"))),
      tabItem(tabName = "model_xf",   fluidRow(story_box("model_xf")),   fluidRow(uiOutput("model_xf_cards"))),
      tabItem(tabName = "model_tree", fluidRow(story_box("model_tree")), fluidRow(uiOutput("model_tree_cards"))),
      tabItem(tabName = "model_other",fluidRow(story_box("model_other")),fluidRow(uiOutput("model_other_cards"))),
      
      # ---- EDA.Rmd subtabs (by notebook) ----
      tabItem(tabName = "eda_load_filter", fluidRow(story_box("eda_load_filter")), fluidRow(uiOutput("eda_load_cards"))),
      tabItem(tabName = "eda_logs",        fluidRow(story_box("eda_logs")),        fluidRow(uiOutput("eda_logs_cards"))),
      tabItem(tabName = "eda_metrics",     fluidRow(story_box("eda_metrics")),     fluidRow(uiOutput("eda_metrics_cards"))),
      tabItem(tabName = "eda_traces",      fluidRow(story_box("eda_traces")),      fluidRow(uiOutput("eda_traces_cards"))),
      tabItem(tabName = "eda_overlays",    fluidRow(story_box("eda_overlays")),    fluidRow(uiOutput("eda_overlays_cards"))),
      tabItem(tabName = "eda_ratio_ccf",   fluidRow(story_box("eda_ratio_ccf")),   fluidRow(uiOutput("eda_ratio_cards_nb"))),
      tabItem(tabName = "eda_align_feats", fluidRow(story_box("eda_align_feats")), fluidRow(uiOutput("eda_align_cards"))),
      tabItem(tabName = "eda_labels",      fluidRow(story_box("eda_labels")),      fluidRow(uiOutput("eda_labels_cards"))),
      tabItem(tabName = "eda_rf_baseline", fluidRow(story_box("eda_rf_baseline")), fluidRow(uiOutput("eda_rf_cards_nb"))),
      
      # ---- Cleaning notebook subtabs ----
      tabItem(tabName = "clean_overview",  fluidRow(story_box("clean_overview")),  fluidRow(uiOutput("clean_overview_cards"))),
      tabItem(tabName = "level_status",    fluidRow(story_box("level_status")),    fluidRow(uiOutput("level_status_cards"))),
      tabItem(tabName = "temporal_levels", fluidRow(story_box("temporal_levels")), fluidRow(uiOutput("temporal_levels_cards"))),
      tabItem(tabName = "entities",        fluidRow(story_box("entities")),        fluidRow(uiOutput("entities_cards"))),
      tabItem(tabName = "cpu_error_hosts", fluidRow(story_box("cpu_error_hosts")), fluidRow(uiOutput("cpu_error_hosts_cards"))),
      tabItem(tabName = "corr_missing",    fluidRow(story_box("corr_missing")),    fluidRow(uiOutput("corr_missing_cards")))
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Force proper tab activation even if AdminLTE JS didn't bind on load
  observe({
    sel <- input$tabs
    if (is.null(sel) || !nzchar(sel)) sel <- "brief"
    session$sendCustomMessage("activate-tab", list(id = sel))
  })
  
  # Helper to build one visual card (iframe + notes)
  make_card <- function(file, height) {
    n <- get_note(file)
    box(width = 12, title = pretty_name(file), solidHeader = TRUE,
        iframe_for(file, height = height),
        if (!is.null(n)) div(class = "note-block",
                             hr(), h4(n$analysis),
                             p(tags$b("What this shows: "), n$visual_shows),
                             if (!is.null(n$insight) && nzchar(n$insight)) p(tags$b("Insight: "), n$insight),
                             if (!is.null(n$notebook_source) && nzchar(n$notebook_source))
                               p(tags$small(em(paste("Source:", n$notebook_source))))
        )
    )
  }
  
  # DRY: render a list of cards or show a fallback message if none
  render_card_list <- function(files, height) {
    if (!length(files)) return(no_visuals_box())
    tagList(lapply(files, make_card, height = height %||% "700px"))
  }
  
  # ---- Data ----
  output$data_cards <- renderUI({
    render_card_list(input$data_files, input$data_h)
  })
  
  # ---- ORIGINAL EDA cards ----
  output$eda_time_cards  <- renderUI({ render_card_list(input$eda_time_files,  input$eda_time_h)  })
  output$eda_corr_cards  <- renderUI({ render_card_list(input$eda_corr_files,  input$eda_corr_h)  })
  output$eda_hist_cards  <- renderUI({ render_card_list(input$eda_hist_files,  input$eda_hist_h)  })
  output$eda_trace_cards <- renderUI({ render_card_list(input$eda_trace_files, input$eda_trace_h) })
  output$eda_ratio_cards <- renderUI({ render_card_list(input$eda_ratio_files, input$eda_ratio_h) })
  output$eda_cpuerr_cards<- renderUI({ render_card_list(input$eda_cpuerr_files,input$eda_cpuerr_h)})
  output$eda_ccf_cards   <- renderUI({ render_card_list(input$eda_ccf_files,   input$eda_ccf_h)   })
  
  # ---- Model cards ----
  output$model_rf_cards   <- renderUI({ render_card_list(input$model_rf_files,   input$model_rf_h)   })
  output$model_rnn_cards  <- renderUI({ render_card_list(input$model_rnn_files,  input$model_rnn_h)  })
  output$model_xf_cards   <- renderUI({ render_card_list(input$model_xf_files,   input$model_xf_h)   })
  output$model_tree_cards <- renderUI({ render_card_list(input$model_tree_files, input$model_tree_h) })
  output$model_other_cards<- renderUI({ render_card_list(input$model_other_files,input$model_other_h)})
  
  # ---- EDA.Rmd (by notebook) ----
  output$eda_load_cards     <- renderUI({ render_card_list(input$eda_load_files,     input$eda_load_h)     })
  output$eda_logs_cards     <- renderUI({ render_card_list(input$eda_logs_files,     input$eda_logs_h)     })
  output$eda_metrics_cards  <- renderUI({ render_card_list(input$eda_metrics_files,  input$eda_metrics_h)  })
  output$eda_traces_cards   <- renderUI({ render_card_list(input$eda_traces_files,   input$eda_traces_h)   })
  output$eda_overlays_cards <- renderUI({ render_card_list(input$eda_overlays_files, input$eda_overlays_h) })
  output$eda_ratio_cards_nb <- renderUI({ render_card_list(input$eda_ratio_files,    input$eda_ratio_h)    })
  output$eda_align_cards    <- renderUI({ render_card_list(input$eda_align_files,    input$eda_align_h)    })
  output$eda_labels_cards   <- renderUI({ render_card_list(input$eda_labels_files,   input$eda_labels_h)   })
  output$eda_rf_cards_nb    <- renderUI({ render_card_list(input$eda_rf_files,       input$eda_rf_h)       })
  
  # ---- Cleaning notebook ----
  output$clean_overview_cards  <- renderUI({ render_card_list(input$clean_overview_files,  input$clean_overview_h)  })
  output$level_status_cards    <- renderUI({ render_card_list(input$level_status_files,    input$level_status_h)    })
  output$temporal_levels_cards <- renderUI({ render_card_list(input$temporal_levels_files, input$temporal_levels_h) })
  output$entities_cards        <- renderUI({ render_card_list(input$entities_files,        input$entities_h)        })
  output$cpu_error_hosts_cards <- renderUI({ render_card_list(input$cpu_error_hosts_files, input$cpu_error_hosts_h) })
  output$corr_missing_cards    <- renderUI({ render_card_list(input$corr_missing_files,    input$corr_missing_h)    })
}

shinyApp(ui, server)
