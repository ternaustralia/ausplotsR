
# OpenTelemetry integration test for ausplotsR
# Usage (from project root):
#   Rscript helper-scripts/test_sentry.R
#
# What this does:
#   1. Installs otelsdk if missing
#   2. Starts a stdout exporter so all otel::log_* calls print to the console
#   3. Loads ausplotsR from source
#   4. Triggers real errors to confirm log_warn / log_error fire correctly
#
# You should see lines like:
#   [WARN]  ausplotsR: set_auth called with role=...
#   [ERROR] An exception occurred in set_auth: role must be ...

# ── Step 1: Install packages ──────────────────────────────────────────────────
for (pkg in c("otel", "otelsdk")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

# ── Step 2: Choose backend ────────────────────────────────────────────────────
#
# Option A — Console only (no Sentry, no system deps needed)
#   Uncomment the block below and comment out Option B.
#
# local({
#   make_logger <- function(level) {
#     force(level)
#     function(msg, ...) message(sprintf("[%s] %s", level, msg))
#   }
#   assignInNamespace("log_info",  make_logger("INFO"),  ns = "otel")
#   assignInNamespace("log_warn",  make_logger("WARN"),  ns = "otel")
#   assignInNamespace("log_error", make_logger("ERROR"), ns = "otel")
# })
# message("otel console logger active.\n")

# Option B — Forward logs to Sentry via OTLP
#   Requires: sudo apt-get install -y libprotobuf-dev protobuf-compiler
#   Then:     install.packages("otelsdk")
#
#   Your Sentry DSN looks like:
#     https://<PUBLIC_KEY>@o<ORG_ID>.ingest.sentry.io/<PROJECT_ID>
#   From that, derive:
#     endpoint  <- "https://o<ORG_ID>.ingest.sentry.io/api/<PROJECT_ID>/otlp/"
#     sentry_key <- "<PUBLIC_KEY>"
#
SENTRY_DSN <- Sys.getenv("AUSPLOTS_SENTRY_DSN", unset = "")
if (nchar(SENTRY_DSN) == 0L) stop("Set AUSPLOTS_SENTRY_DSN env var to your Sentry DSN before running.")

# Parse DSN  →  https://<key>@<host>/<project_id>
dsn_match  <- regmatches(SENTRY_DSN, regexpr("https://([^@]+)@([^/]+)/([0-9]+)", SENTRY_DSN, perl = TRUE))
parts      <- regmatches(dsn_match, regexec("https://([^@]+)@([^/]+)/([0-9]+)", dsn_match, perl = TRUE))[[1]]
sentry_key <- parts[2]
sentry_host <- parts[3]
project_id  <- parts[4]
otlp_endpoint <- sprintf("https://%s/api/%s/otlp/", sentry_host, project_id)

otelsdk::start_otlp_exporter(
  endpoint = otlp_endpoint,
  headers  = list(Authorization = paste("Bearer", sentry_key))
)
message(sprintf("otel OTLP exporter active → %s\n", otlp_endpoint))

# ── Step 2: Load ausplotsR from source ───────────────────────────────────────
devtools::load_all(file.path(dirname(normalizePath(sys.frame(1)$ofile)), ".."))

# ── Step 3: Trigger log_info (info-level messages on successful call) ─────────
message("--- set_auth success path ---")
tryCatch(
  suppressMessages(set_auth(role = "test_role", secret = "test_secret")),
  error = function(e) message("[CAUGHT] ", conditionMessage(e))
)

# ── Step 4: Trigger log_error via .sentry_capture_exception ──────────────────
message("\n--- set_auth error path (NULL role) ---")
tryCatch(
  set_auth(role = NULL, secret = "s"),
  error = function(e) message("[CAUGHT] ", conditionMessage(e))
)

message("\n--- species_list error path (NULL input) ---")
tryCatch(
  species_list(NULL, grouping = "by_site"),
  error = function(e) message("[CAUGHT] ", conditionMessage(e))
)

message("\n--- invasion_info error path (wrong type) ---")
tryCatch(
  invasion_info(afa_static = "not a data frame"),
  error = function(e) message("[CAUGHT] ", conditionMessage(e))
)

message("\nDone. Check output above for [INFO] / [WARN] / [ERROR] lines.")

