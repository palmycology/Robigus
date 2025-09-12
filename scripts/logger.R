# logger.R
library(DBI)
library(RSQLite)

# --- Helper to get current week's DB path ---
get_log_db_path <- function() {
  # Year-week format, e.g. "2025_w34"
  year_week <- strftime(Sys.Date(), format = "%Y_w%V")
  paste0("logs_", year_week, ".sqlite")
}

# --- Initialize the log DB if needed ---
init_logger <- function() {
  db_path <- get_log_db_path()
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS logs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp TEXT,
      session_id TEXT,
      ip TEXT,
      action TEXT,
      details TEXT
    )
  ")
  
  dbDisconnect(con)
}

# --- Utility to get IP (works behind proxy too) ---
get_client_ip <- function(session) {
  # Direct REMOTE_ADDR
  ip <- session$request$REMOTE_ADDR
  
  # Proxy fallback: HTTP_X_FORWARDED_FOR
  if (!is.null(session$request$HTTP_X_FORWARDED_FOR)) {
    ip <- session$request$HTTP_X_FORWARDED_FOR
  }
  
  if (is.null(ip)) ip <- "UNKNOWN"
  ip
}

# --- Function to log interaction ---
log_interaction <- function(session, action, details = NULL) {
  db_path <- get_log_db_path()
  
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), db_path)
    
    # Ensure the logs table exists
    dbExecute(con, "
    CREATE TABLE IF NOT EXISTS logs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp TEXT,
      session_id TEXT,
      ip TEXT,
      action TEXT,
      details TEXT
      )
    ")

    dbExecute(
      con,
      "INSERT INTO logs (timestamp, session_id, ip, action, details) VALUES (?,?,?,?,?)",
      params = list(
        as.character(Sys.time()),
        session$token,              # <-- new, unique session ID
        get_client_ip(session),
        action,
        ifelse(is.null(details), "", as.character(details))
      )
    )
    dbDisconnect(con)
  }, error = function(e) {
    message("Logging failed: ", e$message)
  })
}