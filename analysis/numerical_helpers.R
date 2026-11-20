cache_context <- function(files, mode, settings = NULL) {
  packages <- installed.packages()[, c("Package", "Version"), drop = FALSE]
  list(files = tools::md5sum(files), mode = mode, settings = settings,
       R = R.version.string, packages = packages, contrasts = getOption("contrasts"),
       rng = RNGkind())
}

cached_numerical <- function(key, expr) {
  if (tolower(Sys.getenv("FOMO_CACHE", "true")) %in% c("0", "false", "no")) return(force(expr))
  rng <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
  path <- file.path("outputs/cache", paste0(digest::digest(list(key, rng)), ".rds"))
  if (file.exists(path)) {
    result <- readRDS(path)
    if (!is.null(result$rng)) assign(".Random.seed", result$rng, .GlobalEnv)
    for (w in result$warnings) warning(w, call. = FALSE)
    for (m in result$messages) message(m)
    if (!is.null(result$error)) stop(result$error)
    return(result$value)
  }
  warnings <- messages <- character()
  error <- NULL
  value <- tryCatch(withCallingHandlers(force(expr),
    warning = function(w) warnings <<- c(warnings, conditionMessage(w)),
    message = function(m) messages <<- c(messages, conditionMessage(m))),
    error = function(e) { error <<- e; NULL })
  result <- list(value = value, warnings = warnings, messages = messages, error = error,
                 rng = if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(result, tmp)
  if (!file.rename(tmp, path)) warning("Could not save numerical cache")
  if (!is.null(error)) stop(error)
  value
}

matrix_alpha <- function(m) {
  k <- ncol(m)
  if (nrow(m) < 2L) return(NA_real_)
  centered <- sweep(m, 2L, colMeans(m), "-")
  totals <- rowSums(m)
  (k / (k - 1)) * (1 - sum(centered * centered) / sum((totals - mean(totals))^2))
}

scale_alphas <- function(matrices, rows = seq_len(nrow(matrices[[1]]))) {
  a <- vapply(matrices, function(m) matrix_alpha(m[rows, , drop = FALSE]), numeric(1))
  c(consc = a[["Conscientiousness"]], mean_bfi = mean(a[seq_len(5)]), fomo = a[[6]])
}

power_effects <- function(model, effects, term, nsim, seed, workers = NULL) {
  if (!length(effects)) return(list())
  if (is.null(workers)) {
    cores <- parallel::detectCores()
    if (is.na(cores)) cores <- 2L
    workers <- Sys.getenv("FOMO_POWER_WORKERS", max(1L, cores - 1L))
  }
  workers <- suppressWarnings(as.integer(workers))
  if (length(workers) != 1L || is.na(workers) || workers < 1L) stop("Invalid FOMO_POWER_WORKERS")
  workers <- min(workers, length(effects))
  model <- simr::`getData<-`(model, simr::getData(model))
  job <- function(eff, model, term, nsim, seed) {
    suppressMessages(loadNamespace("simr"))
    warnings <- messages <- character()
    error <- NULL
    value <- tryCatch(withCallingHandlers({
      mm <- unserialize(serialize(model, NULL))
      bb <- lme4::fixef(mm); bb[[term]] <- eff
      mm <- simr::`fixef<-`(mm, bb)
      simr::powerSim(mm, test = simr::fixed(term, "t"), nsim = nsim,
                     progress = FALSE, seed = seed)
    }, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
    }, message = function(m) {
      messages <<- c(messages, conditionMessage(m)); invokeRestart("muffleMessage")
    }), error = function(e) { error <<- e; NULL })
    list(value = value, warnings = warnings, messages = messages, error = error)
  }
  if (workers == 1L) return(lapply(effects, job, model = model, term = term, nsim = nsim, seed = seed))
  thread_vars <- c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
                   "VECLIB_MAXIMUM_THREADS", "BLIS_NUM_THREADS")
  old <- setNames(Sys.getenv(thread_vars, unset = NA_character_), thread_vars)
  on.exit({
    Sys.unsetenv(thread_vars[is.na(old)])
    if (any(!is.na(old))) do.call(Sys.setenv, as.list(old[!is.na(old)]))
  }, add = TRUE)
  do.call(Sys.setenv, as.list(setNames(rep("1", length(thread_vars)), thread_vars)))
  pool <- tryCatch(parallel::makePSOCKcluster(workers), error = function(e) NULL)
  if (is.null(pool)) return(lapply(effects, job, model = model, term = term, nsim = nsim, seed = seed))
  on.exit(parallel::stopCluster(pool), add = TRUE)
  parallel::clusterCall(pool, function(paths) invisible(.libPaths(paths)), .libPaths())
  parallel::parLapply(pool, effects, job, model = model, term = term, nsim = nsim, seed = seed)
}
