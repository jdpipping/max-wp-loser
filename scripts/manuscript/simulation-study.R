suppressPackageStartupMessages({
  library(tidyverse)
})

set.seed(20260318)
n_cores_rejection <- 12L
n_cores_null_refine <- 12L
n_null_latent <- 50000L
K_base <- 1000L

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  script_path <- normalizePath("scripts/manuscript/simulation-study.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
out_dir <- file.path(repo_root, "results", "figures", "manuscript", "figures", "simulation")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
source(file.path(repo_root, "R", "plot-style.R"))

logit_safe <- function(p) qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
inv_logit <- plogis

tail_peak_loss <- function(m, p0) {
  val <- (p0 / (1 - p0)) * ((1 - m) / m)
  pmin(pmax(val, 0), 1)
}

simulate_bridge_paths <- function(n,
                                  K = K_base,
                                  sigma = 0.5,
                                  p0_low = 0.25,
                                  p0_high = 0.75) {
  p0 <- runif(n, p0_low, p0_high)
  y <- rbinom(n, size = 1, prob = p0)
  t_grid <- seq(0, 1, length.out = K + 1)

  b <- matrix(0, nrow = n, ncol = K + 1)
  if (K > 1) {
    for (k in seq_len(K - 1)) {
      t_prev <- t_grid[k]
      t_next <- t_grid[k + 1]
      mu_coef <- (1 - t_next) / (1 - t_prev)
      sd_step <- sqrt((t_next - t_prev) * (1 - t_next) / (1 - t_prev))
      b[, k + 1] <- mu_coef * b[, k] + rnorm(n, mean = 0, sd = sd_step)
    }
  }
  b[, K + 1] <- 0

  x <- tcrossprod(y, t_grid) + sigma * b

  p <- matrix(0, nrow = n, ncol = K + 1)
  l0 <- logit_safe(p0)
  if (K >= 1) {
    t_inner <- t_grid[1:K]
    denom <- sigma^2 * (1 - t_inner)
    centered <- x[, 1:K, drop = FALSE] - tcrossprod(rep(1, n), t_inner / 2)
    l_inner <- tcrossprod(l0, rep(1, K)) + sweep(centered, 2, denom, "/")
    p[, 1:K] <- inv_logit(l_inner)
  }
  p[, K + 1] <- y
  p[, 1] <- p0

  list(p0 = p0, y = y, p = p, t = t_grid, sigma = sigma)
}

simulate_bridge_max <- function(n,
                                K = 5000,
                                sigma = 0.5,
                                p0_low = 0.25,
                                p0_high = 0.75) {
  p0 <- runif(n, p0_low, p0_high)
  y <- rbinom(n, size = 1, prob = p0)
  l0 <- logit_safe(p0)

  b_prev <- rep(0, n)
  t_prev <- 0
  max_p <- p0

  if (K > 1) {
    for (k in seq_len(K - 1)) {
      t_next <- k / K
      mu_coef <- (1 - t_next) / (1 - t_prev)
      sd_step <- sqrt((t_next - t_prev) * (1 - t_next) / (1 - t_prev))
      b_next <- mu_coef * b_prev + rnorm(n, mean = 0, sd = sd_step)

      x_next <- t_next * y + sigma * b_next
      l_next <- l0 + (x_next - t_next / 2) / (sigma^2 * (1 - t_next))
      p_next <- inv_logit(l_next)
      max_p <- pmax(max_p, p_next)

      b_prev <- b_next
      t_prev <- t_next
    }
  }

  max_p <- pmax(max_p, y)

  list(p0 = p0, y = y, max_p = max_p, K = K, sigma = sigma)
}

simulate_uvals_from_max_parallel <- function(n,
                                             K,
                                             sigma = 0.5,
                                             p0_low = 0.25,
                                             p0_high = 0.75,
                                             n_cores = 1L) {
  n_workers <- max(1L, min(as.integer(n_cores), as.integer(n)))
  if (.Platform$OS.type != "unix") {
    n_workers <- 1L
  }

  chunk_sizes <- rep.int(n %/% n_workers, n_workers)
  if (n %% n_workers > 0L) {
    chunk_sizes[seq_len(n %% n_workers)] <- chunk_sizes[seq_len(n %% n_workers)] + 1L
  }

  one_chunk <- function(n_chunk) {
    sim <- simulate_bridge_max(
      n = n_chunk,
      K = K,
      sigma = sigma,
      p0_low = p0_low,
      p0_high = p0_high
    )
    idx_loss <- which(sim$y == 0)
    if (length(idx_loss) == 0L) {
      return(numeric(0))
    }
    1 - tail_peak_loss(sim$max_p[idx_loss], sim$p0[idx_loss])
  }

  if (n_workers > 1L) {
    u_chunks <- parallel::mclapply(
      X = chunk_sizes,
      FUN = one_chunk,
      mc.cores = n_workers,
      mc.set.seed = TRUE
    )
  } else {
    u_chunks <- lapply(chunk_sizes, one_chunk)
  }

  uvals <- unlist(u_chunks, use.names = FALSE)
  list(uvals = uvals, n_loss = length(uvals), n_workers = n_workers)
}

warp_paths <- function(p, eta) {
  if (eta <= 0) {
    return(p)
  }
  K <- ncol(p) - 1
  t_grid <- seq(0, 1, length.out = K + 1)
  r_grid <- t_grid / (1 + eta * (1 - t_grid))
  r_grid[length(r_grid)] <- 1

  warped <- t(apply(p, 1, function(row) {
    approx(
      x = t_grid,
      y = row,
      xout = r_grid,
      method = "linear",
      ties = "ordered",
      rule = 2
    )$y
  }))
  storage.mode(warped) <- "double"
  warped
}

apply_distortion <- function(p,
                             y = NULL,
                             type = "none",
                             strength = NA_real_,
                             c_scale = 2.0) {
  if (type == "none") {
    out <- p
  } else if (type == "beta") {
    l <- logit_safe(p)
    out <- inv_logit(strength * l)
  } else if (type == "drift") {
    l <- logit_safe(p)
    K <- ncol(p) - 1
    t_grid <- seq(0, 1, length.out = K + 1)
    t_mat <- matrix(rep(t_grid, each = nrow(p)), nrow = nrow(p), ncol = ncol(p))
    l_hat <- l + strength * t_mat * tanh(l / c_scale)
    out <- inv_logit(l_hat)
  } else if (type == "lag") {
    out <- warp_paths(p, eta = strength)
  } else if (type == "smooth") {
    out <- p
    if (ncol(p) > 2) {
      for (j in 2:(ncol(p) - 1)) {
        out[, j] <- strength * out[, j - 1] + (1 - strength) * p[, j]
      }
    }
  } else {
    stop("Unknown distortion type: ", type)
  }

  if (!is.null(y)) {
    out[, ncol(out)] <- y
  }
  out[, 1] <- p[, 1]
  pmin(pmax(out, 0), 1)
}

observe_feed <- function(p, m = 1, round_to = NA_real_) {
  idx <- seq(1, ncol(p), by = m)
  if (tail(idx, 1) != ncol(p)) {
    idx <- c(idx, ncol(p))
  }
  out <- p[, idx, drop = FALSE]

  if (!is.na(round_to) && round_to > 0) {
    out <- round(out / round_to) * round_to
    out <- pmin(pmax(out, 0), 1)
    out[, 1] <- p[, 1]
    out[, ncol(out)] <- p[, ncol(p)]
  }

  out
}

u_values_from_paths <- function(p_obs, p0, y) {
  idx <- which(y == 0)
  if (length(idx) == 0) {
    return(numeric(0))
  }

  m <- apply(p_obs[idx, , drop = FALSE], 1, max)
  u <- 1 - tail_peak_loss(m, p0[idx])
  pmin(pmax(u, 0), 1)
}

ks_summary <- function(uvals) {
  n <- length(uvals)
  if (n < 5) {
    return(tibble(
      n = n,
      d_upper = NA_real_,
      d_lower = NA_real_,
      p_upper = NA_real_,
      p_lower = NA_real_,
      upper_90 = NA_real_,
      upper_95 = NA_real_,
      upper_99 = NA_real_
    ))
  }

  s <- sort(uvals)
  d_lower <- max((1:n) / n - s)
  d_upper <- max(s - (0:(n - 1)) / n)

  p_upper <- suppressWarnings(stats::ks.test(
    uvals, "punif", 0, 1, alternative = "less", exact = FALSE
  )$p.value)
  p_lower <- suppressWarnings(stats::ks.test(
    uvals, "punif", 0, 1, alternative = "greater", exact = FALSE
  )$p.value)

  tibble(
    n = n,
    d_upper = d_upper,
    d_lower = d_lower,
    p_upper = p_upper,
    p_lower = p_lower,
    upper_90 = mean(uvals >= 0.90),
    upper_95 = mean(uvals >= 0.95),
    upper_99 = mean(uvals >= 0.99)
  )
}

message("Running null calibration under K refinement block")
K_levels <- c(100L, 1000L, 10000L, 100000L, 1000000L)
n_k_refine <- 10000L
null_t_grid <- seq(0, 1, by = 0.01)

label_k <- function(k) paste0("K = ", format(k, big.mark = ",", scientific = FALSE, trim = TRUE))

null_k_profile <- map_dfr(K_levels, function(k_level) {
  message("  simulating null signature for K=", k_level, ", n=", n_k_refine, ", cores=", n_cores_null_refine)
  sim_k <- simulate_uvals_from_max_parallel(
    n = n_k_refine,
    K = k_level,
    sigma = 0.5,
    n_cores = n_cores_null_refine
  )
  uvals_k <- sim_k$uvals

  tibble(
    K = k_level,
    n_loss = sim_k$n_loss,
    curve = label_k(k_level),
    t = null_t_grid,
    upper_gap = map_dbl(null_t_grid, ~ .x - mean(uvals_k <= .x))
  )
})

curve_levels <- label_k(K_levels)
null_k_profile <- null_k_profile |>
  mutate(curve = factor(curve, levels = curve_levels))

null_k_summary <- null_k_profile |>
  group_by(K, curve) |>
  summarise(
    n_loss = first(n_loss),
    min_upper_gap = min(upper_gap),
    max_upper_gap = max(upper_gap),
    mean_abs_upper_gap = mean(abs(upper_gap)),
    gap_t0 = upper_gap[t == 0][1],
    gap_t1 = upper_gap[t == 1][1],
    .groups = "drop"
  )

write_csv(null_k_profile, file.path(out_dir, "null_k_convergence_signature.csv"))
write_csv(null_k_summary, file.path(out_dir, "null_k_convergence_summary.csv"))

k_colors <- setNames(
  paper_palette_seq(length(K_levels), begin = 0.25, end = 0.90),
  curve_levels
)

null_k_plot <- ggplot(null_k_profile, aes(x = t, y = upper_gap, color = curve)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = paper_style$ref, linewidth = 0.9) +
  geom_line(linewidth = 1.0) +
  scale_color_manual(values = k_colors, breaks = curve_levels) +
  labs(
    x = "t",
    y = expression(t - hat(F)[U](t)),
    color = NULL,
    title = paste0("PIT signature: null K refinement (n = ", format(n_k_refine, big.mark = ","), ")")
  ) +
  paper_theme(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )

ggsave(
  file.path(out_dir, "null_k_convergence.png"),
  null_k_plot,
  width = 8.2,
  height = 4.8,
  dpi = 300
)

message("Running discrete-time conservatism under feed coarsening block")
n_null_coarsening <- n_null_latent
coarsen_steps <- c(1L, 2L, 5L, 10L)

label_coarsen <- function(m_step) {
  if (m_step == 1L) {
    return(paste0("every update (K = ", format(K_base, big.mark = ","), ")"))
  }
  paste0("every ", m_step, " updates (K = ", format(K_base, big.mark = ","), ")")
}

profile_from_u <- function(uvals, curve_label, m_step = NA_integer_) {
  upper_gap <- map_dbl(null_t_grid, ~ .x - mean(uvals <= .x))
  tibble(
    m = m_step,
    curve = curve_label,
    t = null_t_grid,
    upper_gap = upper_gap
  )
}

null_latent_base <- simulate_bridge_paths(n = n_null_coarsening, K = K_base, sigma = 0.5)

coarse_profile <- map_dfr(coarsen_steps, function(m_step) {
  p_obs <- observe_feed(null_latent_base$p, m = m_step, round_to = NA_real_)
  uvals <- u_values_from_paths(p_obs, null_latent_base$p0, null_latent_base$y)
  profile_from_u(uvals, curve_label = label_coarsen(m_step), m_step = m_step)
})

null_pit_profile <- coarse_profile
coarsening_levels <- map_chr(coarsen_steps, label_coarsen)
null_pit_profile <- null_pit_profile |>
  mutate(curve = factor(curve, levels = coarsening_levels))

null_tail_summary <- map_dfr(coarsen_steps, function(m_step) {
  p_obs <- observe_feed(null_latent_base$p, m = m_step, round_to = NA_real_)
  uvals <- u_values_from_paths(p_obs, null_latent_base$p0, null_latent_base$y)
  tibble(
    m = m_step,
    n_loss = length(uvals),
    pr_u_ge_090 = mean(uvals >= 0.90),
    pr_u_ge_095 = mean(uvals >= 0.95),
    pr_u_ge_099 = mean(uvals >= 0.99)
  )
})

write_csv(null_pit_profile, file.path(out_dir, "null_pit_profile.csv"))
write_csv(null_tail_summary, file.path(out_dir, "null_tail_summary.csv"))

coarsening_colors <- setNames(rep(paper_style$ref, length(coarsening_levels)), coarsening_levels)
coarsening_colors[coarsening_levels[1]] <- paper_style$ink
coarsening_colors[coarsening_levels[-1]] <- paper_palette_seq(
  length(coarsening_levels) - 1,
  begin = 0.30,
  end = 0.90
)

null_conserv_plot <- ggplot(null_pit_profile, aes(x = t, y = upper_gap, color = curve)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = paper_style$ref, linewidth = 0.85) +
  geom_line(linewidth = 0.95) +
  scale_color_manual(values = coarsening_colors, breaks = coarsening_levels) +
  labs(
    x = "t",
    y = expression(t - hat(F)[U](t)),
    color = NULL,
    title = paste0("PIT signature: feed coarsening (n = ", format(n_null_coarsening, big.mark = ","), ")")
  ) +
  paper_theme(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "null_conservatism.png"),
  null_conserv_plot,
  width = 8.2,
  height = 4.8,
  dpi = 300
)

message("Running directional signatures block")
signature_scenarios <- tribble(
  ~scenario, ~type, ~strength, ~m, ~round_to,
  "Null", "none", NA_real_, 1, NA_real_,
  "Underreaction (beta = 0.9)", "beta", 0.9, 1, NA_real_,
  "Overreaction (beta = 1.1)", "beta", 1.1, 1, NA_real_,
  "Positive drift (delta = 1.0)", "drift", 1.0, 1, NA_real_,
  "Latency (eta = 1.0)", "lag", 1.0, 1, NA_real_,
  "Smoothing (lambda = 0.8)", "smooth", 0.8, 1, NA_real_,
  "Coarsened (m = 10)", "none", NA_real_, 10, NA_real_
)
signature_levels <- signature_scenarios$scenario

sig_latent <- simulate_bridge_paths(n = 5000, K = K_base, sigma = 0.5)
signature_t_grid <- seq(0, 1, by = 0.01)

signature_df <- pmap_dfr(signature_scenarios, function(scenario, type, strength, m, round_to) {
  p_mod <- apply_distortion(sig_latent$p, y = sig_latent$y, type = type, strength = strength)
  p_obs <- observe_feed(p_mod, m = m, round_to = round_to)
  uvals <- u_values_from_paths(p_obs, sig_latent$p0, sig_latent$y)

  tibble(
    scenario = factor(scenario, levels = signature_levels),
    t = signature_t_grid,
    upper_gap = map_dbl(signature_t_grid, ~ .x - mean(uvals <= .x))
  )
})

write_csv(signature_df, file.path(out_dir, "alternative_signatures.csv"))

signature_colors <- setNames(rep(paper_style$ref, length(signature_levels)), signature_levels)
alt_levels <- setdiff(signature_levels, c("Null", "Coarsened (m = 10)"))
signature_colors["Null"] <- paper_style$ink
signature_colors[alt_levels] <- paper_palette_seq(length(alt_levels), begin = 0.30, end = 0.90)

signature_plot <- ggplot(signature_df, aes(x = t, y = upper_gap, color = scenario)) +
  geom_hline(yintercept = 0, color = paper_style$ref, linewidth = 0.8) +
  geom_line(linewidth = 1.0) +
  scale_color_manual(values = signature_colors, breaks = signature_levels) +
  labs(
    x = "t",
    y = expression(t - hat(F)[U](t)),
    color = NULL,
    title = paste0("PIT signature: structural alternatives (n = ", format(nrow(sig_latent$p), big.mark = ","), ")")
  ) +
  paper_theme(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "alternative_signatures.png"),
  signature_plot,
  width = 8.0,
  height = 4.6,
  dpi = 300
)

message("Running rejection-rate block")
scenario_grid <- tribble(
  ~family, ~scenario, ~type, ~strength, ~m, ~round_to,
  "Null", "Null", "none", NA_real_, 1, NA_real_,
  "Underreaction", "beta = 0.8", "beta", 0.8, 1, NA_real_,
  "Underreaction", "beta = 0.9", "beta", 0.9, 1, NA_real_,
  "Overreaction", "beta = 1.1", "beta", 1.1, 1, NA_real_,
  "Overreaction", "beta = 1.2", "beta", 1.2, 1, NA_real_,
  "Predictable drift", "delta = 0.25", "drift", 0.25, 1, NA_real_,
  "Predictable drift", "delta = 0.5", "drift", 0.5, 1, NA_real_,
  "Predictable drift", "delta = 1.0", "drift", 1.0, 1, NA_real_,
  "Latency", "eta = 0.5", "lag", 0.5, 1, NA_real_,
  "Latency", "eta = 1.0", "lag", 1.0, 1, NA_real_,
  "Smoothing", "lambda = 0.3", "smooth", 0.3, 1, NA_real_,
  "Smoothing", "lambda = 0.5", "smooth", 0.5, 1, NA_real_,
  "Smoothing", "lambda = 0.8", "smooth", 0.8, 1, NA_real_,
  "Reporting artifact", "round 0.01", "none", NA_real_, 1, 0.01,
  "Reporting artifact", "round 0.05", "none", NA_real_, 1, 0.05,
  "Reporting artifact", "coarsen m = 5", "none", NA_real_, 5, NA_real_,
  "Reporting artifact", "coarsen m = 10", "none", NA_real_, 10, NA_real_,
  "Reporting artifact", "coarsen m = 10 + round 0.05", "none", NA_real_, 10, 0.05
)

run_rejection_study <- function(n_paths,
                                n_rep = 120,
                                K = K_base,
                                sigma = 0.5,
                                n_cores = 1L) {
  one_rep <- function(b) {
    latent <- simulate_bridge_paths(n = n_paths, K = K, sigma = sigma)
    rep_rows <- vector("list", nrow(scenario_grid))

    for (j in seq_len(nrow(scenario_grid))) {
      sc <- scenario_grid[j, ]
      p_mod <- apply_distortion(
        latent$p,
        y = latent$y,
        type = sc$type,
        strength = sc$strength
      )
      p_obs <- observe_feed(p_mod, m = sc$m, round_to = sc$round_to)
      uvals <- u_values_from_paths(p_obs, latent$p0, latent$y)
      ks <- ks_summary(uvals)

      rep_rows[[j]] <- tibble(
        rep = b,
        n_paths = n_paths,
        family = sc$family,
        scenario = sc$scenario,
        n_loss = ks$n,
        upper_95 = ks$upper_95,
        d_upper = ks$d_upper,
        d_lower = ks$d_lower
      )
    }

    bind_rows(rep_rows)
  }

  if (n_cores > 1L && .Platform$OS.type == "unix") {
    message("  n_paths=", n_paths, ": running ", n_rep, " reps on ", n_cores, " cores")
    res <- parallel::mclapply(
      X = seq_len(n_rep),
      FUN = one_rep,
      mc.cores = n_cores,
      mc.set.seed = TRUE
    )
  } else {
    message("  n_paths=", n_paths, ": running ", n_rep, " reps in serial mode")
    res <- lapply(seq_len(n_rep), one_rep)
  }

  bind_rows(res)
}

rej_raw <- bind_rows(
  run_rejection_study(n_paths = 1000, n_rep = 120, n_cores = n_cores_rejection),
  run_rejection_study(n_paths = 5000, n_rep = 100, n_cores = n_cores_rejection)
)

write_csv(rej_raw, file.path(out_dir, "rejection_rates_raw.csv"))

crit_vals <- rej_raw |>
  filter(scenario == "Null") |>
  group_by(n_paths) |>
  summarise(
    crit_dupper = quantile(d_upper, 0.95, na.rm = TRUE),
    crit_dlower = quantile(d_lower, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(crit_vals, file.path(out_dir, "rejection_rate_critical_values.csv"))

rej_summary <- rej_raw |>
  left_join(crit_vals, by = "n_paths") |>
  mutate(
    reject_upper = as.integer(d_upper > crit_dupper),
    reject_lower = as.integer(d_lower > crit_dlower)
  ) |>
  group_by(family, scenario, n_paths) |>
  summarise(
    mean_n_loss = mean(n_loss),
    mean_upper_95 = mean(upper_95),
    rej_dupper = mean(reject_upper),
    rej_dlower = mean(reject_lower),
    .groups = "drop"
  )

write_csv(rej_summary, file.path(out_dir, "rejection_rates_summary.csv"))

rej_table <- rej_summary |>
  select(scenario, n_paths, rej_dupper, rej_dlower) |>
  pivot_wider(
    names_from = n_paths,
    values_from = c(rej_dupper, rej_dlower),
    names_glue = "{.value}_n{n_paths}"
  ) |>
  mutate(across(starts_with("rej_"), ~ round(.x, 3))) |>
  mutate(scenario = factor(scenario, levels = scenario_grid$scenario)) |>
  arrange(scenario)

write_csv(rej_table, file.path(out_dir, "rejection_rates_table.csv"))

rej_table_full <- rej_summary |>
  select(family, scenario, n_paths, rej_dupper, rej_dlower) |>
  pivot_wider(
    names_from = n_paths,
    values_from = c(rej_dupper, rej_dlower),
    names_glue = "{.value}_n{n_paths}"
  ) |>
  mutate(across(starts_with("rej_"), ~ round(.x, 3))) |>
  mutate(scenario = factor(scenario, levels = scenario_grid$scenario)) |>
  arrange(scenario)

write_csv(rej_table_full, file.path(out_dir, "rejection_rates_table_full.csv"))

main_scenarios <- c(
  "Null",
  "beta = 0.9",
  "beta = 1.1",
  "delta = 0.5",
  "delta = 1.0",
  "eta = 1.0",
  "lambda = 0.5",
  "lambda = 0.8"
)

rej_table_main <- rej_table |>
  filter(as.character(scenario) %in% main_scenarios)

write_csv(rej_table_main, file.path(out_dir, "rejection_rates_table_main.csv"))

message("Saved simulation outputs to: ", out_dir)
