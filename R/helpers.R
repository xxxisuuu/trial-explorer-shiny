## helper funcs for the app. kept outside app.R so it doesnt get huge

arm_cols <- c("Placebo" = "#8D99AE", "Xanomeline Low Dose" = "#F2A65A", "Xanomeline High Dose" = "#C03A51")
short_arm <- c("Placebo" = "PBO", "Xanomeline Low Dose" = "Xan Low", "Xanomeline High Dose" = "Xan High")

# --- demographics table (like a TLF 14.1.x but simple) ---
demog_table <- function(adsl) {
  arms <- levels(adsl$TRT01A)
  N <- table(adsl$TRT01A)

  cont_row <- function(var, lab) {
    out <- sapply(arms, function(a) {
      x <- adsl[[var]][adsl$TRT01A == a]
      sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
    })
    data.frame(Characteristic = lab, Category = "Mean (SD)", t(out), check.names = FALSE)
  }
  cat_rows <- function(var, lab) {
    tb <- table(adsl[[var]], adsl$TRT01A)
    pct <- sweep(tb, 2, N, "/") * 100
    cells <- matrix(sprintf("%d (%.0f%%)", tb, pct), nrow = nrow(tb))
    df <- data.frame(Characteristic = c(lab, rep("", nrow(tb) - 1)), Category = rownames(tb), cells,
                     check.names = FALSE)
    names(df)[-(1:2)] <- arms
    df
  }
  r <- rbind(
    cont_row("AGE", "Age (yrs)"),
    cat_rows("AGEGR1", "Age group"),
    cat_rows("SEX", "Sex"),
    cat_rows("RACE", "Race"),
    cont_row("BMIBL", "BMI (kg/m2)"),
    cont_row("MMSETOT", "MMSE total")
  )
  names(r)[-(1:2)] <- paste0(arms, " (N=", N, ")")
  r
}

# --- AE incidence by PT for 2 arms + risk diff + fisher p ---
ae_compare <- function(adae, adsl, trt, ref = "Placebo") {
  n_trt <- sum(adsl$TRT01A == trt); n_ref <- sum(adsl$TRT01A == ref)
  # subjects w/ the PT, count once per subject
  inc <- adae %>%
    filter(TRTA %in% c(trt, ref)) %>%
    distinct(USUBJID, TRTA, AEBODSYS, AEDECOD) %>%
    count(AEBODSYS, AEDECOD, TRTA) %>%
    tidyr::pivot_wider(names_from = TRTA, values_from = n, values_fill = 0)
  if (!trt %in% names(inc)) inc[[trt]] <- 0
  if (!ref %in% names(inc)) inc[[ref]] <- 0
  inc$a <- inc[[trt]]; inc$b <- inc[[ref]]
  inc$p_trt <- inc$a / n_trt; inc$p_ref <- inc$b / n_ref
  inc$rd <- inc$p_trt - inc$p_ref
  inc$pval <- mapply(function(a, b) {
    fisher.test(matrix(c(a, n_trt - a, b, n_ref - b), 2))$p.value
  }, inc$a, inc$b)
  inc$n_tot <- inc$a + inc$b
  inc
}

# KM to a long df plotly can draw (steps added w/ line.shape = "hv")
km_long <- function(fit) {
  st <- if (is.null(fit$strata)) rep("All", length(fit$time)) else rep(sub("TRTA=", "", names(fit$strata)), fit$strata)
  df <- data.frame(time = fit$time, surv = fit$surv, lo = fit$lower, hi = fit$upper,
                   ncens = fit$n.censor, nrisk = fit$n.risk, arm = st)
  df0 <- data.frame(time = 0, surv = 1, lo = 1, hi = 1, ncens = 0, nrisk = NA, arm = unique(st))
  df <- rbind(df0, df)
  df[order(df$arm, df$time), ]
}
