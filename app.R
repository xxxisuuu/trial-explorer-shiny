# Trial Explorer -- CDISC pilot study (xanomeline in mild-moderate Alzheimer's)
# shiny + bslib + plotly
# data: ADaM from phuse-scripts / cdiscpilot01, see R/prep_data.R

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(survival)
library(DT)

source("R/helpers.R")

if (!file.exists("data/adam.rds")) source("R/prep_data.R")
adam <- readRDS("data/adam.rds")
ADSL <- adam$adsl; ADAE <- adam$adae; ADTTE <- adam$adtte
ARMS <- levels(ADSL$TRT01A)

thm <- bs_theme(version = 5, bg = "#FBFAF7", fg = "#1F2430", primary = "#C03A51",
                base_font = font_google("Inter", local = FALSE), heading_font = font_google("Inter", local = FALSE),
                "border-radius" = "0.6rem")
# font_google needs internet. if offline just comment the font lines out ^

plotly_clean <- function(p) {
  p %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               font = list(family = "Inter, sans-serif", size = 12),
               legend = list(orientation = "h", y = 1.08)) %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
}

ui <- page_navbar(
  title = "Trial Explorer",
  fillable = FALSE,   # let plots keep their own heights, the fill layout squashed the heatmap
  theme = thm,
  sidebar = sidebar(
    width = 270,
    h6("Population filters"),
    checkboxGroupInput("arms", "Treatment arms", ARMS, selected = ARMS),
    checkboxGroupInput("sex", "Sex", c("F", "M"), selected = c("F", "M"), inline = TRUE),
    checkboxGroupInput("agegr", "Age group", levels(ADSL$AGEGR1), selected = levels(ADSL$AGEGR1), inline = TRUE),
    selectInput("race", "Race", c("All", sort(unique(ADSL$RACE))), "All"),
    hr(),
    helpText("Safety population, treatment-emergent AEs. Source: CDISC SDTM/ADaM pilot (cdiscpilot01).")
  ),

  nav_panel("Overview",
    layout_columns(
      fill = FALSE,
      value_box("Subjects", textOutput("vb_n"), theme = "light"),
      value_box("Completed wk 24", textOutput("vb_comp"), theme = "light"),
      value_box("Any TEAE", textOutput("vb_ae"), theme = "light"),
      value_box("Serious AE events", textOutput("vb_sae"), theme = "light")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Demographics & baseline"), DTOutput("demog"), full_screen = TRUE),
      card(card_header("Age at baseline"), plotlyOutput("age_plot"), full_screen = TRUE)
    ),
    card(card_header("Disposition"), plotlyOutput("dispo", height = "280px"), full_screen = TRUE)
  ),

  nav_panel("Adverse events",
    layout_columns(
      fill = FALSE, col_widths = c(4, 4, 4),
      selectInput("ae_trt", "Compare vs placebo", ARMS[-1], selected = "Xanomeline High Dose"),
      sliderInput("ae_top", "Top N preferred terms", 5, 30, 15, step = 1),
      selectInput("ae_sev", "Severity", c("All", "MILD", "MODERATE", "SEVERE"), "All")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Butterfly: % subjects with AE"), plotlyOutput("butterfly", height = "560px"), full_screen = TRUE),
      card(card_header("Volcano: risk difference vs placebo"), plotlyOutput("volcano", height = "560px"), full_screen = TRUE)
    ),
    card(card_header("System organ class x arm (% subjects)"), plotlyOutput("soc_heat", height = "720px"), full_screen = TRUE)
  ),

  nav_panel("Time to event",
    layout_columns(
      col_widths = c(8, 4),
      card(card_header("Time to first dermatologic event (KM)"),
           checkboxInput("km_ci", "Show 95% CI", TRUE),
           plotlyOutput("km", height = "480px"), full_screen = TRUE),
      card(card_header("Summary"), tableOutput("km_tab"),
           p(class = "text-muted small", "HR from Cox model vs placebo, unadjusted. Median = NA when < 50% had an event."))
    )
  ),

  nav_spacer(),
  nav_item(tags$a("Data source", href = "https://github.com/phuse-org/phuse-scripts", target = "_blank"))
)


server <- function(input, output, session) {

  adsl_f <- reactive({
    d <- ADSL %>% filter(TRT01A %in% input$arms, SEX %in% input$sex, AGEGR1 %in% input$agegr)
    if (input$race != "All") d <- d %>% filter(RACE == input$race)
    validate(need(nrow(d) > 0, "No subjects match the filters"))
    d %>% mutate(TRT01A = droplevels(TRT01A))
  })
  adae_f <- reactive({
    d <- ADAE %>% filter(USUBJID %in% adsl_f()$USUBJID)
    if (input$ae_sev != "All") d <- d %>% filter(AESEV == input$ae_sev)
    d
  })

  ## value boxes
  output$vb_n <- renderText(nrow(adsl_f()))
  output$vb_comp <- renderText(sprintf("%.0f%%", 100 * mean(adsl_f()$COMP24FL == "Y")))
  output$vb_ae <- renderText({
    n <- n_distinct(ADAE$USUBJID[ADAE$USUBJID %in% adsl_f()$USUBJID])
    sprintf("%d (%.0f%%)", n, 100 * n / nrow(adsl_f()))
  })
  output$vb_sae <- renderText(sum(ADAE$AESER[ADAE$USUBJID %in% adsl_f()$USUBJID] == "Y"))

  ## overview
  output$demog <- renderDT({
    datatable(demog_table(adsl_f()), rownames = FALSE, options = list(dom = "t", pageLength = 50, ordering = FALSE),
              class = "compact stripe")
  })

  output$age_plot <- renderPlotly({
    d <- adsl_f()
    p <- plot_ly()
    for (a in levels(d$TRT01A)) {
      x <- d$AGE[d$TRT01A == a]
      p <- p %>% add_trace(type = "violin", y = x, x = short_arm[a], name = a, box = list(visible = TRUE),
                           meanline = list(visible = TRUE), points = "all", jitter = 0.35, pointpos = 0,
                           marker = list(size = 4, opacity = 0.5, color = arm_cols[a]), line = list(color = arm_cols[a]),
                           fillcolor = adjustcolor(arm_cols[a], 0.35), hoverinfo = "y")
    }
    p %>% layout(showlegend = FALSE, yaxis = list(title = "Age (years)"),
                 xaxis = list(title = "", categoryorder = "array", categoryarray = unname(short_arm))) %>% plotly_clean()
  })

  output$dispo <- renderPlotly({
    d <- adsl_f() %>% count(TRT01A, DCDECOD) %>% group_by(TRT01A) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()
    # completed first, then by overall freq
    lv <- d %>% group_by(DCDECOD) %>% summarise(n = sum(n)) %>% arrange(DCDECOD != "Completed", desc(n)) %>% pull(DCDECOD)
    d$DCDECOD <- factor(d$DCDECOD, levels = lv)
    pal <- c("#2A9D8F", colorRampPalette(c("#E9C46A", "#E76F51", "#6D597A", "#B5B5B5"))(length(lv) - 1))
    plot_ly(d, y = ~TRT01A, x = ~pct, color = ~DCDECOD, colors = pal, type = "bar", orientation = "h",
            text = ~paste0(DCDECOD, ": ", n, " (", round(pct), "%)"), hoverinfo = "text", textposition = "none") %>%
      layout(barmode = "stack", xaxis = list(title = "% of subjects", ticksuffix = "%"), yaxis = list(title = "")) %>%
      plotly_clean() %>%
      layout(legend = list(orientation = "v", x = 1.02, y = 1, font = list(size = 11)))  # top legend overlapped the bars
  })

  ## AEs
  ae_cmp <- reactive({
    validate(need(all(c("Placebo", input$ae_trt) %in% adsl_f()$TRT01A), "Need placebo + the compared arm in the filter"))
    ae_compare(adae_f(), adsl_f(), input$ae_trt)
  })

  output$butterfly <- renderPlotly({
    d <- ae_cmp() %>% arrange(desc(p_trt + p_ref)) %>% head(input$ae_top)
    d$AEDECOD <- factor(d$AEDECOD, levels = rev(d$AEDECOD))
    plot_ly(d, y = ~AEDECOD) %>%
      add_bars(x = ~-100 * p_ref, name = "Placebo", marker = list(color = arm_cols["Placebo"]),
               text = ~sprintf("Placebo: %d (%.1f%%)", b, 100 * p_ref), hoverinfo = "text", textposition = "none") %>%
      add_bars(x = ~100 * p_trt, name = input$ae_trt, marker = list(color = arm_cols[input$ae_trt]),
               text = ~sprintf("%s: %d (%.1f%%)", short_arm[input$ae_trt], a, 100 * p_trt), hoverinfo = "text",
               textposition = "none") %>%
      layout(barmode = "relative", bargap = 0.25,
             xaxis = list(title = "% subjects", tickvals = seq(-60, 60, 10), ticktext = abs(seq(-60, 60, 10)),
                          zeroline = TRUE, zerolinecolor = "#555"),
             yaxis = list(title = "")) %>%
      plotly_clean()
  })

  output$volcano <- renderPlotly({
    d <- ae_cmp() %>% filter(n_tot >= 2)
    d$lp <- -log10(d$pval)
    d$sig <- d$pval < 0.05
    plot_ly(d, x = ~100 * rd, y = ~lp, type = "scatter", mode = "markers",
            color = ~AEBODSYS, colors = colorRampPalette(c("#2A9D8F", "#E9C46A", "#F4A261", "#C03A51", "#6D597A", "#457B9D"))(n_distinct(d$AEBODSYS)),
            size = ~n_tot, sizes = c(12, 70),
            marker = list(opacity = 0.8),
            text = ~sprintf("<b>%s</b><br>%s<br>%s: %.1f%% | PBO: %.1f%%<br>RD = %.1f pts, p = %.3g",
                            AEDECOD, AEBODSYS, short_arm[input$ae_trt], 100 * p_trt, 100 * p_ref, 100 * rd, pval),
            hoverinfo = "text") %>%
      add_annotations(data = d %>% filter(sig) %>% arrange(pval) %>% head(8),
                      x = ~100 * rd, y = ~lp, text = ~AEDECOD, showarrow = TRUE, arrowsize = 0.5, ax = 25, ay = -25,
                      font = list(size = 11)) %>%
      layout(xaxis = list(title = "Risk difference vs placebo (% points)", zeroline = TRUE),
             yaxis = list(title = "-log10(Fisher p)"),
             shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = -log10(0.05), y1 = -log10(0.05),
                                line = list(dash = "dot", color = "#999"))),
             showlegend = FALSE) %>%
      plotly_clean()
  })

  output$soc_heat <- renderPlotly({
    N <- table(adsl_f()$TRT01A)
    d <- adae_f() %>% distinct(USUBJID, TRTA, AEBODSYS) %>% count(AEBODSYS, TRTA) %>%
      mutate(pct = 100 * n / as.numeric(N[as.character(TRTA)]))
    ord <- d %>% group_by(AEBODSYS) %>% summarise(t = sum(n)) %>% arrange(t) %>% pull(AEBODSYS)
    m <- tidyr::pivot_wider(d %>% select(-n), names_from = TRTA, values_from = pct, values_fill = 0)
    m <- m[match(ord, m$AEBODSYS), ]
    z <- as.matrix(m[, intersect(levels(adsl_f()$TRT01A), names(m))])
    plot_ly(x = short_arm[colnames(z)], y = m$AEBODSYS, z = z, type = "heatmap",
            colors = colorRamp(c("#FBFAF7", "#F2A65A", "#C03A51", "#5B1A2B")),
            text = round(z, 1), texttemplate = "%{text}%", hovertemplate = "%{y}<br>%{x}: %{z:.1f}%<extra></extra>") %>%
      layout(xaxis = list(title = "", side = "top"), yaxis = list(title = "", dtick = 1, tickfont = list(size = 11)),
             margin = list(l = 10)) %>%
      plotly_clean()
  })

  ## TTE
  tte_f <- reactive({
    ADTTE %>% filter(USUBJID %in% adsl_f()$USUBJID) %>% mutate(TRTA = droplevels(TRTA))
  })

  output$km <- renderPlotly({
    d <- tte_f()
    fit <- survfit(Surv(AVAL, 1 - CNSR) ~ TRTA, data = d)
    kd <- km_long(fit)
    p <- plot_ly()
    for (a in unique(kd$arm)) {
      k <- kd[kd$arm == a, ]
      if (input$km_ci) {
        p <- p %>% add_ribbons(x = k$time, ymin = k$lo, ymax = k$hi, line = list(shape = "hv", width = 0),
                               fillcolor = adjustcolor(arm_cols[a], 0.15), showlegend = FALSE, hoverinfo = "none")
      }
      p <- p %>% add_lines(x = k$time, y = k$surv, name = a, line = list(shape = "hv", color = arm_cols[a], width = 3),
                           text = sprintf("%s<br>Day %d: %.1f%%<br>at risk: %s", a, k$time, 100 * k$surv, k$nrisk),
                           hoverinfo = "text")
      cz <- k[k$ncens > 0, ]
      p <- p %>% add_markers(x = cz$time, y = cz$surv, marker = list(symbol = "line-ns-open", size = 8, color = arm_cols[a]),
                             showlegend = FALSE, hoverinfo = "none")
    }
    p %>% layout(xaxis = list(title = "Days since first dose"), yaxis = list(title = "Event-free probability", range = c(0, 1.02),
                 tickformat = ".0%")) %>% plotly_clean()
  })

  output$km_tab <- renderTable({
    d <- tte_f()
    fit <- survfit(Surv(AVAL, 1 - CNSR) ~ TRTA, data = d)
    tb <- summary(fit)$table
    if (is.null(dim(tb))) tb <- t(tb)
    out <- data.frame(Arm = short_arm[sub("TRTA=", "", rownames(tb))], N = tb[, "n.max"], Events = tb[, "events"],
                      `Median (d)` = tb[, "median"], check.names = FALSE)
    out$`HR vs PBO` <- "-"
    if ("Placebo" %in% d$TRTA && nlevels(d$TRTA) > 1) {
      cx <- coxph(Surv(AVAL, 1 - CNSR) ~ relevel(TRTA, "Placebo"), data = d)
      hr <- exp(cbind(coef(cx), confint(cx)))
      out$`HR vs PBO`[-1] <- sprintf("%.2f (%.2f-%.2f)", hr[, 1], hr[, 2], hr[, 3])
    }
    out
  }, striped = TRUE, digits = 0)
}

shinyApp(ui, server)
