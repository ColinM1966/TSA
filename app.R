# TSA Final Report Explorer - R Shiny
# ------------------------------------------------------------
# This app is anchored to the August 2026 TSA final report baseline and
# published 2050 scenario assumptions/results, while retaining a separate
# dynamic annual R implementation for testing alternative assumptions.
#
# Install once if needed:
# install.packages(c("shiny", "tidyverse", "DT", "scales"))
#
# Run with:
# shiny::runApp()

required_packages <- c("shiny", "tidyverse", "DT", "scales")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required package(s): ", paste(missing_packages, collapse = ", "),
      ". Install with: install.packages(c('shiny','tidyverse','DT','scales'))"
    ),
    call. = FALSE
  )
}

library(shiny)
library(tidyverse)
library(DT)
library(scales)

# ---- Report-aligned baseline --------------------------------------------
start_year <- 2023
end_year <- 2050
years <- start_year:end_year

baseline <- list(
  physical_area_ha = 1740363.4,
  productive_area_ha = 1510025,
  cpo_anchor_t = 4507460,
  ffb_yield_t_ha = 16.39,
  oer = 0.204,
  mspo = 0.91,
  rspo = 0.15,  # report does not specify a 2023 RSPO baseline; editable model assumption
  hcv_stock_ha = 770000,
  smallholder_scheme_area_ha = 359907,
  hcv_carbon_tco2_ha = 300  # dynamic-model assumption; report does not give a single central value
)

# Productive-area endpoints below are derived so the production equation
# reproduces the report's 2050 CPO outputs at the report's yield and OER defaults.
scenario_defaults <- tribble(
  ~scenario, ~physical_area_2050, ~productive_area_2050, ~yield_2050, ~oer_2050,
  ~mspo_2050, ~rspo_2050, ~restoration_target_ha, ~peat_target_ha,
  ~hcv_loss_rate, ~smallholder_readiness, ~support_usd_ha,
  ~cert_market_net_usd_t, ~forest_success, ~jobs_proxy_2050,
  ~participants_proxy_2050, ~cert_inclusion_2050,
  "BAU",            1760000, 1550421, 17.36, 0.2060, 0.94, 0.20,      0,     0, 0.0100, 0.4075,  0, -1.01, 0.75, 69650, 12631, 0.382,
  "Compromised SD", 1720000, 1509903, 17.84, 0.2075, 0.97, 0.35,  50000,  5000, 0.0055, 0.5175, 10,  2.93, 0.75, 66706, 16041, 0.499,
  "Feasible SD",    1700000, 1500668, 19.30, 0.2100, 0.99, 0.60, 200000, 12000, 0.0045, 0.6875, 20,  6.85, 0.75, 63912, 21310, 0.677,
  "Optimal SD",     1650000, 1479900, 20.85, 0.2120, 1.00, 0.75, 300000, 25000, 0.0020, 0.8425, 30,  9.04, 0.90, 58767, 26115, 0.840
)

# Fixed reference outputs reported in the August 2026 final report.
report_reference <- tribble(
  ~scenario, ~cpo_mt, ~net_tsa_value_bn, ~restoration_kha, ~hcv_remaining_kha,
  ~avoided_hcv_loss_kha, ~carbon_mt, ~jobs_supported,
  ~production_value_bn, ~net_cert_value_bn, ~carbon_value_bn, ~restoration_cost_bn,
  "BAU",            4.95, 4.450,   0, 587.0,   0.0, 0.00, 69650, 4.455, -0.005, 0.000, 0.000,
  "Compromised SD", 4.99, 4.767,  50, 663.5,  76.5, 1.36, 66706, 4.691,  0.011, 0.068, 0.002,
  "Feasible SD",    5.43, 5.592, 200, 681.7,  94.7, 2.89, 63912, 5.427,  0.030, 0.144, 0.010,
  "Optimal SD",     5.84, 6.523, 300, 729.5, 142.5, 4.87, 58767, 6.132,  0.042, 0.365, 0.016
) %>%
  mutate(implied_production_value_usd_t = production_value_bn * 1e9 / (cpo_mt * 1e6))

report_social <- tribble(
  ~scenario, ~smallholder_area_ha, ~smallholder_readiness, ~participants_proxy,
  ~certification_inclusion, ~support_cost_m, ~net_tsa_value_per_ha,
  "BAU",            359907, 0.4075, 12631, 0.382,  0.0, 2529,
  "Compromised SD", 359907, 0.5175, 16041, 0.499,  3.6, 2772,
  "Feasible SD",    359907, 0.6875, 21310, 0.677,  7.2, 3289,
  "Optimal SD",     359907, 0.8425, 26115, 0.840, 10.8, 3953
)

report_sensitivity <- tribble(
  ~driver, ~downside, ~upside, ~max_impact_bn,
  "CPO price", "-20%", "+20%", 1.085,
  "FFB yield", "-10%", "+10%", 0.546,
  "OER", "-2 pp", "+1 pp", 0.520,
  "Productive area factor", "-5%", "+5%", 0.273,
  "Carbon price", "USD 0", "+50%", 0.144,
  "Smallholder readiness", "-20 pp", "+10 pp", 0.036,
  "Forest sequestration rate", "-50%", "+50%", 0.034,
  "HCVF loss rate / at-risk factor", "0.5% BAU risk", "1.5% BAU risk", 0.032,
  "HCV stock-loss factor", "-40%", "+40%", 0.026,
  "Market access benefit", "-50%", "+50%", 0.021,
  "Compliance cost", "+50%", "-25%", 0.019,
  "Certification premium", "-50%", "+50%", 0.016,
  "RSPO coverage", "-10 pp", "+10 pp", 0.007,
  "MSPO coverage", "-20 pp", "+10 pp", 0.006
)

transition_states <- tribble(
  ~state, ~area_ha, ~interpretation,
  "Productive/reporting oil palm", 1510025, "MPOB reported/productive oil-palm area",
  "Residual/unconfirmed physical oil palm", 230338, "Difference between DOA physical and MPOB reported area",
  "HCV forest in Class II forests", 770000, "HCV forest stock used for conservation modelling",
  "Restoration opportunity / FOLUR target", 200000, "Restoration target proxy, not a verified restored baseline stock",
  "Other managed landscape / improved practices", 626400, "Proxy for other improved-practices land outside the DOA oil-palm footprint"
)

# ---- Helpers -------------------------------------------------------------
progress_fraction <- function(year, shape) {
  p <- pmin(1, pmax(0, (year - start_year) / (end_year - start_year)))
  p^shape
}

forest_seq_rate <- function(age, max_rate, tau) {
  ifelse(age <= 0, 0, max_rate * (1 - exp(-age / tau)))
}

peat_abatement_rate <- function(age, max_rate, tau) {
  ifelse(age <= 0, 0, max_rate * (1 - exp(-age / tau)))
}

build_model <- function(scenarios, pars) {
  annual <- crossing(scenarios, year = years) %>%
    group_by(scenario) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      progress = progress_fraction(year, pars$transition_shape),
      physical_area_ha = baseline$physical_area_ha +
        (physical_area_2050 - baseline$physical_area_ha) * progress,
      productive_area_ha = baseline$productive_area_ha +
        (productive_area_2050 - baseline$productive_area_ha) * progress,
      ffb_yield_t_ha = baseline$ffb_yield_t_ha +
        (yield_2050 - baseline$ffb_yield_t_ha) * progress,
      oer = baseline$oer + (oer_2050 - baseline$oer) * progress,
      mspo = baseline$mspo + (mspo_2050 - baseline$mspo) * progress,
      rspo = baseline$rspo + (rspo_2050 - baseline$rspo) * progress,
      readiness = 0.40 + (smallholder_readiness - 0.40) * progress,
      restoration_cum_ha = restoration_target_ha * progress,
      peat_cum_ha = peat_target_ha * progress,
      restoration_new_ha = pmax(0, restoration_cum_ha - lag(restoration_cum_ha, default = 0)),
      peat_new_ha = pmax(0, peat_cum_ha - lag(peat_cum_ha, default = 0)),
      cpo_t = baseline$cpo_anchor_t *
        (productive_area_ha / baseline$productive_area_ha) *
        (ffb_yield_t_ha / baseline$ffb_yield_t_ha) *
        (oer / baseline$oer)
    ) %>%
    ungroup()

  forest_cohorts <- annual %>%
    transmute(
      scenario,
      cohort_year = year,
      forest_new_ha = restoration_new_ha * pars$forest_share,
      forest_success
    ) %>%
    filter(forest_new_ha > 0)

  if (nrow(forest_cohorts) > 0) {
    forest_carbon <- crossing(forest_cohorts, year = years) %>%
      filter(year >= cohort_year) %>%
      mutate(
        age = year - cohort_year,
        seq_rate = forest_seq_rate(age, pars$forest_seq_max, pars$forest_seq_tau),
        forest_removal_tco2 = forest_new_ha * forest_success * seq_rate
      ) %>%
      group_by(scenario, year) %>%
      summarise(forest_removal_tco2 = sum(forest_removal_tco2), .groups = "drop")
  } else {
    forest_carbon <- tibble(scenario = character(), year = integer(), forest_removal_tco2 = numeric())
  }

  peat_cohorts <- annual %>%
    transmute(scenario, cohort_year = year, peat_new_ha) %>%
    filter(peat_new_ha > 0)

  if (nrow(peat_cohorts) > 0) {
    peat_carbon <- crossing(peat_cohorts, year = years) %>%
      filter(year >= cohort_year) %>%
      mutate(
        age = year - cohort_year,
        abatement_rate = peat_abatement_rate(age, pars$peat_abate_max, pars$peat_tau),
        peat_abatement_tco2 = peat_new_ha * abatement_rate
      ) %>%
      group_by(scenario, year) %>%
      summarise(peat_abatement_tco2 = sum(peat_abatement_tco2), .groups = "drop")
  } else {
    peat_carbon <- tibble(scenario = character(), year = integer(), peat_abatement_tco2 = numeric())
  }

  hcv <- scenarios %>%
    select(scenario, hcv_loss_rate) %>%
    crossing(year = years) %>%
    group_by(scenario) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      hcv_remaining_ha = baseline$hcv_stock_ha * (1 - hcv_loss_rate)^(year - start_year),
      hcv_loss_this_year_ha = ifelse(
        year == start_year,
        0,
        lag(hcv_remaining_ha) - hcv_remaining_ha
      )
    ) %>%
    ungroup()

  bau_hcv <- hcv %>%
    filter(scenario == "BAU") %>%
    select(year, bau_remaining_ha = hcv_remaining_ha)

  hcv <- hcv %>%
    left_join(bau_hcv, by = "year") %>%
    mutate(
      avoided_hcv_loss_cumulative_ha = pmax(0, hcv_remaining_ha - bau_remaining_ha),
      avoided_hcv_tco2_annual_equivalent = ifelse(
        year == start_year,
        0,
        pmax(0, avoided_hcv_loss_cumulative_ha - lag(avoided_hcv_loss_cumulative_ha, default = 0)) *
          pars$hcv_carbon_tco2_ha
      )
    )

  annual_costs <- annual %>%
    mutate(
      forest_new_ha = restoration_new_ha * pars$forest_share,
      other_new_ha = restoration_new_ha * (1 - pars$forest_share),
      forest_cum_ha = restoration_cum_ha * pars$forest_share,
      other_cum_ha = restoration_cum_ha * (1 - pars$forest_share),
      restoration_capex_usd =
        forest_new_ha * pars$forest_capex +
        other_new_ha * pars$other_capex +
        peat_new_ha * pars$peat_capex,
      restoration_maintenance_usd =
        forest_cum_ha * pars$forest_maint +
        other_cum_ha * pars$other_maint +
        peat_cum_ha * pars$peat_maint,
      smallholder_support_usd = baseline$smallholder_scheme_area_ha * support_usd_ha
    ) %>%
    select(
      scenario, year, restoration_capex_usd,
      restoration_maintenance_usd, smallholder_support_usd
    )

  model <- annual %>%
    left_join(forest_carbon, by = c("scenario", "year")) %>%
    left_join(peat_carbon, by = c("scenario", "year")) %>%
    left_join(
      hcv %>% select(
        scenario, year, hcv_remaining_ha,
        avoided_hcv_loss_cumulative_ha,
        avoided_hcv_tco2_annual_equivalent
      ),
      by = c("scenario", "year")
    ) %>%
    left_join(annual_costs, by = c("scenario", "year")) %>%
    mutate(
      forest_removal_tco2 = replace_na(forest_removal_tco2, 0),
      peat_abatement_tco2 = replace_na(peat_abatement_tco2, 0),
      avoided_hcv_tco2_annual_equivalent = replace_na(avoided_hcv_tco2_annual_equivalent, 0),
      total_carbon_tco2 = forest_removal_tco2 + peat_abatement_tco2 + avoided_hcv_tco2_annual_equivalent,
      cpo_price_usd_t = pars$cpo_price,
      carbon_price_usd_t = pars$carbon_price,
      production_value_usd = cpo_t * cpo_price_usd_t,
      certification_market_value_usd = cpo_t * cert_market_net_usd_t,
      carbon_value_usd = total_carbon_tco2 * carbon_price_usd_t,
      operating_cost_usd = cpo_t * pars$operating_cost,
      selected_monetised_value_usd =
        production_value_usd + certification_market_value_usd + carbon_value_usd -
        smallholder_support_usd - restoration_capex_usd - restoration_maintenance_usd -
        operating_cost_usd,
      discount_factor = 1 / (1 + pars$discount_rate)^(year - start_year),
      discounted_selected_value_usd = selected_monetised_value_usd * discount_factor
    )

  list(annual = annual, model = model)
}

# ---- App metadata --------------------------------------------------------
app_version <- "1.2.0"
app_release_date <- "2 October 2026"

# ---- UI -----------------------------------------------------------------
ui <- navbarPage(
  title = "TSA Scenario Explorer",
  id = "main_tab",
  header = tagList(
    tags$head(
      tags$style(HTML("\n        .well { padding: 12px; }\n        .control-label { font-weight: 600; }\n        .small-note { color: #666; font-size: 12px; }\n        .navbar-brand { font-weight: 700; }\n        .callout { background:#f6f8fa; border-left:4px solid #6c757d; padding:12px; margin-bottom:15px; }\n        .mode-box { background:#eef5f8; border:1px solid #d4e2e8; border-radius:5px; padding:10px 14px; margin:10px 15px 4px 15px; }\n        .status-ok { color:#287a3b; font-weight:600; }\n        .status-bad { color:#b33a3a; font-weight:600; }\n        .version-note { color:#777; font-size:11px; margin-top:5px; }\n      "))
    ),
    fluidRow(
      column(
        12,
        div(
          class = "mode-box",
          fluidRow(
            column(
              7,
              radioButtons(
                "user_mode", "Choose how you want to use the app",
                choices = c("Standard / Workshop", "Advanced"),
                selected = "Standard / Workshop", inline = TRUE
              ),
              conditionalPanel(
                condition = "input.user_mode == 'Standard / Workshop'",
                span(class = "small-note", "Workshop mode shows the main strategic choices and keeps technical assumptions at report-aligned defaults.")
              ),
              conditionalPanel(
                condition = "input.user_mode == 'Advanced'",
                span(class = "small-note", "Advanced mode exposes the full scenario, carbon, cost, timing and stress-test assumptions.")
              )
            ),
            column(
              5,
              div(style="text-align:right; padding-top:8px;",
                  actionButton("reset_all", "Reset all to report defaults", class="btn-default"),
                  div(class="version-note", paste0("Version ", app_version, " • ", app_release_date)))
            )
          )
        )
      )
    )
  ),

  tabPanel(
    "Inputs",
    fluidPage(
      div(class = "callout",
          strong("Report-aligned starting point. "),
          "The August 2026 final TSA report provides the baseline and most 2050 scenario targets. Parameters not numerically specified in the report remain clearly identified as model assumptions."),
      uiOutput("input_status"),

      conditionalPanel(
        condition = "input.user_mode == 'Standard / Workshop'",
        fluidRow(
          column(
            5,
            wellPanel(
              h4("1. Choose a scenario and adjust the main strategic inputs"),
              selectInput("edit_scenario", "Scenario", scenario_defaults$scenario, selected = "Feasible SD"),
              numericInput("productive_area", "Productive/reporting area in 2050 (ha)", 1500668, min = 0, step = 5000),
              numericInput("yield2050", "Effective FFB yield in 2050 (t/ha/yr)", 19.30, min = 0, step = 0.1),
              numericInput("oer2050", "OER in 2050 (%)", 21.0, min = 0, max = 100, step = 0.05),
              sliderInput("readiness", "Smallholder readiness (%)", 0, 100, 68.75, step = 0.25),
              numericInput("restoration_target", "Restoration target by 2050 (ha)", 200000, min = 0, step = 10000),
              numericInput("hcv_loss", "Annual HCV loss rate (%)", 0.45, min = 0, max = 5, step = 0.05),
              actionButton("apply_scenario", "Apply scenario changes", class = "btn-primary")
            )
          ),
          column(
            3,
            wellPanel(
              h4("2. Common market assumptions"),
              p(class="small-note", "These apply equally to every scenario."),
              numericInput("cpo_price", "CPO benchmark price (USD/t)", 900, min = 0, step = 25),
              numericInput("carbon_price", "Carbon price (USD/tCO2e)", 50, min = 0, step = 5),
              hr(),
              h4("Workshop guidance"),
              p("Change only one or two assumptions first. Check the Input pathways, Outputs and Report vs R tabs before changing more."),
              p(class="small-note", "Switch to Advanced mode at the top of the app for certification, peat, restoration-carbon, costs, timing and technical stress assumptions.")
            )
          ),
          column(
            4,
            h4("Current scenario assumptions"),
            DTOutput("scenario_table"),
            br(),
            downloadButton("download_scenarios", "Download assumptions CSV")
          )
        )
      ),

      conditionalPanel(
        condition = "input.user_mode == 'Advanced'",
        fluidRow(
          column(
            4,
            wellPanel(
              h4("Scenario assumptions"),
              selectInput("edit_scenario_adv", "Scenario", scenario_defaults$scenario, selected = "Feasible SD"),
              numericInput("physical_area", "Physical planted area in 2050 (ha)", 1700000, min = 0, step = 10000),
              numericInput("productive_area_adv", "Productive/reporting area in 2050 (ha)", 1500668, min = 0, step = 5000),
              numericInput("yield2050_adv", "Effective FFB yield in 2050 (t/ha/yr)", 19.30, min = 0, step = 0.1),
              numericInput("oer2050_adv", "OER in 2050 (%)", 21.0, min = 0, max = 100, step = 0.05),
              sliderInput("mspo2050", "MSPO coverage in 2050 (%)", 0, 100, 99, step = 1),
              sliderInput("rspo2050", "RSPO coverage in 2050 (%)", 0, 100, 60, step = 1),
              sliderInput("readiness_adv", "Smallholder readiness (%)", 0, 100, 68.75, step = 0.25),
              numericInput("restoration_target_adv", "Restoration target by 2050 (ha)", 200000, min = 0, step = 10000),
              numericInput("peat_target", "Peat rehabilitation by 2050 (ha) — model assumption", 12000, min = 0, step = 1000),
              numericInput("hcv_loss_adv", "Annual HCV loss rate (%)", 0.45, min = 0, max = 5, step = 0.05),
              numericInput("support", "Smallholder support (USD/ha/yr)", 20, min = 0, step = 5),
              numericInput("cert_market_net", "Certification/market value before support (USD/t CPO)", 6.85, step = 0.25),
              sliderInput("success", "Restoration success (%) — model assumption", 0, 100, 75, step = 1),
              actionButton("apply_scenario_adv", "Apply advanced scenario changes", class = "btn-primary")
            )
          ),
          column(
            4,
            wellPanel(
              h4("Common economic & timing assumptions"),
              p(class = "small-note", "Applied equally across scenarios."),
              numericInput("cpo_price_adv", "Common CPO benchmark price (USD/t)", 900, min = 0, step = 25),
              numericInput("carbon_price_adv", "Common carbon price (USD/tCO2e)", 50, min = 0, step = 5),
              sliderInput("discount_rate", "Real discount rate (%)", 0, 15, 5, step = 0.5),
              numericInput("operating_cost", "Additional operating cost (USD/t CPO)", 0, min = 0, step = 10),
              sliderInput("transition_shape", "Implementation timing shape", 0.5, 3, 1.3, step = 0.1),
              helpText("1 = linear; >1 delays implementation; <1 front-loads implementation."),
              hr(),
              h4("Carbon & restoration assumptions"),
              sliderInput("forest_share", "Forest share of restoration (%)", 0, 100, 70, step = 5),
              numericInput("forest_seq_max", "Mature forest removal rate (tCO2e/ha/yr)", 12, min = 0, step = 1),
              numericInput("forest_seq_tau", "Forest establishment time constant (years)", 5, min = 0.5, step = 0.5),
              numericInput("peat_abate_max", "Maximum peat abatement (tCO2e/ha/yr)", 10, min = 0, step = 1),
              numericInput("peat_tau", "Peat response time constant (years)", 3, min = 0.5, step = 0.5),
              numericInput("hcv_carbon", "HCV stock-loss factor (tCO2e/ha)", 300, min = 0, step = 25),
              numericInput("forest_capex", "Forest restoration capex (USD/ha)", 1200, min = 0, step = 100),
              numericInput("other_capex", "Other degraded-land capex (USD/ha)", 600, min = 0, step = 100),
              numericInput("peat_capex", "Peat rehabilitation capex (USD/ha)", 1200, min = 0, step = 100)
            )
          ),
          column(
            4,
            h4("Current scenario assumptions"),
            DTOutput("scenario_table_adv"),
            br(),
            downloadButton("download_scenarios_adv", "Download assumptions CSV"),
            br(), br(),
            div(class="callout", strong("Advanced parameters marked as model assumptions "),
                "are not fully specified numerically in the final report and should be treated as sensitivity inputs rather than observed values.")
          )
        )
      )
    )
  ),

  tabPanel(
    "Pathways",
    fluidPage(
      h3("Annual implementation pathways"),
      p("The final report shows milestone years. The R model interpolates annual pathways from the 2023 baseline to 2050."),
      plotOutput("pathway_plot", height = "850px")
    )
  ),

  tabPanel(
    "Outputs",
    fluidPage(
      h3("Dynamic R outputs"),
      plotOutput("output_plot", height = "760px"),
      hr(),
      plotOutput("production_plot", height = "450px")
    )
  ),

  tabPanel(
    "Carbon",
    fluidPage(
      div(class = "callout",
          "The dynamic R model treats restoration and peat rehabilitation as annual cohorts. A hectare treated late in the pathway therefore contributes less by 2050 than one treated early."),
      plotOutput("carbon_time_plot", height = "650px"),
      hr(),
      plotOutput("carbon_2050_plot", height = "500px")
    )
  ),

  tabPanel(
    "Economics",
    fluidPage(
      div(class = "callout",
          "The report's Net TSA Value is a partial annual 2050 indicator. The dynamic model keeps annual cash flows and also reports their discounted sum. It is still not a full profit or welfare measure unless complete costs are supplied."),
      fluidRow(
        column(8, plotOutput("value_components_plot", height = "520px")),
        column(4, h4("Discounted dynamic summary"), DTOutput("npv_table"))
      ),
      hr(),
      plotOutput("value_time_plot", height = "480px"),
      downloadButton("download_results", "Download annual R results CSV")
    )
  ),

  tabPanel(
    "Social",
    fluidPage(
      h3("Report socio-economic indicators"),
      p("Jobs and participant counts are retained as report proxies rather than converted into a new labour-market model."),
      DTOutput("social_table"),
      hr(),
      plotOutput("social_plot", height = "500px")
    )
  ),

  tabPanel(
    "Report vs R",
    fluidPage(
      div(class = "callout",
          strong("Purpose of this tab: "),
          "show where the dynamic R implementation agrees with, or departs from, the published final TSA. Differences are informative because the R model changes timing, carbon cohorts, discounting and treatment of external prices."),
      h3("2050 validation table"),
      DTOutput("validation_table"),
      hr(),
      plotOutput("validation_plot", height = "650px"),
      hr(),
      h4("Published 2050 monetised components"),
      DTOutput("report_economic_table")
    )
  ),

  tabPanel(
    "Stress & sensitivity",
    fluidPage(
      conditionalPanel(
        condition = "input.user_mode == 'Standard / Workshop'",
        fluidRow(
          column(
            4,
            wellPanel(
              h4("Workshop stress test"),
              selectInput("workshop_stress", "Stress case",
                          choices = c("No additional stress", "Report bounded hydrological", "Common combined downside"),
                          selected = "Report bounded hydrological"),
              p(class="small-note", "The selected shock is applied equally to every scenario. Switch to Advanced mode to edit each multiplier.")
            )
          ),
          column(8, plotOutput("stress_plot", height = "520px"), DTOutput("stress_table"))
        )
      ),
      conditionalPanel(
        condition = "input.user_mode == 'Advanced'",
        fluidRow(
          column(
            4,
            wellPanel(
              h4("Common shock"),
              selectInput("stress_preset", "Preset", c("User-defined", "Report bounded hydrological", "Common combined downside")),
              sliderInput("stress_cpo", "CPO price change (%)", -60, 30, -20, step = 5),
              sliderInput("stress_yield", "Yield change (%)", -40, 20, -10, step = 5),
              sliderInput("stress_oer", "OER change (%)", -15, 10, -2, step = 1),
              sliderInput("stress_carbon", "Carbon price change (%)", -100, 100, -20, step = 10),
              sliderInput("stress_cert", "Certification/market value change (%)", -100, 100, -20, step = 10),
              sliderInput("stress_restoration", "Restoration cost change (%)", 0, 100, 20, step = 5),
              p(class = "small-note", "The same external multipliers are applied to every scenario. No SRI is used to shrink world-price shocks.")
            )
          ),
          column(8, plotOutput("stress_plot_adv", height = "520px"), DTOutput("stress_table_adv"))
        )
      ),
      hr(),
      h3("Dynamic two-way sensitivity"),
      fluidRow(
        column(3,
               selectInput("sens_scenario", "Scenario", scenario_defaults$scenario, selected = "Feasible SD"),
               sliderInput("sens_price_range", "CPO price multiplier", 0.5, 1.5, value = c(0.75, 1.25), step = 0.05),
               sliderInput("sens_yield_range", "Yield multiplier", 0.7, 1.3, value = c(0.9, 1.1), step = 0.05),
               numericInput("sens_steps", "Steps on each axis", 5, min = 3, max = 11, step = 1)),
        column(9, plotOutput("sensitivity_plot", height = "560px"))
      ),
      hr(),
      h3("Published one-way sensitivity results"),
      DTOutput("report_sensitivity_table")
    )
  ),

  tabPanel(
    "Transition logic",
    fluidPage(
      h3("Baseline proxy states used in the final report"),
      DTOutput("transition_states_table"),
      br(),
      div(class = "callout",
          "The final report presents scenario-specific transition probability matrices as a decision-support representation rather than a spatial simulation. This R version does not invent new probabilities; it uses the report's explicit annual area, HCV-loss and restoration assumptions directly."),
      tags$ul(
        tags$li("BAU: highest continuing pressure and weakest transition toward restoration."),
        tags$li("Compromised SD: lower HCV loss and better management, but limited restoration movement."),
        tags$li("Feasible SD: stronger transition toward restoration, improved management and reduced HCV loss."),
        tags$li("Optimal SD: strongest HCV retention, restoration and improved-practice outcomes.")
      )
    )
  ),

  tabPanel(
    "About",
    fluidPage(
      h3("Using the two modes"),
      tags$ul(
        tags$li(strong("Standard / Workshop: "), "for facilitated discussion and non-technical users. It exposes only the main strategic levers."),
        tags$li(strong("Advanced: "), "for analysts who need to test technical assumptions, timing, carbon rates, costs and detailed stress multipliers.")
      ),
      h3("What changed from the training spreadsheet"),
      tags$ul(
        tags$li("Uses the final report's 2023 DOA/MPOB baseline rather than training anchors."),
        tags$li("Uses report 2050 yield, OER, MSPO, RSPO, restoration and smallholder-readiness assumptions."),
        tags$li("Runs annually from 2023 to 2050 rather than relying mainly on the endpoint."),
        tags$li("Tracks restoration and peat cohorts through time."),
        tags$li("Uses a common external CPO and carbon price across scenarios in the dynamic model."),
        tags$li("Uses common stress shocks rather than an SRI to make global shocks smaller for stronger scenarios."),
        tags$li("Adds discounting and separates capital expenditure from ongoing maintenance."),
        tags$li("Preserves the report's headline outputs in a separate validation layer rather than forcing them to equal the R results.")
      ),
      strong("Important:"),
      p("The final report remains the source for the published TSA results. The dynamic R model is an alternative analytical implementation for exploring how those results change under different structural assumptions."),
      p(class="version-note", paste0("App version ", app_version, " • ", app_release_date))
    )
  )
)

# ---- Server --------------------------------------------------------------
server <- function(input, output, session) {
  scenarios_rv <- reactiveVal(scenario_defaults)

  # Keep the Standard and Advanced scenario selectors aligned.
  observeEvent(input$edit_scenario, {
    if (!is.null(input$edit_scenario) && !identical(input$edit_scenario_adv, input$edit_scenario)) {
      updateSelectInput(session, "edit_scenario_adv", selected = input$edit_scenario)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$edit_scenario_adv, {
    if (!is.null(input$edit_scenario_adv) && !identical(input$edit_scenario, input$edit_scenario_adv)) {
      updateSelectInput(session, "edit_scenario", selected = input$edit_scenario_adv)
    }
  }, ignoreInit = TRUE)

  load_scenario_into_inputs <- function(scenario_name) {
    row <- scenarios_rv() %>% filter(scenario == scenario_name)
    req(nrow(row) == 1)

    # Standard / Workshop inputs
    updateNumericInput(session, "productive_area", value = row$productive_area_2050)
    updateNumericInput(session, "yield2050", value = row$yield_2050)
    updateNumericInput(session, "oer2050", value = row$oer_2050 * 100)
    updateSliderInput(session, "readiness", value = row$smallholder_readiness * 100)
    updateNumericInput(session, "restoration_target", value = row$restoration_target_ha)
    updateNumericInput(session, "hcv_loss", value = row$hcv_loss_rate * 100)

    # Advanced inputs
    updateNumericInput(session, "physical_area", value = row$physical_area_2050)
    updateNumericInput(session, "productive_area_adv", value = row$productive_area_2050)
    updateNumericInput(session, "yield2050_adv", value = row$yield_2050)
    updateNumericInput(session, "oer2050_adv", value = row$oer_2050 * 100)
    updateSliderInput(session, "mspo2050", value = row$mspo_2050 * 100)
    updateSliderInput(session, "rspo2050", value = row$rspo_2050 * 100)
    updateSliderInput(session, "readiness_adv", value = row$smallholder_readiness * 100)
    updateNumericInput(session, "restoration_target_adv", value = row$restoration_target_ha)
    updateNumericInput(session, "peat_target", value = row$peat_target_ha)
    updateNumericInput(session, "hcv_loss_adv", value = row$hcv_loss_rate * 100)
    updateNumericInput(session, "support", value = row$support_usd_ha)
    updateNumericInput(session, "cert_market_net", value = row$cert_market_net_usd_t)
    updateSliderInput(session, "success", value = row$forest_success * 100)
  }

  observeEvent(input$edit_scenario, {
    req(input$edit_scenario)
    load_scenario_into_inputs(input$edit_scenario)
  }, ignoreInit = FALSE)

  observeEvent(input$edit_scenario_adv, {
    req(input$edit_scenario_adv)
    load_scenario_into_inputs(input$edit_scenario_adv)
  }, ignoreInit = TRUE)

  # Keep common price assumptions aligned when switching between modes.
  observeEvent(input$cpo_price, {
    if (identical(input$user_mode, "Standard / Workshop") && !is.null(input$cpo_price))
      updateNumericInput(session, "cpo_price_adv", value = input$cpo_price)
  }, ignoreInit = TRUE)

  observeEvent(input$carbon_price, {
    if (identical(input$user_mode, "Standard / Workshop") && !is.null(input$carbon_price))
      updateNumericInput(session, "carbon_price_adv", value = input$carbon_price)
  }, ignoreInit = TRUE)

  observeEvent(input$cpo_price_adv, {
    if (identical(input$user_mode, "Advanced") && !is.null(input$cpo_price_adv))
      updateNumericInput(session, "cpo_price", value = input$cpo_price_adv)
  }, ignoreInit = TRUE)

  observeEvent(input$carbon_price_adv, {
    if (identical(input$user_mode, "Advanced") && !is.null(input$carbon_price_adv))
      updateNumericInput(session, "carbon_price", value = input$carbon_price_adv)
  }, ignoreInit = TRUE)

  scenario_input_issues <- reactive({
    req(input$user_mode)
    x <- scenarios_rv()
    if (identical(input$user_mode, "Standard / Workshop")) {
      row <- x %>% filter(scenario == input$edit_scenario)
      issues <- character()
      if (is.null(input$productive_area) || input$productive_area <= 0) issues <- c(issues, "Productive area must be greater than zero.")
      if (nrow(row) == 1 && !is.null(input$productive_area) && input$productive_area > row$physical_area_2050) issues <- c(issues, "Productive/reporting area cannot exceed the scenario's physical planted area.")
      if (is.null(input$yield2050) || input$yield2050 <= 0) issues <- c(issues, "FFB yield must be greater than zero.")
      if (is.null(input$oer2050) || input$oer2050 <= 0 || input$oer2050 > 30) issues <- c(issues, "OER should be greater than 0% and no more than 30% for this training model.")
      if (is.null(input$restoration_target) || input$restoration_target < 0) issues <- c(issues, "Restoration target cannot be negative.")
      if (is.null(input$hcv_loss) || input$hcv_loss < 0 || input$hcv_loss >= 100) issues <- c(issues, "HCV loss rate must be between 0% and 100% per year.")
      if (is.null(input$cpo_price) || input$cpo_price < 0) issues <- c(issues, "CPO price cannot be negative.")
      if (is.null(input$carbon_price) || input$carbon_price < 0) issues <- c(issues, "Carbon price cannot be negative.")
      issues
    } else {
      issues <- character()
      if (is.null(input$physical_area) || input$physical_area <= 0) issues <- c(issues, "Physical planted area must be greater than zero.")
      if (is.null(input$productive_area_adv) || input$productive_area_adv <= 0) issues <- c(issues, "Productive area must be greater than zero.")
      if (!is.null(input$physical_area) && !is.null(input$productive_area_adv) && input$productive_area_adv > input$physical_area) issues <- c(issues, "Productive/reporting area cannot exceed physical planted area.")
      if (is.null(input$yield2050_adv) || input$yield2050_adv <= 0) issues <- c(issues, "FFB yield must be greater than zero.")
      if (is.null(input$oer2050_adv) || input$oer2050_adv <= 0 || input$oer2050_adv > 30) issues <- c(issues, "OER should be greater than 0% and no more than 30% for this model.")
      if (is.null(input$restoration_target_adv) || input$restoration_target_adv < 0) issues <- c(issues, "Restoration target cannot be negative.")
      if (is.null(input$peat_target) || input$peat_target < 0) issues <- c(issues, "Peat rehabilitation target cannot be negative.")
      if (is.null(input$cpo_price_adv) || input$cpo_price_adv < 0) issues <- c(issues, "CPO price cannot be negative.")
      if (is.null(input$carbon_price_adv) || input$carbon_price_adv < 0) issues <- c(issues, "Carbon price cannot be negative.")
      issues
    }
  })

  output$input_status <- renderUI({
    issues <- scenario_input_issues()
    if (length(issues) == 0) {
      div(class="callout status-ok", "Inputs are internally consistent for the current mode.")
    } else {
      div(class="callout status-bad",
          strong("Check these inputs before applying changes:"),
          tags$ul(lapply(issues, tags$li)))
    }
  })

  observeEvent(input$apply_scenario, {
    issues <- scenario_input_issues()
    if (length(issues) > 0) {
      showNotification(paste(issues, collapse = " "), type = "error", duration = 8)
      return()
    }
    x <- scenarios_rv()
    i <- which(x$scenario == input$edit_scenario)
    req(length(i) == 1)
    x$productive_area_2050[i] <- input$productive_area
    x$yield_2050[i] <- input$yield2050
    x$oer_2050[i] <- input$oer2050 / 100
    x$smallholder_readiness[i] <- input$readiness / 100
    x$restoration_target_ha[i] <- input$restoration_target
    x$hcv_loss_rate[i] <- input$hcv_loss / 100
    scenarios_rv(x)
    load_scenario_into_inputs(input$edit_scenario)
    showNotification(paste(input$edit_scenario, "updated from Workshop inputs"), type = "message")
  })

  observeEvent(input$apply_scenario_adv, {
    issues <- scenario_input_issues()
    if (length(issues) > 0) {
      showNotification(paste(issues, collapse = " "), type = "error", duration = 8)
      return()
    }
    x <- scenarios_rv()
    i <- which(x$scenario == input$edit_scenario_adv)
    req(length(i) == 1)
    x$physical_area_2050[i] <- input$physical_area
    x$productive_area_2050[i] <- input$productive_area_adv
    x$yield_2050[i] <- input$yield2050_adv
    x$oer_2050[i] <- input$oer2050_adv / 100
    x$mspo_2050[i] <- input$mspo2050 / 100
    x$rspo_2050[i] <- input$rspo2050 / 100
    x$smallholder_readiness[i] <- input$readiness_adv / 100
    x$restoration_target_ha[i] <- input$restoration_target_adv
    x$peat_target_ha[i] <- input$peat_target
    x$hcv_loss_rate[i] <- input$hcv_loss_adv / 100
    x$support_usd_ha[i] <- input$support
    x$cert_market_net_usd_t[i] <- input$cert_market_net
    x$forest_success[i] <- input$success / 100
    scenarios_rv(x)
    load_scenario_into_inputs(input$edit_scenario_adv)
    showNotification(paste(input$edit_scenario_adv, "updated from Advanced inputs"), type = "message")
  })

  reset_common_inputs <- function() {
    updateNumericInput(session, "cpo_price", value = 900)
    updateNumericInput(session, "carbon_price", value = 50)
    updateNumericInput(session, "cpo_price_adv", value = 900)
    updateNumericInput(session, "carbon_price_adv", value = 50)
    updateSliderInput(session, "discount_rate", value = 5)
    updateNumericInput(session, "operating_cost", value = 0)
    updateSliderInput(session, "transition_shape", value = 1.3)
    updateSliderInput(session, "forest_share", value = 70)
    updateNumericInput(session, "forest_seq_max", value = 12)
    updateNumericInput(session, "forest_seq_tau", value = 5)
    updateNumericInput(session, "peat_abate_max", value = 10)
    updateNumericInput(session, "peat_tau", value = 3)
    updateNumericInput(session, "hcv_carbon", value = 300)
    updateNumericInput(session, "forest_capex", value = 1200)
    updateNumericInput(session, "other_capex", value = 600)
    updateNumericInput(session, "peat_capex", value = 1200)
    updateSelectInput(session, "workshop_stress", selected = "Report bounded hydrological")
    updateSelectInput(session, "stress_preset", selected = "User-defined")
    updateSliderInput(session, "stress_cpo", value = -20)
    updateSliderInput(session, "stress_yield", value = -10)
    updateSliderInput(session, "stress_oer", value = -2)
    updateSliderInput(session, "stress_carbon", value = -20)
    updateSliderInput(session, "stress_cert", value = -20)
    updateSliderInput(session, "stress_restoration", value = 20)
  }

  observeEvent(input$reset_all, {
    scenarios_rv(scenario_defaults)
    reset_common_inputs()
    current <- if (identical(input$user_mode, "Advanced")) input$edit_scenario_adv else input$edit_scenario
    if (is.null(current)) current <- "Feasible SD"
    load_scenario_into_inputs(current)
    showNotification("All assumptions reset to final-report defaults and documented model defaults", type = "message")
  })

  observeEvent(input$stress_preset, {
    if (identical(input$stress_preset, "Report bounded hydrological")) {
      updateSliderInput(session, "stress_cpo", value = 0)
      updateSliderInput(session, "stress_yield", value = -10)
      updateSliderInput(session, "stress_oer", value = 0)
      updateSliderInput(session, "stress_carbon", value = 0)
      updateSliderInput(session, "stress_cert", value = 0)
      updateSliderInput(session, "stress_restoration", value = 20)
    } else if (identical(input$stress_preset, "Common combined downside")) {
      updateSliderInput(session, "stress_cpo", value = -20)
      updateSliderInput(session, "stress_yield", value = -10)
      updateSliderInput(session, "stress_oer", value = -2)
      updateSliderInput(session, "stress_carbon", value = -20)
      updateSliderInput(session, "stress_cert", value = -20)
      updateSliderInput(session, "stress_restoration", value = 20)
    }
  })

  common_pars <- reactive({
    req(input$user_mode)
    cpo_now <- if (identical(input$user_mode, "Advanced")) input$cpo_price_adv else input$cpo_price
    carbon_now <- if (identical(input$user_mode, "Advanced")) input$carbon_price_adv else input$carbon_price
    req(cpo_now, carbon_now)
    list(
      cpo_price = cpo_now,
      carbon_price = carbon_now,
      discount_rate = input$discount_rate / 100,
      operating_cost = input$operating_cost,
      transition_shape = input$transition_shape,
      forest_share = input$forest_share / 100,
      forest_seq_max = input$forest_seq_max,
      forest_seq_tau = input$forest_seq_tau,
      peat_abate_max = input$peat_abate_max,
      peat_tau = input$peat_tau,
      hcv_carbon_tco2_ha = input$hcv_carbon,
      forest_capex = input$forest_capex,
      other_capex = input$other_capex,
      peat_capex = input$peat_capex,
      forest_maint = 75,
      other_maint = 40,
      peat_maint = 50
    )
  })

  model_result <- reactive({
    build_model(scenarios_rv(), common_pars())
  })

  output$scenario_table <- renderDT({
    d <- scenarios_rv() %>%
      transmute(
        Scenario = scenario,
        `Physical area 2050 (ha)` = physical_area_2050,
        `Productive area 2050 (ha)` = productive_area_2050,
        `Yield` = yield_2050,
        `OER %` = oer_2050 * 100,
        `MSPO %` = mspo_2050 * 100,
        `RSPO %` = rspo_2050 * 100,
        `Readiness %` = smallholder_readiness * 100,
        `Restoration ha` = restoration_target_ha,
        `Peat ha*` = peat_target_ha,
        `HCV loss %/yr` = hcv_loss_rate * 100
      )
    DT::datatable(d, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$scenario_table_adv <- renderDT({
    d <- scenarios_rv() %>%
      transmute(
        Scenario = scenario,
        `Physical area 2050 (ha)` = physical_area_2050,
        `Productive area 2050 (ha)` = productive_area_2050,
        `Yield` = yield_2050,
        `OER %` = oer_2050 * 100,
        `MSPO %` = mspo_2050 * 100,
        `RSPO %` = rspo_2050 * 100,
        `Readiness %` = smallholder_readiness * 100,
        `Restoration ha` = restoration_target_ha,
        `Peat ha*` = peat_target_ha,
        `HCV loss %/yr` = hcv_loss_rate * 100
      )
    DT::datatable(d, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$pathway_plot <- renderPlot({
    d <- model_result()$annual %>%
      transmute(
        scenario, year,
        `Physical area (Mha)` = physical_area_ha / 1e6,
        `Productive area (Mha)` = productive_area_ha / 1e6,
        `FFB yield (t/ha/yr)` = ffb_yield_t_ha,
        `OER (%)` = oer * 100,
        `MSPO (%)` = mspo * 100,
        `RSPO (%)` = rspo * 100,
        `Smallholder readiness (%)` = readiness * 100,
        `Restoration (000 ha)` = restoration_cum_ha / 1000
      ) %>%
      pivot_longer(-c(scenario, year), names_to = "indicator", values_to = "value")

    ggplot(d, aes(year, value, colour = scenario)) +
      geom_line(linewidth = 0.9) +
      facet_wrap(~indicator, scales = "free_y", ncol = 2) +
      labs(x = NULL, y = NULL, colour = "Scenario") +
      theme_minimal(base_size = 12)
  })

  output$output_plot <- renderPlot({
    d <- model_result()$model %>%
      filter(year == end_year) %>%
      transmute(
        scenario,
        `CPO production (Mt)` = cpo_t / 1e6,
        `HCV remaining (000 ha)` = hcv_remaining_ha / 1000,
        `Restoration (000 ha)` = restoration_cum_ha / 1000,
        `Carbon (MtCO2e/yr)` = total_carbon_tco2 / 1e6,
        `Selected value (USD bn/yr)` = selected_monetised_value_usd / 1e9
      ) %>%
      pivot_longer(-scenario, names_to = "output", values_to = "value")

    ggplot(d, aes(value, scenario, fill = scenario)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~output, scales = "free_x", ncol = 2) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12)
  })

  output$production_plot <- renderPlot({
    ggplot(model_result()$model, aes(year, cpo_t / 1e6, colour = scenario)) +
      geom_line(linewidth = 1) +
      labs(title = "Annual CPO production", x = NULL, y = "CPO (Mt)", colour = "Scenario") +
      theme_minimal(base_size = 12)
  })

  output$carbon_time_plot <- renderPlot({
    d <- model_result()$model %>%
      select(
        scenario, year,
        `Forest restoration` = forest_removal_tco2,
        `Peat rehabilitation` = peat_abatement_tco2,
        `Avoided HCV loss` = avoided_hcv_tco2_annual_equivalent
      ) %>%
      pivot_longer(-c(scenario, year), names_to = "source", values_to = "tco2")

    ggplot(d, aes(year, tco2 / 1e6, fill = source)) +
      geom_area() +
      facet_wrap(~scenario, ncol = 2) +
      labs(x = NULL, y = "MtCO2e / year", fill = "Source") +
      theme_minimal(base_size = 12)
  })

  output$carbon_2050_plot <- renderPlot({
    d <- model_result()$model %>%
      filter(year == end_year) %>%
      select(
        scenario,
        `Forest restoration` = forest_removal_tco2,
        `Peat rehabilitation` = peat_abatement_tco2,
        `Avoided HCV loss` = avoided_hcv_tco2_annual_equivalent
      ) %>%
      pivot_longer(-scenario, names_to = "source", values_to = "tco2")

    ggplot(d, aes(scenario, tco2 / 1e6, fill = source)) +
      geom_col() +
      labs(title = "2050 dynamic carbon outcome by source", x = NULL, y = "MtCO2e / year", fill = "Source") +
      theme_minimal(base_size = 12)
  })

  output$value_components_plot <- renderPlot({
    d <- model_result()$model %>%
      filter(year == end_year) %>%
      transmute(
        scenario,
        `CPO production value` = production_value_usd,
        `Certification / market` = certification_market_value_usd,
        `Carbon value` = carbon_value_usd,
        `Smallholder support` = -smallholder_support_usd,
        `Restoration capex` = -restoration_capex_usd,
        `Restoration maintenance` = -restoration_maintenance_usd,
        `Additional operating cost` = -operating_cost_usd
      ) %>%
      pivot_longer(-scenario, names_to = "component", values_to = "usd")

    ggplot(d, aes(scenario, usd / 1e9, fill = component)) +
      geom_col() +
      geom_hline(yintercept = 0, linewidth = 0.3) +
      labs(title = "2050 dynamic monetised components", x = NULL, y = "USD bn / year", fill = "Component") +
      theme_minimal(base_size = 12)
  })

  output$value_time_plot <- renderPlot({
    ggplot(model_result()$model, aes(year, selected_monetised_value_usd / 1e9, colour = scenario)) +
      geom_line(linewidth = 1) +
      labs(title = "Annual selected monetised value", x = NULL, y = "USD bn / year", colour = "Scenario") +
      theme_minimal(base_size = 12)
  })

  output$npv_table <- renderDT({
    d <- model_result()$model %>%
      group_by(scenario) %>%
      summarise(
        `Discounted selected value (USD bn)` = sum(discounted_selected_value_usd) / 1e9,
        `Cumulative carbon (MtCO2e)` = sum(total_carbon_tco2) / 1e6,
        `Total restoration capex (USD m)` = sum(restoration_capex_usd) / 1e6,
        .groups = "drop"
      ) %>%
      mutate(across(where(is.numeric), ~round(.x, 2)))
    DT::datatable(d, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$social_table <- renderDT({
    d <- report_social %>%
      mutate(
        `Smallholder readiness %` = smallholder_readiness * 100,
        `Certification inclusion` = round(certification_inclusion, 3)
      ) %>%
      select(
        Scenario = scenario,
        `Smallholder/scheme area (ha)` = smallholder_area_ha,
        `Readiness %` = `Smallholder readiness %`,
        `Participants proxy` = participants_proxy,
        `Certification inclusion` = `Certification inclusion`,
        `Support cost (USD m/yr)` = support_cost_m,
        `Net TSA value/ha (USD)` = net_tsa_value_per_ha
      )
    DT::datatable(d, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$social_plot <- renderPlot({
    d <- scenario_defaults %>%
      transmute(
        scenario,
        `Readiness (%)` = smallholder_readiness * 100,
        `Jobs supported (000)` = jobs_proxy_2050 / 1000,
        `Participants proxy (000)` = participants_proxy_2050 / 1000,
        `Certification inclusion (%)` = cert_inclusion_2050 * 100
      ) %>%
      pivot_longer(-scenario, names_to = "indicator", values_to = "value")

    ggplot(d, aes(value, scenario, fill = scenario)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~indicator, scales = "free_x", ncol = 2) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12)
  })

  validation_data <- reactive({
    dyn <- model_result()$model %>%
      filter(year == end_year) %>%
      transmute(
        scenario,
        r_cpo_mt = cpo_t / 1e6,
        r_value_bn = selected_monetised_value_usd / 1e9,
        r_restoration_kha = restoration_cum_ha / 1000,
        r_hcv_remaining_kha = hcv_remaining_ha / 1000,
        r_carbon_mt = total_carbon_tco2 / 1e6
      )

    report_reference %>%
      select(
        scenario, cpo_mt, net_tsa_value_bn, restoration_kha,
        hcv_remaining_kha, carbon_mt
      ) %>%
      left_join(dyn, by = "scenario")
  })

  output$validation_table <- renderDT({
    d <- validation_data() %>%
      transmute(
        Scenario = scenario,
        `Report CPO Mt` = cpo_mt,
        `R CPO Mt` = r_cpo_mt,
        `Report value USD bn/yr` = net_tsa_value_bn,
        `R selected value USD bn/yr` = r_value_bn,
        `Report restoration kha` = restoration_kha,
        `R restoration kha` = r_restoration_kha,
        `Report HCV remaining kha` = hcv_remaining_kha,
        `R HCV remaining kha` = r_hcv_remaining_kha,
        `Report carbon MtCO2e/yr` = carbon_mt,
        `R carbon MtCO2e/yr` = r_carbon_mt
      ) %>%
      mutate(across(where(is.numeric), ~round(.x, 3)))
    DT::datatable(d, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$validation_plot <- renderPlot({
    d <- validation_data() %>%
      transmute(
        scenario,
        `CPO production (Mt)|Report` = cpo_mt,
        `CPO production (Mt)|Dynamic R` = r_cpo_mt,
        `Selected value (USD bn/yr)|Report` = net_tsa_value_bn,
        `Selected value (USD bn/yr)|Dynamic R` = r_value_bn,
        `Carbon (MtCO2e/yr)|Report` = carbon_mt,
        `Carbon (MtCO2e/yr)|Dynamic R` = r_carbon_mt
      ) %>%
      pivot_longer(-scenario, names_to = "key", values_to = "value") %>%
      separate(key, into = c("metric", "source"), sep = "\\|")

    ggplot(d, aes(scenario, value, fill = source)) +
      geom_col(position = "dodge") +
      facet_wrap(~metric, scales = "free_y", ncol = 1) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_minimal(base_size = 12)
  })

  output$report_economic_table <- renderDT({
    d <- report_reference %>%
      transmute(
        Scenario = scenario,
        `Production value (USD bn/yr)` = production_value_bn,
        `Net certification value (USD bn/yr)` = net_cert_value_bn,
        `Carbon value (USD bn/yr)` = carbon_value_bn,
        `Restoration/rehab cost (USD bn/yr)` = restoration_cost_bn,
        `Net TSA value (USD bn/yr)` = net_tsa_value_bn,
        `Implied production value (USD/t CPO)` = implied_production_value_usd_t
      ) %>%
      mutate(across(where(is.numeric), ~round(.x, 3)))
    DT::datatable(d, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  stress_parameters <- reactive({
    if (identical(input$user_mode, "Standard / Workshop")) {
      case <- input$workshop_stress
      if (identical(case, "No additional stress")) {
        list(cpo = 0, yield = 0, oer = 0, carbon = 0, cert = 0, restoration = 0, label = case)
      } else if (identical(case, "Report bounded hydrological")) {
        list(cpo = 0, yield = -10, oer = 0, carbon = 0, cert = 0, restoration = 20, label = case)
      } else {
        list(cpo = -20, yield = -10, oer = -2, carbon = -20, cert = -20, restoration = 20, label = case)
      }
    } else {
      list(
        cpo = input$stress_cpo,
        yield = input$stress_yield,
        oer = input$stress_oer,
        carbon = input$stress_carbon,
        cert = input$stress_cert,
        restoration = input$stress_restoration,
        label = ifelse(is.null(input$stress_preset), "Advanced common stress", input$stress_preset)
      )
    }
  })

  stress_results <- reactive({
    m <- model_result()$model
    p <- common_pars()
    sp <- stress_parameters()
    m %>%
      mutate(
        stressed_yield = ffb_yield_t_ha * (1 + sp$yield / 100),
        stressed_oer = oer * (1 + sp$oer / 100),
        stressed_cpo_t = baseline$cpo_anchor_t *
          (productive_area_ha / baseline$productive_area_ha) *
          (stressed_yield / baseline$ffb_yield_t_ha) *
          (stressed_oer / baseline$oer),
        stressed_value_usd =
          stressed_cpo_t * cpo_price_usd_t * (1 + sp$cpo / 100) +
          stressed_cpo_t * cert_market_net_usd_t * (1 + sp$cert / 100) +
          total_carbon_tco2 * carbon_price_usd_t * (1 + sp$carbon / 100) -
          smallholder_support_usd -
          restoration_capex_usd * (1 + sp$restoration / 100) -
          restoration_maintenance_usd * (1 + sp$restoration / 100) -
          stressed_cpo_t * p$operating_cost,
        stressed_discounted_value_usd = stressed_value_usd * discount_factor
      )
  })

  make_stress_plot <- function() {
    sp <- stress_parameters()
    base <- model_result()$model %>%
      group_by(scenario) %>%
      summarise(value = sum(discounted_selected_value_usd) / 1e9, .groups = "drop") %>%
      mutate(case = "Base")

    stress <- stress_results() %>%
      group_by(scenario) %>%
      summarise(value = sum(stressed_discounted_value_usd) / 1e9, .groups = "drop") %>%
      mutate(case = sp$label)

    bind_rows(base, stress) %>%
      ggplot(aes(case, value, fill = scenario)) +
      geom_col(position = "dodge") +
      labs(x = NULL, y = "Discounted selected value (USD bn)", fill = "Scenario") +
      theme_minimal(base_size = 12)
  }

  make_stress_table <- function() {
    base <- model_result()$model %>%
      group_by(scenario) %>%
      summarise(base = sum(discounted_selected_value_usd), .groups = "drop")
    stressed <- stress_results() %>%
      group_by(scenario) %>%
      summarise(stressed = sum(stressed_discounted_value_usd), .groups = "drop")
    left_join(base, stressed, by = "scenario") %>%
      transmute(
        Scenario = scenario,
        `Base discounted value (USD bn)` = base / 1e9,
        `Stressed discounted value (USD bn)` = stressed / 1e9,
        `Value retained (%)` = ifelse(base == 0, NA_real_, 100 * stressed / base)
      ) %>%
      mutate(across(where(is.numeric), ~round(.x, 2)))
  }

  output$stress_plot <- renderPlot({ make_stress_plot() })
  output$stress_plot_adv <- renderPlot({ make_stress_plot() })

  output$stress_table <- renderDT({
    DT::datatable(make_stress_table(), options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  output$stress_table_adv <- renderDT({
    DT::datatable(make_stress_table(), options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$sensitivity_plot <- renderPlot({
    m <- model_result()$model %>% filter(scenario == input$sens_scenario)
    req(nrow(m) > 0)
    n <- input$sens_steps
    grid <- crossing(
      price_mult = seq(input$sens_price_range[1], input$sens_price_range[2], length.out = n),
      yield_mult = seq(input$sens_yield_range[1], input$sens_yield_range[2], length.out = n)
    )

    d <- crossing(grid, m) %>%
      mutate(
        cpo_sens_t = cpo_t * yield_mult,
        selected_sens =
          cpo_sens_t * cpo_price_usd_t * price_mult +
          cpo_sens_t * cert_market_net_usd_t +
          carbon_value_usd - smallholder_support_usd -
          restoration_capex_usd - restoration_maintenance_usd -
          cpo_sens_t * common_pars()$operating_cost,
        discounted_sens = selected_sens * discount_factor
      ) %>%
      group_by(price_mult, yield_mult) %>%
      summarise(npv_bn = sum(discounted_sens) / 1e9, .groups = "drop") %>%
      mutate(
        price_label = paste0(round(price_mult * 100), "%"),
        yield_label = paste0(round(yield_mult * 100), "%")
      )

    ggplot(d, aes(price_label, yield_label, fill = npv_bn)) +
      geom_tile() +
      geom_text(aes(label = round(npv_bn, 1)), size = 4) +
      labs(
        title = paste("Sensitivity -", input$sens_scenario),
        x = "CPO price relative to common base",
        y = "Yield relative to pathway",
        fill = "Discounted USD bn"
      ) +
      theme_minimal(base_size = 12)
  })

  output$report_sensitivity_table <- renderDT({
    DT::datatable(report_sensitivity, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$transition_states_table <- renderDT({
    d <- transition_states %>% mutate(area_ha = comma(area_ha, accuracy = 1))
    DT::datatable(d, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$download_scenarios <- downloadHandler(
    filename = function() paste0("TSA_report_aligned_scenario_assumptions_", Sys.Date(), ".csv"),
    content = function(file) write_csv(scenarios_rv(), file)
  )

  output$download_scenarios_adv <- downloadHandler(
    filename = function() paste0("TSA_report_aligned_scenario_assumptions_", Sys.Date(), ".csv"),
    content = function(file) write_csv(scenarios_rv(), file)
  )

  output$download_results <- downloadHandler(
    filename = function() paste0("TSA_dynamic_R_annual_results_", Sys.Date(), ".csv"),
    content = function(file) write_csv(model_result()$model, file)
  )
}

shinyApp(ui, server)
