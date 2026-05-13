library(htmlwidgets)
library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(scales)
library(dplyr)
library(tidyr)

industry <- read_xlsx("by_industry_plotly.xlsx", sheet = "industry")

industry_colors <- c("City Jobs" = "#009F4D",
                     "City Workers" = "#005027",
                     "MSA Jobs" = "#9A9DD2",
                     "MSA Workers" = "#44499C")

industry_fix <- industry |>
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "Year",
    values_to = "Value")

industry_long <- industry_fix |>
  pivot_wider(
    names_from = Type,
    values_from = Value)

manufacturing <- industry_long |>
  filter(Industry == "Manufacturing")

manufacturing_fig <- plot_ly(data = manufacturing,
                        x = ~Year,
                        y = ~Count,
                        color = ~Geography,
                        colors = industry_colors,
                        type = "scatter",
                        mode = "lines+markers") |>
  layout(
    title = "Industry Growth in Austin city",
    yaxis = list(),
    xaxis = list(title = "", tickangle = -30),
    legend = list(
      orientation = "h",
      x = 0.5, 
      xanchor = "center",
      y = 1.07),
    margin = list(l=100, r = 100, t = 100, b = 100)
  )

manufacturing_fig