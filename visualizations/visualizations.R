library(htmlwidgets)
library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(scales)
library(dplyr)
library(tidyr)

age <- read_xlsx("lf_comp_plotly.xlsx", sheet = "age")
edu <- read_xlsx("lf_comp_plotly.xlsx", sheet = "edu")
# Remember race should not be shown as stacked bar chart
race <- read_xlsx("lf_comp_plotly.xlsx", sheet = "race")

# Age 
age$Year <- as.factor(age$Year)
age_colors <- c("16 to 19 years" = "#44499C",
                "20 to 24 years" = "#009F4D",
                "25 to 44 years" = "#FFC600",
                "45 to 54 years" = "#FF8F00",
                "55 to 64" = "#009CDE",
                "65+" = "#22254E")

age_fig <- plot_ly(data=age,
               x = ~Share,
               y = ~Year,
               color = ~Age,
               colors = age_colors,
               type="bar",
               text = ~percent(Share, accuracy = 0.1),
               textposition = "inside",
               insidetextanchor = "middle",
               orientation = "h",
               hovertemplate = paste(
                 "<b>Year:</b> %{y}<br>",
                 "<b>Age Group:</b> %{fullData.name}<br>",
                 "<b>Share:</b> %{x:.1%}<extra></extra>"
               )) |>
  layout(barmode="stack",
         title = list (text = "Labor Force Composition by Age, Austin city"),
         xaxis = list(
           title = "",
           tickformat = ".0%",
           zeroline = FALSE),
         yaxis = list(
           title = "",
           categoryorder = "array",
           categoryarray = c("2024", "2014")),
         legend = list(
           orientation = "h",
           x = 0.5,
           xanchor = "center",
           y = 1.02,
           traceorder='normal'),
         margin = list(l=100, r = 100, t = 100, b = 100)
         )

age_fig

# Educational Attainment
edu$Year <- as.factor(edu$Year)
edu_colors <- c("Less than high school graduate" = "#44499C",
                "High school graduate (includes equivalency)" = "#009F4D",
                "Some college or associate's degree" = "#FFC600",
                "Bachelor's degree or higher" = "#FF8F00")

edu$Edu <- factor(
  edu$Edu,
  levels = c("Less than high school graduate", "High school graduate (includes equivalency)",
             "Some college or associate's degree", "Bachelor's degree or higher"))

edu_fig <- plot_ly(data=edu,
                   x = ~Share,
                   y = ~Year,
                   color = ~Edu,
                   colors = edu_colors,
                   type="bar",
                   text = ~percent(Share, accuracy = 0.1),
                   textposition = "inside",
                   insidetextanchor = "middle",
                   orientation = "h",
                   hovertemplate = paste(
                     "<b>Year:</b> %{y}<br>",
                     "<b>Education:</b> %{fullData.name}<br>",
                     "<b>Share:</b> %{x:.1%}<extra></extra>"
                   )) |>
  layout(barmode="stack",
         title = list (text = "Labor Force Composition by Educational Attainment, Austin city"),
         xaxis = list(
           title = "",
           tickformat = ".0%",
           zeroline = FALSE),
         yaxis = list(
           title = "",
           categoryorder = "array",
           categoryarray = c("2024", "2014")),
         legend = list(
           orientation = "h",
           x = 0.5,
           xanchor = "center",
           y = 1.07,
           traceorder='normal'),
         margin = list(l=100, r = 100, t = 100, b = 100))

edu_fig



# Race
race$Group <- paste(race$Race, race $Year)
race_colors <- c(
  "Asian alone 2014" = "#AAB0E6",
  "Asian alone 2024" = "#44499C",
  "Black or African American alone 2014" = "#99D1B3",
  "Black or African American alone 2024" = "#008743",
  "White alone, not Hispanic or Latino 2014" = "#FFD199",
  "White alone, not Hispanic or Latino 2024" = "#FF8F00",
  "Hispanic or Latino origin (of any race) 2014" = "#FFFA06",
  "Hispanic or Latino origin (of any race) 2024" = "#FFC600")

race$Label <- c('Asian alone',
                'Black or African<br>American alone',
                'White alone,<br>not Hispanic or Latino',
                'Hispanic or Latino<br>origin (of any race)',
                'Asian alone',
                'Black or African<br>American alone',
                'White alone,<br>not Hispanic or Latino',
                'Hispanic or Latino<br>origin (of any race)')

race_fig <- plot_ly(data=race,
                  x = ~Label,
                  y = ~Share,
                  color = ~Group,
                  colors = race_colors,
                  type="bar",
                  width = 5,
                  showlegend = FALSE,
                  hovertemplate = paste(
                    "</b> %{fullData.name}<br>",
                    "<b>Share:</b> %{y}<br>",
                    "<extra></extra>"
                  )) |>
  layout(barmode = "group",
         bargap = .15,
         autosize=TRUE,
         width=1000,
         height=600,
         title = "Labor Force Composition by Race & Ethnicity",
         yaxis = list(title="Share (%)", tickformat = ".0%"),
         xaxis = list(title = "Race & Ethnicity", type = "category"),
         annotations = list(
           list(
             text = "Ligher shade: 2014, Darker shade: 2024",
             x = 0.45, y = 1,
             xref = "paper", yref = "paper",
             showarrow=FALSE)
         ),
         margin = list(l=150, r = 100, t = 100, b = 100))
race_fig


# Earnings by Race & Ethnicity
earnings <- read_xlsx("lf_comp_plotly.xlsx", sheet = "earnings")

earnings_colors <- c("Asian Alone" = "#FFC600",
                     "Black or African American Alone" = "#009CDE",
                     "Hispanic or Latino" = "#008743",
                     "White Alone, Not Hispanic or Latino" = "#44499C",
                     "Total" = "#C6C5C4")
  
earnings <- earnings |> rename(Race = Earnings)
earnings_long <- earnings |>
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "Year",
    values_to = "Earnings")

earnings_fig <- plot_ly(data = earnings_long,
                        x = ~Year,
                        y = ~Earnings,
                        color = ~Race,
                        colors = earnings_colors,
                        type = "scatter",
                        mode = "lines+markers") |>
  layout(
    title = "Median Earnings by Race & Ethnicity, Austin city",
    yaxis = list(tickprefix = "$"),
    xaxis = list(title = "", tickangle = -30),
    legend = list(
      orientation = "h",
      x = 0.5, 
      xanchor = "center",
      y = 1.07),
    margin = list(l=100, r = 100, t = 100, b = 100)
  )

earnings_fig


# Stacked sector chart
sector <- read_xlsx("lf_comp_plotly.xlsx", sheet = "sector")
sector_long <- sector |>
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "Year",
    values_to = "Share")

sector_long$Sector <- factor(
  sector_long$Sector,
  levels = c("Private", "Local Government", "State Government", "Federal Government"))

sector_colors <- c(
  "Private" = "#22254E",
  "Local Government" = "#90CF8E",
  "State Government" = "#008743",
  "Federal Government" = "#005027"
)

sector_fig <- plot_ly(data = sector_long,
                        x = ~Year,
                        y = ~Share,
                        color = ~Sector,
                        colors = sector_colors,
                        type = "scatter",
                        mode = "lines",
                      stackgroup = "one")|>
  layout(
    title = "Employment by Sector, Austin MSA",
    yaxis = list(title = "", tickformat = ".0%"),
    xaxis = list(title = ""),
    legend = list(
      orientation = "h",
      x = 0.5, 
      xanchor = "center",
      y = 1.07),
    margin = list(l=100, r = 100, t = 100, b = 100)
  )
sector_fig


# Industry
industry <- read_xlsx("lf_comp_plotly.xlsx", sheet = "industry")

industry_colors <- c(
  "Austin city" = "#008743",
  "Austin MSA" = "#44499C",
  "Texas" = "#A9A9A9")
  
industry$Industry <- factor(industry$Industry, levels = unique(industry$Industry))
industry$Geography <- factor (industry$Geography, levels = unique(industry$Geography))

industry_fig <- plot_ly(data=industry,
                    x = ~Industry,
                    y = ~Share,
                    color = ~Geography,
                    colors = industry_colors,
                    type="bar",
                    showlegend = TRUE,
                    hovertemplate = paste(
                      "</b> %{x}<br>",
                      "<b>Geography:</b> %{fullData.name}<br>",
                      "<b>Share:</b> %{y}<br>"
                    )) |>
  layout(barmode = "group",
         title = "Industries by Share of Employment for Jobs, 2023",
         yaxis = list(title="Share (%)", tickformat = ".0%"),
         xaxis = list(title = ""),
         legend = list(
           orientation = "h",
           x = 0.5,
           xanchor = "center",
           y = 1.02,
           traceorder='normal'),
         margin = list(l=100, r = 100, t = 100, b = 100)
         )
industry_fig


# Industry Growth chart
ind_growth


# Means of Transportation (line graph, Austin MSA - home)
transpo <- read_xlsx("lf_comp_plotly.xlsx", sheet = "transpo")

transpo_long <- transpo |>
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "Year",
    values_to = "Share")

transpo_colors <- c(
  "Private" = "#22254E",
  "Local Government" = "#90CF8E",
  "State Government" = "#008743",
  "Federal Government" = "#005027"
)

transpo_fig <- plot_ly(data = transpo_long,
                      x = ~Year,
                      y = ~Share,
                      color = ~Transportation,
                      #colors = sector_colors,
                      type = "scatter",
                      mode = "lines",
                      stackgroup = "one")|>
  layout(
    title = "Means of Transportation, Austin MSA",
    yaxis = list(title = "", tickformat = ".0%"),
    xaxis = list(title = ""),
    legend = list(
      orientation = "h",
      x = 0.5, 
      xanchor = "center",
      y = -0.15)
  )
transpo_fig



htmlwidgets::saveWidget(age_fig, "visualizations/labor-force-age/index.html", selfcontained = FALSE)
htmlwidgets::saveWidget(edu_fig, "visualizations/labor-force-edu/index.html", selfcontained = FALSE)
htmlwidgets::saveWidget(race_fig, "visualizations/labor-force-race-ethnicity/index.html", selfcontained = FALSE)
htmlwidgets::saveWidget(earnings_fig, "visualizations/earnings-by-race/index.html", selfcontained = FALSE)
htmlwidgets::saveWidget(sector_fig, "visualizations/jobs-by-sector/index.html", selfcontained = FALSE)
htmlwidgets::saveWidget(industry_fig, "visualizations/industry-share/index.html", selfcontained = FALSE)
