library(htmlwidgets)
library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(scales)
library(dplyr)
library(tidyr)

library(htmltools)
library(bslib)
library(shiny)
library(xfun)


edu_city <- read_xlsx("lf_comp_plotly.xlsx", sheet = "edu")
# Remember race should not be shown as stacked bar chart
race_city <- read_xlsx("lf_comp_plotly.xlsx", sheet = "race")

# AGE ------------------------------------------------------------ 
age_city <- read_xlsx("lf_comp_plotly.xlsx", sheet = "age_city")
age_msa <- read_xlsx("lf_comp_plotly.xlsx", sheet = "age_msa")

age_colors <- c("16 to 19 years" = "#44499C",
                "20 to 24 years" = "#009F4D",
                "25 to 44 years" = "#FFC600",
                "45 to 54 years" = "#FF8F00",
                "55 to 64" = "#009CDE",
                "65+" = "#22254E")

age_city$Year <- as.factor(age_city$Year)
age_city_fig <- plot_ly(data=age_city,
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
           y = 1.1,
           traceorder='normal'),
         margin = list(l=100, r = 100, t = 100, b = 100)
  )

age_city_fig


age_msa$Year <- as.factor(age_msa$Year)
age_msa_fig <- plot_ly(data=age_msa,
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
         title = list (text = "Labor Force Composition by Age, Austin MSA"),
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
           y = 1.1,
           traceorder='normal'),
         margin = list(l=100, r = 100, t = 100, b = 100)
  )

age_msa_fig


age_city_fig <- age_city_fig |> layout(height=550)
age_msa_fig <- age_msa_fig |> layout(height=550)

age_widget <- page_fluid(
  style="padding:0;",
  theme=bs_theme(version = 5),
  navset_tab(
    nav_panel("Austin city", age_city_fig),
    nav_panel("Austin MSA", age_msa_fig)
  )
)
age_widget


htmltools::save_html(
  age_widget,
  file = "labor-force-age.html",
  libdir = "lf_age_files")



# Educational Attainment -------------------------------------------
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



# Tabbed HTML widget
tabs <- tagList(
  tags$style(HTML("
  .tab-button {
  border: none;
  padding: 10px 16px;
  cursor: pointer;
  background-color: #eee;
  font-size: 15px;
  }
  
  .tab-button.active {
  background-color: #ccc;
  font-weight: bold;
  }
  
  .tab-content {
  display: none;
  padding-top: 15px;
  }
  
  .tab-content.active {
  display: block;
  }
                  ")),
  tags$div(
    tags$button(
      "Austin city",
      class = "tab-button active",
      onclick = "openTab(event, 'city')"
    ),
    
    tags$button(
      "Austin MSA",
      class = "tab-button",
      onclick = "openTab(event, 'msa')"
    )
  ),
  
  tags$div(
    id = "city",
    class = "tab-content active",
    age_city_fig
  ),
  
  tags$div(
    id = "msa",
    class = "tab-content",
    edu_fig
  ),
  
  tags$script(HTML("
    function openTab(evt, tabName) {
      var i, tabcontent, tabbuttons;
  
    tabcontent = document.getElementsByClassName('tab-content');
    for (i = 0; i < tabcontent.length; i++) { 
      tabcontent[i].classList.remove('active');
  }
  
    tabbuttons = document.getElementsByClassName('tab-button');
    for (i = 0; i < tabbuttons.length; i++) { 
      tabbuttons[i].classList.remove('active');
  }
  
    document.getElementById(tabName).classList.add('active');
    evt.currentTarget.classList.add('active');
  }
                   "))
  )

browsable(tabs)