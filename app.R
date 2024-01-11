# Load required packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(readxl)
library(dplyr)
library(DT)
library(ggplot2)

# Define UI
ui <- fluidPage(
  navbarPage(
    title = "Gene Expression Analyzer",
    tabPanel("Upload",
             fluidPage(
               fileInput("file", "Choose an Excel file", accept = ".xlsx"),
               DTOutput("raw_data_table")
             )),
    tabPanel("Summary",
             fluidPage(
               DTOutput("stats_1_table"),
               DTOutput("stats_2_table")
             )),
    tabPanel("Plot",
             fluidPage(
               fluidRow(
                 column(6, textInput("plot_title_1", "Enter title for plot 1", "")),
                 column(6, numericInput("font_size_title_1", "Font Size for Title", value = 12, min = 1))
               ),
               fluidRow(
                 column(6, textInput("x_axis_label_1", "Enter X-axis label for plot 1", "")),
                 column(6, numericInput("font_size_x_label_1", "Font Size for X-axis Label", value = 10, min = 1))
               ),
               fluidRow(
                 column(6, textInput("y_axis_label_1", "Enter Y-axis label for plot 1", "")),
                 column(6, numericInput("font_size_y_label_1", "Font Size for Y-axis Label", value = 10, min = 1))
               ),
               fluidRow(
                 column(12, textInput("bar_colors_1", "Enter bar colors for plot 1 (comma separated)", "")),
                 column(12, actionButton("apply_changes_1", "Apply Colors"))
               ),
               fluidRow(
                 column(12, plotOutput("summary_plot_1", height = "400px"))
               ),
               fluidRow(
                 column(12, downloadButton("download_plot_1", "Download Plot 1"))
               ),
               div(style = "height: 100px;"),
               fluidRow(
                 column(6, textInput("plot_title_2", "Enter plot title for plot 2", "")),
                 column(6, numericInput("font_size_title_2", "Font Size for Title", value = 12, min = 1))
               ),
               fluidRow(
                 column(6, textInput("x_axis_label_2", "Enter X-axis label for plot 2", "")),
                 column(6, numericInput("font_size_x_label_2", "Font Size for X-axis Label", value = 10, min = 1))
               ),
               fluidRow(
                 column(6, textInput("y_axis_label_2", "Enter Y-axis label for plot 2", "")),
                 column(6, numericInput("font_size_y_label_2", "Font Size for Y-axis Label", value = 10, min = 1))
               ),
               fluidRow(
                 column(12, textInput("bar_colors_2", "Enter bar colors for plot 2 (comma separated)", "")),
                 column(12, actionButton("apply_changes_2", "Apply Colors"))
               ),
               fluidRow(
                 column(12, plotOutput("summary_plot_2"))
               ),
               fluidRow(
                 column(12, downloadButton("download_plot_2", "Download Plot 2"))
               ),
               div(style = "height: 100px;")
             )),
    tabPanel("ANOVA",
             fluidPage(
               radioButtons("anova_type", "Choose ANOVA Type:", c("One Factor", "Two Factor")),
               actionButton("run_anova", "Run ANOVA"),
               verbatimTextOutput("anova_result_text"),
               verbatimTextOutput("posthoc_result_text"),
               actionButton("show_posthoc_analysis", "Show Post-Hoc Analysis")
             )
    )
  )
)
server <- function(input, output, session) {
  rv <- reactiveValues(raw_data = NULL, summary_stats_1 = NULL, summary_stats_2 = NULL, anova_result = NULL, posthoc_result = NULL)
  
  plot_1_colors <- reactiveVal(c("dimgray", "darkgray"))
  plot_2_colors <- reactiveVal(c("pink", "lightblue"))
  
  observeEvent(input$file, {
    req(input$file)
    if (tolower(tools::file_ext(input$file$name)) != "xlsx") {
      return("Please upload a .xlsx file.")
    }
    df <- read_excel(input$file$datapath)
    rv$raw_data <- df
    rv$summary_stats_1 <- df %>%
      group_by(Treatment) %>%
      summarise(
        mean = mean(Expression),
        n = n(),
        sd = sd(Expression),
        sem = sd(Expression) / sqrt(n())
      )
    rv$summary_stats_2 <- df %>%
      group_by(Treatment, Sex) %>%
      summarise(
        mean = mean(Expression),
        n = n(),
        sd = sd(Expression),
        sem = sd(Expression) / sqrt(n())
      )
  })
  
  output$raw_data_table <- renderDT({
    datatable(rv$raw_data, options = list(pageLength = 10))
  })
  
  output$stats_1_table <- renderDT({
    datatable(rv$summary_stats_1)
  })
  
  output$stats_2_table <- renderDT({
    datatable(rv$summary_stats_2)
  })
  
  output$summary_plot_1 <- renderPlot({
    req(rv$raw_data)
    bar_colors <- plot_1_colors()
    ggplot(rv$summary_stats_1, aes(x = Treatment, y = mean, fill = Treatment)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), position = position_dodge(width = 0.9), width = 0.25) +
      labs(title = input$plot_title_1, x = input$x_axis_label_1, y = input$y_axis_label_1) +
      theme_bw() +
      theme(
        plot.title = element_text(size = input$font_size_title_1, hjust = 0.5, family = "sans", face = "bold"),
        axis.title.x = element_text(size = input$font_size_x_label_1, family = "sans"),
        axis.title.y = element_text(size = input$font_size_y_label_1, family = "sans"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18)
      ) +
      scale_fill_manual(values = bar_colors)
  })
  
  output$summary_plot_2 <- renderPlot({
    req(rv$raw_data)
    bar_colors <- plot_2_colors()
    ggplot(rv$summary_stats_2, aes(x = Treatment, y = mean, fill = Sex)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), position = position_dodge(width = 0.9), width = 0.25) +
      labs(title = input$plot_title_2, x = input$x_axis_label_2, y = input$y_axis_label_2) +
      theme_bw() +
      theme(
        plot.title = element_text(size = input$font_size_title_2, hjust = 0.5, family = "sans", face = "bold"),
        axis.title.x = element_text(size = input$font_size_x_label_2, family = "sans"),
        axis.title.y = element_text(size = input$font_size_y_label_2, family = "sans"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18)
      ) +
      scale_fill_manual(values = bar_colors)
  })
  
  observeEvent(input$apply_changes_1, {
    bar_colors <- unname(strsplit(input$bar_colors_1, ",")[[1]])
    if (length(bar_colors) == 0) {
      bar_colors <- c("black", "darkgray")
    }
    plot_1_colors(bar_colors)
  })
  
  observeEvent(input$apply_changes_2, {
    bar_colors <- unname(strsplit(input$bar_colors_2, ",")[[1]])
    if (length(bar_colors) == 0) {
      bar_colors <- c("red", "blue")
    }
    plot_2_colors(bar_colors)
  })
  
  observeEvent(input$run_anova, {
    req(rv$raw_data)
    if (input$anova_type == "One Factor") {
      anova_result <- aov(Expression ~ Treatment, data = rv$raw_data)
    } else {
      anova_result <- aov(Expression ~ Treatment * Sex, data = rv$raw_data)
    }
    rv$anova_result <- anova_result
    output$anova_result_text <- renderPrint({
      summary(rv$anova_result)
    })
  })
  
  observeEvent(input$show_posthoc_analysis, {
    req(rv$anova_result)
    if (input$anova_type == "One Factor") {
      rv$posthoc_result <- TukeyHSD(rv$anova_result)
    } else {
      rv$posthoc_result <- TukeyHSD(aov(Expression ~ Treatment * Sex, data = rv$raw_data))
    }
    output$posthoc_result_text <- renderPrint({
      print(rv$posthoc_result)
    })
  })
  
  savePlotPNG <- function(plot, filename) {
    ggsave(filename, plot, device = "png", width = 8, height = 6, units = "in", dpi = 300)
  }
  
  output$download_plot_1 <- downloadHandler(
    filename = function() {
      paste("summary_plot_1.png")
    },
    content = function(file) {
      plot_1 <- ggplot(rv$summary_stats_1, aes(x = Treatment, y = mean, fill = Treatment)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), position = position_dodge(width = 0.9), width = 0.25) +
        labs(title = input$plot_title_1, x = input$x_axis_label_1, y = input$y_axis_label_1) +
        theme_bw() +
        theme(
          plot.title = element_text(size = input$font_size_title_1, hjust = 0.5, family = "sans", face = "bold"),
          axis.title.x = element_text(size = input$font_size_x_label_1, family = "sans"),
          axis.title.y = element_text(size = input$font_size_y_label_1, family = "sans"),
          axis.text.x = element_text(size = 18),
          axis.text.y = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18)
        ) +
        scale_fill_manual(values = plot_1_colors())
      savePlotPNG(plot_1, file)
    }
  )
  
  output$download_plot_2 <- downloadHandler(
    filename = function() {
      paste("summary_plot_2.png")
    },
    content = function(file) {
      plot_2 <- ggplot(rv$summary_stats_2, aes(x = Treatment, y = mean, fill = Sex)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), position = position_dodge(width = 0.9), width = 0.25) +
        labs(title = input$plot_title_2, x = input$x_axis_label_2, y = input$y_axis_label_2) +
        theme_bw() +
        theme(
          plot.title = element_text(size = input$font_size_title_2, hjust = 0.5, family = "sans", face = "bold"),
          axis.title.x = element_text(size = input$font_size_x_label_2, family = "sans"),
          axis.title.y = element_text(size = input$font_size_y_label_2, family = "sans"),
          axis.text.x = element_text(size = 18),
          axis.text.y = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18)
        ) +
        scale_fill_manual(values = plot_2_colors())
      savePlotPNG(plot_2, file)
    }
  )
  
  output$posthoc_result_text <- renderPrint({
  })
}

shinyApp(ui, server)
