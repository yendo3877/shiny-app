# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(multcompView)
library(readr)

# Load the data
d <- read_csv("2228_hypo3_ino80.csv")
d$Genotype <- as.factor(d$Genotype)
d$Temp <- as.factor(d$Temp)

# Define UI
ui <- fluidPage(
  titlePanel("ANOVA with Tukey HSD Letters"),
  sidebarLayout(
    sidebarPanel(
      selectInput("yvar", "Choose variable for ANOVA:",
                  choices = names(d)[sapply(d, is.numeric)]),
      selectInput("group", "Group by:",
                  choices = c("Genotype", "Temp", "Genotype:Temp"))
    ),
    mainPanel(
      plotOutput("anovaPlot"),
      tableOutput("anovaTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$anovaPlot <- renderPlot({
    req(input$yvar, input$group)
    
    # Create grouping variable
    if (input$group == "Genotype:Temp") {
      d$Group <- interaction(d$Genotype, d$Temp)
    } else {
      d$Group <- d[[input$group]]
    }
    d$Group <- factor(d$Group)
    
    # Build formula
    form <- as.formula(paste(input$yvar, "~ Group"))
    
    # Run ANOVA
    aov_res <- aov(form, data = d)
    hsd <- TukeyHSD(aov_res, "Group")
    
    # Try to get compact letter display
    if (!is.null(hsd$Group) && !is.null(rownames(hsd$Group))) {
      pvals <- hsd$Group[, "p adj"]
      names(pvals) <- rownames(hsd$Group)
      letters <- multcompLetters(pvals)$Letters
      letter_df <- data.frame(Group = names(letters), Letter = letters)
    } else {
      letter_df <- data.frame(Group = levels(d$Group), Letter = "")
    }
    
    # Calculate group means
    summary_df <- d %>%
      group_by(Group) %>%
      summarise(Mean = mean(.data[[input$yvar]], na.rm = TRUE)) %>%
      left_join(letter_df, by = "Group")
    
    # Create plot
    ggplot(d, aes(x = Group, y = .data[[input$yvar]])) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.5) +
      geom_text(data = summary_df, aes(x = Group, y = Mean + 0.2, label = Letter),
                color = "blue", size = 5) +
      theme_minimal() +
      xlab("Group") + ylab(input$yvar) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$anovaTable <- renderTable({
    req(input$yvar, input$group)
    
    # Create grouping variable
    if (input$group == "Genotype:Temp") {
      d$Group <- interaction(d$Genotype, d$Temp)
    } else {
      d$Group <- d[[input$group]]
    }
    d$Group <- factor(d$Group)
    
    form <- as.formula(paste(input$yvar, "~ Group"))
    summary(aov(form, data = d))[[1]]
  }, rownames = TRUE)
}

# Run the app
shinyApp(ui = ui, server = server)
