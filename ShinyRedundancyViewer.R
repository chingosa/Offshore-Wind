library(shiny)
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(knitr)

file_path <- './ClassificationData.xlsx'
data <- read_excel(path = file_path, sheet = 1)
data$prj <- substr(data$Code,1,3)
data$prj <- factor(data$prj, levels = c("BIW", "YVW", "SFR", "OWO", "REV", "KHW", 'CVW', "SUN", "NEW", "MFW", "ASS", "EPW"))

#Setting All unclassified Items to prj specific
data$Name[is.na(data$Name)] <- 'Prj Specific'
data$sect <- substr(data$Code,4,4)

#Retrieving the Original Codified Measures
ogMeasures <- "./OriginalMeasures.xlsx"
dfList <- list()
for (i in c(1:length(excel_sheets(ogMeasures)))){
  dfList[[i]] <- read_excel(ogMeasures, sheet = i)
}

#--------------------------------------------------------------------
#Defining Functions
getSingleRow <- function(CodeList, df_list) {
  matching_df <- NULL
  
  for (df in df_list) {
    if (first(CodeList) %in% df$Code) {
      matching_df <- df
      matching_df <- filter(matching_df, matching_df$Code %in% CodeList)
      
      break
    }
  }
  if (!is.null(matching_df)) {
    result <- matching_df
    return(result)
  } 
}
#--------------------------------------------------------------------
counts <- function(data = data, name, prj = NULL){
  result <- data %>%
    group_by(Name, prj) %>%
    summarize(count = n()) 
  
  all_combinations <- expand.grid(
    Name = unique(data$Name),
    prj = unique(data$prj)
    
  )
  
  # Left join the result with all_combinations to include all combinations
  result <- all_combinations %>%
    left_join(result, by = c("Name", 'prj')) %>%
    mutate(count = coalesce(count, 0))
  
  if (is.null(prj)){
    result <- result[(result$Name == name), ]
    plt <- ggplot(result, aes(prj, count))+ geom_histogram(stat = 'identity')+labs(title = paste0('Count of ', name," by Project"), x = 'Projects', y= 'Count')+theme_light()
    return(plt)
    
  }else{
    df <- data[(data$Name == name)&(data$prj == prj),]
    tabList <- list()
    j <- 1
    
    for (i in unique(df$sect[(df$prj == prj)])){
      dfSect <- df[df$sect == i,]
      dfSectCodes <- first(list(df$Code[df$sect == i]))
      
      tabList[[j]] <- getSingleRow(dfSectCodes, dfList)
      j <- j+1
    }
    
    
    
    return(tabList)
    
    
  }
  
}

#Unique Prj names and Measures
unique_column1 <- unique(data$Name)
unique_column2 <- unique(data$prj)

#Making sure it works
result<-counts(data, "Pile Driving/HRG", "EPW")



#Defining UI--------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Redundancy Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for column1
      selectInput("input_column1", "Protected Resource:",
                  choices = unique_column1),
      
      # Dropdown for column2
      selectInput("input_column2", "Project:",
                  choices = unique_column2)
    ),
    
    mainPanel(
      # Graphical output
      plotOutput("output_plot"),
      
      # Verbatim text output
      uiOutput("checkboxes")
    )
  )
)


# Defining Server-----------------------------------------------------------------
server <- function(input, output) {
  
  # Render graphical output
  output$output_plot <- renderPlot({
    counts(data, input$input_column1) 
  })

  #Rendering The Matching Items as a df
  
  output$checkboxes <- renderUI({
    tabList <- counts(data, input$input_column1, input$input_column2)
    
    output_list <- lapply(seq_along(tabList), function(i) {
      outputId <- paste0("table_", i)
      renderTable(tabList[[i]], outputId = outputId)
    })
    
    tagList(output_list, tags$hr(style = "border: none; height: 4px; background-color: black;"))
  })
  
  
}


# Run the app
shinyApp(ui, server)