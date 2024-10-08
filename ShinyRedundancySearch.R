library(shiny)
library(openxlsx)
library(readxl)
library(shiny)
library(shinyjs)
library(dplyr)
library(shinythemes)
library(bslib)


#Retrieving the Original Codified Measures
ogMeasures <- "./OriginalMeasures.xlsx"
dfList <- list()
for (i in c(1:length(excel_sheets(ogMeasures)))){
  dfList[[i]] <- read_excel(ogMeasures, sheet = i)
}

#Retrieving classData
file_path <- "./ClassificationData.xlsx"
classData <- read_excel(file_path, sheet = 1) #Grabbing the data from excel

#Get single row function
getSingleRow <- function(Code, df_list) {
  matching_df <- NULL
  for (df in df_list) {
    if (Code %in% df$Code) {
      matching_df <- df
      break
    }
  }
  
# Extract the single row from the matching dataframe
  if (!is.null(matching_df)) {
    result <- matching_df[matching_df$Code == Code, ]
    return(result)
  } 
}

#Grabbing Class Data Again?
if (file.exists("./ClassificationData.xlsx")) {
  codeList<- read_excel("./ClassificationData.xlsx")#Grabbing the data from excel
  codeList_react <- reactiveVal(codeList)
  print("Data Successfully Retrieved")
}
#------------------------------------------------------------------------------------------------------------------
#Defining UI
ui <- fluidPage(
                
                useShinyjs(),  # Enable ShinyJS
                #Defining positioning
                tags$head(
                  tags$style(
                    HTML('
           .sticky-sidebar {
             position: sticky;
             top: 0;
             height: 100vh; /* Adjust this value as needed */
             overflow-y: auto; /* Enable vertical scrolling if the content exceeds the height */
           }
           ')
                  )
                ),
                sidebarLayout(
                  sidebarPanel(
                    #SEARCH BAR the items list is the stand in for now
                    
                    checkboxGroupInput('Phase_bx', 'Phase', choices = c('PreC'="PC", 'Con'="C", 'OM'='O', 'DeCom'='DC'), inline = T),
                    checkboxGroupInput('Loc_bx', 'Location', choices = c('Site', "Cable", 'OnShore'), inline = T),
                    checkboxGroupInput('UpDown_bx', 'Above-Below', choices = c('AboveWater', "BelowWater", 'NA'), inline = T),
                    checkboxGroupInput('MonMit_bx', 'Mon/Mit', choices = c('Monitoring'='Mon', 'Mitigation'='Mit'), inline = T),
                    checkboxGroupInput('Prot_bx', 'Protected Resource', choices = c("MarineMams", "Turts", "Birds", "Bats", "Benthic", "AirQ", "WaterQ", "AcouAbove", "AcouBelow","Fish", "EssFishHabs", "CultArcheo", "CoastalHabs", "WetLands", "RecTourism", "RecComFisheries","VesselNav", "SocioEcon", "EnviroJustice", "PubHealth", "Visual", "FAA-DOD","Other"), inline = TRUE),
                    #CATAGORICAL SETTING
                    selectInput('category_select', 'Select Category', choices = NULL),
                    conditionalPanel(
                      condition = "input.category_select == 'NEW'",  # Show the text box when 'NEW' is selected
                      textInput(inputId = "new_category", label = "Enter New Category")
                    ),
                    actionButton("enter_bt", "Enter", class = "btn-success")
                  ), mainPanel(
                    # Dynamic UI generation of each measure
                    uiOutput("checkboxes")
                  )
                ))

#------------------------------------------------------------------------------------------------------------------
#Definign Server
server <- function(input, output, session) {
  #Enabling reactivity 
  filteredList <- reactive({
    filteredData <- read_excel("./ClassificationData.xlsx")
    filteredData <- filteredData[is.na(filteredData$Name), ]
    #Filtering by input
    filteredData <- filter(filteredData,( 
      ((filteredData$PreC & ('PC' %in% input$Phase_bx)) |
         (filteredData$Con & ('C' %in% input$Phase_bx)) |
         (filteredData$OM & ('O' %in% input$Phase_bx)) |
         (filteredData$Dcom & ('DC' %in% input$Phase_bx)))&
        
        ((filteredData$Mon & ('Mon' %in% input$MonMit_bx)) |
           (filteredData$Mit & ('Mit' %in% input$MonMit_bx))) &
        
        ((grepl("Site", filteredData$Loc) & ('Site' %in% input$Loc_bx))|
           (grepl("Cable",  filteredData$Loc) & ('Cable' %in% input$Loc_bx))|
           (grepl("OnShore" , filteredData$Loc) & ('OnShore' %in% input$Loc_bx))) &
        
        ((grepl("AboveWater", filteredData$AB) & ('AboveWater' %in% input$UpDown_bx))|
           (grepl("BelowWater", filteredData$AB) & ('BelowWater' %in% input$UpDown_bx))) &
        
        ((grepl("MarineMams", filteredData$Prot) & ('MarineMams' %in% input$Prot_bx))|
           (grepl("Turts" ,  filteredData$Prot) & ('Turts' %in% input$Prot_bx))|
           (grepl("Birds" ,  filteredData$Prot) & ('Birds' %in% input$Prot_bx))|
           (grepl("Bats" ,  filteredData$Prot) & ('Bats' %in% input$Prot_bx))|
           (grepl("Benthic" ,  filteredData$Prot) & ('Benthic' %in% input$Prot_bx))|
           (grepl("AirQ" ,  filteredData$Prot) & ('AirQ' %in% input$Prot_bx))|
           (grepl("WaterQ" ,  filteredData$Prot) & ('WaterQ' %in% input$Prot_bx))|
           (grepl("AcouAbove" ,  filteredData$Prot) & ('AcouAbove' %in% input$Prot_bx))|
           (grepl("AcouBelow" ,  filteredData$Prot) & ('AcouBelow' %in% input$Prot_bx))|
           (grepl("Fish" ,  filteredData$Prot) & ('Fish' %in% input$Prot_bx))|
           (grepl("EssFishHabs",  filteredData$Prot) & ('EssFishHabs' %in% input$Prot_bx))|
           (grepl("CultArcheo" ,  filteredData$Prot) & ('CultArcheo' %in% input$Prot_bx))|
           (grepl("CoastalHabs" ,  filteredData$Prot) & ('CoastalHabs' %in% input$Prot_bx))|
           (grepl("WetLands" , filteredData$Prot) & ('WetLands' %in% input$Prot_bx))|
           (grepl("RecTourism" ,  filteredData$Prot) & ('RecTourism' %in% input$Prot_bx))|
           (grepl("RecComFisheries" ,  filteredData$Prot) & ('RecComFisheries' %in% input$Prot_bx))|
           (grepl("VesselNav" ,  filteredData$Prot) & ('VesselNav' %in% input$Prot_bx))|
           (grepl("SocioEcon" ,  filteredData$Prot) & ('SocioEcon' %in% input$Prot_bx))|
           (grepl("EnviroJustice" , filteredData$Prot) & ('EnviroJustice' %in% input$Prot_bx))|
           (grepl("PubHealth" ,  filteredData$Prot) & ('PubHealth' %in% input$Prot_bx))|
           (grepl("Visual" ,  filteredData$Prot) & ('Visual' %in% input$Prot_bx))|
           (grepl("FAA-DOD" , filteredData$Prot) & ('FAA-DOD' %in% input$Prot_bx))|
           (grepl("Other", filteredData$Prot) & ('Other' %in% input$Prot_bx)))
    ))
    #returning the matching codes
    as.vector(filteredData$Code)
  })
  
  
  # Update the choices in the select Input based on the reactive expression
  observe({
    codeList <- read_excel("./ClassificationData.xlsx")
    unique_categories<-unique(codeList$Name)
    updateSelectInput(session, "category_select", choices = c("NEW",unique_categories))
  })
  
  
  # Render check box inputs and verbatim text outputs dynamically based on the list of items
  output$checkboxes <- renderUI({
    lapply(filteredList(), function(item) {
      tagList(
        
        checkboxInput(inputId = paste0("checkbox_", item), label = paste0(item, ": ", classData$tit[classData$Code == item]), value = FALSE),
        tableOutput(outputId = paste0("text_", item)),
        tags$hr(style = "border: none; height: 4px; background-color: black;")
        
      )
    })
  })
  
  # Update verbatim text outputs dynamically based on the checkboxes
  observe({
    lapply(filteredList(), function(item) {
      #checkbox_value <- input[[paste0("checkbox_", item)]]
      output[[paste0("text_", item)]] <- renderTable({getSingleRow(item, dfList)})
    })
  })
  
  observeEvent(input$enter_bt, { #ENTER BUTTON - What happens
    codeList <- read_excel("./ClassificationData.xlsx")
    
    for (item in filteredList()){
      codeList$Name[codeList$Code == item] <- ifelse(input[[paste0("checkbox_", item)]], ifelse(input$category_select == "NEW", input$new_category,input$category_select), NA)
      updateCheckboxInput(session, inputId = paste0("checkbox_", item), value = FALSE)
    }
    
    write.xlsx(codeList,"./ClassificationData.xlsx" )
    
  })
}

shinyApp(ui, server)
