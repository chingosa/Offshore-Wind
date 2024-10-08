#Libraries----------------------------------------------------------------------------------------------------------
library(shiny)
library(openxlsx)
#Paths and Functions------------------------------------------------------------------------------------------------
file_path <- "./ClassificationData.xlsx"

#Grabbing Classification Data
if (file.exists(file_path)) {
  codeList<- read_excel(file_path)#Grabbing the data from excel
  codeList_react <- reactiveVal(codeList)
  print("Data Successfully Retrieved")
}

#Retrieving the Original Codified Measures
ogMeasures <- "./OriginalMeasures.xlsx"
dfList <- list()
for (i in c(1:length(excel_sheets(ogMeasures)))){
  dfList[[i]] <- read_excel(ogMeasures, sheet = i)
}

#Function for extracting Single Rows from 
getSingleRow <- function(cL, row, df_list) {
  Code <- as.character(cL[row, 1])
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

#Table retrieval
getCode <- function(cL, row) {
  Code <- as.character(cL[row, 1])
  return(Code)
}              



# Define UI--------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Classification of Offshore Wind Line Items"),
  mainPanel(
    
    tableOutput("table1"), #Display of the Measure
    textInput("Tit_bx", "Title-Brief", ""), #Title input
    checkboxGroupInput('Phase_bx', 'Phase', choices = c('PreC'="PC", 'Con'="C", 'OM'='O', 'DeCom'='DC'), inline = T), #Checkbox input
    checkboxGroupInput('Loc_bx', 'Location', choices = c('Site', "Cable", 'OnShore'), inline = T),
    checkboxGroupInput('UpDown_bx', 'Above-Below', choices = c('AboveWater', "BelowWater", 'NA'), inline = T),
    checkboxGroupInput('MonMit_bx', 'Mon/Mit', choices = c('Monitoring'='Mon', 'Mitigation'='Mit'), inline = T),
    checkboxGroupInput('Prot_bx', 'Protected Resource', choices = c("MarineMams", "Turts", "Birds", "Bats", "Benthic", "AirQ", "WaterQ", "AcouAbove", "AcouBelow","Fish", "EssFishHabs", "CultArcheo", "CoastalHabs", "WetLands", "RecTourism", "RecComFisheries","VesselNav", "SocioEcon", "EnviroJustice", "PubHealth", "Visual", "FAA-DOD","Other"), inline = TRUE),
    checkboxGroupInput('Flag_bx', 'Flags', choices = c('Dev Operating Procedure'='DevOp', 'Additional Flag 1'='F1', 'Additional Flag 2'='F2'), inline = T),
    
    #Enter function
    actionButton("enter_bt", "Enter", class = "btn-success"),
    actionButton("skip_bt", "Skip"),
    
  )
)

# server.R-------------------------------------------------------------------------------------------------------

# Define server
server <- function(input, output, session) {
  # setting current row
  current_row <- reactiveVal(1)
  
  #Rendering the line item table
  output$table1 <- renderTable({
    getSingleRow(codeList, current_row(), dfList)
  }) 
  
  #When you enter data -> All this shhhhh happens
  observeEvent(input$enter_bt, {   #When You press this button what happens? update values - step forward, work toward the tagging system
    codeList<-codeList_react()
    
    #Inputting Data
    codeList$Seen[codeList$Code == getCode(codeList, current_row())]<-TRUE #denotes that this measure has seen
    
    codeList$tit[codeList$Code == getCode(codeList, current_row())]<-input$Tit_bx
    
    codeList$PreC[codeList$Code == getCode(codeList, current_row())]<-ifelse("PC" %in% input$Phase_bx, TRUE, FALSE)
    codeList$Con[codeList$Code == getCode(codeList, current_row())]<-ifelse("C" %in% input$Phase_bx, TRUE, FALSE)
    codeList$OM[codeList$Code == getCode(codeList, current_row())]<-ifelse("O" %in% input$Phase_bx, TRUE, FALSE)
    codeList$Dcom[codeList$Code == getCode(codeList, current_row())]<-ifelse("DC" %in% input$Phase_bx, TRUE, FALSE)
    
    codeList$Loc[codeList$Code == getCode(codeList, current_row())]<-paste(input$Loc_bx, collapse = ", ")
    
    codeList$AB[codeList$Code == getCode(codeList, current_row())]<-paste(input$UpDown_bx, collapse = ", ")
    
    codeList$Mon[codeList$Code == getCode(codeList, current_row())]<-ifelse("Mon" %in% input$MonMit_bx, TRUE, FALSE)
    codeList$Mit[codeList$Code == getCode(codeList, current_row())]<-ifelse("Mit" %in% input$MonMit_bx, TRUE, FALSE)
    
    codeList$Prot[codeList$Code == getCode(codeList, current_row())]<-paste(input$Prot_bx, collapse = ", ")
    
    codeList$flag1[codeList$Code == getCode(codeList, current_row())]<-ifelse("F1" %in% input$Flag_bx, TRUE, FALSE)
    codeList$flag2[codeList$Code == getCode(codeList, current_row())]<-ifelse("F2" %in% input$Flag_bx, TRUE, FALSE)
    codeList$DevOp[codeList$Code == getCode(codeList, current_row())]<-ifelse("DevOp" %in% input$Flag_bx, TRUE, FALSE)
    
    
    #Saving Data to ./ClassfiedData.xlsx
    codeList_react(codeList)
    write.xlsx(codeList_react(), file_path)
    print('answers saved')
    
    #Progresses current Row By one
    current_row(current_row() + 1)
    #Reset row index if all rows have been cycled
    if (current_row() > nrow(codeList)) {
      current_row(1)
    }
    
    #If the current row has already been seen it skips it
    while (codeList[current_row(),2] == TRUE){
      current_row(current_row() + 1)
      # Reset row index if all rows have been cycled
      if (current_row() > nrow(codeList)) {
        current_row(1)
      }
    }
    
    #Clearing Inputs
    updateCheckboxGroupInput(session, "MonMit_bx", selected = character(0))
    updateCheckboxGroupInput(session, "Phase_bx", selected = character(0))
    updateCheckboxGroupInput(session, "Prot_bx", selected = character(0))
    updateCheckboxGroupInput(session, "Flag_bx", selected = character(0))
    updateCheckboxGroupInput(session, "Loc_bx", selected = character(0))
    updateCheckboxGroupInput(session, "UpDown_bx", selected = character(0))
    updateTextInput(session, "Tit_bx", value = "")
  })
  observeEvent(input$skip_bt, {
    current_row(current_row() + 1)
    if (current_row() > nrow(codeList)) {
      current_row(1)
    }
    
    #If the current row has alread been seen it skips it
    while (codeList[current_row(),2] == TRUE){
      current_row(current_row() + 1)
      # Reset row index if all rows have been cycled
      if (current_row() > nrow(codeList)) {
        current_row(1)
      }
    }
    
  })
  
}
#Runs app
shinyApp(ui = ui, server = server)

#Run after
if (file.exists(file_path)) {
  codeList<- read_excel(file_path)#Grabbing the data from excel
  codeList_react <- reactiveVal(codeList)
  print("Data Successfully Retrieved")
}
