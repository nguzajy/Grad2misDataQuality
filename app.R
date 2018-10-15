#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
require(plyr)
library(dplyr)
library(shiny)
library(RCurl)
require(RPostgreSQL)
require(reshape2)
require(tables)
library(xlsx)
library(rsconnect)
library(shinydashboard)
require(plyr)
library(dplyr)
library(shiny)
library(RCurl)
require(RPostgreSQL)
require(reshape2)
library(rpivotTable)
require(tables)
library(DT)
library(xlsx)
library(rsconnect)
library(xml2)
library(rvest)
library(tidyr)

source("sitesettings.R")

options(knitr.table.format="html")
options(scipen=999) #avoids printing exponential notations such 1e+10

options(warn = -1) #suppresses warnings

year <- c(format(Sys.Date(), "%Y"))

dashTitle <- paste0("GRAD2MIS DQ")
ui <- dashboardPage(
  dashboardHeader(title = dashTitle),
  ## Sidebar content
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Data Quality: Duplicates", tabName = "vsdashboard", icon = icon("dashboard")
    )
  )),
  ## Body content
  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "vsdashboard",
            fluidPage(
              titlePanel("List of Duplicate Beneficiary Primary members in each VESA"),
              
              verbatimTextOutput("duplicateyear_filtered_row"),
              downloadButton(outputId = "download_duplicateyear_filtered",
                             label = "Download Data"),
              fluidRow(DT::dataTableOutput('duplicate_dty')),hr(),
              titlePanel("Beneficiary numbers appearing in more than one VESA"),
              
              verbatimTextOutput("duplicateqtr_filtered_row"),
              downloadButton(outputId = "download_duplicateqtr_filtered",
                             label = "Download Data"),
              fluidRow(DT::dataTableOutput('duplicate_dtq'))
            ))
    
  ))
)


server <- function(input, output, session) {
  
  year3 <- "2018"
  
  
  #Yearly DATA
  duplicateDataYear <- reactive({
    
    #Gets a list of of VESA, PSNP and duplicate amount for the year
    
    duplicateAmount <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/oQ1yhF36aLD/data.csv"), userpwd=userpass, httpauth = 1L)
    
    duplicateAmount <- read.table(text = duplicateAmount, sep =",", header = TRUE, stringsAsFactors = FALSE)
    
    dup_idx <- duplicated(duplicateAmount)
    duplicateAmount <- duplicateAmount[dup_idx, ]
    rownames(duplicateAmount) <- c()
    #n_occur <- data.frame(table(duplicateAmount$psnp_number))
    #n_occur[n_occur$Freq > 1,]
    #duplicateAmount <- duplicateAmount[duplicateAmount$psnp_number %in% n_occur$Var1[n_occur$Freq > 1],]
    #Summarize and aggregate the duplicates data for the year
    
    return(duplicateAmount)})
  
  
  #Quarterly Data
  duplicateDataQuarter <- reactive({
    
    #Gets a list of of VESA, PSNP and duplicate amount for the year
    
    duplicateAmount <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/oQ1yhF36aLD/data.csv"), userpwd=userpass, httpauth = 1L)
    
    duplicateAmount <- read.table(text = duplicateAmount, sep =",", header = TRUE, stringsAsFactors = FALSE)
    
    
    n_occur <- data.frame(table(duplicateAmount$psnp_number))
    n_occur[n_occur$Freq > 1,]
    duplicateAmount <- duplicateAmount[duplicateAmount$psnp_number %in% n_occur$Var1[n_occur$Freq > 1],]
    duplicateAmount <- duplicateAmount[!(duplicated(duplicateAmount) | duplicated(duplicateAmount, fromLast = TRUE)), ]
    rownames(duplicateAmount) <- c()
    
    #duplicateAmount <- duplicateAmount[!duplicated(duplicateAmount), ]
    #dup_idx <- duplicated(duplicateAmount)
    #duplicateAmount <- duplicateAmount[dup_idx, ]
    #Summarize and aggregate the duplicates data for the year
    return(duplicateAmount)})
  
  
  # access the value of the widget with input$date, e.g.
  output$value <- renderPrint({ input$date_from })
  
  output$duplicate_dty = DT::renderDataTable(
    duplicateDataYear(),
    filter = 'top',
    options = list(scrollX = TRUE)
  )
  
  output$duplicate_dtq = DT::renderDataTable(
    duplicateDataQuarter(),
    filter = 'top',
    options = list(scrollX = TRUE)
  )
  
  output$duplicateyear_filtered_row <- 
    renderText({
      paste0(length(input$duplicate_dty_rows_all)," Duplicate Beneficiary Primary Members")})
  
  output$duplicateqtr_filtered_row <- 
    renderText({
      paste0(length(input$duplicate_dtq_rows_all)/2," Beneficiary numbers appearing in more than one VESA")})
  
  
  output$download_duplicateyear_filtered <- 
    downloadHandler(
      filename = "Vesa duplicate This Year.xlsx",
      content = function(file){
        write.xlsx(duplicateDataYear()[input$duplicate_dty_rows_all, ], file, sheetName = "VESA duplicate", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  output$download_duplicateqtr_filtered <- 
    downloadHandler(
      filename = "Vesa duplicate Quarterly.xlsx",
      content = function(file){
        write.xlsx(duplicateDataQuarter()[input$duplicate_dtq_rows_all, ], file, sheetName = "VESA duplicate", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
}
# Run the application 
shinyApp(ui = ui, server = server)

