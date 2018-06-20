library(DT)
library(limma)
library(shiny)
library(reshape2)
library(data.table)

#User Interface#

ui = navbarPage(
  
  title = strong('Profile AAV'), #add a title to the interface
  
  tabPanel(
    'Venn Diagram',      #this command line will create a tab panel titled 'Venn Diagram' (1st one from the left)
    
    sidebarPanel( 
      
      checkboxInput(inputId = 'header',label = 'header',value = TRUE),    #this command line will create a check box for user to select/deselect a header 
      
      fileInput('csv','Choose a CSV File',accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),   #create an import function to import a CSV file
      
      tags$hr(),  
      
      radioButtons(inputId='sep',label = 'Separator',choices = c(Comma=',',Semicolon=';',Tab='\t',Space=''),selected=','),   #enable user to select separators(user can pick from comma, semicolon, tab, space)
      
      checkboxGroupInput("sero", "Serotype", choices = NULL)    #display serotype options
      
      ,actionButton('Click','GO')                                  #make a clickable botton which displayed the message 'go'
      
      ,checkboxGroupInput('organ', 'Tissue types', choices = NULL)   #display tissue type options
      
      ,textInput("gene_sequence", "Gene Sequence", "")
      
      ,verbatimTextOutput("sequence_output")
      
      ,downloadButton('downloadData','Download')                #create a download function so user can download the new dataset.
      
      ,h6("*Download Intersections")                                  #display a 'Download Overlap' message 
      
    ),
    
    mainPanel(plotOutput("VennResult"))),                         #display a venn diagram
  
  tabPanel('Data Table', dataTableOutput('Data_selected'), width = 12),        #2nd tab panel displays the main dataset
  
  tabPanel('Overlapes',dataTableOutput("unique_sequence"))   #unique tab panel displays the overlape sequences
  
)
