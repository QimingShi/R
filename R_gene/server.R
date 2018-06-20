library(DT)
library(limma)
library(shiny)
library(reshape2)
library(data.table)

#Server#

server = function(input,output,session){
  
  data <- reactive({
    inFile <- input$csv
    if (is.null(inFile)) return(NULL)         
    data<-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  })
  
  observe({
    input_df <- data()
    out_df <- NULL
    datalist = list()
    vars<-input$gene_sequence
    vector_position <- NULL
    vars
    # get the data location which match the input string
    datalist <- grepl(vars, input_df[,3])
    out_df <- data()[datalist,]
    output$Data_selected <- renderDataTable(
      out_df,
      options = list(pageLength = 30)
    )
  })
  
  
  ##update the organs data
  observe({
    if(input$Click==0) return()
    isolate({
      updateCheckboxGroupInput(session, "organ", choices = c(unique(as.character(data()$Tissue[which(data()$Serotype==input$sero)]))))
    })
  })
  ##update the serotype data
  observe({
    if(is.null(data())){return()}
    dsnames<- as.character(data()[,4])
    cb_options<-list()
    cb_options[dsnames]<-dsnames
    updateCheckboxGroupInput(session,"sero",
                             label    = "serotype",
                             choices  = cb_options[],
    )
  }) 
  ##function for data table
  observe({
    out_df <- NULL
    datalist = list()
    if(is.null(data())){return()}
    vars<-input$organ
    
    for (j in vars){
      ##datalist is a data frame output
      datalist <- data()[data()[,1] == j,]
      ##get output dataframe together
      out_df <- rbind(out_df, datalist)
    }
    output$Data_selected <- renderDataTable(
      out_df,
      options = list(pageLength = 30),
    )
    
  })
  
  
  ##Create Venn Diagram and intersections
  output$VennResult <-renderPlot({
    inFile <- input$csv
    if(is.null(inFile)){return()}
    validate(
      need(input$organ != "", "Please select serotype(s) then press 'GO'.")
    )
    vars<-input$organ
    tissuetypes<-split(data()[,3], data()[,1])
    tissuetypes_1<-lapply(tissuetypes[names(tissuetypes)],function(x) unique(x))
    lapply(tissuetypes_1, function(x) length(x))
    select_organ_tissue<-tissuetypes_1[names(tissuetypes_1)%in%vars]
    universe<-as.character(unique(data()[,3]))
    Counts <- matrix(0, nrow=length(universe), ncol=length(vars))
    colnames(Counts)<-vars
    for (i in 1:length(universe)){
      for (j in vars){
        Counts[i,j]<- universe[i] %in% select_organ_tissue[[j]]
      }
    }
    Result=vennDiagram(vennCounts(Counts),circle.col=c("red","green","blue","black","purple"),mar=rep(0,4),cex=2,lwd=1.5)
    Result
    ##overlap will be bigger than 1 when sum rows
    intersect_counts<-rowSums(Counts, na.rm = TRUE)
    judge <-(intersect_counts>1)
    ##find the locationS of all vectors which meet the criteria
    vector_position <- which(!is.na(match(judge, TRUE)))
    ##Create the intersection
    recievinglist <- NULL
    # list_with_position<- NULL
    # vector_pos<- NULL
    
    for (i in vector_position){
      ##rbind -- row bind together, cbind -- column bind together
      # vector_pos <- rbind(vector_pos, i)
      recievinglist <- rbind(recievinglist, universe[i])
      # list_with_position <- cbind(vector_pos,recievinglist)
    }
    df_recieving <-NULL
    selected_recieving <-NULL
    recieve<-NULL
    recieving = list()
    ##find all sequence in original database
    for (i in 1:length(recievinglist)){
      recieving <- data()[data()[,3] == recievinglist[i],]
      df_recieving <- rbind(df_recieving, recieving)
    }
    ##selected all the sequence in previous step, then select the organ types based on those sequence
    for (i in vars){
      recieve <- df_recieving[df_recieving[,1]==i,]
      selected_recieving <- rbind(selected_recieving,recieve)
      ##change the data to character, since it will return factor(number of location) afterwords
      setDT(selected_recieving)[,Serotypes := as.character(Serotypes)]
      setDT(selected_recieving)[,ID := as.character(ID)]
      setDT(selected_recieving)[,Tissue := as.character(Tissue)]
    }
    
    output$unique_sequence<-renderDataTable(
      # if (selected_recieving != ''){return()},
      ##use aggregate functon to sum the dataset by one column, '.'represents whole dataset except sequence colunm, then set datatable column to the right order
      setcolorder(data.table(aggregate(.~ sequence+Serotypes, data = selected_recieving, paste, collapse = ",   ",na.action = na.omit)),c("Tissue", "ID", "sequence","Serotypes")),
      options = list(pageLength = 30)
    )
  })
  
  output$downloadData <-downloadHandler(
    filename = function() {
      
      paste("data-", Sys.Date(), ".csv", sep="")
      
    },
    
    content = function(file) {
      
      vars<-input$organ   #change as the tissues change
      
      tissuetypes<-split(data()[,3],data()[,1]) #split data according to tissue types
      
      tissuetypes_2<-lapply(tissuetypes[names(tissuetypes)],function(x) unique(x)) #find the unique tissuetypes
      
      wahh<-tissuetypes_2[names(tissuetypes_2)%in%vars] #select sequence
      
      combos<-Reduce(c,lapply(0:length(wahh), function(x) combn(1:length(wahh),x,simplify=FALSE)))
      
      combos1<-lapply(combos,`length<-`, max(lengths(combos)))
      
      combos_intersect<-lapply(combos1,function(x) Reduce(intersect,wahh[x]))
      
      combos_intersect_2<-combos_intersect[lapply(combos_intersect,length)>0]
      
      combos_intersect_3<-data()[which(data()[,3] %in% unlist(combos_intersect_2)),]
      
      write.csv(melt(combos_intersect_3), file)
      
    }) #............>>>find intersections
  
}

