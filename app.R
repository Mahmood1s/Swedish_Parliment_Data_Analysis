
devtools::install_github("https://github.com/Mahmood1s/lab5" , upgrade = "always",force = TRUE)
library(lab5)


ui <- fluidPage(
    
    
    pageWithSidebar(
        
        headerPanel("Swedish Parliment Member Details"),
        
        sidebarPanel(
            selectInput("report_type", "Select Report", choices=c("Please Select","Member Appointments","Members by County","Gender Participation")),
            conditionalPanel(condition = "input.report_type=='Member Appointments'",
                             uiOutput("Report_Controls")),
            conditionalPanel(condition = "input.report_type=='Members by County'",
                             uiOutput("Report_Controls1")),
            conditionalPanel(condition = "input.report_type=='Gender Participation'",
                             uiOutput("Report_Controls2"))
            
        ),
        
        mainPanel(
            conditionalPanel(condition = "input.report_type != 'Gender Participation'",
                             tableOutput("ReportTable")),
            conditionalPanel(condition = "input.report_type == 'Gender Participation'",
                             plotOutput("plotchart"))
        )
    )
)


server <- function(input, output,session) {
   
    checkvar <- 0
    my_df <<- get_mem("")
    
    observeEvent(input$counties,{
        
        temp <- which(my_df[,3]==input$counties)
        mem <- my_df[temp,2]
        updateSelectInput(session,inputId = "member_name1",choices =mem )
        
    })
    
    output$Report_Controls1 <- renderUI({
        
        if(checkvar==1)
        {
            temp <- which(my_df[,3]==input$counties)
            mem <- my_df[temp,2]
        }
        else
        {
            mem <- my_df[,2]
        }
        mem <- my_df[,2]
        county1 <- unique(my_df[,3])
        names(county1[1]) <- "county"
        
        tagList(         selectInput("counties", "Select County", choices=county1),
                         selectInput("member_name1", "Select Member From County", choices=mem)
        )
    })
    
    output$Report_Controls <- renderUI({
        
        checkvar<<-1
        query_data <- my_df
        mem <- query_data[2]
        
        selectInput("member_name", "Select Member Name", choices=mem)
        
    })
    
    output$Report_Controls2 <- renderUI({
        
        county2 <- unique(my_df[,3])
        county2 <- append("ALL",county2)
        names(county2[1]) <- "county"
        selectInput("counties1", "Select County", choices=county2)
    })
    
    output$ReportTable <- renderTable({
        
        #mem_name <- input$member_name
        # print(mem_name)
        #  print(my_df)
        #  first_name <- strsplit(mem_name," ")[[1]]
        #  second_name <- strsplit(mem_name," ")[[1]]
        #  print(first_name[[1]])
        #  print(second_name[[3]])
        if(input$report_type=="Member Appointments")
        {
            idex <- which(my_df[,2]== input$member_name)
            id <- my_df[idex,1]
            query_data <- get_memdata(id,"")
            data_table <- query_data
            data_table
        }
        else if(input$report_type=="Members by County")
        {    
            idex <- which(my_df[,2]== input$member_name1)
            id <- my_df[idex,1]
            query_data <- get_memdata(id,"")
            data_table <- query_data
            data_table
        }
        
    })
    
    output$plotchart <- renderPlot({
        if(is.null(input$counties1) | input$counties1 == "ALL")
            counties =""
        else
            counties = input$counties1
        dataframe <- get_genderdata(counties)
        tab <- table(dataframe$valkrets,dataframe$kon)
        
        if(input$counties1=="ALL")
            barplot(tab[,1],main = "Gender Distribution",col = c("darkblue","red"),legend = colnames(tab))
        else
            barplot(tab[1,],main = "Gender Distribution",col = c("darkblue","red"),legend = colnames(tab),xlab = input$counties1)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
