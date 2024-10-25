library(shiny)
library(shinyFiles)
if (!require(gpttools.lilly))
{
  devtools::install_github("EliLillyCo/gpttools.lilly")
}
library(gpttools.lilly)
library(gt)
library(bslib)
library(readxl)
library(jsonlite)
library(plyr)
library(dplyr)
library(rvest)

# Define UI
ui <- fluidPage(
  
  # Custom scripts
  tags$script("
            Shiny.addCustomMessageHandler('txt', function (txt) {
                navigator.clipboard.writeText(txt);
            });
        "),
  
  # Application title
  titlePanel("Generative AI validation"),
  
  # Sidebar
  fluidRow(
    column(
      4,
      textInput(
        'api_base',
        'API Base',
        "https://openai-lrl-gsslearning-instance.openai.azure.com/"
      ),
      passwordInput('api_key',
                    'API Key',
                    "eb8d1c70ceaf463cab161c1153aef9be"),
      fileInput('input_data',
                'Input data',
                buttonLabel = 'Select ARDS', 
                accept = c('.csv', '.xlsx','.xls'),
                multiple = TRUE)
    ),
    column(
      7,
      textAreaInput(
        'chat_message',
        'Chat message',
        height = '12pc',
        width = '100%',
        resize = 'vertical',
        placeholder = 'Type your prompt here'
      )
    ),
    column(1,
           actionButton(
             'chat_btn',
             label = '',
             icon("chevron-right")
           ))
  )
  ,
  fluidRow(
    column(
    12,
    textAreaInput(
      'chat_response',
      'GAI Response: Edit and debug the returned code here',
      height = '25pc',
      width = '100%',
      resize = 'vertical',
      placeholder = 'No response yet'
    )
  )),
  fluidRow(
    column(
    3,
    actionButton(
      'run_btn',
      label = div('Run the R code!', icon("chevron-right")),
      height = '10pc',
      width = '100%'
    )
  ),
  column(
    3,
    actionButton(
      'copy_btn',
      label = div('Copy to clipboard', icon("chevron-right")),
      height = '10pc',
      width = '100%'
    )
  ),
  column(
    3,
    shinySaveButton("export", 
                    div('Export R script', icon("chevron-right")), 
                    "Save script as ...", 
                    filetype=list('R script'=".R"))
  ),
  column(
    3,
    actionButton(
      'run_compare',
      label = div('Compare with primary', icon("chevron-right")),
      height = '10pc',
      width = '100%'
    )
  )
  ),
  fluidRow(column(12,
                  gt_output('html_output'))),
  HTML(
    '<style>
#chat_response {background-color: rgb(252, 245, 229);
                font-family: Consolas;}
#html_output {min-height: 20pc;
              border: grey 1px solid;
              border-radius: 5px;
              margin-bottom: 20px;
              margin-top: 20px;}
#chat_btn {margin-top: 1.5pc;
           height: 12pc;}
#export {width: 100%;}
</style>'
  )
)


# Define server logic
server <- function(input, output, session) {
  
  api_key <- reactive(return(input$api_key))
  api_base <- reactive(return(input$api_base))
  
  load_data <- reactive({ 
    file_paths <- input$input_data$datapath
    data_list <- list() #initialise empty list  to store dataframes from uploaded files
    
    #begin looping through file paths, seq_along generates numbers from 1 to length of file path allowing loop to proess each filepath individually
    for (i in seq_along(file_paths)) {
      df_ <- if (grepl('csv$', file_paths[i])) {
        read.csv(file_paths[i])
      } else {
        read_excel(file_paths[i])
      }
   #   print(paste("Loaded data for file:", file_paths[i]))
      data_list[[i]] <- df_
    }
 #   print("Data list created")
    return(data_list)
  })
  
  
  prev_data_path <- reactiveVal()
  prev_data_path <- ''
  
  chats <- reactiveValues()
  
  observeEvent(input$chat_btn,
               {withProgress({
                 if (length(chats[['message']]) < 1) {
                   chats[['message']] <- list(
                     list(
                       'role' = "system",
                       'content' = 'You are a statistical analyst chatbot.
You will provide solutions in the R programming language, with no explanations.
Any text that isnt code should be commented with #.
                       You will always provide code using the {{gt}} package and generate a {{gt}} object.'
                     ),
                     list(
                       'role' = "user",
                       'content' = glue(input$chat_message)
                     )
                   )
                 } else{
                   chats[['message']] <- list(list(
                     'role' = "user",
                     'content' = glue(input$chat_message)
                   ))
                 }
                 
                 if (!is.null(input$input_data$datapath)) {
              #     print("Files uploaded:")
               #    print(input)
               #    print(input$input_data$datapath)
                   
                   curr_data_path <- input$input_data$datapath
if (!identical(curr_data_path, prev_data_path)) {
                     data_list <- load_data()
              #       print("Data list loaded")
                     
                     # df_ <- toJSON(load_data())
                     df_message <- 'Use following data for all queries: {data_list}.
                     Do not repeat this data, instead use the function "read.csv" or "read_excel" with the path variable of {input$input_data$datapath}
                     to load to a dataframe called df.'
                     if (any(grepl('system', chats[['message']]))) {
                       sys_n <- grep('system', chats[['message']])
                       chats[['message']][[sys_n]][['content']] <- 
                         glue(chats[['message']][[sys_n]][['content']], 
                              ' ',
                              df_message)
                     } else{
                       sys_n <- length(chats[['message']])+1
                       chats[['message']][[sys_n]] <- 
                         list(
                           'role' = "system",
                           'content' = glue(df_message)
                         )
                     }
                   }
                 }
                 
                 if (!is.null(chats[['chat_history']])) {
                   chats[['message']] <- c(chats[['chat_history']], chats[['message']])
                 }
                 # Ensure the message content is properly formatted
                 for (i in seq_along(chats[['message']])) {
                   if (!is.character(chats[['message']][[i]][['content']])) {
                     chats[['message']][[i]][['content']] <- as.character(chats[['message']][[i]][['content']])
                   }
                 }
                 
                 
                 # Sanitize input
                 sanitize_for_json <- function(text) {
                   return(jsonlite::toJSON(text, auto_unbox = TRUE))
                 }
                 
                 for (i in seq_along(chats[['message']])) {
                   chats[['message']][[i]][['content']] <- sanitize_for_json(chats[['message']][[i]][['content']])
                 }
                 
                 # Debugging
                # print("Formatted message content:")
                # print(chats[['message']])
                 
                 
                 chats[['openai_response']] <- query_openai_azure(
                   deployment_name = "gpt-4-80k",
                   body = list(messages = chats[['message']]),
                   authentication = "api",
                   api_key = api_key(),
                   base_url = api_base()
                 )
                 
                 if(!is.null(chats[['openai_response']][['error']])){
                   err_mess<-chats[['openai_response']][['error']][['message']]
                   showModal(modalDialog(
                     title = "Your message to the AI has failed with the error below",
                     err_mess,
                     easyClose = TRUE
                   ))
                 } else{
                   chats[['prev_message']] <- chats[['message']]
                   chats[['prev_response']] <-
                     list(as.list(chats[['openai_response']][['choices']][['message']]))
                   chats[['chat_history']] <-
                     c(chats[['prev_message']], chats[['prev_response']])
                   
                   
                   response_txt <- paste0(if (length(chats[['chat_history']]) > 3)
                   {
                     paste(
                       '# Previous Content',
                       '\n',
                       input$chat_response,
                       '\n\n#-----------------\n',
                       '# New Content',
                       '\n'
                     )
                   } else{
                     ''
                   },
                   chats[['prev_response']][[1]][['content']])
                   response_txt <- gsub('\\`\\`\\`(\\w|)', '', response_txt)
                   
                   
                   updateTextAreaInput(inputId = 'chat_response',
                                       value = paste(unlist(response_txt), collapse = '\n'))
                 }
               })
                 
               })
# gt_table_list generating reactive expression for Shiny app to automatically update outputs when input changes 
  gt_table_list <- reactive({
    withCallingHandlers({
      if(input$chat_response!=''){
     #   print("Evaluating chat response")
     #   print(input$chat_response)
        eval(parse(text = input$chat_response))
      }else{
        if(!is.null(input$input_data$datapath)){
          data_list <- load_data()
          # Generate gt tables for each data frame in the data list
          gt_tbl_list <-lapply(data_list, gt) #applies the gt function to each data frame and assigns the results to data_list
          return(gt_tbl_list)

          # df_<-load_data()
          # gt_tbl<-gt(df_)
        }else{
          # If no input data is provided, return a list with an empty gt table
          return(list(gt(data.frame())))
          
          }
      }
    }, 
    error = function(e){
      # If an error occurs, capture the traceback and display it in a modal dialog
      errors<-unlist(traceback())
      errors<-errors[(grep('runApp', errors)+1):length(errors)]
      errors <- paste0('Error:', e, '', 
                       'Traceback:', paste0(errors, collapse = ''))
      showModal(modalDialog(
        title = "An error occured! Traceback shown below",
        HTML(errors),
        easyClose = TRUE
      ))
    })
   # tbl <- ls()[grepl('gt_tbl', sapply(ls(), function(i) {
    #  class(get(i))
   # }))]
   # return(get(tbl))
  })
  
  #Defining UI output
  output$html_output <- renderUI({
    gt_tables <- gt_table_list()
    gt_output_list <- lapply(seq_along(gt_tables), function(i) {
      render_gt(gt_tables[[i]])
    }) # lapply to iterate over the list of gt tables calling render_gt for each table
    do.call(tagList, gt_output_list) # combines the list of rendered gt tables into a single UI using taglist function. 
  }) %>% bindEvent(input$ run_btn) #binding renderUI expression to the run button. 
  comp<-reactiveValues()
  
  observeEvent(input$run_compare, {
    showModal(modalDialog(
      fileInput('comp_file',
                'Select your primary comparator table',
                buttonLabel = 'Select Table',
                accept = '.html'),
      HTML('<b>Select your output folder for the comparison report</b><br>'),
      shinyDirButton('output_folder', 
                     'Select your output folder', 
                     'Please select a folder', 
                     FALSE),
      footer=tagList(
        actionButton('submit', 'Submit'),
        modalButton('cancel')
      ),
      easyClose = TRUE
    ))
    
    volumes = getVolumes()()
    observe({
      shinyDirChoose(input, 'output_folder', 
                     defaultPath = if(!is.null(input$input_data$datapath)){
                        input$input_data$datapath
                       }else{
                         ''
                       }, roots = volumes)
    })
    
    # only store the information if the user clicks submit
    observeEvent(input$submit, {
      removeModal()
      withProgress({
        comp[['file']] <- input$comp_file$datapath
        comp[['primary']] <- read_html(comp[['file']])  %>% html_table()
        comp[['ir']] <- xml2::read_html(as_raw_html(gt_table())) %>% html_table()
        
        chats[['message']]<- list( list(
          'role' = "system",
          'content' = glue('For this response only, provide comments.')
        ),list(
          'role' = "user",
          'content' = glue('Compare the data in these html tables: {comp[["primary"]]}, and {comp[["ir"]]}.
                           Return only differences in the data observations and variables.
                           Include # before every line.')
        )
        )
        
        chats[['openai_response']] <- query_openai_azure(
          deployment_name = "gpt-4-80k",
          body = list(messages = chats[['message']]),
          authentication = "api",
          api_key = api_key(),
          base_url = api_base()
        )
        
        chats[['prev_message']] <- chats[['message']]
        chats[['prev_response']] <-
          list(as.list(chats[['openai_response']][['choices']][['message']]))
        chats[['chat_history']] <-
          c(chats[['prev_message']], chats[['prev_response']])
        
        
        response_txt <- paste0(if (length(chats[['chat_history']]) > 0)
        {
          paste(
            '# Previous Content',
            '\n',
            input$chat_response,
            '\n\n#-----------------\n',
            '# New Content',
            '\n'
          )
        } else{
          ''
        },
        chats[['prev_response']][[1]][['content']])
        response_txt <- gsub('\\`\\`\\`(\\w|)', '', response_txt)
        
        
        updateTextAreaInput(inputId = 'chat_response',
                            value = paste(unlist(response_txt), collapse = '\n'))
      })
      })
  })
  
  observe({
      volumes = getVolumes()()
      shinyFileSave(input, "export", 
                    defaultPath = if(!is.null(input$input_data$datapath)){
                      input$input_data$datapath
                      }else{
                        ''
                        }, roots = volumes)
      fileinfo <- parseSavePath(volumes, input$export)
      if(nrow(fileinfo) > 0){
        write(input$chat_response, fileinfo$datapath)
      }
    })
  
}
# Run the application
shinyApp(ui = ui, server = server)
