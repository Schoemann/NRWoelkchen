# wordclouds for shiny

# increase upload file size
options(shiny.maxRequestSize = 100*1024^2)
#.libPaths("c:/r/packages")
# phantomJS----
# in order to download i need to add phantom js to PATH
# path_to_phantomjs_exe = "phantom/phantomjs-2.1.1-windows/bin"
# if (is.null(grep("phantomjs", Sys.getenv("PATH"))) > 0) {
#   message("phantomjs detected")
# } else {
#   paths = Sys.getenv("PATH")
#   paths = paste0(paths,";",
#                  path_to_phantomjs_exe)
#   Sys.setenv("PATH" = paths)
#   message("phantomjs added")
# }

#libraries ----
library("tm")
library("SnowballC")
library("dplyr")
library("shiny")
library("shinythemes")
library("wordcloud2")
library("shinydashboard")
library("shinyWidgets")
library("tools")

# setting seed
set.seed(42)

# Wordcloud page ----
wordcloud_page = tabPanel(
  "Wortwolken",
  fluidRow(
    column(
      3,
      #File Upload
      wellPanel(
        shiny::fileInput(
          inputId = "file_upload_wc", 
          label = "Datei hochladen",
          buttonLabel = "Durchsuchen",
          placeholder = "..."
          ),
      dropMenu(
        actionButton(
          "go", 
          "Optionen"
          ),
        tags$h3("Optionen"),
        selectInput(
          "language_wc",
          "Dokumentensprache",
          choices = list(
            "deutsch" = "german", 
            "englisch" = "english", 
            "-------"  = c(
              "danish",
              "dutch", 
              "finnish", 
              "french", 
              "hungarian",
              "italian", 
              "norwegian", 
              "portuguese", 
              "russian", 
              "spanish", 
              "swedish"
            )
          ),
          selected = "german"
        ),
        shinyWidgets::awesomeCheckbox(
          "stemming_wc",
          label = "Stammformreduktion",
          value = FALSE,
          status = "info"
          )
        ),
      downloadButton(
        "downloadButton_wc",
        "Download Wordcloud"
        )
      ) # closing well panel
  ), # closing sidebar
  # main panel
  column(
    9,
    tabsetPanel(
      #einzelne optionen
      tabPanel(
        "Wortanzahl",
        sliderInput(
          inputId = "select_number_wc", 
          label = "Anzahl der Wörter (in %)",
          min = 0,
          max = 100,
          value = 100,
          step = 5,
          post = "%",
          animate = animationOptions(
            interval = 5000, 
            loop = TRUE
          )
        ),
        textOutput(
          "word_plotted_wc"
        )
      ),
      tabPanel(
        "Schriftgröße", 
        sliderInput(
          inputId = "select_font_size_wc",
          label = "Schriftgröße",
          min = 0.1,
          max = 3,
          value = 1,
          step = 0.05,
          animate = animationOptions(
            interval = 5000, 
            loop = TRUE
            )
          )
        ),
      tabPanel(
        "Schriftart", 
        shiny::selectInput(
          inputId = "select_font_wc",
          label = "Schriftart",
          choices = list(
            "Arial" = "Segoe UI",
            "Times New Roman" = "serif",
            "Courier" = "mono"
          ),
          selected = "Segoe UI"
        )
      ),
      tabPanel(
        "Schriftfarbe", 
        shiny::selectInput(
          inputId = "select_font_color_wc", 
          label = "Schriftfarbe",
          multiple = T,
          choices = list(
            "einfarbig" = c(
              "Schwarz" = "#000000",
              "Grau" = "#ACACAC",
              "Grün" = "#009036",
              "Rot" = "#e2001a",
              "Weiß" = "#FFFFFF"),
            "NRW Design" = c(
              "Nachtblau" = "#003064",
              "Sonnenblau" = "#009ee0",
              "Wiesengrün" = "#b1c800",
              "Sonnengelb" = "#f29400",
              "Blutorange" = "#e75112"),
            "bunt" = c(
              "hell" = "random-light",
              "dunkel" = "random-dark"
            )
          ),
          selected = "#000000"
        )
      ),
      tabPanel(
        "Hintergrund", 
        shiny::selectInput(
          inputId = "select_background_wc",
          label = "Hintergrundfarbe",
          choices = list(
            "einfarbig" = c(
              "Schwarz" = "#000000",
              "Grau" = "#ACACAC",
              "Grün" = "#009036",
              "Rot" = "#e2001a",
              "Weiß" = "#FFFFFF"),
            "NRW Design" = c(
              "Nachtblau" = "#003064",
              "Sonnenblau" = "#009ee0",
              "Wiesengrün" = "#b1c800",
              "Sonnengelb" = "#f29400",
              "Blutorange" = "#e75112"
            )
          ),
          selected = "#FFFFFF"
        )
      ),
      tabPanel(
        "Form", 
        shiny::selectInput(
          inputId = "select_shape_wc",
          label = "Form der Wortwolke",
          choices = list(
            "Kreis" = "circle",
            "Herz" = "cardioid",
            "Diamant" = "diamond",
            "Karo" = "triangle-forward",
            "Dreieck" = "triangle",
            "Pentagon" = "pentagon",
            "Stern" = "star"
          ),
          selected = "circle"
        )
      ),
      tabPanel(
        "Leinwandgröße",
        shiny::selectInput(
          inputId = "plot_size_wc",
          label = "Größe Leinwand",
          choices = list(
            "klein (400x400)" = "s",
            "mittel (640x640)" = "sm",
            "normal (800x800)" = "m",
            "groß (960x960)" = "ml",
            "größer (1200x1200)" = "l",
            "sehr groß (1920x1920)" = "xl"
          ),
          selected = "sm"
        )
      )
    ) # closing tabset panel
  ) # closing column of main panel
), #closing first fluid row
br(),
fluidRow(
  # Show Wordcloud
  column(
    width = 12,
    align = "center",
    uiOutput(
      "ui_plot_wc"
    )
  )
), # closing second fluid row
#document analysis summary
# fluidRow(
#   column(4,
#          shinydashboard::infoBox(
#            "Wörter im Dokument", 
#            uiOutput("words_total_wc"), 
#            subtitle = "Anzahl Wörter", 
#            icon = icon("book-open",lib = "font-awesome")
#          )
#   ),
#   column(4,
#          shinydashboard::infoBox(
#            title = "Top", 
#            value = uiOutput("words_top_wc"),
#            subtitle = "Häufigstes Wort",
#            icon = icon("trophy",lib = "font-awesome"),
#            color = "purple"
#          )
#   ),
#   column(
#     width = 4,
#     shinydashboard::infoBox(
#       "mittlere Wortlänge", 
#       uiOutput("words_mean_wc"), 
#       subtitle = "Buchstaben pro Wort", 
#       icon = icon("poll",lib = "font-awesome")
#     )
#   )
# ), # closing third fluid row (boxes)
# Table of words
fluidRow(
  column(12,
         DT::DTOutput("table_of_words_wc")
         )
  )
)

# Lettercloud page ----
lettercloud_page = tabPanel(
  #"Textwolken",
  title = HTML('Textwolken <span class="label label-warning">beta</span>'),
  #icon = icon("beta"),
  fluidRow(
    column(
      3,
      wellPanel(
        #File Upload
        shiny::fileInput(
          inputId = "file_upload_lc", 
          label = "Datei hochladen",
          buttonLabel = "Durchsuchen",
          placeholder = "..."
        ),
        shinyWidgets::dropMenu(
          actionButton(
            "go_lc", 
            "Optionen"
            ),
          tags$h3("Optionen"),
          selectInput(
          "language_lc",
          "Dokumentensprache",
          choices = list(
            "deutsch" = "german", 
            "englisch" = "english", 
             "_____" = c(
              "danish",
              "dutch", 
              "finnish", 
              "french", 
              "hungarian",
              "italian", 
              "norwegian", 
              "portuguese", 
              "russian", 
              "spanish", 
              "swedish"
            )
          )
        ),
        shinyWidgets::awesomeCheckbox(
          inputId = "stemming_lc",
          label = "Stammformreduktion",
          value = FALSE,
          status = "info"
          )
        )
      )
    ),
    column(
      9,
      mainPanel(
        tabsetPanel(
          #einzelne optionen
          tabPanel(
            "Wortanzahl",
            shiny::sliderInput(
              inputId = "select_number_lc", 
              label = "Anzahl der Wörter (in %)",
              min = 0,
              max = 100,
              value = 100,
              step = 5,
              post = "%",
              animate = animationOptions(
                interval = 5000, 
                loop = TRUE
              )
            ),
            textOutput("word_plotted_lc")
          ),
          tabPanel(
            "Wort",
            shinyWidgets::searchInput(
              inputId = "word_lc",
              label = "Wort eingeben",
              placeholder = "Ihr Text",
              btnSearch = icon("check-square"),
              btnReset = icon("trash"),
              value = "42"
            )
          ),
          tabPanel(
            "Schriftfarbe", 
            shiny::selectInput(
              inputId = "select_font_color_lc", 
              label = "Schriftfarbe",
              multiple = TRUE,
              choices = list(
                "einfarbig" = c(
                  "Schwarz" = "#000000",
                  "Grau" = "#ACACAC",
                  "Grün" = "#009036",
                  "Rot" = "#e2001a",
                  "Weiß" = "#FFFFFF"),
                "NRW Design" = c(
                  "Nachtblau" = "#003064",
                  "Sonnenblau" = "#009ee0",
                  "Wiesengrün" = "#b1c800",
                  "Sonnengelb" = "#f29400",
                  "Blutorange" = "#e75112"),
                "bunt" = c(
                  "hell" = "random-light",
                  "dunkel" = "random-dark"
                )
              ),
              selected = "#000000"
            )
          ),
          tabPanel(
            "Hintergrund", 
            shiny::selectInput(inputId = "select_background_lc",
                               label = "Hintergrundfarbe",
                               choices = list(
                                 "einfarbig" = c(
                                   "Schwarz" = "#000000",
                                   "Grau" = "#ACACAC",
                                   "Grün" = "#009036",
                                   "Rot" = "#e2001a",
                                   "Weiß" = "#FFFFFF"),
                                 "NRW Design" = c(
                                   "Nachtblau" = "#003064",
                                   "Sonnenblau" = "#009ee0",
                                   "Wiesengrün" = "#b1c800",
                                   "Sonnengelb" = "#f29400",
                                   "Blutorange" = "#e75112"
                                 )
                               ),
                               selected = "#FFFFFF"
                               )
            ),
          tabPanel(
            "Leinwandgröße",
            shiny::selectInput(
              inputId = "plot_size_lc",
              label = "Leinwandgröße",
              choices = list(
                "automatisch" = "auto",
                "klein (400x400)" = "s",
                "mittel (640x640)" = "sm",
                "normal (800x800)" = "m",
                "groß (960x960)" = "ml",
                "größer (1200x1200)" = "l",
                "sehr groß (1920x1920)" = "xl"
              ),
              selected = "s"
            )
          )
          )
        )
      )
    ),
  br(),
  fluidRow(
    # Show Wordcloud
    column(width = 12,
           align = "center",
           uiOutput("ui_plot_lc")
    )
  ),
  #document analysis summary ---
  fluidRow(
    column(4,
           shinydashboard::infoBox(
             "Wörter im Dokument", 
             uiOutput("words_total_lc"), 
             subtitle = "Anzahl Wörter", 
             icon = icon("book-open",lib = "font-awesome")
           )
    ),
    column(4,
           shinydashboard::infoBox(
             title = "Top", 
             value = uiOutput("words_top_lc"),
             subtitle = "Häufigstes Wort",
             icon = icon("trophy",lib = "font-awesome"),
             color = "purple"
           )
    ),
    column(
      width = 4,
      shinydashboard::infoBox(
        "mittlere Wortlänge", 
        uiOutput("words_mean_lc"), 
        subtitle = "Buchstaben pro Wort", 
        icon = icon("poll",lib = "font-awesome")
      )
    )
  ), # closing document analysis summary
  # Table of words
  fluidRow(
    column(
      12,
      DT::DTOutput("table_of_words_lc")
    )
  ) # closing table of words
)

# Picturecloud Page ----
picturecloud_page = tabPanel(
  "Bildwolken",
  fluidRow(
    column(3,
           wellPanel(
             #File Upload
             shiny::fileInput(
               inputId = "file_upload_pc", 
               label = "Datei hochladen",
               buttonLabel = "Durchsuchen",
               placeholder = "..."
             ),
             shiny::fileInput(
               inputId = "file_upload_shape_pc", 
               label = "Maske hochladen",
               buttonLabel = "Durchsuchen",
               placeholder = "...",
               accept = c('image/png')
             ),
             dropMenu(
               actionButton(
                 "go_pc", 
                 "Optionen"
               ),
               tags$h3("Optionen"),
               selectInput(
                 "language_pc",
                 "Dokumentensprache",
                 choices = list(
                 "deutsch" = "german", 
                 "englisch" = "english", 
                 "___"  = c(
                   "danish",
                   "dutch", 
                   "finnish", 
                   "french", 
                   "hungarian",
                   "italian", 
                   "norwegian", 
                   "portuguese", 
                   "russian", 
                   "spanish", 
                   "swedish"
                 )
               )
             ),
             shinyWidgets::awesomeCheckbox(
               "stemming_pc",
               label = "Stammformreduktion",
               value = FALSE,
               status = "info"
               )
             )
           ) # closing well panel
    ),
    column(
      9,
      mainPanel(
        tabsetPanel(
          #einzelne optionen
          tabPanel(
            "Wortanzahl",
            shiny::sliderInput(
              inputId = "select_number_pc", 
              label = "Anzahl der Wörter (in %)",
              min = 0,
              max = 100,
              value = 100,
              step = 5,
              post = "%",
              animate = animationOptions(
                interval = 5000, 
                loop = TRUE
              )
            ),
            textOutput("word_plotted_pc")
          ),
          tabPanel(
            "Schriftfarbe", 
            shiny::selectInput(
              inputId = "select_font_color_pc", 
              label = "Schriftfarbe",
              choices = list(
                "einfarbig" = c(
                  "Schwarz" = "#000000",
                  "Grau" = "#ACACAC",
                  "Grün" = "#009036",
                  "Rot" = "#e2001a",
                  "Weiß" = "#FFFFFF"),
                "NRW Design" = c(
                  "Nachtblau" = "#003064",
                  "Sonnenblau" = "#009ee0",
                  "Wiesengrün" = "#b1c800",
                  "Sonnengelb" = "#f29400",
                  "Blutorange" = "#e75112"),
                "bunt" = c(
                  "hell" = "random-light",
                  "dunkel" = "random-dark"
                )
              )
            )
          ),
          tabPanel(
            "Leinwandgröße",
            shiny::selectInput(
              inputId = "plot_size_pc",
              label = "Größe Leinwand",
              choices = list(
                "klein (400x400)" = "s",
                "mittel (640x640)" = "sm",
                "normal (800x800)" = "m",
                "groß (960x960)" = "ml",
                "größer (1200x1200)" = "l",
                "sehr groß (1920x1920)" = "xl"
              ),
              selected = "s"
            )
          )
        )
      )
    )
  ),
  br(),
  fluidRow(
    # Show Wordcloud
    column(
      12,
      align = "center",
      uiOutput("ui_plot_pc")
    )
  ),
  fluidRow(
    column(4,
           shinydashboard::infoBox(
             "Wörter im Dokument", 
             uiOutput("words_total_pc"), 
             subtitle = "Anzahl Wörter", 
             icon = icon("book-open",lib = "font-awesome")
           )
    ),
    column(4,
           shinydashboard::infoBox(
             title = "Top", 
             value = uiOutput("words_top_pc"),
             subtitle = "Häufigstes Wort",
             icon = icon("trophy",lib = "font-awesome"),
             color = "purple"
           )
    ),
    column(
      width = 4,
      shinydashboard::infoBox(
        "mittlere Wortlänge", 
        uiOutput("words_mean_pc"), 
        subtitle = "Buchstaben pro Wort", 
        icon = icon("poll",lib = "font-awesome")
      )
    )
  ),
  # Table of words
  fluidRow(
    DT::dataTableOutput("table_of_words_pc")
  )
  # )
)
# how to page ----
howto_page = tabPanel("Anleitung",
                      htmlOutput("readme")                      
)
welcome_page = tabPanel("Willkommen",
                      htmlOutput("welcome")                      
)

# User Interface ----
ui <- navbarPage(
  title = "NRWölkchen",
  id = "someID",
  welcome_page,
  # Panel: Wordcloud
  wordcloud_page,
  # Panel: Lettercloud
  lettercloud_page,
  # Panel: Picturecloud
  picturecloud_page,
  howto_page,
  windowTitle = "NRWölkchen - Wortwolken",
  theme = shinytheme("readable"),
  #footer = "some footer",
  #inverse = TRUE,
  collapsible = TRUE,
  selected = "Willkommen",
  shinyWidgets::useShinydashboard(),
  shinyWidgets::useSweetAlert(
    theme = "dark"
  )
)

# Server ----

server = function(input, output, session) {
# function to read documents ----
  word_count = function(link,stem = FALSE, stop_language = "ger") {
    # integrate file extension detection
    file_extension = tools::file_ext(link)
    valid_file_ext = c("pdf", "txt", "html", "rtf", "docx", "doc")
    if (!file_extension %in% valid_file_ext) {
      stop(paste("invalid file extension. Must be one of", paste0(valid_file_ext,sep = "",collapse = ", ")))
    }
    # switch depending upon file_extension until stemming
    word_text = switch(file_extension,
                       pdf = {
                         textreadr::read_document(link)
                       },
                       {
                         textreadr::read_document(link,encoding = "UTF-8")
                       }
    )
    word_text = word_text %>%  #returns vector of length of pages!
      strsplit(.,"\n") %>%                  #split vector by row breaks
      unlist() %>%                          #coerce to string vectors
      strsplit(.,"\r") %>%                  #split vector by breaks
      trimws(.) %>%                         #remove whitespaces at beginning or end
      paste0(.,collapse = " ") %>%          #make one large character vector
      gsub("  "," ",.) %>%                  #remove double spaces
      gsub('„',"",.) %>%                    #remove quotation marks
      gsub("“","",.)                        #dito
    
    if (stem == T) { # doesnt work well with german data
      word_text = stemDocument(word_text,language = stop_language)
    }
    
    
    df = data.frame(doc_id = seq(1,length(word_text)),
                    text = word_text, 
                    stringsAsFactors = FALSE)
    
    
    df_corpus = Corpus(DataframeSource(df))
    
    docs = df_corpus %>% 
      tm_map(.,removeWords,stopwords(stop_language)) %>% #removing stopwords
      tm_map(.,removePunctuation)                #removing punctuation
    
    
    df = TermDocumentMatrix(docs,                #create Matrix of Word counts
                            control = list(tolower = FALSE)) %>% 
      as.matrix(.) %>%                           
      as.data.frame(.) %>% 
      mutate(freq = `1`,
             word = row.names(.)) %>% 
      select(word,freq)
    
    
    df$word = gsub("“","",df$word)
    
    #stopwords with capital at the beginning
    stopwords2 = lapply(1:length(stopwords(stop_language)),FUN = function(x) {
      paste0(toupper(substr(stopwords(stop_language)[x],1,1)),substr(stopwords("ger")[x],2,nchar(stopwords("ger")[x])))
    }) %>% 
      unlist(.)
    
    #build flag vector if a stopword matches the word in matrix
    flag = unlist(lapply(1:nrow(df),
                         FUN = function(x) {
                           df$word[x] %in% c(stopwords(stop_language),"dass",
                                             stopwords2,"Dass")
                         }
    )
    ) 
    
    flag[grepl("–",as.character(df$word))] = TRUE # removing hyphen
    
    
    df = df %>% 
      filter(!flag) %>%                              #removing stopwords
      filter(nchar(word) > 3 | substr(word,1,1) %in% LETTERS) %>%                    #keeping words with more than 2 letters
      group_by(word) %>%                             #summarizing same words and...
      summarize(freq = sum(freq)) %>%                #counting them..
      ungroup(.) %>% 
      select(word,freq) %>%                       # keeping word, frequency and percentage
      arrange(desc(freq)) %>% 
      mutate(word2 = word) %>% 
      tibble::column_to_rownames(.,var = "word2") %>% 
      select(word,freq)
    #return df 
    df
  }
  getPage = function() {
    return(
      includeHTML("nrw_woelkchen.html")
    )
  }
  output$readme = renderUI({
    getPage()
  })
  getPage2 = function() {
    return(
      includeHTML("welcome_woelkchen.html")
    )
  }
  output$welcome = renderUI({
    getPage2()
  })
  #server Worcloud ----
  df_uploaded_wc = reactive({
    req(input$file_upload_wc)
    progressSweetAlert(
      session = session,
      id = "progress_upload_wc",
      display_pct = TRUE, 
      value = 0,
      total = 100,
      striped = T,
      title = 'Lese Datei und zähle Wörter\n☕'
    )
    file_name = input$file_upload_wc$datapath
    updateProgressBar(
      session = session,
      id = "progress_upload_wc",
      value = 50
    )
    txt = word_count(
      link = file_name,
      stem = input$stemming_wc, 
      stop_language = input$language_wc
    )
    updateProgressBar(
      session = session,
      id = "progress_upload_wc",
      value = 100
    )
    Sys.sleep(0.9)
    closeSweetAlert(session = session)
    sendSweetAlert(session = session,
                   title = "Datei analysiert",
                   type = "success")
    txt
  })
  plotted_wc = reactive({
    req(df_uploaded_wc())
    if (is.null(df_uploaded_wc())) {return(NULL)}
    set.seed(42)
    if("random-dark" %in% input$select_font_color_wc) {
      color_sel = "random-dark"
    } else if ("random-light" %in% input$select_font_color_wc) {
      color_sel = "random-light"
    } else {
      color_sel = sample(x = input$select_font_color_wc,
                         size = df_uploaded_wc() %>%
                           head(round(input$select_number_wc / 100 * nrow(.),0)) %>% 
                           nrow(.), replace = T
                         )
    }
    if (is.null(color_sel)) {color_sel = "#000000"}
    wordcloud2::wordcloud2(
      data = df_uploaded_wc() %>%
        head(round(input$select_number_wc / 100 * nrow(.),0)),
      size = input$select_font_size_wc %>%
        as.numeric(.),
      fontFamily = input$select_font_wc,
      color = color_sel,
      backgroundColor = input$select_background_wc,
      shuffle = FALSE,
      shape = input$select_shape_wc
      # ellipticity = input$select_ellipticity,
      # rotateRatio = input$select_rotateRatio,
    )
  })
  output$word_plotted_wc = renderText({
    req(df_uploaded_wc())
    word_counted = df_uploaded_wc() %>%
      nrow(.)
    if (is.null(word_counted)) {
      return()
    } else {
      word_plotted = round(input$select_number_wc / 100 * word_counted,0)
      paste("Dargestellt werden", word_plotted,"von", word_counted,"einzigartigen Wörter")
    }
  })
  output$wordcloud_plot_wc = wordcloud2::renderWordcloud2({
    set.seed(42)
    req(plotted_wc())
    plotted_wc()
  })
  output$ui_plot_wc = renderUI({
    req(df_uploaded_wc())
    set.seed(42)
    switch(input$plot_size_wc,
           "s" = {
             plot.width = "400"
             plot.height = "400"
           },
           "sm" = {
             plot.width = "640"
             plot.height = "640"
           },
           
           "m" = {
             plot.width = "800"
             plot.height = "800"
           },
           "ml" = {
             plot.width = "960"
             plot.height = "960"
           },
           "l" = {
             plot.width = "1200"
             plot.height = "1200"
           },
           "xl" = {
             plot.width = "1920"
             plot.height = "1920"
           }
    )
    
    # #wordcloud plot here
    shinyWidgets::addSpinner(
      wordcloud2::wordcloud2Output(
        "wordcloud_plot_wc",
        width = plot.width,
        height = plot.height
      )
    )
  })
  output$words_total_wc = renderText({
    req(df_uploaded_wc())
    prettyNum(
      x = sum(df_uploaded_wc()[,"freq"]),
      big.mark = ".",
      decimal.mark = ","
    )
  })
  output$words_top_wc = renderText({
    req(df_uploaded_wc())
    res = df_uploaded_wc() %>%
      head(1) %>%
      pull(word)
    res
  })
  output$words_mean_wc = renderText({
    req(df_uploaded_wc())
    prettyNum(
      x = df_uploaded_wc() %>%
        mutate(word_len = nchar(word) * freq) %>%
        summarize(mittel = mean(word_len)),
      big.mark = ".",
      decimal.mark = ","
    )
  })
  output$table_of_words_wc = DT::renderDataTable({
    req(df_uploaded_wc())
    DT::datatable(
      df_uploaded_wc(),
      rownames = F
    )  
  })
  output$downloadButton_wc = downloadHandler(filename = paste(format(Sys.Date(),"%Y_%m_%d"),"_Wordcloud", '.png', sep = ''),content = function(file) {
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    withProgress(message = 'Bereite Grafik vor',
                 detail = '☕',
                 min = 0,
                 max = 1,
                 value = 0.5, {
                   htmlwidgets::saveWidget(plotted_wc(),
                                           "temp.html",
                                           selfcontained = F
                   )
                   webshot::webshot("temp.html",
                                    file = file,
                                    delay = 5,
                                    selector = "canvas#canvas"
                   )
                   incProgress(amount = 1/2,
                               message = "Starte Download",
                               detail = "🍦")
                   Sys.sleep(0.9)
                 })
  })
  
  # server Lettercloud ----
  df_uploaded_lc = reactive({
    req(input$file_upload_lc)
    progressSweetAlert(
      session = session,
      id = "progress_upload_lc",
      display_pct = TRUE,
      value = 0,
      total = 100,
      striped = T,
      title = 'Lese Datei und zähle Wörter\n☕'
    )
    file_name = input$file_upload_lc$datapath
    updateProgressBar(
      session = session,
      id = "progress_upload_lc",
      value = 50
    )
    txt = word_count(
      link = file_name,
      stem = input$stemming_lc,
      stop_language = input$language_lc
    )
    updateProgressBar(
      session = session,
      id = "progress_upload_lc",
      value = 100
    )
    Sys.sleep(0.9)
    closeSweetAlert(session = session)
    sendSweetAlert(session = session,
                   title = "Datei analysiert",
                   type = "success")
    txt
  })
  plotted_lc = reactive({
    req(df_uploaded_lc())
    if (is.null(df_uploaded_lc())) {return(NULL)}
    set.seed(42)
    color_sel = sample(x = input$select_font_color_lc,
                       size = df_uploaded_lc() %>%
                         head(round(input$select_number_lc / 100 * nrow(.),0)) %>%
                         nrow(.),replace = T
    )
    wordcloud2::letterCloud(
      data = df_uploaded_lc() %>%
        head(round(input$select_number_lc / 100 * nrow(.),0)),
      word = input$word_lc,
      wordSize = 2,
      color = color_sel,
      backgroundColor = input$select_background_lc
    )
  })
  output$word_plotted_lc = renderText({
    word_counted = df_uploaded_lc() %>%
      nrow(.)
    if (is.null(word_counted)) {
      return()
    } else {
      word_plotted = round(input$select_number_lc / 100 * word_counted,0)
      paste("Dargestellt werden", word_plotted,"von", word_counted,"einzigartigen Wörter")
    }
  })
  output$wordcloud_plot_lc = wordcloud2::renderWordcloud2({
    set.seed(42)
    plotted_lc()
  })
  output$ui_plot_lc = renderUI({
    req(input$file_upload_lc)
    switch(input$plot_size_lc,
           "auto" = {
             plot.width = NULL
             plot.height = NULL
           },
           "s" = {
             plot.width = "400"
             plot.height = "400"
           },
           "sm" = {
             plot.width = "640"
             plot.height = "640"
           },

           "m" = {
             plot.width = "800"
             plot.height = "800"
           },
           "ml" = {
             plot.width = "960"
             plot.height = "960"
           },
           "l" = {
             plot.width = "1200"
             plot.height = "1200"
           },
           "xl" = {
             plot.width = "1920"
             plot.height = "1920"
           }
           )

    # #wordcloud plot here
    shinyWidgets::addSpinner(
      wordcloud2::wordcloud2Output(
        "wordcloud_plot_lc",
        width = plot.width,
        height = plot.height
      ),
      spin = "folding-cube"
    )
  })
  output$words_total_lc = renderText({
    req(df_uploaded_lc())
    prettyNum(
      x = sum(df_uploaded_lc()[,"freq"]),
      big.mark = ".",
      decimal.mark = ","
    )
  })
  output$words_top_lc = renderText({
    req(df_uploaded_lc())
    res = df_uploaded_lc() %>%
      head(1) %>%
      pull(word)
    res
  })
  output$words_mean_lc = renderText({
    req(df_uploaded_lc())
    prettyNum(
      x = df_uploaded_lc() %>%
        mutate(word_len = nchar(word) * freq) %>%
        summarize(mittel = mean(word_len)),
      big.mark = ".",
      decimal.mark = ","
    )
  })
  output$table_of_words_lc = DT::renderDataTable({
    req(df_uploaded_lc())
    DT::datatable(
      df_uploaded_lc(),
      rownames = F
    )
  })
  output$downloadButton_lc = downloadHandler(filename = paste(format(Sys.Date(),"%Y_%m_%d"),"_Lettercloud", '.png', sep = ''),content = function(file) {
    #graphic = plotted_lc()
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    withProgress(message = 'Bereite Grafik vor',
                 detail = '☕',
                 min = 0,
                 max = 1,
                 value = 0.5, {
                   htmlwidgets::saveWidget(
                     widget = plotted_lc(),
                     file = "temp_lc.html",
                     selfcontained = F,
                     background = "transparent"
                   )
                   webshot::webshot("temp_lc.html",
                                    file = file,
                                    delay = 5,
                                    selector = '<div id="ui_plot_pc" class="shiny-html-output"></div>'
                   )
                   incProgress(amount = 1/2,
                               message = "Starte Download",
                               detail = "🍦")
                   Sys.sleep(0.9)
                 })
  })

  # server Picturecloud ----
  df_uploaded_pc = reactive({
    req(input$file_upload_pc)
    progressSweetAlert(
      session = session,
      id = "progress_upload_pc",
      display_pct = TRUE,
      value = 0,
      total = 100,
      striped = T,
      title = 'Lese Datei und zähle Wörter\n☕'
    )
    file_name = input$file_upload_pc$datapath
    updateProgressBar(
      session = session,
      id = "progress_upload_pc",
      value = 50
    )
    txt = word_count(
      link = file_name,
      stem = input$stemming_pc,
      stop_language = input$language_pc
    )
    updateProgressBar(
      session = session,
      id = "progress_upload_pc",
      value = 100
    )
    Sys.sleep(0.9)
    closeSweetAlert(session = session)
    sendSweetAlert(session = session,
                   title = "Datei analysiert",
                   type = "success")
    txt
  })
  output$wordcloud_plot_pc = wordcloud2::renderWordcloud2({
    req(input$file_upload_shape_pc)
    req(input$file_upload_pc)
    wordcloud2::wordcloud2(
      data = df_uploaded_pc(),
      figPath = input$file_upload_shape_pc$datapath,
      color = input$select_font_color_pc
    )
  })
  output$ui_plot_pc = renderUI({
    req(input$file_upload_shape_pc)
    req(input$file_upload_pc)
    switch(input$plot_size_pc,
           "s" = {
             plot.width = "400"
             plot.height = "400"
           },
           "sm" = {
             plot.width = "640"
             plot.height = "640"
           },

           "m" = {
             plot.width = "800"
             plot.height = "800"
           },
           "ml" = {
             plot.width = "960"
             plot.height = "960"
           },
           "l" = {
             plot.width = "1200"
             plot.height = "1200"
           },
           "xl" = {
             plot.width = "1920"
             plot.height = "1920"
           }
    )
    shinyWidgets::addSpinner(
      wordcloud2::wordcloud2Output(
        "wordcloud_plot_pc",
        width = plot.width,
        height = plot.height
      ),
      spin = "folding-cube"
    )
  })
  output$words_total_pc = renderText({
    req(df_uploaded_pc())
    prettyNum(
      x = sum(df_uploaded_pc()[,"freq"]),
      big.mark = ".",
      decimal.mark = ","
    )
  })
  output$words_top_pc = renderText({
    req(df_uploaded_pc())
    res = df_uploaded_pc() %>%
      head(1) %>%
      pull(word)
    res
  })
  output$words_mean_pc = renderText({
    req(df_uploaded_pc())
    prettyNum(
      x = df_uploaded_pc() %>%
        mutate(word_len = nchar(word) * freq) %>%
        summarize(mittel = mean(word_len)),
      big.mark = ".",
      decimal.mark = ","
    )
  })
  output$table_of_words_pc = DT::renderDataTable({
    req(df_uploaded_pc())
    DT::datatable(
      df_uploaded_pc(),
      rownames = F
    )
  })
  output$downloadButton_pc = downloadHandler(filename = paste(format(Sys.Date(),"%Y_%m_%d"),"_Picturecloud", '.png', sep = ''),content = function(file) {
    graphic = plotted_pc()
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    withProgress(message = 'Bereite Grafik vor',
                 detail = '☕',
                 min = 0,
                 max = 1,
                 value = 0.5, {
                   htmlwidgets::saveWidget(
                     widget = graphic,
                     file = "temp_pc.html",
                     selfcontained = F
                   )
                   webshot::webshot("temp_pc.html",
                                    file = file,
                                    delay = 10,
                                    cliprect = "viewport"
                   )
                   incProgress(amount = 1/2,
                               message = "Starte Download",
                               detail = "🍦")
                   Sys.sleep(0.9)
                 })
  })
}

shinyApp(ui = ui, server = server)
