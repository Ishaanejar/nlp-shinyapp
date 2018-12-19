#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#
#ud_model_english <- udpipe_download_model(language = "english")  # 3.05 secs
#ud_model_spanish <- udpipe_download_model(language = "spanish")  #  secs
#ud_model_hindi <- udpipe_download_model(language = "hindi")  # secs
# Define server logic required to draw a histogram

options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output,session) {
  
  set.seed=2092014 
  
  ##############Read Data#######################
  data <- reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      filePath <- input$file$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      fileText  =  str_replace_all(fileText, "<.*?>", "") 
      return(fileText)
    }
  })
 #################### Pos tags and Language######################################
  postags <- reactive({
    l<- input$lang
    if(l=="en"){
      postaglist=input$xposchoice
      return(postaglist)
      }
    else{
      postaglist=input$xposchoice
      postaglist<- gsub("JJ","ADJ",postaglist)
      postaglist<-gsub("NN","NOUN",postaglist)
      postaglist<-gsub("NNP","PROPN",postaglist)
      postaglist<-gsub("RB","ADV",postaglist)
      postaglist<-gsub("VB","VERB",postaglist)
    return(postaglist)
      }
    return(postaglist)
    
  })
  
  output$text  <- renderPrint({
    
    validate(
    need(input$file$datapath != "", "Please upload a text file"))
       })
  
################Annotated Data Formation##########################
    anndata  <- reactive({
   
      #english_model = udpipe_load_model("D:/Isha/taba/TABA Tutorial/english-ud-2.0-170801.udpipe")  # file_model only needed       
      #english_model = udpipe_load_model("D:\\CBA\\T12\\TABA\\test shiny\\test1\\english-ud-2.0-170801.udpipe")  # file_model only needed
      #x <- udpipe_annotate(english_model, x = data())
      
    if (input$lang=="en") 
      {
      model = udpipe_load_model("D:\\Isha\\taba\\TABA Tutorial\\english-ud-2.0-170801.udpipe")
      #model = udpipe_load_model("D:\\CBA\\T12\\TABA\\test shiny\\test1\\english-ud-2.0-170801.udpipe")
        }
      else {
        validate(
          need(input$modelfile$datapath != "", "Please upload a model file")
                  )  
      model=udpipe_load_model(input$modelfile$datapath)
      }
      
      validate(need(input$file$datapath != "", "Please upload a text file"))
      x <- udpipe_annotate(model, x = data())
      x <- as.data.frame(x)
      return(x)
  
  })
 
  
  ################Annotated Text Rendering ##########
     output$anntext  <- renderDataTable({ 
       x<- anndata()
       if(input$lang=="en"){
       x1 = subset(x, xpos %in% postags())
       }else{
       x1 = subset(x, upos %in% postags())
       }
       return(x1)
       })
    
###############NounExtraction######################
  
  output$phrasextractnoun<-renderTable({
    all_nouns = anndata() %>% subset(., upos %in% "NOUN") 
    top_nouns = txt_freq(all_nouns$lemma)  
    head(top_nouns, 50)	
  })
  
######################### Verb Extraction###########################
  output$phrasextractverb<-renderTable({
    
    all_verbs = anndata() %>% subset(., upos %in% "VERB") 
    top_verbs = txt_freq(all_verbs$lemma)
    head(top_verbs, 50)
 
  })
 ########################Collocations################################   
    output$collocations<-renderTable({
      
      collocateddata <- keywords_collocation(x = anndata(),  
                                             term = "token", 
                                             group = c("doc_id", 
                                                       "paragraph_id", "sentence_id"),
                                             ngram_max = 4) 
      collocateddata
    }) 
    
#########Looking at which words are followed by another word##############
  
  output$cooccurrences_noun_adj<-renderTable({
    validate(need(length(input$xposchoice)>2 , "Please select atleast 2 xpos tags"))  
  
        cooccurrences_noun_adj <- cooccurrence(
          if(input$lang=="en"){
      x = subset(anndata(), xpos %in% postags())
                                        }
          else{ x = subset(anndata(), upos %in% postags())}, 
      term = "lemma", 
      group = c("doc_id", "paragraph_id", "sentence_id"))
    return(cooccurrences_noun_adj)
    
                                              })    
  
 #####Looking at which words are located in the same document/sentence/paragraph.
  
  output$gen_cooccur<-renderTable({
    adata=anndata()
    validate(need(length(input$xposchoice)>2 , "Please select atleast 2 xpos tags"))  
   
    if(input$lang=="en"){
      gen_cooccur <- cooccurrence(x = adata$lemma, 
                                  relevant = adata$xpos %in% postags())
    }
    else{
      gen_cooccur <- cooccurrence(x = adata$lemma, 
                                  relevant = adata$upos %in% 
                                    postags())}
    return(gen_cooccur)
                                })
  
  
#####Looking at which words are in the neighbourhood of the word as in follows the word within skipgram number of words
  
  output$cooccur_skips_ngrams<-renderTable({
    validate(need(length(input$xposchoice)>2 , "Please select atleast 2 xpos tags")) 
    adata=anndata() 
    if(input$lang=="en"){
     cooccur_skips_ngrams <- cooccurrence(x = adata$lemma, 
                                         relevant = adata$xpos %in% 
                                           postags() , 
                                         skipgram = 4) }
    
    else{   
      cooccur_skips_ngrams <- cooccurrence(x = adata$lemma, 
                                           relevant = adata$upos %in% 
                                             postags() , 
                                           skipgram = 4) 
      }
    
#############Co-occurence Graph#####################
    
    output$plot1 = renderPlot({
      validate(need(length(input$xposchoice)>2 , "Please select atleast 3 xpos tags")) 
        cooccurrences_noun_adj <- cooccurrence(  
           if(input$lang=="en"){
        x = subset(anndata(), xpos %in% postags())}else{x = subset(anndata(), upos %in% postags())}
                , 
        term = "lemma", 
        group = c("doc_id", "paragraph_id", "sentence_id"))
      
      wordnetwork <- head(cooccurrences_noun_adj, 50)
      wordnetwork <- igraph::graph_from_data_frame(wordnetwork)
      
      ggraph(wordnetwork, layout = "auto") +  
        geom_edge_link(aes(width = cooc, edge_alpha = cooc), 
                       edge_colour = "orange") +  
        geom_node_text(aes(label = name), col = "black", size = 5) +
        theme_graph(base_family = "Calibri") +  
        theme(legend.position = "none") +
        labs(title = "Graph upto with 3 levels", 
             subtitle = "Nouns & Adjective")->g
      
      return(g)    
    })
    
    
    
    
  }) 
  
  
  
  
      

})
