library(igraph)
library(shiny)
library(stringr)

build <- function(parts) {
  tam <- length(parts)
  nodes <- c()
  edges <- c()
  inodes <- c()
  vacios <- c()
  
  for (i in 1:tam) {
    parts[i] <- gsub("[()]", "", parts[i])
    subcadenas <- str_split(parts[i], ",")[[1]]
    edges <- c(edges, subcadenas[3])
    nodes <- c(nodes, subcadenas[1], subcadenas[2])
    inodes <- c(inodes, subcadenas[1])
    
    if (subcadenas[3] == "-") {
      vacios <- c(vacios, i)
    }
  }
  
  unique.edges <- sort(unique(subset(edges,edges  != "-")))
  unique.nodes <- unique(nodes)
  
  tam <- length(unique.nodes)
  type.node <- rep(1, tam)
  type.frame <- rep(1,tam)
  type.edge <-  rep(1,length(edges))
  
  for (i in 1:tam) {
    
    pos <- which(inodes == unique.nodes[i])
    existentes <- sort(unlist(edges[pos]))
    existentes.unicos <- unique(existentes)
    
    type.edge[pos[which(duplicated(edges[pos]) | duplicated(edges[pos], fromLast = TRUE))]]<-3
    
    if (!all(unique.edges %in% gsub("-","",existentes.unicos))){
      type.frame[i]<-2
    }
  }
  
  type.node["S" == unique.nodes]<-2
  type.node[grep("\\.", unique.nodes)]<-3
  type.node["S." == unique.nodes]<-4
  type.edge[vacios]<-2
  
  mapping.colors <- c("white","green","red","yellow")#nada, inicial, final y ambas
  mapping.edge <-c("black","#FF00D4","#9300FF")#esta bien, es vacio, esta repetida
  mapping.frame <- c("black","#00FFCD")#no faltan y faltan transiciones
  
  node.frame <- mapping.frame[type.frame]
  node.colors <- mapping.colors[type.node]
  node.edge <- mapping.edge[type.edge]
  
  lista_columnas <- list(nodes = nodes, edges = edges, node.colors = node.colors, node.frame = node.frame, node.edge = node.edge)
  return(lista_columnas)
}

ui <- fluidPage(

  titlePanel("Opcional"),
  
  sidebarLayout(sidebarPanel(
    textAreaInput(
        inputId = "myinputtext",
        label = "Enter DFA productions:",
        value = "(S,B.,a)\n(S,A,b)\n(B.,A,-)\n(B.,A,b)\n(B.,C,b)"
      )
    ),
    
    mainPanel(
      verbatimTextOutput("outputtext"),
      div(style = "display: flex;",
          div(style = "flex:1;",
              plotOutput("tColors")),
          div(style = "flex:1.2;",
              plotOutput("dfaPlot")
          )))
    )
)

server <- function(input, output) {
  
  output$outputtext <- renderText({
    grammar <- input$myinputtext
    paste0("Output: \n", grammar)
  })
  
  output$dfaPlot <- renderPlot({
    grammar <- input$myinputtext
    parts <- strsplit(grammar, "\n")[[1]]
    
    one.rule <- length(parts)
    
    if (one.rule > 0) {
      graph.data <- build(parts)
      
      g <- graph(graph.data$nodes, directed = TRUE)
      layout <- layout_with_fr(g)
      curves <- curve_multiple(g)
      
      plot(g,edge.label = graph.data$edges,
           vertex.color = graph.data$node.colors,
           vertex.frame.color = graph.data$node.frame,
           vertex.label.color = "black",
           edge.color = graph.data$node.edge,
           edge.arrow.size = .5,
           edge.curved = curves,
           layout = layout,
           vertex.size = 45)
    
      } else {
      g.e <- make_empty_graph()
      plot(g.e)
    }
  })
  
  output$tColors <- renderPlot({
    g.e <- make_empty_graph()
    plot(g.e)
    
    mapping.colors <- c("white","green","red","yellow","black","#FF00D4","#9300FF","black","#00FFCD")
    legend(x = -1,y = 0.5,legend = c("Nodo-Estados","Nodo-Nodo inicial","Nodo-Nodo final","Nodo-Nodo inicial y final","Flecha-Arista normal","Flecha-Arista con epsilon","Flecha-Arista repetida","Borde-No faltan aristas","Borde-Faltan aristas"),fill = mapping.colors,bty = "n")
  })
}

shinyApp(ui = ui, server = server)