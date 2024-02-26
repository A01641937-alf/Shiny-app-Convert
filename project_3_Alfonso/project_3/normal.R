library(igraph)
library(shiny)

build <- function(parts) {
  nodes <- c()
  edges <- c()
  
  for (i in 1:length(parts)) {
    parts[i] <- gsub(" ","",gsub("->", "", parts[i]))
    edges <- c(edges, substring(parts[i],2,2))
    if(nchar(parts[i]) == 3){
      nodes <- c(nodes, substring(parts[i],1,1), substring(parts[i],3,3))
    }
    else{
      nodes <- c(nodes, substring(parts[i],1,1), "Z")
    }
  }
  
  unique.nodes <- unique(nodes)
  type.node <- rep(1, length(unique.nodes))
  
  type.node["S"==unique.nodes]<- 2
  type.node["Z"==unique.nodes]<- 3
  
  return(list(nodes = nodes, edges = edges, type.node = type.node))
}

ui <- fluidPage(

  titlePanel("Normal"),
  
  sidebarLayout(sidebarPanel(
      textAreaInput(
        inputId = "myinputtext",
        label = "Enter DFA productions:",
        value = "S->aB\nS->bA\nB->bA\nB->cA\nB->a"
      )
    ),
    
    mainPanel(
      verbatimTextOutput("outputtext"),
      div(style = "display: flex;",
          div(style = "flex:1;",
              plotOutput("tColors")),
          div(style = "flex:1.2;",
              plotOutput("dfaPlot")
          )
      )
    )
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
    
    l <- length(parts)
    
    if (l > 0) {
      graph.data <- build(parts)
      g <- graph(graph.data$nodes, directed = TRUE)
      layout <- layout_with_fr(g)
      mapping.colors <- c("white","green","red")
      
      node.colors <- mapping.colors[graph.data$type.node]
      curves <- curve_multiple(g)
      
      plot(g,edge.label = graph.data$edges,vertex.color = node.colors,vertex.frame.color = "black",vertex.label.color = "black",edge.arrow.size = .5,edge.curved = curves,layout = layout, vertex.size = 20)
    
      } else {
      g.e <- make_empty_graph()
      plot(g.e)
    }
  })
  
  output$tColors <- renderPlot({
    g <- make_empty_graph()
    set.seed(42)
    plot(g)
    
    mapping.colors <- c("white","green","red")
    legend(x = -1.1,y = 1.05,legend = c("No es inicial ni final","Es inicial","Es final"),fill = mapping.colors,bty = "n")
  })
}

shinyApp(ui = ui, server = server)