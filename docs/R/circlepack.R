#' Circlepack figure used to show relative abundance/representation of survey respondents,
#' especially when each observation (respondent) often counts as multiple types
#' 
#' @param type Column name representing 
#' @param survey data
type_representation <- function(type, data) {
  types <- data %>%
    dplyr::pull({{type}})
  
  n <- nrow(data)
  
  types <- strsplit(types, ", ") %>%
    unlist()
  types.prct <- (table(types) / n) * 100
  types.dt <- as.data.frame(types.prct)
  
  # Edges
  nodes <- types.dt %>%
    dplyr::select(demographic = types, size = Freq) %>%
    dplyr::mutate(size = round(size, 0),
           label = paste0(demographic, "\n", size, "%"))
  
  # Graph obj and graph
  graph <- tidygraph::tbl_graph(nodes)
  set.seed(1)
  ggraph(graph, 'circlepack', weight = size) +
    ggraph::geom_node_circle(aes(fill = demographic)) +
    ggplot2::scale_fill_brewer(type = "qual", palette = 2) +
    ggraph::geom_node_label(aes(label = label)) +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "FALSE") +
    ggplot2::guides(fill = guide_legend(title = "demographic represented")) +
    ggplot2::theme_void() #+ geom_node_label(aes(label=name), size=1)
}