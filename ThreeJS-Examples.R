library(threejs)


############################## GLOBE VISUALIZATION ######################################
globejs(flights)

globejs(img = system.file("images/world.jpg", package = "threejs"),
        value = 40, color = "#00ffff", arcsColor = "#99aaff",
        arcsHeight = 0.4, arcsLwd = 1, arcsOpacity = 0.2, atmosphere = TRUE,
        bg = "black", height = NULL, width = NULL)

############################# 3X NETWORK DIAGRAMS  ######################################
set.seed(1)
g <- sample_islands(3, 10, 5/10, 1)
i <- cluster_optimal(g)
(graphjs(g, vertex.color=c("orange", "green", "blue")[i$membership], vertex.shape="sphere"))
# Les Miserables Character Co-appearance Data
data("LeMis")
(graphjs(LeMis))
# ...plot Character names
LeMis 11
(graphjs(LeMis, vertex.shape=V(LeMis)$label))
# SNAP Facebook ego network dataset
data("ego")
(graphjs(ego, bg="black"))
## Not run:
# A shiny example
shiny::runApp(system.file("examples/graph", package="threejs"))
# A graph amination that shows several layouts
data("LeMis")
graphjs(LeMis,
        layout=list(
          layout_randomly(LeMis, dim=3),
          layout_on_sphere(LeMis),
          layout_with_drl(LeMis, dim=3), # note! somewhat slow...
          layout_with_fr(LeMis, dim=3, niter=30)),
        main=list("random layout", "sphere layout", "drl layout", "fr layout"),
        fpl=300)
# A simple graph animation illustrating edge modification
g <- make_ring(5) - edges(1:5)
graph_list <- list(
  g + edge(1, 2),
  g + edge(1, 2) + edge(2, 3),
  g + edge(1, 2) + edge(2, 3) + edge(3, 4),
  g + edge(1, 2) + edge(2, 3) + edge(3, 4) + edge(4, 5),
  g + edge(1, 2) + edge(2, 3) + edge(3, 4) + edge(4, 5) + edge(5, 1))
graphjs(graph_list, main=paste(1:5),
        vertex.color=rainbow(5), vertex.shape="sphere", edge.width=3)
# see `demo(package="threejs") for more animation demos.
# A crosstalk example
library(crosstalk)
library(DT)
data(LeMis)
sd = SharedData$new(data.frame(Name = V(LeMis)$label))
print(bscols(
  graphjs(LeMis, brush=TRUE, crosstalk=sd),
  datatable(sd, rownames=FALSE, options=list(dom='tp'))
))

############################### LIST OF ALL FUNCTIONS ########################################

ls("package:threejs")
