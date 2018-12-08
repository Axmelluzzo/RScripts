
##List all functions: lsf.str("package:CignaNetwork")


RandomWordGenerator <- function(NumberOfWords){
  library(RCurl)
  library(stringr)
  x <- getURL("http://git.sys.cigna.com/M23509/words/raw/master/WordFile")
  z<- as.matrix(x)
  QQ<- str_split_fixed(z,"\n", n=Inf)
  XMC<- matrix(sample(QQ,NumberOfWords,replace = TRUE),NumberOfWords,1)
  ACL<-data.frame(XMC)
}


















HTTPaddRFile <- function(url) {
  fname <- "HereYourFile.R"
  download.file(url, fname, method="wget")
  myEnv <- new.env()
  sys.source(fname, envir = myEnv)
  unlink(fname)
}













HTTPaddExcelFile <- function(url,filename) {
  fname <- paste("~/",filename,sep = "")
  download.file(url, fname, method="wget")
}





















AddAllV8Files<- function(){
  HTTPaddRFile("http://git.sys.cigna.com/M23509/words/raw/master/JS.R")
  HTTPaddRFile("http://git.sys.cigna.com/M23509/words/raw/master/RcppExports.R")
  HTTPaddRFile("http://git.sys.cigna.com/M23509/words/raw/master/callback.R")
  HTTPaddRFile("http://git.sys.cigna.com/M23509/words/raw/master/onLoad.R")
  HTTPaddRFile("http://git.sys.cigna.com/M23509/words/raw/master/tab_complete.R")
  HTTPaddRFile("http://git.sys.cigna.com/M23509/words/raw/master/V8.R")
}


















AddAllThreeJSFiles <- function(){
  Download3JSWebGL <- function(url) {
    fname <- "~/WebGl.js"
    download.file(url, fname, method="wget")
  }
  Download3JSWebGL("http://git.sys.cigna.com/M23509/words/raw/master/WebGL.js")

  Download3JS <- function(url) {
    fname <- "~/three.js"
    download.file(url, fname, method="wget")
  }
  Download3JS("http://git.sys.cigna.com/M23509/words/raw/master/three.js")

  Download3JSOrbit <- function(url) {
    fname <- "~/OrbitControls.js"
    download.file(url, fname, method="wget")
  }
  Download3JSOrbit("http://git.sys.cigna.com/M23509/words/raw/master/OrbitControls.js")
}



















CreateNetworkSource <- function (NodeList, EdgeList) {
  library(igraph)
  NetworkSource <<- graph.data.frame(vertices = NodeList, d = EdgeList, directed = TRUE)

}

























GenerateNetwork <- function(NetworkSource, NodeSource, NodeColorColumn = NA, NodeSizeColumn = NA, NodeNameColumn = NA){
  library(threejs)

  if (is.na(NodeColorColumn) && !is.na(NodeSizeColumn) && !is.na(NodeNameColumn)){
    A<- as.data.frame(NodeSource)
    SZE<- A[,NodeSizeColumn]
    NME<- A[,NodeNameColumn]
    graphjs(NetworkSource, vertex.label = NME, vertex.size = SZE, bg = "dimgrey",
            edge.color = "azure2", edge.width = .02, main = "Cigna Network Diagram", edge.alpha = .5)

  } else if (is.na(NodeSizeColumn) && !is.na(NodeColorColumn) && !is.na(NodeNameColumn)){
    A<- as.data.frame(NodeSource)
    CLR<- A[,NodeColorColumn]
    NME<- A[,NodeNameColumn]
    graphjs(NetworkSource, vertex.label = NME, vertex.color = CLR, bg = "dimgrey",
            edge.color = "azure2", edge.width = .02, main = "Cigna Network Diagram", edge.alpha = .5)
  }
  else if (is.na(NodeNameColumn) && !is.na(NodeColorColumn) && !is.na(NodeSizeColumn)){

    A<- as.data.frame(NodeSource)
    CLR<- A[,NodeColorColumn]
    SZE<- A[,NodeSizeColumn]
    graphjs(NetworkSource, vertex.color = CLR, vertex.size = SZE, bg = "dimgrey",
            edge.color = "azure2", edge.width = .02, main = "Cigna Network Diagram", edge.alpha = .5)
  }

  else if (is.na(NodeColorColumn) && is.na(NodeNameColumn) && !is.na(NodeSizeColumn)){
    A<- as.data.frame(NodeSource)
    SZE<- A[,NodeSizeColumn]
    graphjs(NetworkSource,  vertex.size = SZE, bg = "dimgrey",
            edge.color = "azure2", edge.width = .02, main = "Cigna Network Diagram", edge.alpha = .5)
  }

  else if (is.na(NodeColorColumn) && is.na(NodeSizeColumn) && !is.na(NodeNameColumn)){
    A<- as.data.frame(NodeSource)
    NME<- A[,NodeNameColumn]
    graphjs(NetworkSource, vertex.label = NME, bg = "dimgrey",
            edge.color = "azure2", edge.width = .02, main = "Cigna Network Diagram", edge.alpha = .5)
  }

  else if (is.na(NodeSizeColumn) && is.na(NodeNameColumn) && !is.na(NodeColorColumn)){
    A<- as.data.frame(NodeSource)
    CLR<- A[,NodeColorColumn]
    graphjs(NetworkSource, vertex.color = CLR, bg = "dimgrey",
            edge.color = "azure2", edge.width = .02, main = "Cigna Network Diagram", edge.alpha = .5)
  }

  else if (is.na(NodeSizeColumn) && is.na(NodeNameColumn) && is.na(NodeColorColumn)){
    graphjs(NetworkSource, bg = "dimgrey",
            edge.color = "azure2", edge.width = .02, main = "Cigna Network Diagram", edge.alpha = .5)
  }
  else {

    A<- as.data.frame(NodeSource)
    CLR<- A[,NodeColorColumn]
    SZE<- A[,NodeSizeColumn]
    NME<- A[,NodeNameColumn]
    graphjs(NetworkSource, vertex.label = NME, vertex.color = CLR, vertex.size = SZE, bg = "dimgrey",
            edge.color = "azure2", edge.width = .02, main = "Cigna Network Diagram", edge.alpha = .5)
  }

}
































CreateRandomNetworkSource<- function(ID_Key, Number_of_Users, Number_of_Applications,
                                     Number_of_Databases, DB_App, User_App){
  Number_of_Nodes <- Number_of_Users+Number_of_Applications+Number_of_Databases
  #Users Primary Key List
  UsersEquationRange <- 1:Number_of_Users
  UsersEquationNodeList <- paste(ID_Key,UsersEquationRange, sep = "")
  #Applications Primary Key List
  ApplicationsEquationRangeStart <- Number_of_Users+1
  ApplicationsEquationRange<-ApplicationsEquationRangeStart:(ApplicationsEquationRangeStart+Number_of_Applications-1)
  ApplicationsEquationNodeList<- paste(ID_Key,ApplicationsEquationRange, sep = "")
  #Databases Primary Key List
  DatabasesEquationRangeStart<- (Number_of_Users+Number_of_Applications+1)
  DatabasesEquationRange<- DatabasesEquationRangeStart:(DatabasesEquationRangeStart+Number_of_Databases-1)
  DatabasesEquationNodeList<- paste(ID_Key,DatabasesEquationRange, sep = "")
  #Combining all 3 elemnents into Primary Key List
  PrimaryKeyNodeListCombinedElements<- c(UsersEquationNodeList,ApplicationsEquationNodeList,DatabasesEquationNodeList)
  NodeListPrimaryKey<-as.matrix(PrimaryKeyNodeListCombinedElements,Number_of_Nodes,1)
  #Add Attributes
  ###########################################Color
  #User
  UserColorAtt <- sample("darkgoldenrod", Number_of_Users, replace = TRUE)
  #Application
  ApplicationColorAtt <- sample("deepskyblue3", Number_of_Applications, replace = TRUE)
  #Databases
  DatabasesColorAtt <- sample ("lawngreen", Number_of_Databases, replace = TRUE)
  #Total Color
  AttCombinedElements <- c(UserColorAtt,ApplicationColorAtt,DatabasesColorAtt)
  NodeNetworkSource <- cbind(NodeListPrimaryKey,AttCombinedElements)
  colnames(NodeNetworkSource)<- c("ID","Color")
  ###########################################Size
  #User
  UserSizeAtt <- rep(1.75,Number_of_Users)
  #Application
  ApplicationSizeAtt <- rep(2.5, Number_of_Applications)
  #Databases
  DatabasesSizeAtt <- rep(5, Number_of_Databases)
  #Total Size
  SizeCombinedElements <- c(UserSizeAtt,ApplicationSizeAtt,DatabasesSizeAtt)
  NodeNetworkSource <- cbind(NodeNetworkSource,SizeCombinedElements)
  ############################################Names
  NodeNames <- RandomWordGenerator(Number_of_Nodes)
  NodeListG <- cbind(NodeNetworkSource,NodeNames)
  colnames(NodeListG)<- c("ID","Color","Size","Name")
  NodeList<<-NodeListG
  #From List Element Creation
  FromUsers<- sample(UsersEquationNodeList,size=(User_App),replace = TRUE)
  FromDatabases <- sample(DatabasesEquationNodeList, size=DB_App, replace = TRUE)
  Application_Times <- User_App+DB_App
  ToApplications <- sample(ApplicationsEquationNodeList, size=Application_Times, replace = TRUE)
  #From List Combining of Elements
  FromList <- c(FromUsers,FromDatabases)
  FromMatrix <- as.matrix(FromList,nrow=Application_Times,ncol=1)
  EdgeListComplete <- cbind(FromList,ToApplications)
  colnames(EdgeListComplete)<-c("From","To")
  #Master Source
  EdgeList<<-as.data.frame(EdgeListComplete)
}





























DegreesOfSeparation <- function(EdgeList, StartingNode){
  #1ST Connection File
StartingNodeConnections<-EdgeList[EdgeList[,'From']==StartingNode,]
StartingNodeConnectionsDF<-as.data.frame(StartingNodeConnections)
#1st Strictly TO's
FirstDegreeConnectionsP1<-as.matrix(StartingNodeConnectionsDF[,2])
FirstDegreeConnections<- unique(StartingNodeConnectionsDF[,2])
FirstDegreeSeparation<- as.data.frame(StartingNodeConnectionsDF$To)
colnames(FirstDegreeSeparation)<-"To"
#Searched To's in TO column, New list of From's to index
UniqueFirstDegreeTos<-unique(FirstDegreeSeparation)
SecondDegreeSeparationWithFirst<- subset(EdgeList, EdgeList[,2]==FirstDegreeConnections)
######### Making Sure Test File
#Remove Starting Node from new connections
SecondDegreeWithoutFirst <- subset(SecondDegreeSeparationWithFirst,SecondDegreeSeparationWithFirst[,1]!=StartingNode)
#Second Degree List of connections
SecondDegreeConnections <- SecondDegreeWithoutFirst[,1]
SecondDegreeUniqueConnections<-unique(SecondDegreeConnections)
## Formatting Print Output
a<-as.vector(FirstDegreeConnectionsP1)
b<- gsub(pattern="c(\\\\\\\\\\\\\\\\)", x=a, replacement = TRUE)
## Print output
invisible(cat(paste("Your Starting Node is",StartingNode)))
cat(". Its 1st degree of separation contain the elements : ")
cat(b)
cat(" ")
invisible(cat("The 2nd degree of separation contain the elements: "))
cat(SecondDegreeUniqueConnections)
cat(" ")
cat(" ")
cat(" ")
cat(" ")
cat(" ")
cat(" ")
}

