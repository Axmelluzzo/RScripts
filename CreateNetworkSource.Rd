\name{CreateNetworkSource}
\alias{CreateNetworkSource}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{CreateNetworkSource()
%%  ~~function to do ... ~~
}
\description{Create a data source capable of illustrating a network diagram
}
\usage{
CreateNetworkSource(NodeList,EdgeList)
}
\arguments{
  \item{NodeList}{
This should be a list of nodes, none of which are repeated. Minimum of 1 column with a unique identifier, other descriptive columns can be brought in without interference.
}
\item{EdgeList}{
This should be a list of connections, preferably in a "From" and "To" format at the row level which refers to nodes using the unique identifier defined in the Node source.
}
}
\details{
This function creates a data source capable of mapping out a network diagram. It requires the combination of 2 datasouces, a Node list and an Edge list, both of which need to be leveraged according the the arguments mentioned above.
}
\value{
The returned value will be one data source, titled Network Source. You can find this in your Global Environment
}
\references{
iGraph, threeJS
}
\author{
Alexander Melluzzo
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
CreateRandomNetworkSource
DegreesOfSeparation
}
\examples{

NodeList <- paste("s",1:5, sep="")
ConnList <- sample(NodeList,20, replace = TRUE)
EdgeList <- matrix(ConnList,5,2)
colnames(EdgeList)<- c("From","To")
CreateNetworkSource(NodeList,EdgeList)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
