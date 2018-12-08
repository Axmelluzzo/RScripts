\name{DegreesOfSeparation}
\alias{DegreesOfSeparation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DegreesOfSeparation
%%  ~~function to do ... ~~
}
\description{
Defines nodes at x degrees of separation from the starting node.
}
\usage{
DegreesOfSeparation(EdgeList, StartingNode)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{EdgeList}{
This is the list of connections for the network diagram, most appropriately formatted as a two-columnm data source indicating a "From" and "To" variable. **THIS MUST BE A DATA FRAME**
}
  \item{StartingNode}{
This is the name of the node which you would like to analyze the degrees of separation for. It should include the name in its entirety. For example : if your Unique Identifier is "s", then you would put "s1" or "s2" or etc.
}
}
\details{
This calculation is done by first analyzing the connections coming "From" your starting node, then looking at what those elements are directly connected to.
}
\value{
The value returned is in text format
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Alexander Melluzzo
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
a<-paste("s",1:10, sep = "")
EdgeList<-matrix(sample(a,100,replace = TRUE),25,2)
colnames(EdgeList)<-c("From","To")
EdgeList<-as.data.frame(EdgeList)
StartingNode<-"s1"
DegreesOfSeparation(EdgeList,StartingNode)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
