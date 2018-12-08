\name{CreateRandomNetworkSource}
\alias{CreateRandomNetworkSource}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{CreateRandomNetworkSource
%%  ~~function to do ... ~~
}
\description{
Creates a randomly generated Node and Edge source for network diagram visualization.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CreateRandomNetworkSource(ID_Key, Number_of_Users, Number_of_Applications, Number_of_Databases, DB_App, User_App)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ID_Key}{
This will be the character used as a unique identifier in the node source. For Example : "s" would generate "s1","s2","s3",etc.
}
  \item{Number_of_Users}{
The number of unique Users you wanted in the datasource.
}
  \item{Number_of_Applications}{
The number of unique Applications you wanted in the datasource.
}
  \item{Number_of_Databases}{
The number of unique Databases you wanted in the datasource.
}
  \item{DB_App}{
The number of unique connections between Databases and Applications.
}
  \item{User_App}{
The number of unique connections between Users and Applications .
}
}
\details{
This function creates a completely randomized dataset by using the sample() function across generated values. This function should only be used in the event that you do not have Node or Edge data already.
}
\value{
The values returned are Node List and Edge List, which can then be used to create a network diagram source using the CreateNetworkSource() function.
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
CreateNetworkSource
DegreesOfSeparation
}
\examples{
CreateRandomNetworkSource("s",4,6,5,10,10)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
