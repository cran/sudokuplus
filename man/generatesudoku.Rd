\name{generatesudoku}
\alias{generatesudoku}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{

Randomly Generate a Sudoku Puzzle in 9*9, 12*12, or 16*16

%%  ~~function to do ... ~~
}
\description{

Creates a 9x9 Sudoku grid or Monster Sudoku which is 12*12 or 16*16 suitable for use by\code{\link{playsudoku}}.


%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
generatesudoku(rank = 9, Date = "2012-05-15", web = TRUE)
}
\arguments{
  \item{rank}{

Define the type of Sudoku

%%     ~~Describe \code{rank} here~~
}
  \item{Date}{

Define which day the puzzle come from

%%     ~~Describe \code{Date} here~~
}
  \item{web}{

Get to know whether you have connected to the Internet

%%     ~~Describe \code{web} here~~
}
}
\details{

Firstly, the rank can only be 9, 12, or 16.Because there are three types sudoku here. Then when web is TRUE, it will fetch a puzzle from Internet based on the date you select. However, if web is FALSE, it will randomly generate a 9*9 or 16*16 puzzle, not including 12*12. And in the puzzle, 10 stands for A, 11 stands for B and so on.


%%  ~~ If necessary, more details than the description above ~~
}
\value{

A list containing the rank of puzzle, the date of puzzle (NULL when web is FALSE) and the puzzle itself

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{

http://www.dailysudoku.com/sudoku/

%% ~put references to the literature/web site here ~
}
\author{

Maosheng Zhang <1519850656@qq.com> and Caiyan Lai<546535232@qq.com>

%%  ~~who you are~~
}
\note{

The format of Date must be "YYYY-MM-DD". Furthermore, the YYYY should have four digits, and MM should have two digits, and DD cannot be sure. 
For example, "2012-06-4", "2012-05-30"

%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

\code{\link{solvesudoku}},	\code{\link{playsudoku}}

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
z <- generatesudoku(rank = 9 , web = FALSE)
#generate a 9*9 sudoku randomly
z$Rank                                                          
#output the rank of sudoku
z$Puzzle                                                        
#output the sudoku itself

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sudoku }

