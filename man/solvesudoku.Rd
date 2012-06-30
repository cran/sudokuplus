\name{solvesudoku}
\alias{solvesudoku}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{

Solve a Sudoku Puzzle 

%%  ~~function to do ... ~~
}
\description{

Solve a Sudoku Puzzle in 9*9, 12*12 or 16*16

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
solvesudoku(z, Date = NULL, web = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{

The puzzle need solving

%%     ~~Describe \code{z} here~~
}
  \item{Date}{

The date of puzzle if it come from Internet

%%     ~~Describe \code{Date} here~~
}
  \item{web}{

Get to know whether you have connected to the Internet

%%     ~~Describe \code{web} here~~
}
}
\details{

Firstly, z must be a puzzle, only puzzle. Because when you generate a sudoku it includes some information of the sudoku. So type assign z$Puzzle to z is ok. When the rank of the puzzle is 12, Date is required and web must be TRUE. And the format of date must be same as we have illustrated before

%%  ~~ If necessary, more details than the description above ~~
}
\value{

A matrix, corresponding to the solve of the puzzle

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{

Zhengmairuo Gan<lepallion-lyric@163.com> and Yuzhen Hua<441277231@qq.com>

%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

\code{\link{playsudoku}},	\code{\link{generatesudoku}}

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
z <- generatesudoku(rank = 9, web = FALSE)
#generate the sudoku
solvesudoku(z$Puzzle)
#solve the sudoku
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sudoku }

