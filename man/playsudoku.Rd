\name{playsudoku}
\alias{playsudoku}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{

Interactively play a game of Sudoku

%%  ~~function to do ... ~~
}
\description{

Interactively play a game of 9*9 Sudoku with hints and undo. Besides, 12*12 and 16*16 MonsterSudoku is also allowed without hints

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
playsudoku(z = NULL, rank = 9, hist.len = 100, solve = TRUE, display = c("guess", "windows", "tk"), hscale = 1.25, vscale = 1.25, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{

The sudoku you generate before or you can set it NULL

%%     ~~Describe \code{z} here~~
}
  \item{rank}{

The rank of sudoku

%%     ~~Describe \code{rank} here~~
}
  \item{hist.len}{

Integer representing the number of history steps to

%%     ~~Describe \code{hist.len} here~~
}
  \item{solve}{

Logical indicating if the solution should be computed

%%     ~~Describe \code{solve} here~~
}
  \item{display}{

Type of display.  The default 'guess' uses a windows
    graphics device if getOption('device')=='windows', otherwise it uses
    tk (requiring the 'tkrplot' package).

%%     ~~Describe \code{display} here~~
}
  \item{hscale}{

Passed to tkrplot

%%     ~~Describe \code{hscale} here~~
}
  \item{vscale}{

Passed to tkrplot

%%     ~~Describe \code{vscale} here~~
}
  \item{\dots}{

Arguments passed to \code{\link{generatesudoku}}

%%     ~~Describe \code{\dots} here~~
}
}
\details{
The function can only play the puzzle you generate before or its own generate when you set the puzzle NULL. And according to your selection, there will be a sudoku table appearing in R GUI windows. And move your mouse to empty cell and type corresponding numbers or letters you want to fill. And there are also some special keys with special function. The more details will come out in R console as following:
" ?                                 -- this help", 
        "1-9 & A,B,C,D,E,F,G (for Monster)  -- insert digit", 
        "0,' '                              -- clear cell", "r                                  -- replot the puzzle", 
        "q                                  -- quit", "h (only for classic)               -- hint/help", 
        "w                                  -- correct wrong entries (show in red)", 
        "u                                  -- undo last entry", 
        "s                                  -- show number in cell", 
        "x                                  -- show all (solve the puzzle)"

%%  ~~ If necessary, more details than the description above ~~
}
\value{

An invisible matrix records the puzzle when you fill a number in one step. Until the matrix is completely same as the solve matrix, it will come to the end.

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{

David Brahm sudoku package

%% ~put references to the literature/web site here ~
}
\author{

Zhengmairuo Gan<lepallion-lyric@163.com> and Yuzhen Hua<441277231@qq.com>

%%  ~~who you are~~
}
\note{

Some special keys with special functions will be only used in 9*9 puzzle

%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
## Not run:
library(sudoku) 
#require sudoku package
\dontrun{playsudoku(rank=9,web=FALSE)}
#play a 9*9 sudoku in R GUI
## End(Not run)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sudoku }

