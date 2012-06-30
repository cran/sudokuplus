playsudoku <- function (z = NULL, rank = 9, hist.len = 100, solve = TRUE, display = c("guess", 
    "windows", "tk"), hscale = 1.25, vscale = 1.25, ...) 
{
    if (!require(sudoku))
	stop("'sudoku' package needed\n")
    dsp <- substring(match.arg(display), 1, 1)
    if (dsp == "t" && rank !=9)
	 stop("This platform can only be used in 9*9 puzzle!")
    if (dsp == "g") 
        dsp <- switch(.Platform$OS.type, windows = "w", "t")
    if (dsp == "t" && !require(tkrplot)) 
        stop("'tkrplot' package needed\n")
    if (identical(z, 0)) {
        z <- matrix(0, 9, 9)
        solve <- FALSE
    }
    if (is.null(z)){
        z1 <- generatesudoku(rank,...)
		z <- z1$Puzzle
		date=z1$Date
		rank=z1$Rank
	}
	if (length(z) == 1) 
        z <- readSudoku(z)
    if (solve) {
        cat("Solving...")
	  cat("done!\n")
	  if (is.null(date))
		zz <- solvesudoku(z,web=FALSE)
	  if (!is.null(date))
		zz <- solvesudoku(z,date,web=TRUE)

    }
    cols <- ifelse(z, "blue", "black")
    hst <- list(z)
    ah <- function(newz) {
        hst <<- c(hst, list(newz))
        if (length(hst) > hist.len) 
            hst <<- hst[-1]
    }
    replot <- function() {
			#########Generate relative table for the sudoku, and corresponding digit will print in the table!
		par(mar = c(1, 3, 5, 1))
		if (rank == 9){
			plot(0.5:9.5, 0.5:9.5, type = "n", axes = FALSE, xlab = "", ylab = "",main="9*9 puzzle")
			names=matrix(c("R9","R8","R7","R6","R5","R4","R3","R2","R1","C1","C2","C3","C4","C5","C6","C7","C8","C9"),ncol=2)
			rownames(names)<-names[,1]
			colnames<-names[,2]
			#++++++++++++++++++++++++++++++++ Creat sudoku table +++++++++++++++++++++++++++++#
			axis(side=2,at=1:9,labels=names[,1],col="white",las=1, cex.axis=0.85)
  			text(1:9, par("usr")[4]+0.5, adj = 0,labels = colnames,xpd = TRUE, cex=0.85)
			segments(0.5:9.5, rep(0.5, 10), 0.5:9.5, rep(9.5, 10), col = "gold")
			segments(rep(0.5, 10), 0.5:9.5, rep(9.5, 10), 0.5:9.5, col = "chocolate")
			segments(c(0, 3, 6, 9) + 0.5, rep(0.5, 4), c(0, 3, 6, 9) + 0.5, rep(9.5, 4), lwd = 3)
			segments(rep(0.5, 4), c(0, 3, 6, 9) + 0.5, rep(9.5, 4), c(0, 3, 6, 9) + 0.5, lwd = 3)
			#++++++++++++++++++++++++++++++++ Creat the initial sudoku +++++++++++++++++++++++++++++#
			for (i in 1:9) for (j in 1:9) if (z[i, j]) {
				   if (cols[i, j] == "red") 
					  text(j, 10 - i, "X", col = "pink", cex = 3)
				   text(j, 10 - i, z[i, j], col = cols[i, j], font = ifelse(cols[i, 
						j] == "blue", 2, 1), cex = ifelse(cols[i, j] == 
						"blue", 2, 1.8))
			}
		}
		if (rank == 12){
			plot(0.6:7.8,0.6:7.8, type="n", axes=FALSE, xlab="", ylab="",main="12*12 Monster Sudoku")
			names=matrix(c("R12","R11","R10","R9","R8","R7","R6","R5","R4","R3","R2","R1","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12"),ncol=2)
			rownames<-names[,1]
			colnames<-names[,2]
			#++++++++++++++++++++++++++++++++ creat sudoku table +++++++++++++++++++++++++++++#
			text(par("usr")[1]-0.1, seq(0.6,7.2,by=0.6)+0.2, adj = 0,labels = rownames,xpd = TRUE, cex=0.85)
  			text(seq(0.6,7.2,by=0.6) + 0.2, par("usr")[4]+0.2, adj = 0,labels = colnames,xpd = TRUE, cex=0.85)
			segments(seq(0.6,7.8,by=0.6), rep(0.6,13),seq(0.6,7.8,by=0.6), rep(7.8,13), col="gold")
			segments(rep(0.6,13), seq(0.6,7.8,by=0.6), rep(7.8,13), seq(0.6,7.8,by=0.6), col="chocolate")
			segments(c(0.6,3,5.4,7.8), rep(0.6,4),c(0.6,3,5.4,7.8), rep(7.8,4), lwd=3)
			segments(rep(0.6,5), c(0.6,2.4,4.2,6,7.8), rep(7.8,5), c(0.6,2.4,4.2,6,7.8), lwd=3)
			#++++++++++++++++++++++++++++++++ Creat the initial sudoku +++++++++++++++++++++++++++++#
			for (i in 1:12) for (j in 1:12){
				if (z[i, j] == 0){
					if (cols[i, j] == "red") 
      				   text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25, "X", col = "pink", cex = )
					text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25, "", col = cols[i, j], font = ifelse(cols[i, j] == "blue", 2, 1), 
						cex = ifelse(cols[i, j] == "blue", 2, 1.8))
					}
				else if (z[i, j] < 10) {
           			if (cols[i, j] == "red") 
           				text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25, "X", col = "pink", cex = )
					text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25, z[i, j], col = cols[i, j], font = ifelse(cols[i, j] == "blue", 2, 1), 
						cex = ifelse(cols[i, j] == "blue", 2, 1.8))
				}
				else if (z[i, j] == 10){
					if (cols[i, j] == "red") 
            			text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25, "X", col = "pink", cex = )
					text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25, "A", col = cols[i, j], font = ifelse(cols[i, j] == "blue", 2, 1), 
						cex = ifelse(cols[i, j] == "blue", 2, 1.8))
				}
				else if (z[i, j] == 11){
           			 if (cols[i, j] == "red") 
             			text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25, "X", col = "pink", cex = )
           			 text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25, "B", col = cols[i, j], font = ifelse(cols[i, j] == "blue", 2, 1), 
						cex = ifelse(cols[i, j] == "blue", 2, 1.8))
				}
				else{
           			 if (cols[i, j] == "red") 
             			text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25, "X", col = "pink", cex = )
           			 text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25, "C", col = cols[i, j], font = ifelse(cols[i, j] == "blue", 2, 1), 
						  cex = ifelse(cols[i, j] == "blue", 2, 1.8))
					 }
			}
		}
		if(rank == 16){
			 plot(0.5:8.5,0.5:8.5, type="n", axes=FALSE, xlab="", ylab="",main="16*16 Monster Sudoku")
			 names=matrix(c("R16","R15","R14","R13","R12","R11","R10","R9","R8","R7","R6","R5","R4","R3","R2","R1","C1","C2","C3",
			 "C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16"),ncol=2)
			 rownames<-names[,1]
			 colnames<-names[,2]
			 #++++++++++++++++++++++++++++++++ creat sudoku table +++++++++++++++++++++++++++++#
			 text(par("usr")[1]-0.1, seq(0.5,8.0,by = 0.5)+0.1, adj = 0,labels = rownames,xpd = TRUE, cex=0.7)
  			 text(seq(0.5,8.0,by = 0.5) + 0.1, par("usr")[4]+0.2, adj = 0,labels = colnames,xpd = TRUE, cex=0.7)
			 segments(seq(0.5,8.5,by=0.5), rep(0.5,17),seq(0.5,8.5,by=0.5), rep(8.5,17), col="gold")
			 segments(rep(0.5,17), seq(0.5,8.5,by=0.5), rep(8.5,17), seq(0.5,8.5,by=0.5), col="chocolate")
			 segments(c(0.5,2.5,4.5,6.5,8.5), rep(0.5,6),c(0.5,2.5,4.5,6.5,8.5), rep(8.5,5), lwd=3,col="red")
			 segments(rep(0.5,5), c(0.5,2.5,4.5,6.5,8.5), rep(8.5,5), c(0.5,2.5,4.5,6.5,8.5), lwd=3,col="red")
			 #++++++++++++++++++++++++++++++++ Creat the initial sudoku +++++++++++++++++++++++++++++#
			 for (i in 1:16) for (j in 1:16){
				if (z[i, j] < 10 && z[i, j] >0) { 
           				if (cols[i, j] == "red") 
                				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "X", col = "pink", cex = )
           				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, z[i, j], col = cols[i, j], font = ifelse(cols[i, j] == "blue", 1, 1), 
					cex = ifelse(cols[i, j] == "blue", 1, 1.8))
				}
				if (z[i, j] == 10){
            			if (cols[i, j] == "red") 
                				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "X", col = "pink", cex = )
            			text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "A", col = cols[i, j], font = ifelse(cols[i, j] == "blue", 1, 1), 
						cex = ifelse(cols[i, j] == "blue", 1, 1.8))
				}
				if (z[i, j] == 11){
            			if (cols[i, j] == "red") 
                				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "X", col = "pink", cex = )
            			text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "B", col = cols[i, j], font = ifelse(cols[i, j] == "blue", 1, 1), 
						cex = ifelse(cols[i, j] == "blue", 1, 1.8))
				}
				if (z[i, j] == 12){
            			if (cols[i, j] == "red") 
               				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "X", col = "pink", cex = )
            			text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "C", col = cols[i, j], font = ifelse(cols[i, j] == "blue", 1, 1), 
		       			cex = ifelse(cols[i, j] == "blue", 1, 1.8))
				}
				if (z[i, j] == 13){
          	  			if (cols[i, j] == "red") 
              				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "X", col = "pink", cex = )
            			text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "D", col = cols[i, j], font = ifelse(cols[i, j] == "blue",1, 1), 
						cex = ifelse(cols[i, j] == "blue", 1, 1.8))
				}
				if (z[i, j] == 14){
            			if (cols[i, j] == "red") 
                				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "X", col = "pink", cex = )
            			text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "E", col = cols[i, j], font = ifelse(cols[i, j] == "blue", 1, 1), 
						cex = ifelse(cols[i, j] == "blue", 1, 1.8))
				}
				if (z[i, j] == 15){
            			if (cols[i, j] == "red") 
                				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "X", col = "pink", cex = )
            			text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "F", col = cols[i, j], font = ifelse(cols[i, j] == "blue", 1, 1), 
						cex = ifelse(cols[i, j] == "blue", 1, 1.8))
				}
				if (z[i, j] == 16){
					if (cols[i, j] == "red") 
               				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "X", col = "pink", cex = )
           				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25, "G", col = cols[i, j], font = ifelse(cols[i, j] == "blue", 1, 1), 
						cex = ifelse(cols[i, j] == "blue", 1, 1.8))
				 }
			}
		}	
    }
    if (dsp == "t") {
        tt <- tktoplevel()
        tkwm.title(tt, "Sudoku")
        img <- tkrplot(tt, replot, hscale = hscale, vscale = vscale)
        txt <- tktext(tt, bg = "white", font = "courier")
        scr <- tkscrollbar(tt, repeatinterval = 5, command = function(...) tkyview(txt, 
            ...))
        tkconfigure(txt, yscrollcommand = function(...) tkset(scr, 
            ...))
        tkpack(img, side = "top")
        tkpack(txt, side = "left", fill = "both", expand = TRUE)
        tkpack(scr, side = "right", fill = "y")
        iw <- as.numeric(tcl("image", "width", tkcget(img, "-image")))
        ih <- as.numeric(tcl("image", "height", tkcget(img, "-image")))
    }
    showz <- function() switch(dsp, w = replot(), t = tkrreplot(img))
    showz()
    cc <- function(x, y) {      ####locate the mouse and return to relative cell
        if (dsp == "t") {
            x <- (as.real(x) - 1)/iw
            y <- 1 - (as.real(y) - 1)/ih
        }
		y9 <- c(0.8238897,0.7396631,0.6554364,0.5696783,0.4854517,0.4027564,0.3169984,0.2312403,0.1470137,0.06278699)
		x9 <- c(0.1194487,0.2128637,0.3016846,0.3920368,0.4839204,0.5742726,0.6646249,0.7549771,0.8468607,0.93721301)
		y12 <- c(0.8468606,0.7810107,0.7151608,0.6493108,0.5834609,0.5191423,0.4548238,0.3874425,0.3215925,
				0.2588054,0.1929555,0.1240427,0.0612556)
		x12 <- c(0.1194487,0.1898928,0.26033695,0.33078106,0.39816239,0.47166929,0.54058201,0.61102613,0.67993885,
				0.74885157,0.82235847,0.8897398,0.96018391)
		y16 <- c(0.8223583,0.7718223,0.7258805,0.6799387,0.6309341,0.5849923,0.5375191,0.4900459,0.4410413,0.3920367,
				0.3460948,0.2970902,0.2511484,0.2021438,0.156202,0.1071974,0.0612556)
		x16 <- c(0.1209801,0.171516,0.2235835,0.274119,0.32618688,0.3767228,0.42879026,0.47932626,0.5283308,0.58192965,
				0.63246564,0.68147024,0.735069,0.785605,0.836141,0.8882084,0.937213)
		if (rank == 9){
			xcc=x9
			ycc=y9
			}
		if (rank == 12){
			xcc=x12
			ycc=y12
			}
		if (rank == 16){
			xcc=x16
			ycc=y16
			}
		xc=c(xcc,x)
		yc=c(ycc,y)
		sortxc=sort(xc)
		sortyc=sort(yc)
		c(rank + 2 - which(sortyc == y),which(sortxc == x) - 1)
	}
		
  
    help.txt <- paste(" ?                                 -- this help", 
					  "1-9 & A,B,C,D,E,F,G (for Monster)  -- insert digit", 
                      "0,' '                              -- clear cell", 
                      "r                                  -- replot the puzzle", 
                      "q                                  -- quit",
                      "h (only for classic)               -- hint/help", 
                      "w                                  -- correct wrong entries (show in red)", 
                      "u                                  -- undo last entry",
					  "s                                  -- show number in cell",
                      "x                                  -- show all (solve the puzzle)", "\n", sep = "\n")
    type <- function(s) switch(dsp, w = cat(s), t = {
        tkinsert(txt, "end", s)
        tksee(txt, "end")
    })
    ij <- c(5, 5)
    mm.w <- function(buttons, x, y) {
        ij <<- cc(x, y)
        return()
    }
    mm.t <- function(x, y) {
        ij <<- cc(x, y)
        return()
    }
    kb <- function(A) {
        i <- ij[1]
        j <- ij[2]
        z[cols == "red"] <<- 0
        cols[cols == "red"] <<- "black"
        key <- switch(A, ` ` = "0", `/` = "?", tolower(A))
        if (key == "q") 
            switch(dsp, t = tkdestroy(tt), w = return(1))
        if (key %in% c(0:9, "h", "s")){
			if ((rank == 9) && (i < 1 || i > 9 || j <  1 || j > 9) ){
			   type("Must be over puzzle cell\n")
			   return()
			}
			if ((rank == 12) && (i < 1 || i > 12 || j <  1 || j > 12)){
			   type("Must be over puzzle cell\n")
			   return()
		    }
		    if ((rank == 16) && (i < 1 || i > 16 || j <  1 || j > 16)){
			   type("Must be over puzzle cell\n")
			   return()
			}
        }
        if (key %in% c("s", "x", "w") && !solve) {
            type("Solution not available\n")
            return()
        }
        if (key %in% c(0:9, "s", "x", "w")) 
            ah(z)
        if (key %in% 0:9 && z[i, j] == 0) {
            z[i, j] <<- as.real(key)
            cols[i, j] <<- "black"
        }
		if (key %in% c("a","b","c","d","e","f","g")){
		  if (key == "a"){ 	
			  key = 10
			  if (rank == 9)
				text(j,10-i,"A",cex = 2)
			  if (rank == 12)
				text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25,"A",cex=2)
			  if (rank == 16)
				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25,"A",cex=1)
			}
		  if (key == "b"){
              	  key = 11
			  if (rank == 9)
				text(j,10-i,"B",cex = 2)
			  if (rank == 12)
				text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25,"B",cex=2)
			  if (rank == 16)
				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25,"B",cex=1)
			}
		  if (key == "c"){
              key = 12
			 if (rank == 9)
				text(j,10-i,"C",cex = 2)
			  if (rank == 12)
				text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25,"C",cex=2)
			  if (rank == 16)
				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25,"C",cex=1)
			}
          if (key == "d"){
		      key = 13
			  if (rank == 9)
				text(j,10-i,"D",cex = 2)
			  if (rank == 12)
				text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25,"D",cex=2)
			  if (rank == 16)
				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25,"D",cex=1)
			}
		  if (key == "e"){
              key = 14
			  if (rank == 9)
				text(j,10-i,"E",cex = 2)
			  if (rank == 12)
				text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25,"E",cex=2)
			  if (rank == 16)
				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25,"E",cex=1)
			}
		  if (key == "f"){
			  key = 15
			  if (rank == 9)
				text(j,10-i,"F",cex = 2)
			  if (rank == 12)
				text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25,"F",cex=2)
			  if (rank == 16)
				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25,"F",cex=1)
			}
		  if (key == "g"){
			  key = 16
			  if (rank == 9)
				text(j,10-i,"G",cex = 2)
			  if (rank == 12)
				text(j * 0.6 + 0.25, 7.8 - i * 0.6 + 0.25,"G",cex=2)
			  if (rank == 16)
				text(j * 0.5 + 0.25, 8.5 - i * 0.5 + 0.25,"G",cex=1)
			}
		  z[i, j]  <<-as.real(key)
		  cols[i, j] <<- "black"
		}
        if (key == "?") 
            type(help.txt)
        if (key == "h") 
            type(hintSudoku(z, i, j))
        if (key == "w") {
            cols[z != 0 & z != zz] <<- "red"
            if (!any(cols == "red")) {
                type("All Correct\n")
                return()
            }
        }
        if (key == "u") {
            h <- length(hst)
            z <<- hst[[h]]
            if (h > 1) 
                hst <<- hst[-h]
        }
        if (key == "s") {
            z[i, j] <<- zz[i, j]
            cols[i, j] <<- "green3"
        }
        if (key == "x") {
            cols[z != zz] <<- "green3"
            z <<- zz
        }
        if (key %in% c(0:9, "r", "w", "u", "s", "x")) 
            showz()
        if (solve && all(z == zz)) 
            type("You got it!\n")
        return()
    }
    kb("?")
    if (solve && is.null(zz)) {
        type("Puzzle not solvable.\n")
        solve <- FALSE
    }
    switch(dsp, w = getGraphicsEvent("Ready!", onMouseMove = mm.w, 
        onKeybd = kb), t = {
        tkbind(img, "<Motion>", mm.t)
        tkbind(tt, "<Key>", kb)
        tkwait.window(tt)
    })
    return(invisible(z))
}
