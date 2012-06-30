generatesudoku <- function(rank = 9, Date = "2012-05-15", web = TRUE){
if(rank !=9 && rank != 12 && rank !=16)
	stop("Please input a correct rank, 9 or 12 or 16!")
if(web){
	if (rank == 9)
       	th = paste("http://www.dailysudoku.com/sudoku/pdf/",substr(Date,1,4),sep='')
	if (rank == 12 || rank == 16){
		th = paste("http://www.dailysudoku.com/sudoku/","monster/pdf/",sep='')
		th = paste(th,substr(Date,1,4),sep='')
	}	
	th = paste(th,"/",sep='')
	th = paste(th,substr(Date,6,7),sep='')
	th = paste(th,"/",sep='')
	th = paste(th,Date,sep='')
	th = paste(th,"_S2_N1.pdf",sep='')
	th = url(th)
	tmp <- readLines(th)      #When you connect to the Internet, you can fetch a sudoku on the Internet.
	close(th)                 #This sudoku can be 9*9 or 12*12 or 16*16
    tmp1 <- grep('Tf',tmp,value=TRUE,fixed=TRUE)
    if (rank != 9)
		tmp2 = tmp1[5:length(tmp1) - 1]
    if (rank == 9)
		tmp2 = tmp1[4:length(tmp1) - 1]
    judge = as.numeric(substr(tmp1[10],8,10))
    if (judge == 15 && rank !=12)
		stop("This day did not has corresponding puzzle. If you change rank into 12 will be OK!")
	if (judge == 11 && rank !=16)
		stop("This day did not has corresponding puzzle. If you change rank into 16 will be OK!")
	tmp43 = rep(0,length(tmp2))
	tmp53 = rep(0,length(tmp2))
	tmp02 = rep(0,length(tmp2))
	for (i in 2:length(tmp2)){
		for (j in 1:40){
			if (substr(tmp2[i],j,j + 1) == "Tf") tmp43[i] = substr(tmp2[i],j + 3,j + 9)
			if (substr(tmp2[i],j,j + 1) == "Td") tmp02[i] = substr(tmp2[i],j + 4,j + 4)
			if (substr(tmp2[i],j,j + 1) == "Td"){
				for (n in 2:10) 
					if (substr(tmp2[i],j - n,j - n) == " "){
						tmp53[i] = substr(tmp2[i],j - n + 1,j - n + 3)
						break
                     }
			}
		}		
	}
	tmp02[which(tmp02 == "A")] = "10"
	tmp02[which(tmp02 == "B")] = "11"
	tmp02[which(tmp02 == "C")] = "12"
	tmp02[which(tmp02 == "D")] = "13"
	tmp02[which(tmp02 == "E")] = "14"
	tmp02[which(tmp02 == "F")] = "15"
	tmp02[which(tmp02 == "0")] = "16"
	if (rank != 16){
		tmp4 = as.numeric(tmp43[-1])
		tmp5 = as.numeric(tmp53[-1])
		tmp6 = as.numeric(tmp02[-1])
	}
	if (rank == 16){
		for (i in 1:3) 
			if (tmp53[i] == "0" && tmp53[i+1] != "0"){
				if (i == 1){
					tmp4 = as.numeric(tmp43[-i])
					tmp5 = as.numeric(tmp53[-i])
					tmp6 = as.numeric(tmp02[-i])
				}
				if (i == 2){
					tmp4 = as.numeric(tmp43[-c(1,i)])
					tmp5 = as.numeric(tmp53[-c(1,i)])
					tmp6 = as.numeric(tmp02[-c(1,i)])
				}
				if (i == 3){
					tmp4 = as.numeric(tmp43[-c(1,2,i)])
					tmp5 = as.numeric(tmp53[-c(1,2,i)])
					tmp6 = as.numeric(tmp02[-c(1,2,i)])
				}
			}
	}
	ii = tmp4
	jj = tmp5
	kk = tmp6
	zz = matrix(0,rank,rank)
	mii = min(ii)
	mij = max(jj)
	mai = max(ii)
	maj = min(jj)
	dsi = sort(ii)
	dsii = rep(0,length(ii)-1)
	dsj = sort(jj)
	dsjj = rep(0,length(jj)-1)
	for (n in 1:(length(ii)-1))
		dsii[n] = dsi[n + 1]-dsi[n]
	for (n in 1:(length(ii)-1))
		dsjj[n] = dsj[n + 1]-dsj[n]
	di = max(dsii) %/% 0.1/10
	dj = max(dsjj) %/% 0.1/10
	for (i in 1:rank){
		for (j in 1:rank){
			for (k in 1:length(kk)){
				if (i <= (rank/2)){ 
					if (abs(((i - 1) * di + mii) - ii[k]) <= 8 && abs((mij - (j - 1) * dj) - jj[k]) <= 8)
					zz[j,i] = kk[k]
				}
				else{
					if (abs((mai - (rank - i) * di) - ii[k]) <= 8 && abs((maj + (rank - j) * dj) - jj[k]) <= 8)
					zz[j,i] = kk[k]
				}
			}
		}
	}
}
if (web == FALSE){           #When you do not connect to the Internet,it will generate a sudoku suited your suggestion.
	if (rank == 9){
		Nblank = round(runif(1,30,64))
		z <- c(1:9,4:9,1:3,7:9,1:6,2:9,1,5:9,1:4,8:9,1:7,3:9,1:2,6:9,1:5,9,1:8)
		z <- matrix(sample(9)[z], 9,9)
		for (i in 1:5) 
			z <- z[replicate(3, sample(3)) + 3*rep(sample(0:2), each=3),replicate(3, sample(3)) + 3*rep(sample(0:2), each=3)]
		for (bi in seq(0,6,3)) for (bj in seq(0,6,3)) {
			idx <- data.matrix(expand.grid(bi + 1:3, bj + 1:3))
			z[idx[sample(1:9, Nblank%/%9), ]] <- 0
	    }
		zz = z
	}
	if (rank == 16){
		Nblank = round(runif(1,80,150))
		z <- c(1:16,5:16,1:4,9:16,1:8,13:16,1:12,2:16,1,6:16,1:5,10:16,1:9,14:16,1:13,3:16,1:2,7:16,1:6,11:16,1:10,15:16,1:14,4:16,1:3,8:16,1:7,12:16,1:11,16,1:15)
		z <- matrix(sample(16)[z], 16,16)
		for (i in 1:5) 
			z <- z[replicate(4, sample(4)) + 4*rep(sample(0:3), each=4),replicate(4, sample(4)) + 4*rep(sample(0:3), each=4)]
		for (bi in seq(0,12,4)) for (bj in seq(0,12,3)) {
			idx <- data.matrix(expand.grid(bi + 1:4, bj + 1:4))
			z[idx[sample(1:16, Nblank%/%16), ]] <- 0
       }
	   zz = z
    }
    if (rank == 12)
		stop("If web is FALSE, the rank of puzzle can only be 9 or 16!")	

}
if (web == TRUE)
	zzz = list(Rank = rank, Date = Date, Puzzle = zz)
else
	zzz = list(Rank = rank, Date = NULL, Puzzle = zz)
zzz
}
