solvesudoku <- function (z, Date = NULL, web = TRUE){
rank = ncol(z)
if ((rank == 9 || rank == 16) && !require(sudoku))        #When the rank is 9 or 16, we can use the solve function to solve it!
	stop("'sudoku' package needed\n")
if (require(sudoku))
	if (rank == 9 || rank ==16){	
	zz=solveSudoku(z)
	}

if (rank == 12){		     #When the rank is 12, we will fetch the solution from Internet!
	if (web == FALSE)
		stop("It can not work without Internet!")
	if (web == TRUE){
	    	if (is.null(Date))
			stop("Please input a date!(format YY-MM-DD e.g 2010-04-1)")
		th = paste("http://www.dailysudoku.com/sudoku/","monster/pdf/",sep='')
		th = paste(th,substr(Date,1,4),sep='')
		th = paste(th,"/",sep='')
		th = paste(th,substr(Date,6,7),sep='')
		th = paste(th,"/",sep='')
		th = paste(th,Date,sep='')
		th = paste(th,"_S2_N1_X.pdf",sep='')
		th = url(th)
		tmp <- readLines(th)
		close(th)
		tmp2 <- grep('Tf',tmp,value=TRUE,fixed=TRUE)
		tmp222 = 0;N = 0
		for (n in 10:length(tmp2)-4){
			if (tmp222 == 0) 
				for (i in 5:40) 
					if (substr(tmp2[n],i,i+3) == "http"){
						tmp222 = 1
						tmp22 = rep(0,length(tmp2)-n-5)
						N = n
					}
			if (tmp222 == 1) 
				tmp22[n+1-N] = tmp2[n+3]
		}
		tmp3=tmp22[5:length(tmp2)-1]
		tmp43 = rep(0,length(tmp22))
		tmp53 = rep(0,length(tmp22))
		tmp02 = rep(0,length(tmp22))
		for (i in 2:length(tmp22)){
			for (j in 1:40){
				if (substr(tmp22[i],j,j+1) == "Tf") 
					tmp43[i] = substr(tmp22[i],j+3,j+9)
				if (substr(tmp22[i],j,j+1) == "Td") 
					tmp02[i] = substr(tmp22[i],j+4,j+4)
				if (substr(tmp22[i],j,j+1) == "Td"){
					for (n in 2:10) 
						if (substr(tmp22[i],j-n,j-n) == " "){
							tmp53[i] = substr(tmp22[i],j-n+1,j-n+3)
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
		for (i in 1:3) 
			if (tmp53[i] == "0"&&tmp53[i+1] != "0"){
				if(i==1){
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
		ii = tmp4
		jj = tmp5
		kk = tmp6
		zz = matrix(0,rank,rank)
		mii = min(ii);mij = max(jj)
		mai = max(ii);maj = min(jj)
		dsi = sort(ii);dsii = rep(0,length(ii)-1)
		dsj = sort(jj);dsjj = rep(0,length(jj)-1)
		for (n in 1:(length(ii)-1)) 
			dsii[n]=dsi[n+1]-dsi[n]
		for (n in 1:(length(ii)-1)) 
			dsjj[n]=dsj[n+1]-dsj[n]
		di = max(dsii)%/%0.1/10;dj = max(dsjj)%/%0.1/10
		for (i in 1:rank){
		   for (j in 1:rank){
			  for (k in 1:length(kk)){
				 if (i <= (rank/2)){
					if (abs(((i - 1)* di + mii) - ii[k]) <= 13 && abs((mij - (j - 1)*dj) - jj[k]) <= 13)
						zz[j,i] = kk[k]
				 }
				 else{
					if(abs((mai - (rank - i)*di) - ii[k])<=13&&abs((maj + (rank - j)*dj) - jj[k])<=13)
						zz[j,i] = kk[k]
				 }
			   }
		   }
	    }
    }
  }
  zz
}

