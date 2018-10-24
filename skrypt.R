distance <- function(model, data) {
  R1 = model[[2]]
  actual_length = length(model) / 3
  for (j in 2:actual_length) {
    R1 = rbind(R1, model[[j*3 - 1]])
  }
  Q1 = qr.Q(qr(as.matrix(R1)))
  R2 = qr.R(qr(as.matrix(R1)))
  Q2list = list()
  for(j in 1:actual_length) {
    Q2list[[j]] = Q1[(j-1)*2+1:2,]
  }
  Q3list = list()
  for(j in 1:actual_length) {
    Q3list[[j]] = model[[j*3-2]] %*% Q2list[[j]]
  }
  Vlist = list()
  for(j in 1:actual_length) {
    Vlist[[j]] = t(Q3list[[j]]) %*% model[[j*3]]
  }
  sumV = Vlist[[1]]
  for(j in 2:actual_length) {
    sumV = sumV+Vlist[[j]]
  }
  parameters = solve(R2) %*% sumV

  result = solve(R2) %*% sumV
  calculated = t(result) %*% t(data$X)
  square_sum = sum((data$y - calculated)^2)
  square_avg = square_sum / length(data$y)
  print("----SQUARE AVG-------")
  print(square_avg)
}

linearRegCV2dim <- function(X, y, folds, n) {
  start.time <- Sys.time()
  mlist = list()
  Xlist = list()
  ylist = list()
  neutral = list(
    Q=qr.Q(qr(matrix(rep(0, len=4), nrow = 2))), 
    R=qr.R(qr(matrix(rep(0, len=4), nrow = 2))),
    y = c(0, 0)
  )
  size = n %/% folds
  for (j in 1:folds) {
    Xlist[[j]] = X[(j-1)*size+1:size,]
    ylist[[j]] = y[(j-1)*size+1:size]
    qr_partial = qr(as.matrix(Xlist[[j]]))
    mlist[[j]] = list(
      Q=qr.Q(qr_partial), 
      R=qr.R(qr_partial),
      y = y[(j-1)*size+1:size]
    )
  }

  plist = list()
  plist[[1]] = c(neutral)
  for (j in 2:(folds + 1)) {
    plist[[j]] = append(mlist[[j-1]], plist[[j-1]])
  }

  slist = list()
  slist[[folds + 1]] = c(neutral)
  
  for (j in folds:1) {
    slist[[j]] = append(mlist[[j]], slist[[j+1]])
  }

  m = list()
  for (j in 1:folds) {
    m = append(plist[[j]], slist[[j+1]])
    distance(m, list(X=Xlist[[j]], y=ylist[[j]]))
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}
