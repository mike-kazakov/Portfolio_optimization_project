for (t in min_t:max_t) {
  for (k in min_k:max_k) {
    skip_to_next <- FALSE
    tryCatch({
      result <- ER_sim(t,k)
      sims <- rbind(sims,result)
    },error=function(e){skip_to_next <<- TRUE})
  }
}
rownames(sims) <- NULL
write.csv(sims, "sims.csv")