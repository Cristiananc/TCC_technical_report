summaries_genealogy_reconstruction <- function(n.sim, reconstructions, prior.name, ntaxa) {
  summaries_df <- do.call(rbind, lapply(1:n.sim, function(i){
    data.frame(reconstructions[[i]]$summary, replicate = i,
               prior = prior.name, ntaxa = ntaxa)}))
  return(summaries_df)
}