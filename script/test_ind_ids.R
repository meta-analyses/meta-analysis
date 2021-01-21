for (uid in unique(dataset$id)){
  td <- filter(dataset, id == uid)

  # By default run the analysis with Hamling method to approximate covariance
  res <- metaAnalysis(td, ptitle = "", returnval = T, covMethed = T, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(res) || is.na(res)){
    res <- metaAnalysis(td, ptitle = "", returnval = T, covMethed = F, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase maxQuantile to 90th percent
  if (is.null(res) || is.na(res)){
    res <- metaAnalysis(td, ptitle = "", returnval = T, covMethed = F, minQuantile = 0, maxQuantile = 0.9, lout = 1000)
  }

}