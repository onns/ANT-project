#'
#' Calculates and plots a heatmap of the "confusion matrix" M for the given subset of pages.
#' M[i,j] is the probability that a 1-NN classifier classifies a random query from
#' class i as belonging to j, given our dataset.
#'
#' @param filesPath Path containing the crossDistance matrices.
#' @param webpages List of the pages you want to restrict the classifier to.
#' @param showPercentages Logical. TRUE displays the value of M[i,j] in the
#' corresponding cell. Defaults to FALSE.
#' @param P Timeseries averaging parameter
#' @param weight Either 'none' (default) for simple majority vote, 'inverse' for
#' 1/distance weighting.
#' @param pathOut Path to save the confusion matrix and heatmap image.
#' @param matrixFileName R object file containing the confusion matrix.
#' @param heatmapFileName pdf file to save the heatmap to.

drawHeatmap = function(filesPath,
                       webpages = 'all',
                       showPercentages = F,
                       P = 5,
                       windowType = 'sakoechiba',
                       windowSize = 100,
                       maxLength = 1000,
                       sampFun = 'sum',
                       stepPattern = 'symmetric1',
                       pathOut,
                       matrixFileName = 'confusionMatrix',
                       heatmapFileName = 'heatmap'){

  library(gplots)

  if (!file.exists(matrixFileName)){
    print('Building confusion matrix - this might take a while...', quote = FALSE)

    m <- 0
    countIter <- 0

    files <- list.files(path = filesPath, pattern = paste('crossDTWmatrix', maxLength, P, windowType, stepPattern, windowSize, sep = '_'))

    for (file in files)
    {
      tmp <- stringr::str_split(stringr::str_replace(file, '.dat', ''), '_')[[1]]
      tmp <- stringr::str_split(tmp[length(tmp)], '-')[[1]]
      
      cols <- paste0(tmp[1], '.json')
      rows <- paste0(tmp[2], '.json')

      aux <- readRDS(paste0(filesPath, file))
      
      if (length(webpages) == 1)
      {
        if (webpages == 'all')
        {
          webpages <- stringr::str_replace(rownames(aux), '_[0-9]+\\.json$', '')
        }
      }
      
      aux <- aux[which(rownames(aux) %in% paste(webpages, rows, sep = '_')), which(colnames(aux) %in% paste(webpages, cols, sep = '_'))]
      
      #1 for the assigned 1NN, 0 otherwise
      for (row in 1:nrow(aux)){
        aux[row,][aux[row,]!=min(aux[row,])] <- 0
      }

      m <- m + (aux > 0)
      countIter <- countIter+1
    }

    m <- m/countIter
    print('Saving matrix...', quote=F)
    
    m <- m[order(diag(m)), order(diag(m))] #permute in increasing order of confusion prob
    saveRDS(m, paste0(pathOut, matrixFileName, '.dat'))
  }
  else
  {
    m <- readRDS(matrixFilename)
  }
  
  if (length(webpages) == 1)
  {
    if (webpages == 'all')
    {
      webpages <- as.list(rownames(m))
    }
  }

  if (showPercentages)
  {
    cnote <- as.character(round(100*m)/100)
    cnote[cnote == '0'] <- ''
    cnote <- matrix(cnote, nrow(m))
  }
  else
  {
    cnote <- matrix(rep('', length(m)), nrow(m))
  }

  #Plot heatmap
  q <- length(as.vector(m)[m==0])/length(m)
  ncols <- 10
  brks <- seq(0,1,length.out=100)
  colours <- rev(heat.colors(length(diff(brks))))
  txt <- matrix(as.character(round(m*100)/100), nrow(m))
  txt[txt=='0'] <- NA

  pdf(paste0(pathOut, heatmapFileName, '.pdf'))
  hm1 <- heatmap.2(m,
                 Rowv = FALSE, Colv = FALSE, dendrogram = 'none',
                 breaks = brks,
                 col = colours,
                 trace = 'none',
                 cellnote = cnote,
                 notecex = 1,
                 notecol = 'black',
                 margins = c(5,5),
                 offsetRow = 1, offsetCol = 1,
                 key = FALSE,
                 labCol = FALSE,
                 labRow = stringr::str_replace(rownames(m), '_[0-9]+\\.json', ''),
                 lmat = rbind(c(5,4,2), c(6,1,3)), lhei = c(1,5), lwid = c(1,7,1))

  dev.off()
}
