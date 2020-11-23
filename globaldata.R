#Loading Libraries
  require(shiny)
  require(shinyhelper)
  require(shinyWidgets)
  require(shinybusy)
  require(readr)
  require(readxl)
  require(plotly)
  require(ggplot2)
  require(openxlsx)
  require(drc)
  
#Loading data for ui
  #Dataset Summaries
    Dataset_Summaries <- read.delim("./www/Dataset_Summaries/Dataset_Summaries.txt", sep = "\t")
    Dataset_ccls_per_cpd <- readRDS("./www/Dataset_Summaries/ccls_per_cpd.rds")
    Dataset_cpds_per_ccl <- readRDS("./www/Dataset_Summaries/cpds_per_ccl.rds")
    Dataset_rse <- readRDS("./www/Dataset_Summaries/dataset_rse.rds")
  #Compound and cell line availability
    compound_ccl_availability <- readRDS("./www/CCL_Availability_by_Compound.rds")
    ccl_compound_availability <- readRDS("./www/Compound_Availability_by_Cell_Line.rds")
    compound_ccl_availability_successful <- readRDS("./www/CCL_Availability_Successful_by_Compound.rds")
    ccl_compound_availability_successful <- readRDS("./www/Compound_Availability_Successful_by_Cell_Line.rds")
  #Dataset tested concentration information
    Dataset_Tested_Concentrations <- readRDS("./www/Dataset_Compound_Concentrations.rds")
    for(i in c(2:69, 74)){
      Dataset_Tested_Concentrations[,i] <- as.numeric(Dataset_Tested_Concentrations[,i])
    }
  #Csustained data
    Csustained <- as.data.frame(read_xlsx("./www/Csustained.xlsx", na = "NA"))
  #Cell line and compound information
    Cell_Line_Harm <- as.data.frame(read_xlsx("./www/Harmonized_CCL_Data_11_3_2020_manual_updates.xlsx", na = "NA"))
      Cell_Line_Harm$Dataset[grepl("CTRPv2", Cell_Line_Harm$Dataset)] <- "CTRPv2"
      Cell_Line_Harm$Dataset[grepl("GDSC1", Cell_Line_Harm$Dataset)] <- "GDSC1"
      Cell_Line_Harm$Dataset[grepl("GDSC2", Cell_Line_Harm$Dataset)] <- "GDSC2"
      Cell_Line_Harm$Dataset[grepl("PRISM_Repurposing", Cell_Line_Harm$Dataset)] <- "PRISM_Repurposing"
      Cell_Line_Harm$African_Ancestry <- as.numeric(Cell_Line_Harm$African_Ancestry)
      Cell_Line_Harm$Native_American_Ancestry <- as.numeric(Cell_Line_Harm$Native_American_Ancestry)
      Cell_Line_Harm$`East_Asian_(North)_Ancestry` <- as.numeric(Cell_Line_Harm$`East_Asian_(North)_Ancestry`)
      Cell_Line_Harm$`East_Asian_(South)_Ancestry` <- as.numeric(Cell_Line_Harm$`East_Asian_(South)_Ancestry`)
      Cell_Line_Harm$South_Asian_Ancestry <- as.numeric(Cell_Line_Harm$South_Asian_Ancestry)
      Cell_Line_Harm$`European_(North)_Ancestry` <- as.numeric(Cell_Line_Harm$`European_(North)_Ancestry`)
      Cell_Line_Harm$`European_(South)_Ancestry` <- as.numeric(Cell_Line_Harm$`European_(South)_Ancestry`)
    Compound_Harm <- as.data.frame(read_excel("./www/Harmonized_Compound_Data_11_1_2020_manual_updates.xlsx"))
      Compound_Harm$Dataset[grepl("CTRPv2", Compound_Harm$Dataset)] <- "CTRPv2"
      Compound_Harm$Dataset[grepl("GDSC1", Compound_Harm$Dataset)] <- "GDSC1"
      Compound_Harm$Dataset[grepl("GDSC2", Compound_Harm$Dataset)] <- "GDSC2"
      Compound_Harm$Dataset[grepl("PRISM_Repurposing", Compound_Harm$Dataset)] <- "PRISM_Repurposing"
  #Loading filename maps
    Compound_Filenames <- readRDS("./www/Compound_Filename_Master.rds")
    Cell_Line_Filenames <- readRDS("./www/Cell_Line_Filename_Master.rds")
  #Removing compound and cell line records when no files exist for those cell lines and compounds (i.e. no results)
    Cell_Line_Harm <- Cell_Line_Harm[Cell_Line_Harm$Harmonized_Cell_Line_ID %in% Cell_Line_Filenames$cell_lines,]
    Compound_Harm <- Compound_Harm[Compound_Harm$Harmonized_Compound_Name %in% Compound_Filenames$drugs,]
  #Making data frames of cell line and compound options/synonyms
    Compound_Option_df <- unique(Compound_Harm[,c("Harmonized_Compound_Name", "Compound_Synonyms")])
    for(i in 1:nrow(Compound_Option_df)){
      proper_name <- Compound_Option_df$Harmonized_Compound_Name[i]
      synonyms <- unlist(strsplit(Compound_Option_df$Compound_Synonyms[i], ":\\|:"))
      synonyms <- c(synonyms, gsub(" ", "", synonyms))
      synonyms <- c(synonyms, gsub("[[:punct:]]", "", synonyms))
      synonyms <- synonyms[! tolower(synonyms) %in% tolower(proper_name)]
      synonyms <- synonyms[! duplicated(tolower(synonyms))]
      if(length(synonyms) > 0){
        Compound_Option_df$Compound_Synonyms[i] <- paste0(proper_name, " [[", paste(synonyms, collapse = "; "), "]]")
      } else {
       Compound_Option_df$Compound_Synonyms[i] <- proper_name
      }
    }

    Cell_Line_Option_df <- unique(Cell_Line_Harm[,c("Harmonized_Cell_Line_ID", "Synonyms")])
    for(i in 1:nrow(Cell_Line_Option_df)){
      proper_name <- Cell_Line_Option_df$Harmonized_Cell_Line_ID[i]
      synonyms <- unlist(strsplit(Cell_Line_Option_df$Synonyms[i], ":\\|:"))
      synonyms <- c(synonyms, gsub(" ", "", synonyms))
      synonyms <- c(synonyms, gsub("[[:punct:]]", "", synonyms))
      synonyms <- synonyms[! tolower(synonyms) %in% tolower(proper_name)]
      synonyms <- synonyms[! duplicated(tolower(synonyms))]
      if(length(synonyms) > 0){
        Cell_Line_Option_df$Synonyms[i] <- paste0(proper_name, " [[", paste(synonyms, collapse = "; "), "]]")
      } else {
        Cell_Line_Option_df$Synonyms[i] <- proper_name
      }
    }

  #Making Simplified versions of compound and cell line harmonizers for sorting purposes
    Simple_Cell_Line_Harm <- unique(Cell_Line_Harm[,c("Harmonized_Cell_Line_ID", "Gender", "Numeric_Age_in_Years", "Diseases", "Simple_Cancer_Type", "African_Ancestry", "Native_American_Ancestry", "East_Asian_(North)_Ancestry", "East_Asian_(South)_Ancestry", "South_Asian_Ancestry", "European_(North)_Ancestry", "European_(South)_Ancestry")])
    Cell_Line_Diseases <- strsplit( Simple_Cell_Line_Harm$Diseases, ":\\|:")
      names(Cell_Line_Diseases) <-  Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID
    
    Simple_Compound_Harm <- unique(Compound_Harm[,c("Harmonized_Compound_Name", "Compound_Molecular_Targets", "Compound_MOA", "Compound_Clinical_Phase")])
    Compound_Molecular_Targets <- strsplit(Simple_Compound_Harm$Compound_Molecular_Targets, ":\\|:")
      names(Compound_Molecular_Targets) <- Simple_Compound_Harm$Harmonized_Compound_Name
    Unique_Compound_Molecular_Targets <- sort(unique(unlist(Compound_Molecular_Targets)))
    Compound_MOAs <- strsplit(Simple_Compound_Harm$Compound_MOA, ":\\|:")
      names(Compound_MOAs) <- Simple_Compound_Harm$Harmonized_Compound_Name
    Unique_Compound_MOAs <- sort(unique(unlist(Compound_MOAs)))
    
#Writing function for 4-parameter log-logistic curve
  #b = slope
  #c = lower asymptote
  #d = upper asymptote
  #e = EC50 (note, this is NOT log(EC50))
  #b_c_d_e should be input as a character vector with each element consisting of the string
  #"b_c_d_e" with the numeric values separated by an underscore in that order
  #x should be dose (not log(dose))
  ll.4 <- Vectorize(function(x, b_c_d_e){
    b_c_d_e <- as.data.frame(do.call(rbind, lapply(strsplit(b_c_d_e, "_"), as.numeric)))
    colnames(b_c_d_e) <- c("b", "c", "d", "e")
    if(x > 0){
      return((b_c_d_e$c + (b_c_d_e$d - b_c_d_e$c)/(1 + exp(b_c_d_e$b*(log(x)-log(b_c_d_e$e))))))
    } else {
      return(b_c_d_e$d)
    }
  })
  
#Writing function for 4-parameter log-logistic curve
  #b = slope
  #c = lower asymptote
  #d = upper asymptote
  #e = EC50 (note, this is NOT log(EC50))
  #b_c_d_e should be input as a character vector with each element consisting of the string
  #"b_c_d_e" with the numeric values separated by an underscore in that order
  #x should be log(dose)
  ll.4.AUC <- function(x, b_c_d_e){
    b_c_d_e <- as.data.frame(do.call(rbind, lapply(strsplit(b_c_d_e, "_"), as.numeric)))
    colnames(b_c_d_e) <- c("b", "c", "d", "e")
    return((b_c_d_e$c + (b_c_d_e$d - b_c_d_e$c)/(1 + exp(b_c_d_e$b*(x-log(b_c_d_e$e))))))
  }
  
#Writing function to calculate AUC values
  #lower and upper should be given as raw concentrations (NOT log(conc))
  AUC <- Vectorize(function(b_c_d_e, lower, upper){
    lower <- log(as.numeric(lower))
    upper <- log(as.numeric(upper))
    Results <- try(integrate(ll.4.AUC, lower, upper, b_c_d_e = b_c_d_e), silent = TRUE)
    if(! class(Results) == "try-error"){
      return(Results$value/(upper-lower))
    } else {
      Results <- try(integrate(ll.4.AUC, lower, upper, b_c_d_e = b_c_d_e, rel.tol = 1e-15), silent = TRUE)
      if(! class(Results) == "try-error"){
        return(Results$value/(upper-lower))
      } else {
        return(NA)
      }
    }
  })
  
#Writing function to unique and remove NA, "NA", and "" values
  clean_vector <- function(x){
    return(unique(x[! x %in% c("", NA, "NA")]))
  }
  
  
#For some reason, having trouble getting R to recognize drc specific version of estfun. Writing required predict.drc functions explicitly to get around problem.
  estfun.drc <- function (x, ...) 
  {
      xderiv1 <- x$deriv1
      indexMat0 <- x$indexMat2
      if (is.matrix(indexMat0)) {
          indMat <- t(indexMat0)
      }
      else {
          indMat <- t(matrix(indexMat0, nrow = 1))
      }
      colnames(indMat) <- colnames(x$indexMat)
      curveID <- x$dataList[["curveid"]]
      xderiv2Fct <- function(xderiv1, indMat, curveID) {
          xderiv2 <- xderiv1[, rep(1:ncol(xderiv1), apply(indMat, 
              1, function(x) {
                  length(unique(x))
              }))]
          cnInd <- colnames(indMat)
          for (i in 1:ncol(indMat)) {
              xderiv2[curveID != cnInd[i], indMat[, i]] <- 0
          }
          xderiv2
      }
      if (identical(x$type, "continuous")) {
          xderiv2 <- xderiv2Fct(xderiv1, indMat, curveID)
          rval <- (weights(x) * residuals(x)) * xderiv2
      }
      if (identical(x$type, "binomial")) {
          nTotal <- weights(x)
          nObs <- x[["dataList"]][["resp"]] * nTotal
          fittedVal <- fitted(x)
          rval0 <- nObs/fittedVal + (nObs - nTotal)/(1 - fittedVal)
          rval0[!is.finite(rval0)] <- 0
          xderiv2 <- xderiv2Fct(xderiv1, indMat, curveID)
          rval <- xderiv2 * rval0
          rval
      }
      if (identical(x$type, "Poisson")) {
          resp <- x[["dataList"]][["resp"]]
          fittedVal <- fitted(x)
          rval0 <- resp/fittedVal - 1
          xderiv2 <- xderiv2Fct(xderiv1, indMat, curveID)
          rval <- xderiv2 * rval0
          rval
      }
      if (identical(x$type, "event")) {
          Ft1 <- predict(x, data.frame(x$data[, 1], x$data[, 5]))
          Ft2 <- predict(x, data.frame(x$data[, 2], x$data[, 5]))
          Ft2[!is.finite(x$data[, 2])] <- 1
          diffF <- Ft2 - Ft1
          DFt1.0 <- x[["fct"]]$deriv1(x$data[, 1], t(x[["parmMat"]][, 
              as.character(x$data[, 5])]))
          DFt2.0 <- x[["fct"]]$deriv1(x$data[, 2], t(x[["parmMat"]][, 
              as.character(x$data[, 5])]))
          DFt2.0[!is.finite(x$data[, 2]), ] <- 0
          diffDF.0 <- DFt2.0 - DFt1.0
          diffDF <- xderiv2Fct(diffDF.0, indMat, x$data[, 5])
          rval <- (x$data[, 3]/diffF) * diffDF
          if (FALSE) {
              dataList <- x[["data"]]
              xderiv1 <- x[["fct"]]$deriv1(dataList[, 1], 
                  t(x[["parmMat"]][, as.character(dataList[, 
                    4])]))
              print(xderiv1)
              xderiv2 <- xderiv2Fct(xderiv1, indMat, data[, 4])
              resp <- c(0, dataList[, 3])
              fittedVal <- c(0, predict(dataList[, 2]))
              rval <- xderiv2 * (diff(resp)/diff(fittedVal))
          }
      }
      colnames(rval) <- names(coef(x))
      rval
  }
  
  bread.drc <- function (x, ...) 
  {
      if (identical(x$type, "continuous")) {
          breadMat <- vcov(x)/(summary(x)$rse[1]^2) * unlist(x$sumList[1])
      }
      else {
          breadMat <- vcov(x) * unlist(x$sumList[1])
      }
      return(breadMat)
  }
  
  meat.drc <- function (x, adjust = FALSE, ...) 
  {
      if (is.list(x) && !is.null(x$na.action)) 
          class(x$na.action) <- "omit"
      psi <- estfun.drc(x, ...)
      k <- NCOL(psi)
      n <- NROW(psi)
      rval <- crossprod(as.matrix(psi))/n
      if (adjust) 
          rval <- n/(n - k) * rval
      rownames(rval) <- colnames(rval) <- colnames(psi)
      return(rval)
  }
  
  
  sandwich <- function (x, bread. = bread.drc, meat. = meat.drc, ...) 
  {
      if (is.list(x) && !is.null(x$na.action)) 
          class(x$na.action) <- "omit"
      if (is.function(bread.)) 
          bread. <- bread.(x)
      if (is.function(meat.)) 
          meat. <- meat.(x, ...)
      n <- NROW(estfun.drc(x))
      return(1/n * (bread. %*% meat. %*% bread.))
  }
  
  predict.drc <- function (object, newdata, se.fit = FALSE, interval = c("none", 
      "confidence", "prediction"), level = 0.95, na.action = na.pass, 
      od = FALSE, vcov. = vcov, ...) 
  {
      interval <- match.arg(interval)
      respType <- object[["type"]]
      dataList <- object[["dataList"]]
      doseDim <- ncol(dataList[["dose"]])
      if (is.null(doseDim)) {
          doseDim <- 1
      }
      if (missing(newdata)) {
          doseVec <- dataList[["dose"]]
          if (identical(respType, "event")) {
              groupLevels <- as.character(dataList[["plotid"]])
          }
          else {
              groupLevels <- as.character(dataList[["curveid"]])
          }
      }
      else {
          dName <- dataList[["names"]][["dName"]]
          if (any(names(newdata) %in% dName)) {
              doseVec <- newdata[, dName]
          }
          else {
              doseVec <- newdata[, 1]
          }
          cName <- dataList[["names"]][["cNames"]]
          if (any(names(newdata) %in% cName)) {
              groupLevels <- as.character(newdata[, cName])
          }
          else {
              groupLevels <- rep(1, nrow(newdata))
          }
      }
      noNewData <- length(groupLevels)
      powerExp <- (object$curve)[[2]]
      if (!is.null(powerExp)) {
          doseVec <- powerExp^doseVec
      }
      parmMat <- object[["parmMat"]]
      pm <- t(parmMat[, groupLevels, drop = FALSE])
      sumObj <- summary(object, od = od)
      vcovMat <- vcov.(object)
      indexMat <- object[["indexMat"]]
      retMat <- matrix(0, noNewData, 4)
      colnames(retMat) <- c("Prediction", "SE", "Lower", 
          "Upper")
      objFct <- object[["fct"]]
      retMat[, 1] <- objFct$fct(doseVec, pm)
      deriv1 <- objFct$deriv1
      if (is.null(deriv1)) {
          return(retMat[, 1])
      }
      if (!identical(interval, "none")) {
          if (identical(respType, "continuous")) {
              tquan <- qt(1 - (1 - level)/2, df.residual(object))
          }
          else {
              tquan <- qnorm(1 - (1 - level)/2)
          }
      }
      if (se.fit || (!identical(interval, "none"))) {
          if (identical(interval, "prediction")) {
              sumObjRV <- sumObj$resVar
          }
          else {
              sumObjRV <- 0
          }
          piMat <- indexMat[, groupLevels, drop = FALSE]
          for (rowIndex in 1:noNewData) {
              parmInd <- piMat[, rowIndex]
              varCov <- vcovMat[parmInd, parmInd]
              dfEval <- deriv1(doseVec[rowIndex], pm[rowIndex, 
                  , drop = FALSE])
              varVal <- dfEval %*% varCov %*% dfEval
              retMat[rowIndex, 2] <- sqrt(varVal)
              if (!se.fit) {
                  retMat[rowIndex, 3:4] <- retMat[rowIndex, 1] + 
                    (tquan * sqrt(varVal + sumObjRV)) * c(-1, 1)
              }
          }
      }
      keepInd <- 1
      if (se.fit) {
          keepInd <- c(keepInd, 2)
      }
      if (!identical(interval, "none")) {
          keepInd <- c(keepInd, 3, 4)
      }
      return(retMat[, keepInd])
  }
  
#Writing a predict function for drc objects that handles errors when calculating SE
  predict.handle.errors.drc <- function(object, newdata, se.fit = F, vcov. = vcov){
    temp <- suppressWarnings(try(predict.drc(object, newdata = newdata, se.fit = se.fit, vcov. = vcov.), silent = T))
    if(length(class(temp)) == 1){
      if(class(temp) == "try-error"){
        temp <- predict.drc(object, newdata = newdata)
        temp <- cbind(temp, rep(NA, length(temp)))
        names(temp) <- c("Prediction", "SE")
      }
    }
    return(temp)
  }
  