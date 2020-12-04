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
    Dataset_ccls_per_cpd_successful <- readRDS("./www/Dataset_Summaries/ccls_per_cpd_passed_QC.rds")
    Dataset_cpds_per_ccl_successful <- readRDS("./www/Dataset_Summaries/cpds_per_ccl_passed_QC.rds")
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
    Cell_Line_Harm <- as.data.frame(read_xlsx("./www/Harmonized_CCL_Data_v1.0.xlsx", na = "NA"))
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
    Compound_Harm <- as.data.frame(read_excel("./www/Harmonized_Compound_Data_v1.0.xlsx"))
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
    
  #Creating list of cell lines (successfully tested) available for each compound
    Dataset_ccls_successful <- list(NULL)
    datasets <- names(compound_ccl_availability_successful[[1]])
    for(i in 1:length(datasets)){
      Dataset_ccls_successful[[i]] <- sort(unique(unlist(lapply(compound_ccl_availability_successful, function(x){return(x[datasets[i]])}))))
    }
    names(Dataset_ccls_successful) <- datasets
    
  #Creating list of compounds (successfully tested) available for each compound
    Dataset_compounds_successful <- list(NULL)
    for(i in 1:length(datasets)){
      Dataset_compounds_successful[[i]] <- sort(unique(unlist(lapply(ccl_compound_availability_successful, function(x){return(x[datasets[i]])}))))
    }
    names(Dataset_compounds_successful) <- datasets
    
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
  
#Pre-rendering plots for welcome page to make the app feel more responsive
  welcome_cancer_type_plots <- list(NULL)
  for(i in 1:length(datasets)){
    temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% Dataset_ccls_successful[[datasets[i]]],]
    temp_dataset_ccl_data$Simple_Cancer_Type[temp_dataset_ccl_data$Simple_Cancer_Type == "unknown"] <- "unknown cancer type"
    plot_data <- as.data.frame.table(table(temp_dataset_ccl_data$Simple_Cancer_Type))
    plot_data <- plot_data[order(plot_data$Freq, decreasing = TRUE),]
    plot_data <- rbind(plot_data[! plot_data$Var1 == "unknown cancer type",], plot_data[plot_data$Var1 == "unknown cancer type",])
    
    Gender_Unknown <- NA
    Gender_Female <- NA
    Gender_Male <- NA
    for(j in 1:nrow(plot_data)){
      Gender_Unknown[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Sex unspecified",])
      Gender_Female[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Female",])
      Gender_Male[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Male",])
    }
    
    plot_data$Var1 <- factor(plot_data$Var1, levels = plot_data$Var1)
    
    welcome_cancer_type_plots[[i]] <- plot_ly(x = plot_data$Var1, y = Gender_Unknown, type = "bar", name = "Unknown Gender", marker = list(color = "lightgray")) %>%
                                      add_trace(y = Gender_Male, name = "Male", marker = list(color = rgb(65,105,225, maxColorValue = 255))) %>%
                                      add_trace(y = Gender_Female, name = "Female", marker = list(color = rgb(186,85,211, maxColorValue = 255))) %>%
                                      layout(yaxis = list(title = "# of Cell Lines"), xaxis = list(tickangle = 45), barmode = "stack", margin = list(b = 150, l = 50))
    
  }
  names(welcome_cancer_type_plots) <- datasets
  
  welcome_age_plots <- list(NULL)
  for(i in 1:length(datasets)){
    temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% Dataset_ccls_successful[[datasets[i]]],]
    completeness <- paste0("(data for ", nrow(temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$Numeric_Age_in_Years),]), " of ", nrow(temp_dataset_ccl_data), " cell lines)")
    temp_dataset_ccl_data <- temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$Numeric_Age_in_Years),]
    
    p <- ggplot(temp_dataset_ccl_data, aes(x = Numeric_Age_in_Years, y = after_stat(scaled))) +
                geom_density(color = "darkblue", fill = "lightblue") +
                theme_light() +
                labs(x = paste("Patient Age (in Years) when Cell Line was Derived ", completeness), y = "Scaled Density")

    welcome_age_plots[[i]] <- ggplotly(p)
  }
  names(welcome_age_plots) <- datasets

  welcome_ancestry_plots <- list(NULL)
  for(i in 1:length(datasets)){
    temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% Dataset_ccls_successful[[datasets[i]]],]
    completeness <- paste0("(data for ", nrow(temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$African_Ancestry),]), " of ", nrow(temp_dataset_ccl_data), " cell lines)")
    temp_dataset_ccl_data <- temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$African_Ancestry),]
    # plot_data <- data.frame(Ancestry_Percentage = c(temp_dataset_ccl_data$African_Ancestry,
    #                          temp_dataset_ccl_data$Native_American_Ancestry,
    #                          temp_dataset_ccl_data$`East_Asian_(North)_Ancestry`,
    #                          temp_dataset_ccl_data$`East_Asian_(South)_Ancestry`,
    #                          temp_dataset_ccl_data$South_Asian_Ancestry,
    #                          temp_dataset_ccl_data$`European_(North)_Ancestry`,
    #                          temp_dataset_ccl_data$`European_(South)_Ancestry`)*100,
    #                        Ancestry = c(rep("African", nrow(temp_dataset_ccl_data)),
    #                          rep("Native American", nrow(temp_dataset_ccl_data)),
    #                          rep("East Asian (North)", nrow(temp_dataset_ccl_data)),
    #                          rep("East Asian (South)", nrow(temp_dataset_ccl_data)),
    #                          rep("South Asian", nrow(temp_dataset_ccl_data)),
    #                          rep("European (North)", nrow(temp_dataset_ccl_data)),
    #                          rep("European (South)", nrow(temp_dataset_ccl_data))))
    # 
    # p <- ggplot(plot_data, aes(x = Ancestry_Percentage, group = Ancestry, fill = Ancestry)) +
    #             geom_density(adjust=1.5, position="fill") +
    #             theme_light() +
    #             labs(x = paste("Cell Line Ancestry Percentage", completeness), y = "Scaled Density")
    # n_ccls <- nrow(temp_dataset_ccl_data)
    # p <- ggplot(plot_data, aes(x = Ancestry_Percentage, y = after_stat(scaled), group = Ancestry, color = Ancestry)) +
    #             geom_density(adjust=1.5) +
    #             theme_light() +
    #             labs(x = paste("Cell Line Ancestry Percentage", completeness), y = "Scaled Density")
    
    plot_data <- temp_dataset_ccl_data[,grepl("_Ancestry", colnames(temp_dataset_ccl_data))]*100
    rownames(plot_data) <- temp_dataset_ccl_data$Harmonized_Cell_Line_ID
    plot_data <- plot_data[hclust(dist(plot_data))$order,]
    colnames(plot_data) <- gsub("_Ancestry", "", colnames(plot_data))
    colnames(plot_data) <- gsub("_", " ", colnames(plot_data))
    x <- factor(rownames(plot_data), levels = rownames(plot_data))
    
    # fig <- plot_ly(x = x, y = plot_data[,1], name = colnames(plot_data)[1], type = "scatter", mode = "none", stackgroup = "one")
    # for(j in 2:ncol(plot_data)){
    #   fig <- fig %>% add_trace(y = plot_data[,j], name = colnames(plot_data)[j])
    # }
    # fig <- fig %>% layout(xaxis = list(title = "Tested Cell Lines", showticklabels = FALSE), yaxis = list(title = "% Ancestry Makeup"))
    # plot_colors <- RColorBrewer::brewer.pal(12, "Paired")
    plot_colors <- c("#6A3D9A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#A6CEE3", "#1F78B4")
    fig <- plot_ly(x = x, y = plot_data[,1], name = colnames(plot_data)[1], type = "bar", marker = list(color = plot_colors[1])) %>%
      add_trace(y = plot_data[,2], name = colnames(plot_data)[2], marker = list(color = plot_colors[2])) %>%
      add_trace(y = plot_data[,3], name = colnames(plot_data)[3], marker = list(color = plot_colors[3])) %>%
      add_trace(y = plot_data[,4], name = colnames(plot_data)[4], marker = list(color = plot_colors[4])) %>%
      add_trace(y = plot_data[,5], name = colnames(plot_data)[5], marker = list(color = plot_colors[5])) %>%
      add_trace(y = plot_data[,6], name = colnames(plot_data)[6], marker = list(color = plot_colors[6])) %>%
      add_trace(y = plot_data[,7], name = colnames(plot_data)[7], marker = list(color = plot_colors[7])) %>%
      layout(barmode = "stack", xaxis = list(title = paste("Tested Cell Lines", completeness), showticklabels = FALSE), yaxis = list(title = "% Ancestry Makeup"), bargap = 0, legend = list(traceorder = "normal"))
    
    # welcome_ancestry_plots[[i]] <- ggplotly(p)
    welcome_ancestry_plots[[i]] <- fig
  }
  names(welcome_ancestry_plots) <- datasets
  
  welcome_clinical_phase_plots <- list(NULL)
  for(i in 1:length(datasets)){
    temp_dataset_compound_data <- Simple_Compound_Harm[Simple_Compound_Harm$Harmonized_Compound_Name %in% Dataset_compounds_successful[[datasets[i]]],]
    temp_dataset_compound_data$Compound_Clinical_Phase[temp_dataset_compound_data$Compound_Clinical_Phase == "NA"] <- "Unknown"
    
    plot_data <- as.data.frame.table(table(temp_dataset_compound_data$Compound_Clinical_Phase))
    plot_data$Var1 <- factor(plot_data$Var1, levels = c("Unknown", "Preclinical", "Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3", "Phase 3", "Launched", "Withdrawn"))
    # plot_data <- plot_data[match(c("Unknown", "Preclinical", "Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3", "Phase 3", "Launched", "Withdrawn"), plot_data$Var1),]
    
    fig <- plot_ly(x = plot_data$Var1, y = plot_data$Freq, type = "bar")
    welcome_clinical_phase_plots[[i]] <- layout(fig,
                                        xaxis = list(title = paste("Clinical Phase")),
                                        yaxis = list(title = paste("# of Compounds")))
  }
  names(welcome_clinical_phase_plots) <- datasets
  
  n_Compounds_Per_Cell_Line_Plots <- list(NULL)
  for(i in 1:length(datasets)){
    #For attempted tests
      selected_n_cpds_per_ccl <- Dataset_cpds_per_ccl[[datasets[i]]]
      plot_data <- as.data.frame.table(table(selected_n_cpds_per_ccl$x), stringsAsFactors = FALSE)
      plot_data$Var1 <- as.numeric(plot_data$Var1)
      if(nrow(plot_data) > 1){
        plot_range <- min(plot_data$Var1):max(plot_data$Var1)
      } else {
        plot_range <- (plot_data$Var1-5):(plot_data$Var1+5)
      }
      missing_values <-  plot_range[! plot_range %in% plot_data$Var1]
      extra_data <- cbind(missing_values, 0)
      colnames(extra_data) <- colnames(plot_data)
      plot_data <- rbind(plot_data, extra_data)
      plot_data <- plot_data[order(plot_data$Var1, decreasing = FALSE),]
    #For successful tests
      selected_n_cpds_per_ccl_successful <- Dataset_cpds_per_ccl_successful[[datasets[i]]]
      plot_data_successful <- as.data.frame.table(table(selected_n_cpds_per_ccl_successful$x), stringsAsFactors = FALSE)
      plot_data_successful$Var1 <- as.numeric(plot_data_successful$Var1)
      if(nrow(plot_data_successful) > 1){
        plot_range <- min(plot_data_successful$Var1):max(plot_data_successful$Var1)
      } else {
        plot_range <- (plot_data_successful$Var1-5):(plot_data_successful$Var1+5)
      }
      missing_values <-  plot_range[! plot_range %in% plot_data_successful$Var1]
      extra_data <- cbind(missing_values, 0)
      colnames(extra_data) <- colnames(plot_data_successful)
      plot_data_successful <- rbind(plot_data_successful, extra_data)
      plot_data_successful <- plot_data_successful[order(plot_data_successful$Var1, decreasing = FALSE),]
      
    full_plot_data <- merge(plot_data, plot_data_successful, by = "Var1", all.x = TRUE, all.y = TRUE)
    full_plot_data$Freq.x[is.na(full_plot_data$Freq.x)] <- 0
    full_plot_data$Freq.y[is.na(full_plot_data$Freq.y)] <- 0

    
    n_Compounds_Per_Cell_Line_Plots[[i]] <- plot_ly(data = full_plot_data, x = ~Var1, y = ~Freq.x, type = "bar", name = "Attempted") %>%
                                            add_trace(y = ~Freq.y, name = "Passed QC") %>%
                                            layout(xaxis = list(title = paste("# of Compounds Tested per Cell Line")), yaxis = list(title = paste("Cell Line Count")), barmode = 'group')

  }
  names(n_Compounds_Per_Cell_Line_Plots) <- datasets
  
  n_Cell_Lines_Per_Compound_Plots <- list(NULL)
  for(i in 1:length(datasets)){
    #For attempted tests
      selected_n_ccls_per_cpd <- Dataset_ccls_per_cpd[[datasets[i]]]
      plot_data <- as.data.frame.table(table(selected_n_ccls_per_cpd$x), stringsAsFactors = FALSE)
      plot_data$Var1 <- as.numeric(plot_data$Var1)
      if(nrow(plot_data) > 1){
        plot_range <- min(plot_data$Var1):max(plot_data$Var1)
      } else {
        plot_range <- (plot_data$Var1-5):(plot_data$Var1+5)
      }
      missing_values <-  plot_range[! plot_range %in% plot_data$Var1]
      extra_data <- cbind(missing_values, 0)
      colnames(extra_data) <- colnames(plot_data)
      plot_data <- rbind(plot_data, extra_data)
      plot_data <- plot_data[order(plot_data$Var1, decreasing = FALSE),]
    #For successful tests
      selected_n_ccls_per_cpd_successful <- Dataset_ccls_per_cpd_successful[[datasets[i]]]
      plot_data_successful <- as.data.frame.table(table(selected_n_ccls_per_cpd_successful$x), stringsAsFactors = FALSE)
      plot_data_successful$Var1 <- as.numeric(plot_data_successful$Var1)
      if(nrow(plot_data_successful) > 1){
        plot_range <- min(plot_data_successful$Var1):max(plot_data_successful$Var1)
      } else {
        plot_range <- (plot_data_successful$Var1-5):(plot_data_successful$Var1+5)
      }
      missing_values <-  plot_range[! plot_range %in% plot_data_successful$Var1]
      extra_data <- cbind(missing_values, 0)
      colnames(extra_data) <- colnames(plot_data_successful)
      plot_data_successful <- rbind(plot_data_successful, extra_data)
      plot_data_successful <- plot_data_successful[order(plot_data_successful$Var1, decreasing = FALSE),]
      
    full_plot_data <- merge(plot_data, plot_data_successful, by = "Var1", all.x = TRUE, all.y = TRUE)
    full_plot_data$Freq.x[is.na(full_plot_data$Freq.x)] <- 0
    full_plot_data$Freq.y[is.na(full_plot_data$Freq.y)] <- 0
    
    n_Cell_Lines_Per_Compound_Plots[[i]] <- plot_ly(data = full_plot_data, x = ~Var1, y = ~Freq.x, type = "bar", name = "Attempted") %>%
                                            add_trace(y = ~Freq.y, name = "Passed QC") %>%
                                            layout(xaxis = list(title = paste("# of Cell Lines Tested per Compound")), yaxis = list(title = paste("Compound Count")), barmode = 'group')
  }
  names(n_Cell_Lines_Per_Compound_Plots) <- datasets
  
  Dataset_Residual_Standard_Error_Plots <- list(NULL)
  for(i in 1:length(datasets)){
    RSE_plot_data <- data.frame(Dataset_rse[[datasets[i]]])
    colnames(RSE_plot_data) <- "RSE"
    p <- ggplot(RSE_plot_data, aes(x = RSE, y = after_stat(scaled))) +
                geom_density(color = "darkblue", fill = "lightblue") +
                theme_light() +
                labs(x = paste0("Residual Standard Error for Dose-Response Curves (n = ", nrow(RSE_plot_data), ")"), y = "Scaled Density")
    Dataset_Residual_Standard_Error_Plots[[i]] <- ggplotly(p)
  }
  names(Dataset_Residual_Standard_Error_Plots) <- datasets
  