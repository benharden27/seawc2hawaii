library(sea)
library(tidyverse)
master_folder <- "~/data/SEA/wc2hawaii"
subfolders <- list.files(master_folder, pattern = "S[0-9]{3}")

for (i in 1:length(subfolders)) {

  print(i)
  # set route folder
  root_folder <- file.path(master_folder,subfolders[i])

  ctd_fold <- file.path(root_folder,"CTD","Cnv")
  if(length(list.files(ctd_fold, "\\.cnv")) == 0) {
    ctd_fold <- file.path(root_folder,"cnv")
    if(length(list.files(ctd_fold, "\\.cnv")) == 0) {
      ctd <- NULL
    } else {
      ctd <- sea::read_ctd_fold(ctd_fold)
    }
  } else {
    ctd <- sea::read_ctd_fold(ctd_fold)
  }

  a<-list(ctd=ctd)

  # a$adcp$u <- ifelse(a$adcp$u>1,NA,a$adcp$u)
  # a$adcp$v <- ifelse(a$adcp$v>1,NA,a$adcp$v)

  for (j in 1:length(a$ctd)) {

    all_fields <- names(a$ctd[[j]]@metadata$dataNamesOriginal)
    ii <- stringr::str_which(all_fields,"oxygen")
    oxygen_mL <- NA
    oxygen_mM <- NA
    if(length(ii)>0) {
      for (iii in ii) {
        original <- a$ctd[[j]]@metadata$dataNamesOriginal[[iii]]
        if(stringr::str_detect(original,"sbeox0Mm")) {
          oxygen_mM <- a$ctd[[j]]@data[[iii]]
        }
        if(stringr::str_detect(original,"sbeox0ML")) {
          oxygen_mL <- a$ctd[[j]]@data[[iii]]
        }
      }
    }

    a$ctd[[j]]@data$oxygen <- oxygen_mM
    a$ctd[[j]]@data$oxygen2 <- oxygen_mL

    if(is.null(a$ctd[[j]]@data$par)) {
      par = NA
    } else {
      par <- a$ctd[[j]]@data$par
    }


    station <- paste0(subfolders[i],"_",stringr::str_pad(a$ctd[[j]]@metadata$station,3,pad = "0"))

    if(is.null(a$ctd[[j]]@data$theta)) {
      a$ctd[[j]]@data$theta <- oce::swTheta(a$ctd[[j]]@data$salinity,a$ctd[[j]]@data$temperature,
                                            a$ctd[[j]]@data$pressure)
    }

    ii <- stringr::str_which(all_fields,"fluor")
    if(length(ii) > 0) {
      for (iii in ii) {
        original <- a$ctd[[j]]@metadata$dataNamesOriginal[[iii]]
        if(stringr::str_detect(original,"flSP")) {
          a$ctd[[j]]@data$fluorescence <- a$ctd[[j]]@data[[iii]]
        } else {
          a$ctd[[j]]@data[[iii]] <- NA
        }
      }
    }

    if(is.null(a$ctd[[j]]@data$fluorescence)) {
      a$ctd[[j]]@data$fluorescence <- NA
    }

    if(is.null(a$ctd[[j]]@data$oxygen)) {
      a$ctd[[j]]@data$oxygen <- NA
    }
    ctd_add <- tibble::tibble(dep = a$ctd[[j]]@data$depth,
                              pres = a$ctd[[j]]@data$pressure,
                              temp = a$ctd[[j]]@data$temperature,
                              theta = a$ctd[[j]]@data$theta,
                              sigtheta = a$ctd[[j]]@data$sigmaTheta,
                              sal = a$ctd[[j]]@data$salinity,
                              fluor = a$ctd[[j]]@data$fluorescence,
                              par = par,
                              oxygen = a$ctd[[j]]@data$oxygen,
                              oxygen2 = a$ctd[[j]]@data$oxygen2,
                              lon = a$ctd[[j]]@metadata$longitude,
                              lat = a$ctd[[j]]@metadata$latitude,
                              station = station,
                              cruise = subfolders[i])
    if (j == 1){
      ctd <- ctd_add
    } else {
      ctd <- dplyr::bind_rows(ctd,ctd_add)
    }
  }

  a$ctd2 <- ctd


  # then get datasheets
  files <- list.files(file.path(root_folder,"SHIPDATA"),pattern = "\\.xls")

  # hourly work
  hourly_file <- find_datasheet(files,"(H|h)ourlywork")
  if (length(hourly_file) == 1) {
    hourly <- try(read_hourly(file.path(root_folder,"SHIPDATA",hourly_file)))
    if(inherits(hourly,"try-error")) {
      warning("Hourly file cannot be opened. Returning NULL dataset.")
      hourly <- NULL
    }
  } else {
    hourly <- NULL
  }

  # hydrowork
  hydro_file <- find_datasheet(files,"(H|h)ydrowork")
  if (length(hydro_file) == 1) {
    hydro <- try(read_hydrocast(file.path(root_folder,"SHIPDATA",hydro_file)))
    if(inherits(hydro,"try-error")) {
      warning("Hydrowork file cannot be opened. Returning NULL dataset.")
      hydro <- NULL
    }
  } else {
    hyrdo <- NULL
  }

  # Nueston
  neuston_file <- find_datasheet(files,"(N|n)euston")
  if (length(neuston_file) == 1) {
    neuston <- try(sea::read_neuston(file.path(root_folder,"SHIPDATA",neuston_file)))
    if(inherits(neuston,"try-error")) {
      warning("Neuston file cannot be opened. Returning NULL dataset.")
      neuston <- NULL
    }
  } else {
    neuston <- NULL
  }

  # surfsamp
  surf_file <- find_datasheet(files,"(S|s)urf")
  if (length(neuston_file) == 1) {
    surf <- try(sea::read_surfsamp(file.path(root_folder,"SHIPDATA",surf_file)))
    if(inherits(surf,"try-error")) {
      warning("Surfsamp file cannot be opened. Returning NULL dataset.")
      surf <- NULL
    }
  } else {
    surf <- NULL
  }
#
#   if (i == 8) {
#     ii <- which.min(hourly$lat)
#     hourly$lat[1:ii] <- -hourly$lat[1:ii]
#   }

  a$hourly <- hourly
  a$neuston <- neuston
  a$surf <- surf
  a$hydro <- hydro

  assign(subfolders[i],a)

}

library(usethis)
for (name in subfolders)
  do.call("use_data", list(as.name(name), overwrite = TRUE))
