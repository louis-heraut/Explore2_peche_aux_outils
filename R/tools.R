# Copyright 2024 Louis Héraut (louis.heraut@inrae.fr)*1
#
# *1   INRAE, France
#
# This file is part of Explore2 R toolbox.
#
# Explore2 R toolbox is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# Explore2 R toolbox is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Explore2 R toolbox.
# If not, see <https://www.gnu.org/licenses/>.


get_DRIAS_netcdf = function (Urls, outdir) {
    if (!dir.exists(outdir)) {
        dir.create(outdir, showWarnings=FALSE)
    }
    options(timeout=300)
    nUrls = length(Urls)
    
    for (i in 1:nUrls) {
        url = Urls[i]
        print(paste0(i, "/", nUrls, " -> ",
                     round(i/nUrls*100, 2), "%"))
        file = basename(url)
        format = gsub(".*[.]", "", file)
        path = file.path(outdir, file)
        download.file(url, destfile=path, mode="wb")
        # writeBin(httr::content(httr::GET(url), "raw"), path)
        if (format == "zip") {
            unzip(path, exdir=outdir)
            Paths = list.files(gsub(".zip", "", path),
                               full.names=TRUE)
            file.rename(Paths,
                        file.path(outdir, basename(Paths)))
            unlink(unique(dirname(Paths)), recursive=TRUE)
            unlink(path, recursive=TRUE)
        }
    }
}


read_DRIAS_netcdf = function (Paths, Codes) {
    
    Codes_pattern =
        paste0("(", paste0(Codes, collapse=")|("), ")")
    
    data = dplyr::tibble()
    
    for (path in Paths) {
        NC = ncdf4::nc_open(path)

        variable = gsub("[_].*", "", basename(path))

        Date = ncdf4::ncvar_get(NC, "time") +
            as.Date("1950-01-01")
        nDate = length(Date)

        Codes_NC = ncdf4::ncvar_get(NC, "code")
        nCodes_NC = length(Codes_NC)

        Codes = Codes_NC[grepl(Codes_pattern, Codes_NC)]
        Codes = sort(Codes)
        nCodes = length(Codes)
        
        Id = match(Codes, Codes_NC)
        Id = Id[!is.na(Id)]
        if (all(is.na(Id))) {
            ncdf4::nc_close(NC)
            return (NULL)
        }

        start = min(Id)
        count = max(Id) - start + 1
        Id = Id - start + 1
        X = ncdf4::ncvar_get(NC, variable,
                             start=c(start, 1),
                             count=c(count, -1))
        X = matrix(X, nrow=count)
        X = X[Id,, drop=FALSE]
        X = c(t(X))
        X[is.nan(X)] = NA

        path_info = unlist(strsplit(basename(path), "_"))

        if (grepl("TIMEseries", basename(path))) {
            data_tmp =
                dplyr::tibble(EXP=path_info[9],
                              GCM=path_info[10],
                              RCM=path_info[11],
                              BC=path_info[8],
                              HM=gsub("[.].*", "", path_info[12]),
                              code=rep(Codes, each=nDate),
                              date=rep(Date, times=nCodes),
                              !!variable:=X)
        } else {
            data_tmp =
                dplyr::tibble(EXP=path_info[4],
                              GCM=path_info[3],
                              RCM=path_info[6],
                              BC=path_info[8],
                              HM=path_info[9],
                              code=rep(Codes, each=nDate),
                              date=rep(Date, times=nCodes),
                              !!variable:=X)
        }

        data = dplyr::bind_rows(data, data_tmp)
        
        ncdf4::nc_close(NC)
    }

    # Il y a une erreur dans le formatage des noms de fichier pour J2000
    data$BC = gsub("MF-ADAMONT-SAFRAN-France-1980-2011",
                   "MF-ADAMONT-SAFRAN-1980-2011", data$BC)

    data$climate_chain = paste(data$EXP,
                               data$GCM,
                               data$RCM,
                               data$BC,
                               sep="|")
    data = dplyr::relocate(data, climate_chain, .after=HM)
    data$chain = paste(data$climate_chain,
                       data$HM,
                       sep="|")
    data = dplyr::relocate(data, chain, .after=climate_chain)
    return (data)
}



get_breaks = function(X) {
    breaks = "10 years"
    Xmin = round(lubridate::year(min(X)), -1)
    Xmax = round(lubridate::year(max(X)), -1)
    if (Xmax-Xmin <= 1) {
        Xmin = lubridate::year(X)[1]
        Xmax = lubridate::year(X)[1] + 1
    }
    res = seq.Date(from=as.Date(paste0(Xmin, "-01-01")),
                   to=as.Date(paste0(Xmax, "-01-01")),
                   by=breaks)
    return (res)
}

get_minor_breaks = function(X) {
    breaks = "10 years"
    minor_breaks = "2 years"
    Xmin = round(lubridate::year(min(X)), -1)
    Xmax = round(lubridate::year(max(X)), -1)
    if (Xmax-Xmin <= 1) {
        Xmin = lubridate::year(X)[1]
        Xmax = lubridate::year(X)[1] + 1
    }
    res = seq.Date(from=as.Date(
                       as.Date(paste0(Xmin, "-01-01")) -
                       lubridate::duration(breaks)),
                   to=as.Date(
                       as.Date(paste0(Xmax, "-01-01")) +
                       lubridate::duration(breaks)),
                   by=minor_breaks)
    return (res)
}

delta_labels <- function(x) {
    sapply(x, function(y) {
        if (is.na(y)) {
            y
        } else if (y > 0) {
            paste0("+", y, "%")
        } else if (y == 0) {
            y 
        } else {
            paste0(y, "%")
        }
    })
}
