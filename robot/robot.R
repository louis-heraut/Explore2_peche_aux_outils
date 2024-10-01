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


#    _    _    _               _    _            
#   /_\  | |_ | |_  ___  _ _  | |_ (_) ___  _ _  
#  / _ \ |  _||  _|/ -_)| ' \ |  _|| |/ _ \| ' \ 
# /_/ \_\ \__| \__|\___||_||_| \__||_|\___/|_||_| ____________________
#
# Ce script permet d'automatiser l'exploration d'un site web. 
# Cependant, en réutilisant tout ou partie de ce script, vous 
# assumez l'entière responsabilité des risques suivants :
#
# - Violation des conditions d'utilisation : L'exploration non 
# autorisée d'un site peut entraîner des sanctions légales ou un
# blocage de votre accès.
#
# - Atteinte à la vie privée : Le scraping de données sensibles peut
# violer les lois sur la protection des données.
#
# - Surcharge des serveurs : Un taux de requêtes non contrôlé peut
# entraîner une surcharge et être interprété comme une attaque, avec
# des conséquences légales.
#
# - Blocage IP : Les mécanismes anti-bots peuvent bloquer votre
# adresse IP.
#
# - Risques juridiques : Vous pourriez être tenu responsable de
# toute violation des droits d'auteur ou autres lois.
#
# En utilisant ce script, vous reconnaissez et acceptez que vous êtes
# seul responsable de toute action ou conséquence légale liée à son
# exécution.
# ____________________________________________________________________


# Load necessary library
if(!require(rvest)) install.packages("rvest", dependencies=TRUE)
if(!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
if(!require(tidyr)) install.packages("tidyr", dependencies=TRUE)


# Base URL for the root directory
DRIAS_base_url = "https://climatedata.umr-cnrm.fr/public/dcsc/projects/DRIAS/EXPLORE2-Hydro/"


crawl_url = function (base_url) {
    webpage = read_html(base_url)
    urls = webpage %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href")
    urls = urls[-1]
    urls = paste0(base_url, urls)
    names(urls) = gsub("[/]$", "",
                       gsub(base_url, "", urls))
    return (urls)
}


## INDICATEURS _______________________________________________________
crawl_DRIAS_indicateurs = function(base_url, sleep=0.5) {

    urls = crawl_url(base_url)
    urls = urls[!grepl("(AQUI-FR)|(MONA)|(Recharge)", urls)]
    HM = gsub("EXPLORE2-2024_", "", names(urls))
    URL = dplyr::tibble(HM=HM, url=urls)
    Sys.sleep(sleep)

    URL$tmp = NA
    for (i in 1:nrow(URL)) {
        url = paste0(URL$url[i], "Indicateurs_Debit/")
        hm = URL$HM[i]
        urls_tmp = crawl_url(url)
        URL_tmp = dplyr::tibble(EXP=names(urls_tmp),
                                url=urls_tmp)
        URL$tmp[URL$HM == hm] = list(URL_tmp)
        Sys.sleep(sleep)
    }
    URL = tidyr::unnest(dplyr::select(URL, -url), tmp)

    URL$tmp = NA
    for (i in 1:nrow(URL)) {
        url = URL$url[i]
        hm = URL$HM[i]
        exp = URL$EXP[i]
        urls_tmp = crawl_url(url)
        files = names(urls_tmp)
        files_info = strsplit(files, "_")
        Indicateurs = paste0(sapply(files_info, "[", 2),
                             "_",
                             sapply(files_info, "[", 3))
        BC = sapply(files_info, "[", 5)
        URL_tmp = dplyr::tibble(indicateur=Indicateurs,
                                BC=BC,
                                url=urls_tmp)
        URL$tmp[URL$HM == hm &
                URL$EXP == exp] = list(URL_tmp)
        Sys.sleep(sleep)
    }
    URL = tidyr::unnest(dplyr::select(URL, -url), tmp)
    
    URL = dplyr::relocate(URL, EXP, .before=HM)
    URL = dplyr::relocate(URL, BC, .before=HM)

    return (URL)
}

URL_DRIAS_indicateurs = crawl_DRIAS_indicateurs(DRIAS_base_url)
write.table(URL_DRIAS_indicateurs,
            file=file.path("robot", "URL_DRIAS_indicateurs.csv"),
            quote=TRUE, sep=",",
            row.names=FALSE)


## PROJECTIONS _______________________________________________________
crawl_DRIAS_projections = function(base_url, sleep=0.5) {

    urls = crawl_url(base_url)
    urls = urls[!grepl("(AQUI-FR)|(MONA)|(Recharge)", urls)]
    HM = gsub("EXPLORE2-2024_", "", names(urls))
    URL = dplyr::tibble(HM=HM, url=urls)
    Sys.sleep(sleep)
    
    URL$tmp = NA
    for (i in 1:nrow(URL)) {
        url = URL$url[i]
        hm = URL$HM[i]
        urls_tmp = crawl_url(url)
        urls_tmp = urls_tmp[!grepl("Indicateurs",
                                   urls_tmp) &
                            !grepl("SAFRAN",
                                   urls_tmp)]
        URL_tmp = dplyr::tibble(GCM=names(urls_tmp),
                                url=urls_tmp)
        URL$tmp[URL$HM == hm] = list(URL_tmp)
        Sys.sleep(sleep)
    }
    URL = tidyr::unnest(dplyr::select(URL, -url), tmp)

    URL$tmp = NA
    for (i in 1:nrow(URL)) {
        url = URL$url[i]
        hm = URL$HM[i]
        gcm = URL$GCM[i]
        urls_tmp = crawl_url(url)
        URL_tmp = dplyr::tibble(RCM=names(urls_tmp),
                                url=urls_tmp)
        URL$tmp[URL$HM == hm &
                URL$GCM == gcm] = list(URL_tmp)
        Sys.sleep(sleep)
    }
    URL = tidyr::unnest(dplyr::select(URL, -url), tmp)

    URL$tmp = NA
    for (i in 1:nrow(URL)) {
        url = URL$url[i]
        hm = URL$HM[i]
        gcm = URL$GCM[i]
        rcm = URL$RCM[i]
        urls_tmp = crawl_url(url)
        URL_tmp = dplyr::tibble(EXP=names(urls_tmp),
                                url=urls_tmp)
        URL$tmp[URL$HM == hm &
                URL$GCM == gcm &
                URL$RCM == rcm] = list(URL_tmp)
        Sys.sleep(sleep)
    }
    URL = tidyr::unnest(dplyr::select(URL, -url), tmp)
    
    URL$tmp = NA
    for (i in 1:nrow(URL)) {
        url = URL$url[i]
        hm = URL$HM[i]
        gcm = URL$GCM[i]
        rcm = URL$RCM[i]
        exp = URL$EXP[i]
        urls_tmp = crawl_url(url)
        files = names(urls_tmp)
        files_info = strsplit(files, "_")
        Variables = sapply(files_info, "[", 1)
        BC = sapply(files_info, "[", 8)
        Ok = Variables == "debit"
        urls_tmp = urls_tmp[Ok]
        Variables = Variables[Ok]
        BC = BC[Ok]
        BC = gsub("France-", "", BC)
        URL_tmp = dplyr::tibble(variable=Variables,
                                BC=BC,
                                url=urls_tmp)
        URL$tmp[URL$HM == hm &
                URL$GCM == gcm &
                URL$RCM == rcm &
                URL$EXP == exp] = list(URL_tmp)
        Sys.sleep(sleep)
    }
    URL = tidyr::unnest(dplyr::select(URL, -url), tmp)
    
    URL = dplyr::relocate(URL, EXP, .before=HM)
    URL = dplyr::relocate(URL, BC, .after=RCM)
    URL = dplyr::relocate(URL, HM, .after=BC)

    return (URL)
}

URL_DRIAS_projections = crawl_DRIAS_projections(DRIAS_base_url)
write.table(URL_DRIAS_projections,
            file=file.path("robot", "URL_DRIAS_projections.csv"),
            quote=TRUE, sep=",",
            row.names=FALSE)
