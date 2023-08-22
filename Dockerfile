ARG BIOC_VERSION

FROM bioconductor/bioconductor_docker:${BIOC_VERSION}

COPY . /opt/BiocBook

RUN apt-get update && apt-get install gdebi-core -y
# RUN curl -LO https://quarto.org/download/latest/quarto-linux-amd64.deb && gdebi --non-interactive quarto-linux-amd64.deb
RUN curl -LO https://github.com/quarto-dev/quarto-cli/releases/download/v1.4.309/quarto-1.4.309-linux-amd64.deb && gdebi --non-interactive quarto-1.4.309-linux-amd64.deb
RUN Rscript -e 'install.packages("pak", repos = "https://r-lib.github.io/p/pak/devel/")'
RUN Rscript -e 'write(paste0("R_BIOC_VERSION=", gsub(".[0-9]*$$", "", as.character(packageVersion("BiocVersion")))), paste0(Sys.getenv("R_HOME"), "/etc/Renviron.site"), append = TRUE)'
RUN Rscript -e 'write(paste0("BIOCBOOK_PACKAGE=", gsub(".*: ", "", grep("Package: ", readLines("/opt/BiocBook/DESCRIPTION"), value = TRUE))), paste0(Sys.getenv("R_HOME"), "/etc/Renviron.site"), append = TRUE)'
RUN Rscript -e 'write(paste0("BIOCBOOK_IMAGE=", tolower(Sys.getenv("BIOCBOOK_PACKAGE"))), paste0(Sys.getenv("R_HOME"), "/etc/Renviron.site"), append = TRUE)'
RUN Rscript -e 'pak::pkg_install("/opt/BiocBook/", ask = FALSE, dependencies = c("Depends", "Imports", "Suggests"))'
RUN Rscript -e 'rcmdcheck::rcmdcheck("/opt/BiocBook/", args = c("--no-manual", "--no-vignettes", "--timings"), build_args = c("--no-manual", "--keep-empty-dirs", "--no-resave-data"), error_on = "warning", check_dir = "check")'
RUN Rscript -e 'BiocCheck::BiocCheck(dir("check", "tar.gz$$", full.names = TRUE), `quit-with-status` = TRUE, `no-check-R-ver` = TRUE, `no-check-bioc-help` = TRUE)'
