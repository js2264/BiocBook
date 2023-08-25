ARG BIOC_VERSION
FROM bioconductor/bioconductor_docker:${BIOC_VERSION}
COPY . /opt/pkg

# Install book package 
RUN Rscript -e 'repos <- BiocManager::repositories() ; remotes::install_local(path = "/opt/pkg/", repos=repos, dependencies=TRUE, build_vignettes=FALSE, upgrade=TRUE)'

## Check installed book package with rcmdcheck and BiocCheck following BioC recommendations
RUN Rscript -e 'rcmdcheck::rcmdcheck("/opt/pkg/", args = c("--no-manual", "--no-vignettes", "--timings"), build_args = c("--no-manual", "--keep-empty-dirs", "--no-resave-data"), error_on = "warning", check_dir = "check")'
RUN Rscript -e 'BiocCheck::BiocCheck(dir("check", "tar.gz$$", full.names = TRUE), `quit-with-status` = TRUE, `no-check-R-ver` = TRUE, `no-check-bioc-help` = TRUE)'
