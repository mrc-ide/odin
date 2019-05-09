FROM rocker/r-ver:3.6.0

RUN apt-get update && apt-get -y install \
        libcurl4-openssl-dev \
        libssl-dev \
        libv8-dev \
        libxml2-dev \
        zlib1g-dev

COPY docker/bin /usr/local/bin/

RUN install_packages --repo=https://mrc-ide.github.io/drat \
        R6 \
        dde \
        cinterpolate \
        crayon \
        deSolve \
        devtools \
        digest \
        jsonlite \
        jsonvalidate \
        knitr \
        ring \
        rmarkdown \
        testthat

RUN install_remote ropensci/jsonvalidate@use-ajv

COPY . /src
RUN R CMD INSTALL /src
