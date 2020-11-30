FROM rocker/r-ver:4.0.3

RUN apt-get update && apt-get -y install --no-install-recommends \
        git \
        libcurl4-openssl-dev \
        libssl-dev \
        libv8-dev \
        libxml2-dev \
        zlib1g-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

COPY docker/bin /usr/local/bin/

RUN install_packages --repo=https://mrc-ide.github.io/drat \
        R6 \
        dde \
        cinterpolate \
        deSolve \
        devtools \
        digest \
        glue \
        jsonlite \
        jsonvalidate \
        knitr \
        mockery \
        pkgbuild \
        pkgload \
        ring \
        rlang \
        rmarkdown \
        testthat

COPY . /src
RUN R CMD INSTALL /src
