############################################################
# Dockerfile to build R Simulator
############################################################
FROM rocker/r-ver:4.1.0
LABEL maintainer='carlos.tighe@insight-centre.org'

# GET UPDATES AND PACKAGES NEEDED
RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev

# install plumber
# RUN R -e "install.packages('plumber')"

# Install R packages
RUN install2.r --error \
    readr \
    plumber \
    dplyr \
    stringr\
    socialmixr\
    devtools
# copy app code into docker image
COPY / /

# expose the port on our docker image
# whatever port we expose we need to map the computers port to the docker image port
# we will use port 80 in our docker image because thats generally the port used
EXPOSE 8000

# specify what happens when the container starts
ENTRYPOINT [ "Rscript", "run_api.R" ]
