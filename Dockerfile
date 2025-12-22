FROM rocker/shiny:4.3.2

RUN apt-get update && apt-get install -y \
  libgdal-dev libgeos-dev libproj-dev libudunits2-dev \
  libcurl4-openssl-dev libssl-dev pandoc \
  chromium \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server

COPY renv.lock /srv/shiny-server/renv.lock
RUN R -e "install.packages('renv'); renv::restore()"

COPY . /srv/shiny-server/

EXPOSE 3838
