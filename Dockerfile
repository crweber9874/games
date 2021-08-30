FROM alpine:3.14

# Alpine update & upgrade
RUN apk update && apk upgrade

FROM rocker/rstudio:latest

#RUN R -e "install.packages('lavaan', dependencies=TRUE)"
#RUN R -e "install.packages('tidyverse', dependencies=TRUE)"


