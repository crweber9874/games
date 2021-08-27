FROM alpine:3.14

# Alpine update & upgrade
RUN apk update && apk upgrade

#minimal install of python3 and pip3 and clear apk cache
RUN apk --update --no-cache add musl gcc build-base freetype-dev libpng-dev openblas-dev python3-dev python3 \
    py3-pip

#best practice for pip installs is to run pip cache purge after all pip install calls
# Example install procedure is bellow 
# pip install numpy
# pip cache purge