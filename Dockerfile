# Build with: docker build --build-arg VERSION=NN.N -t elpaca-NN.N .
# Add --no-cache arg to force init regeneration.
ARG VERSION
FROM silex/emacs:$VERSION-ci
RUN useradd --create-home --shell /bin/bash docker
USER docker
WORKDIR /home/docker/.emacs.d/
RUN curl --remote-name-all \
    "https://raw.githubusercontent.com/progfolio/elpaca/master/doc/early-init.el" \
    "https://raw.githubusercontent.com/progfolio/elpaca/master/doc/init.el"
ARG REF=nil
RUN sed -i "s|:ref nil|:ref ${REF}|g" init.el
