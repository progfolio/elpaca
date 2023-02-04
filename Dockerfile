ARG VERSION
FROM silex/emacs:$VERSION-ci
ARG REF=nil
RUN useradd --create-home --shell /bin/bash docker
USER docker
WORKDIR /home/docker
RUN mkdir -p .emacs.d/
WORKDIR /home/docker/.emacs.d/
RUN echo "(setq package-enable-at-startup nil)" > early-init.el
RUN curl "https://raw.githubusercontent.com/progfolio/elpaca/master/doc/init.el" > init.el
RUN sed -i "s|:ref nil|:ref ${REF}|g" init.el
