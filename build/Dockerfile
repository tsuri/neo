ARG DEBIAN_VERSION=bullseye
FROM debian:${DEBIAN_VERSION} AS build

COPY scripts /scripts

RUN /scripts/install_dependencies ${DEBIAN_VERSION}

ARG EMACS_VERSION=master
RUN /scripts/download $EMACS_VERSION

ARG EMACS_CONFIG
#RUN cd /emacs && ./autogen.sh  && ./configure $EMACS_CONFIG && make -j8 bootstrap && make -j8 && make install
RUN /scripts/build /emacs $EMACS_CONFIG

#---------------------------------------------------------------------------------------------#

FROM debian:${DEBIAN_VERSION}

COPY scripts /scripts

RUN /scripts/install_dependencies

ARG GROUP_ID=1000
ARG USER_ID=1000
ARG USER=mav

RUN groupadd -g ${GROUP_ID} ${USER}
RUN useradd -l -m -u ${USER_ID} -g ${USER} ${USER}
RUN adduser ${USER} sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN chown --changes --silent --no-dereference --recursive ${USER_ID}:${GROUP_ID} /home/${USER}
USER mav
WORKDIR /home/${USER}

COPY --from=build /usr/local /usr/local

CMD ["emacs", "--debug-init"]
