FROM ocaml/opam:debian-ocaml-5.1 AS base

ARG USER_NAME
ARG UID
ARG GID
ARG OCAML_COMPILER
USER root

USER root
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update
RUN apt-get update && apt-get install -y sudo git inotify-tools build-essential libssl-dev liblapacke-dev libopenblas-dev pkg-config zlib1g-dev libffi-dev libgmp-dev libpcre3-dev tmux fish software-properties-common wget netbase emacs autoconf

    # Pyenv
RUN apt-get install -y libssl-dev

    # Setup user
ENV USER_NAME=${USER_NAME}
ENV OCAML_COMPILER=${OCAML_COMPILER}

RUN echo "${USER_NAME} ALL=(ALL:ALL) NOPASSWD:ALL" > /etc/sudoers.d/${USER_NAME}
RUN chmod 440 /etc/sudoers.d/${USER_NAME}
RUN chown root:root /etc/sudoers.d/${USER_NAME}
    # Create a fake groupid with host groupid+000
RUN groupadd -g ${GID}000 ${USER_NAME}
RUN useradd -m ${USER_NAME} --uid=${UID} --gid=${GID}000 -s /bin/bash
# RUN adduser --uid ${UID} --disabled-password --gecos '' ${USER_NAME}
RUN passwd -l ${USER_NAME}
RUN chown -R ${USER_NAME}:${USER_NAME} /home/${USER_NAME}

USER ${USER_NAME}
ENV HOME=/home/${USER_NAME}

RUN opam init -a -y -c ${OCAML_COMPILER} --disable-sandboxing
RUN opam install -y dune bonsai cohttp-async brr cohttp-lwt-unix pyml
WORKDIR /home/${USER_NAME}/magizhchi
