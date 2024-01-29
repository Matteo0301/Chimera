FROM debian:bullseye-slim as base

ENV LANG C.UTF-8

ARG GHC_VERSION=9.4.7
ARG CABAL_VERSION=latest
ARG HLS_VERSION=latest
ARG LLVM_VERSION=15

ENV USERNAME=vscode \
    USER_UID=1000 \
    USER_GID=1000 \
    DEBIAN_FRONTEND=noninteractive \
    GHC_VERSION=${GHC_VERSION} \
    CABAL_VERSION=${CABAL_VERSION} \
    HLS_VERSION=${HLS_VERSION} \
    LLVM_VERSION=${LLVM_VERSION}

RUN ulimit -n 8192

RUN apt-get update
RUN apt-get install -y --no-install-recommends apt-utils bash build-essential ca-certificates curl gcc git gnupg libffi-dev libffi7 libgmp-dev libgmp-dev libgmp10 libicu-dev libncurses-dev libncurses5 libnuma1 libnuma-dev libtinfo5 lsb-release make procps software-properties-common sudo wget xz-utils z3 zlib1g-dev libpcre3 libpcre3-dev


RUN groupadd --gid ${USER_GID} ${USERNAME} && \
    useradd -ms /bin/bash -K MAIL_DIR=/dev/null --uid ${USER_UID} --gid ${USER_GID} -m ${USERNAME} && \
    echo ${USERNAME} ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/${USERNAME} && \
    chmod 0440 /etc/sudoers.d/${USERNAME}

FROM base as llvm

RUN wget -O /tmp/llvm.sh https://apt.llvm.org/llvm.sh && chmod +x /tmp/llvm.sh && /tmp/llvm.sh ${LLVM_VERSION} all && rm /tmp/llvm.sh

RUN for i in /usr/bin/*-15; do sudo ln -s $i ${i%-*}; done

FROM llvm as ghcup

USER ${USER_UID}:${USER_GID}
WORKDIR /home/${USERNAME}
ENV PATH="/home/${USERNAME}/.local/bin:/home/${USERNAME}/.cabal/bin:/home/${USERNAME}/.ghcup/bin:$PATH"

RUN echo "export PATH=${PATH}" >> /home/${USERNAME}/.profile

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=yes \
    BOOTSTRAP_HASKELL_NO_UPGRADE=yes \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=yes \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK_HOOK=yes \
    BOOTSTRAP_HASKELL_GHC_VERSION=${GHC_VERSION} \
    BOOTSTRAP_HASKELL_CABAL_VERSION=${CABAL_VERSION} \
    BOOTSTRAP_HASKELL_INSTALL_HLS=${HLS_VERSION}

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

FROM ghcup as tooling

# RUN ghcup install ghc ${GHC_VERSION} --set
# RUN ghcup install cabal ${CABAL_VERSION} --set
# RUN ghcup install hls ${HLS_VERSION} --set

# Update Cabal.
RUN cabal update && cabal new-install cabal-install


# Configure cabal
# RUN cabal user-config update -f && \
#     sed -i 's/-- ghc-options:/ghc-options: -haddock/g' ~/.cabal/config

FROM tooling as packages

# Install global packages.
# Versions are pinned, since we don't want to accidentally break anything (by always installing latest).
RUN cabal install \
    haskell-dap\
    ghci-dap \
    haskell-debug-adapter \
    hlint \
    apply-refact \
    retrie \
    fourmolu

RUN cabal v2-install stan --install-method=copy --overwrite-policy=always
RUN cabal install headroom
RUN cabal install weeder

ENV DEBIAN_FRONTEND=dialog

ENTRYPOINT ["/bin/bash"]
