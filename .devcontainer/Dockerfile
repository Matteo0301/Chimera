FROM debian:latest as base

ENV LANG C.UTF-8

ARG GHC_VERSION=9.6.3
ARG CABAL_VERSION=9.10.0
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
RUN apt-get install -y --no-install-recommends apt-utils bash build-essential ca-certificates curl gcc git gnupg libffi-dev libffi8 libgmp-dev libgmp-dev libgmp10 libicu-dev libncurses-dev libncurses5 libnuma1 libnuma-dev libtinfo5 lsb-release make procps software-properties-common sudo wget xz-utils z3 zlib1g-dev vim

RUN wget -O /tmp/llvm.sh https://apt.llvm.org/llvm.sh && chmod +x /tmp/llvm.sh && /tmp/llvm.sh ${LLVM_VERSION} && rm /tmp/llvm.sh

RUN groupadd --gid ${USER_GID} ${USERNAME} && \
    useradd -ms /bin/bash -K MAIL_DIR=/dev/null --uid ${USER_UID} --gid ${USER_GID} -m ${USERNAME} && \
    echo ${USERNAME} ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/${USERNAME} && \
    chmod 0440 /etc/sudoers.d/${USERNAME}

USER ${USER_UID}:${USER_GID}
WORKDIR /home/${USERNAME}
ENV PATH="/home/${USERNAME}/.local/bin:/home/${USERNAME}/.cabal/bin:/home/${USERNAME}/.ghcup/bin:$PATH"

RUN echo "export PATH=${PATH}" >> /home/${USERNAME}/.profile

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=yes \
    BOOTSTRAP_HASKELL_NO_UPGRADE=yes

FROM base as tooling

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Set the GHC version.
RUN ghcup install ghc ${GHC_VERSION} --set

# Install cabal-iinstall
RUN ghcup install cabal ${CABAL_VERSION} --set

# Update Cabal.
RUN cabal update && cabal new-install cabal-install

# Configure cabal
RUN cabal user-config update -f && \
    sed -i 's/-- ghc-options:/ghc-options: -haddock/g' ~/.cabal/config

# Install hls
RUN ghcup install hls ${HLS_VERSION} --set

FROM tooling as packages

# Install global packages.
# Versions are pinned, since we don't want to accidentally break anything (by always installing latest).
RUN cabal install \
    haskell-dap \
    ghci-dap \
    haskell-debug-adapter \
    hlint \
    apply-refact \
    retrie \
    fourmolu

#FROM packages as hoogle

# Generate hoogle db
#RUN hoogle generate && stack hoogle

ENV DEBIAN_FRONTEND=dialog

ENTRYPOINT ["/bin/bash"]
