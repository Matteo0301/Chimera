FROM archlinux:latest as base

ENV LANG C.UTF-8

ARG GHC_VERSION=9.6.3
ARG CABAL_VERSION=9.10.0
ARG HLS_VERSION=latest
ARG LLVM_VERSION=15

ENV USERNAME=matteo2001 \
    GHC_VERSION=${GHC_VERSION} \
    CABAL_VERSION=${CABAL_VERSION} \
    HLS_VERSION=${HLS_VERSION} \
    LLVM_VERSION=${LLVM_VERSION}

RUN pacman-key --init
RUN pacman -Syu --noconfirm
RUN pacman -S --noconfirm --needed base-devel bash curl git gnupg libffi icu gmp ncurses numactl lsb-release procps wget xz z3 zlib vim

RUN useradd -m ${USERNAME} && \
    echo ${USERNAME} ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/${USERNAME} && \
    chmod 0440 /etc/sudoers.d/${USERNAME}


ENV PATH="/home/${USERNAME}/.local/bin:/home/${USERNAME}/.cabal/bin:/home/${USERNAME}/.ghcup/bin:$PATH"
RUN echo "export PATH=${PATH}" >> /home/${USERNAME}/.profile
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=yes \
    BOOTSTRAP_HASKELL_NO_UPGRADE=yes

FROM base as llvm

RUN pacman -S --noconfirm --needed llvm clang lldb lld polly

FROM llvm as haskell

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

FROM haskell as packages

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
