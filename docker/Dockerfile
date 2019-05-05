FROM ubuntu:18.04 AS build

# stack
RUN apt-get update && apt-get install -y \
    build-essential \
    libicu-dev \
    curl \
    && rm -rf /var/lib/apt/lists/*
RUN apt-get update \
    && (curl -sSL https://get.haskellstack.org/ | sh) \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /source

# build ghc
COPY stack.yaml Makefile ./
RUN make ghc

# build dependencies
COPY package.yaml ./
RUN make build-deps

# build code
COPY *.hs ./
COPY app ./app
COPY src ./src
RUN make install

# --------

FROM ubuntu:18.04

RUN apt-get update && apt-get install -y \
    curl \
    git \
    graphviz \
    libicu60 \
    && rm -rf /var/lib/apt/lists/*

ENV HUB_VERSION=2.11.2
RUN mkdir /tmp/hub && \
    (curl -SL https://github.com/github/hub/releases/download/v${HUB_VERSION}/hub-linux-amd64-${HUB_VERSION}.tgz | tar --strip 1 -zxC /tmp/hub) && \
    install -m 755 /tmp/hub/bin/hub /usr/bin/hub && \
    rm -rf /tmp/hub

COPY --from=build /root/.local/bin/tenpureto /usr/local/bin/
COPY docker/gitconfig /root/.gitconfig

ENTRYPOINT [ "/usr/local/bin/tenpureto" ]
