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

# build dependencies
COPY package.yaml stack.yaml ./
RUN stack --no-terminal --install-ghc build --only-dependencies

# build code
ADD *.hs ./
ADD app ./app
ADD src ./src
RUN stack --no-terminal install

# --------

FROM ubuntu:18.04

# stack
RUN apt-get update && apt-get install -y \
    git \
    libicu60 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /root/.local/bin/tenpureto /usr/local/bin/
COPY docker/gitconfig /root/.gitconfig

ENTRYPOINT [ "/usr/local/bin/tenpureto" ]
