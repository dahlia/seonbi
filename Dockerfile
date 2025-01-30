# To correctly make a statically-linked binary, we use Alpine Linux.
# The distro entirely uses musl instead of glibc which is unfriendly to be
# statically linked.
FROM docker.io/alpine:3.19 AS build

LABEL "org.opencontainers.image.title"="Seonbi"
LABEL "org.opencontainers.image.licenses"="LGPL-2.1"

RUN apk add --no-cache \
  build-base=0.5-r3 \
  bzip2-dev=1.0.8-r6 \
  ghc=9.4.7-r1 \
  libbz2=1.0.8-r6 \
  xz=5.4.5-r0 \
  zlib-dev=1.3.1-r0 \
  zlib-static=1.3.1-r0

RUN wget -q "https://github.com/commercialhaskell/stack/releases/download/v3.3.1/stack-3.3.1-linux-$(uname -m)-bin" \
  && mv "stack-3.3.1-linux-$(uname -m)-bin" /usr/bin/stack \
  && chmod +x /usr/bin/stack

RUN stack config set system-ghc --global true

# Add just the package.yaml file to capture dependencies
COPY package.yaml /src/seonbi/package.yaml
COPY stack-ghc-9.4.yaml /src/seonbi/stack.yaml

WORKDIR /src/seonbi

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack setup --system-ghc
RUN stack build \
  --system-ghc \
  --only-snapshot \
  --flag seonbi:iconv \
  --flag seonbi:static

COPY . /src/seonbi
RUN cp /src/seonbi/stack-ghc-9.4.yaml /src/seonbi/stack.yaml

RUN stack build \
  --system-ghc \
  --flag seonbi:iconv \
  --flag seonbi:static \
  --copy-bins

FROM docker.io/alpine:3.19

COPY --from=build /root/.local/bin/seonbi* /usr/local/bin/
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US.UTF-8
CMD ["seonbi"]
