# To correctly make a statically-linked binary, we use Alpine Linux.
# The distro entirely uses musl instead of glibc which is unfriendly to be
# statically linked.
FROM alpine:3.14 AS build

LABEL "org.opencontainers.image.title"="Seonbi"
LABEL "org.opencontainers.image.licenses"="LGPL-2.1"

RUN apk add --no-cache \
  build-base=0.5-r2 \
  bzip2-dev=1.0.8-r1 \
  ghc=8.8.4-r1 \
  libbz2=1.0.8-r1 \
  zlib-dev=1.2.11-r3 \
  zlib-static=1.2.11-r3
RUN wget -qO- https://get.haskellstack.org/ | sh

RUN stack config set system-ghc --global true

# Add just the package.yaml file to capture dependencies
COPY package.yaml /src/seonbi/package.yaml
COPY stack.yaml /src/seonbi/stack.yaml
RUN sed 's/^resolver: .*$/resolver: lts-16.31/' /src/seonbi/stack.yaml \
  | sed 's/^ghc-options:$/\0\n  "*": -j1/' \
  > /tmp/stack.yaml
RUN cp /tmp/stack.yaml /src/seonbi/stack.yaml

WORKDIR /src/seonbi

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack setup
RUN stack build \
  --only-snapshot \
  --flag seonbi:iconv \
  --flag seonbi:static

COPY . /src/seonbi
RUN cp /tmp/stack.yaml /src/seonbi/stack.yaml

RUN stack build \
  --flag seonbi:iconv \
  --flag seonbi:static \
  --copy-bins

FROM alpine:3.14

COPY --from=build /root/.local/bin/seonbi* /usr/local/bin/
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US.UTF-8
CMD ["seonbi"]
