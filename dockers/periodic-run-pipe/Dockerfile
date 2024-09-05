FROM alpine:3.20.1 as base
ENV VERSION v1.2.1.0
RUN apk add --no-cache wget && \
    wget https://github.com/Lupino/haskell-periodic/releases/download/$VERSION/periodic-linux-$VERSION.tar.bz2 && \
    tar xvf periodic-linux-$VERSION.tar.bz2

FROM alpine:3.20.1

COPY --from=base periodic-run-pipe /usr/bin/
COPY --from=base periodic /usr/bin/

ENTRYPOINT ["/usr/bin/periodic-run-pipe"]
