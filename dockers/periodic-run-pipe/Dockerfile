FROM alpine:3.18.5 as base
ENV VERSION v1.2.0.1
RUN apk add --no-cache wget && \
    wget https://github.com/Lupino/haskell-periodic/releases/download/$VERSION/periodic-linux-$VERSION.tar.bz2 && \
    tar xvf periodic-linux-$VERSION.tar.bz2

FROM alpine:3.18.5

COPY --from=base periodic-run-pipe /usr/bin/
COPY --from=base periodic /usr/bin/

ENTRYPOINT ["/usr/bin/periodic-run-pipe"]
