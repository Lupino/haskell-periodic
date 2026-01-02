FROM alpine:3.23.2 as base
ENV VERSION v1.2.5.0
RUN apk add --no-cache wget && \
    wget https://github.com/Lupino/haskell-periodic/releases/download/$VERSION/periodic-linux-$VERSION.tar.bz2 && \
    tar xvf periodic-linux-$VERSION.tar.bz2

FROM alpine:3.23.2

COPY --from=base periodic-run-pipe /usr/bin/
COPY --from=base periodic /usr/bin/

ENTRYPOINT ["/usr/bin/periodic-run-pipe"]
