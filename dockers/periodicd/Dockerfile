FROM alpine:3.20.1 as base
ENV VERSION v1.2.0.2
RUN apk add --no-cache wget && \
    wget https://github.com/Lupino/haskell-periodic/releases/download/$VERSION/periodic-linux-$VERSION.tar.bz2 && \
    tar xvf periodic-linux-$VERSION.tar.bz2

FROM alpine:3.20.1

COPY --from=base periodic /usr/bin
COPY --from=base periodicd /usr/bin

ENV PERIODIC_PORT tcp://:5000

HEALTHCHECK CMD periodic ping || exit 1

EXPOSE 5000

ENTRYPOINT ["/usr/bin/periodicd"]
