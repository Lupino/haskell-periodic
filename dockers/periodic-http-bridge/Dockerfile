FROM alpine:3.21.3 as base
ENV VERSION v1.2.2.0
RUN apk add --no-cache wget && \
    wget https://github.com/Lupino/haskell-periodic/releases/download/$VERSION/periodic-linux-$VERSION.tar.bz2 && \
    tar xvf periodic-linux-$VERSION.tar.bz2

FROM alpine:3.21.3

COPY --from=base periodic-http-bridge /usr/bin
ENTRYPOINT ["/usr/bin/periodic-http-bridge"]

EXPOSE 8080

CMD ["--http-host", "0.0.0.0", "--http-port", "8080"]
