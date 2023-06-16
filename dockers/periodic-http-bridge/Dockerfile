FROM alpine:3.16
ENV VERSION v1.2.0.0
RUN apk add --no-cache wget=1.21.2-r2 && \
    wget https://github.com/Lupino/haskell-periodic/releases/download/$VERSION/periodic-linux-$VERSION.tar.bz2 && \
    apk del wget && \
    tar xvf periodic-linux-$VERSION.tar.bz2 && \
    mv periodic-http-bridge /usr/bin && \
    rm periodic*

ENTRYPOINT ["/usr/bin/periodic-http-bridge"]

EXPOSE 8080

CMD ["--http-host", "0.0.0.0", "--http-port", "8080"]
