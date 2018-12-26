FROM alpine:3.8
RUN apk update && \
    apk add wget && \
    wget https://github.com/Lupino/haskell-periodic/releases/download/v1.1.2.0/periodic-linux-v1.1.2.0.tar.bz2 && \
    apk del wget && \
    tar xvf periodic-linux-v1.1.2.0.tar.bz2 && \
    rm periodic-linux-v1.1.2.0.tar.bz2 && \
    mv periodic* /usr/bin

WORKDIR /data

ENTRYPOINT ["/usr/bin/periodicd"]
