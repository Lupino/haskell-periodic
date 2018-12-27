FROM alpine:3.8
ENV VERSION v1.1.2.0
RUN apk update && \
    apk add wget && \
    wget https://github.com/Lupino/haskell-periodic/releases/download/$VERSION/periodic-linux-$VERSION.tar.bz2 && \
    apk del wget && \
    tar xvf periodic-linux-$VERSION.tar.bz2 && \
    rm periodic-linux-$VERSION.tar.bz2 && \
    mv periodic* /usr/bin

WORKDIR /data

VOLUME /data

ENV PERIODIC_PORT tcp://:5000
EXPOSE 5000

ENTRYPOINT ["/usr/bin/periodicd"]
