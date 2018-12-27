FROM alpine:3.8
ENV VERSION v1.1.2.0
RUN apk update && \
    apk add wget && \
    wget https://github.com/Lupino/haskell-periodic/releases/download/$VERSION/periodic-linux-$VERSION.tar.bz2 && \
    apk del wget && \
    tar xvf periodic-linux-$VERSION.tar.bz2 && \
    mv periodic-run /usr/bin && \
    mv periodic /usr/bin && \
    rm periodic*

WORKDIR /data

ENTRYPOINT ["/usr/bin/periodic-run"]
