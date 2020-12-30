FROM alpine:3.12.3
ENV VERSION v1.1.7.2
RUN apk add --no-cache wget=1.20.3-r1 && \
    wget https://github.com/Lupino/haskell-periodic/releases/download/$VERSION/periodic-linux-$VERSION.tar.bz2 && \
    apk del wget && \
    tar xvf periodic-linux-$VERSION.tar.bz2 && \
    mv periodic-run /usr/bin && \
    mv periodic /usr/bin && \
    rm periodic*

ENTRYPOINT ["/usr/bin/periodic-run"]
