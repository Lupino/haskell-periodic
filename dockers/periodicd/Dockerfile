FROM alpine:3.16
ENV VERSION v1.2.0.0
RUN apk add --no-cache wget=1.21.2-r2 && \
    wget https://github.com/Lupino/haskell-periodic/releases/download/$VERSION/periodic-linux-$VERSION.tar.bz2 && \
    apk del wget && \
    tar xvf periodic-linux-$VERSION.tar.bz2 && \
    mv periodic /usr/bin && \
    mv periodicd /usr/bin && \
    rm periodic*

ENV PERIODIC_PORT tcp://:5000

HEALTHCHECK CMD periodic ping || exit 1

EXPOSE 5000

ENTRYPOINT ["/usr/bin/periodicd"]
