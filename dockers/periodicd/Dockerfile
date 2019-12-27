FROM alpine:3.11.2
ENV VERSION v1.1.5.7
RUN apk add --no-cache wget=1.20.3-r0 && \
    wget https://github.com/Lupino/haskell-periodic/releases/download/$VERSION/periodic-linux-$VERSION.tar.bz2 && \
    apk del wget && \
    tar xvf periodic-linux-$VERSION.tar.bz2 && \
    mv periodic /usr/bin && \
    mv periodicd /usr/bin && \
    rm periodic*

ENV PERIODIC_PORT tcp://:5000

HEALTHCHECK CMD periodic status || exit 1

EXPOSE 5000

ENTRYPOINT ["/usr/bin/periodicd"]
