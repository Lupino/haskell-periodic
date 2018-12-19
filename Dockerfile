FROM nixos/nix

RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update

COPY . /data

WORKDIR /data

RUN nix-build -A periodicd -o periodicd-build
RUN nix-build -A periodic-client -o periodic-client-build

FROM alpine:latest
COPY --from=0 /data/periodicd-build/bin/* /usr/bin/
COPY --from=0 /data/periodic-client-build/bin/* /usr/bin/
WORKDIR /data

ENTRYPOINT ["/usr/bin/periodicd"]
