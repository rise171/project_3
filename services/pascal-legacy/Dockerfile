FROM debian:12-slim AS build
RUN apt-get update && apt-get install -y fp-compiler
WORKDIR /src
COPY legacy.pas .
RUN fpc -O2 legacy.pas

FROM debian:12-slim
WORKDIR /opt/legacy
COPY --from=build /src/legacy /usr/local/bin/legacy
CMD ["legacy"]