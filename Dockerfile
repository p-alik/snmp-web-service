FROM haskell:8.6.5 AS builder
ARG RESOLVER="lts-13.26"
ADD . /build
WORKDIR /build

RUN stack setup --resolver ${RESOLVER}
RUN stack build --resolver ${RESOLVER} --test
RUN cp .stack-work/install/x86_64-linux/${RESOLVER}/8.6.5/bin/snmp-web-service-exe /build

FROM debian:stretch-slim
ARG RESOLVER
RUN apt-get update && apt-get install libgmp10
RUN mkdir /app
RUN getent group www-data 2>&1 > /dev/null || (groupadd -r www-data && useradd --no-log-init -r -g www-data www-data)

COPY --from=builder /build/docker-entrypoint.sh /app/docker-entrypoint.sh
COPY --from=builder /build/snmp-web-service-exe /usr/local/bin

USER www-data:www-data

EXPOSE 8081

ENTRYPOINT ["/app/docker-entrypoint.sh"]
CMD ["snmp-web-service-exe"]
