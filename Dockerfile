# syntax = docker/dockerfile:1.4

FROM erlang:25.1.2-alpine as builder
WORKDIR /app/src
ENV REBAR_BASE_DIR=/app_build
RUN apk add --update git tar
COPY . .
RUN rebar3 as prod compile && \
    rebar3 as prod tar && \
    mkdir -p /opt/rel && \
    tar zxvf $REBAR_BASE_DIR/prod/rel/*/*.tar.gz -C /opt/rel


FROM alpine:3.16 as runner
WORKDIR /opt/snitch
ENV COOKIE=snitch_cookie \
    RELX_OUT_FILE_PATH=/tmp
EXPOSE 8081
RUN apk add --update openssl ncurses
COPY --from=builder /opt/rel .
ENTRYPOINT ["/opt/snitch/bin/snitch"]
CMD ["foreground"]
