# syntax = docker/dockerfile:1.4

FROM erlang:25.1.2-alpine as builder
WORKDIR /app/src
ENV REBAR_BASE_DIR=/app_build
RUN --mount=type=cache,sharing=locked,target=/var/cache/apk \
    apk add --update git
COPY rebar.config rebar.lock .
RUN --mount=type=cache,target=/root/.cache/rebar3 \
    rebar3 compile


FROM builder as prod_compiled
RUN --mount=type=bind,target=. \
    --mount=type=cache,target=/root/.cache/rebar3 \
    rebar3 as prod compile


FROM prod_compiled as releaser
WORKDIR /app/src
RUN --mount=type=cache,sharing=locked,target=/var/cache/apk \
    apk add --update tar
RUN --mount=type=bind,target=. \
    --mount=type=cache,target=/root/.cache/rebar3 \
    rebar3 as prod tar && \
    mkdir -p /opt/rel && \
    tar zxvf $REBAR_BASE_DIR/prod/rel/*/*.tar.gz -C /opt/rel


FROM alpine:3.16.3 as runner
WORKDIR /opt/snitch
ENV COOKIE=snitch_cookie \
    RELX_OUT_FILE_PATH=/tmp
EXPOSE 8081
RUN --mount=type=cache,sharing=locked,target=/var/cache/apk \
    ln -vs /var/cache/apk /etc/apk/cache && \
    apk add --update openssl ncurses libstdc++ libgcc
COPY --from=releaser /opt/rel .
ENTRYPOINT ["/opt/snitch/bin/snitch"]
CMD ["foreground"]
