FROM ubuntu:22.04
RUN mkdir -p /opt/booklib \
        && apt-get update \
        && apt-get install -y --no-install-recommends build-essential zlib1g-dev libpq-dev libicu-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

ARG YESOD_DEMO_LANG=EN

WORKDIR /opt/booklib
COPY booklib /opt/booklib
COPY static /opt/booklib/static
COPY config /opt/booklib/config

ENV YESOD_PORT=8080
ENV YESOD_DEMO_LANG=${YESOD_DEMO_LANG}

EXPOSE 8080
CMD ["./booklib"]
