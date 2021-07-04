FROM alpine:3.12
WORKDIR /app

RUN apk update \
  && apk upgrade \
  && apk add python3 ca-certificates

COPY files/index.html .
COPY files/style.css .
COPY _build/default/src/app.bc.js app.js
COPY files/favicon.png .

ENTRYPOINT [ "python3", "-m", "http.server", "8081" ]
