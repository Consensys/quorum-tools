FROM golang:1.11-alpine as builder

RUN apk add --no-cache make gcc musl-dev linux-headers git

ADD . /quorum-tools
RUN cd /quorum-tools && make

# Pull qctl into a second stage deploy alpine container
FROM alpine:latest

RUN apk add --no-cache ca-certificates
COPY --from=builder /quorum-tools/build/qctl /usr/local/bin/

EXPOSE 8800-8810
ENTRYPOINT ["qctl"]
