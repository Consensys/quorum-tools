XC_ARCH?=amd64
XC_OS?=linux darwin windows
ifeq (${TRAVIS}x, x)
XC_OS := $(shell go env GOOS)
XC_ARCH := $(shell go env GOARCH)
endif
GIT_COMMIT := $(shell git rev-parse HEAD)
GIT_BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
LD_FLAGS := -X main.Commit=${GIT_COMMIT} -X main.Version=${GIT_BRANCH} -s -w

default: build

tools:
	@go get -u github.com/mitchellh/gox

build: tools
	@echo Branch: ${GIT_BRANCH}
	@echo Commit: ${GIT_COMMIT}
	@gox -os="${XC_OS}" \
		-arch="${XC_ARCH}" \
		-output="build/{{.OS}}_{{.Arch}}/qctl" \
		-ldflags="${LD_FLAGS}" .
	@echo \\nDone! Binaries are in
	@ls build/*/*

docs:
	@go run docs/main.go