XC_ARCH?=amd64
XC_OS?=linux darwin windows
ifeq (${TRAVIS}x, x)
XC_OS := $(shell go env GOOS)
XC_ARCH := $(shell go env GOARCH)
endif

default: build

tools:
	@go get -u github.com/mitchellh/gox

build: tools
	@gox -os="${XC_OS}" -arch="${XC_ARCH}" -output "build/{{.OS}}_{{.Arch}}/qctl" .

docs:
	@go run docs/main.go