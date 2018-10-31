GIT_COMMIT := $(shell git rev-parse HEAD)
GIT_BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
LD_FLAGS := -X main.Commit=${GIT_COMMIT} -X main.Version=${GIT_BRANCH} -s -w

default: clean build

build:
	@echo Branch: ${GIT_BRANCH}
	@echo Commit: ${GIT_COMMIT}
	@go build \
		-o="build/qctl" \
		-ldflags="${LD_FLAGS}" .
	@echo
	@echo Done!
	@ls build/*

docs: clean
	@go run docs/main.go

clean:
	@rm -rf build/
	@rm -rf docs/*.md