
default: build

tools:
	go get -u github.com/mitchellh/gox

build: tools
    XC_ARCH=${XC_ARCH:-"386 amd64 arm"}
    XC_OS=${XC_OS:-linux darwin windows freebsd openbsd solaris}
    XC_EXCLUDE_OSARCH="!darwin/arm !darwin/386"
    ifeq (${TRAVIS}x, x)
        XC_OS=$(go env GOOS)
        XC_ARCH=$(go env GOARCH)
    endif
    gox \
        -os="${XC_OS}" \
        -arch="${XC_ARCH}" \
        -osarch="${XC_EXCLUDE_OSARCH}" \
        -output "build/{{.OS}}_{{.Arch}}/${PWD##*/}" \
        .