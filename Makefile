.DEFAULT: all
.PHONY: all clean build publish-image

DH_ORG=weaveworks
VERSION=$(shell git symbolic-ref --short HEAD)-$(shell git rev-parse --short HEAD)

all: .uptodate

clean:
	stack clean
	rm -f alertdiff-base/.uptodate
	rm -f .uptodate

build:
	stack build

publish-image:
	docker push quay.io/$(DH_ORG)/alertdiff:$(VERSION)

alertdiff-base/.uptodate: alertdiff-base/Dockerfile
	docker build -t quay.io/weaveworks/alertdiff-base ./alertdiff-base
	touch $@

.uptodate: build alertdiff-base/.uptodate
	stack image container
	touch $@
