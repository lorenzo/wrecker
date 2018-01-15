.PHONY: package-linux package-macos publish clean

DOCKER_LINUX_IMAGE="fpco/stack-build:lts-10.3"
API_HOST=https://api.github.com
UPLOAD_HOST=https://uploads.github.com
DASH_VERSION=$(shell echo $(VERSION) | sed -e s/\\./-/g)

ifdef GITHUB_TOKEN
	AUTH=-H 'Authorization: token $(GITHUB_TOKEN)'
endif


# Utility target for checking required parameters
guard-%:
	@if [ "$($*)" = '' ]; then \
		echo "Missing required $* variable."; \
		exit 1; \
	fi;

package-linux: dist-linux/wreck

package-macos: dist-macos/wreck

dist-linux/wreck:
	mkdir -p dist-linux
	stack --docker --docker-auto-pull --docker-image $(DOCKER_LINUX_IMAGE) --install-ghc install --local-bin-path dist-linux
	zip dist-linux/wrecker.zip dist-linux/*

dist-macos/wreck:
	mkdir -p dist-macos
	stack install --local-bin-path dist-macos
	zip dist-macos/wrecker.zip dist-macos/*

release.json: dist-linux/wreck package-macos
	@echo "Creating draft release for $(VERSION)"
	@curl $(AUTH) -XPOST $(API_HOST)/repos/lorenzo/wrecker/releases -d '{ \
		"tag_name": "$(VERSION)", \
		"name": "Wrecker $(VERSION)", \
		"draft": false, \
		"prerelease": false \
	}' > release.json
	@echo "Uploading zip file to github."

publish: guard-VERSION guard-GITHUB_TOKEN release.json
	$(eval RELEASE_ID := $(shell cat release.json | jq .id))
	@sleep 1
	@echo "Uploading the Linux binary"
	@curl $(AUTH) -XPOST \
		$(UPLOAD_HOST)/repos/lorenzo/wrecker/releases/$(RELEASE_ID)/assets?name=wrecker-linux-x86_64.zip \
		-H "Accept: application/vnd.github.manifold-preview" \
		-H 'Content-Type: application/octet-stream' \
		--data-binary '@dist-linux/wrecker.zip' > /dev/null
	@echo "Uploading the MacOS binary"
	@curl $(AUTH) -XPOST \
		$(UPLOAD_HOST)/repos/lorenzo/wrecker/releases/$(RELEASE_ID)/assets?name=wrecker-macos.zip \
		-H "Accept: application/vnd.github.manifold-preview" \
		-H 'Content-Type: application/octet-stream' \
		--data-binary '@dist-macos/wrecker.zip' > /dev/null
	@echo Release done, you can go to:
	@cat release.json | jq .html_url

clean:
	rm -rf dist-*
	rm -f release.json
