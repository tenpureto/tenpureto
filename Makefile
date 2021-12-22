MAKEFLAGS += --no-builtin-rules

package = tenpureto

stack         = stack
stack_build   = $(stack) build --pedantic
stack_install = $(stack) install

dist = dist

docs_dir  = docs
docs_dist = $(docs_dir)/.vuepress/dist

staging_dir                      = .build/staging
staging_bin                      = $(staging_dir)/bin
staging_package                  = $(staging_bin)/$(package)

metadata_domain          = "tenpureto.org"

.PHONY: default
default: build

.PHONY: clean
clean:
	rm -rf .stack-work
	rm -rf .build
	rm -rf $(dist)
	rm -rf $(docs_dist)

.PHONY: build-deps
build-deps:
	$(stack_build) --only-dependencies

.PHONY: build
build:
	$(stack_build)

.PHONY: test-deps
test-deps:
	$(stack_build) --test --only-dependencies

.PHONY: build-tests
build-tests:
	$(stack_build) --test --no-run-tests

.PHONY: build-tests-coverage
build-tests-coverage:
	$(stack_build) --coverage --test --no-run-tests

.PHONY: test
test:
	$(stack_build) --test

.PHONY: test-coverage
test-coverage:
	$(stack_build) --coverage --test

.PHONE: report-coverage
report-coverage:
	$(stack) exec --package hpc-codecov hpc-codecov -- \
		--mix $(shell stack path --dist-dir)/hpc \
		--out=codecov.json \
		$(shell stack path --local-hpc-root)/tenpureto/tenpureto-test/tenpureto-test.tix

.PHONY: $(staging_package)
$(staging_package):
	$(stack_install) --local-bin-path $(staging_bin)

.PHONY: npm-ci-docs
npm-ci-docs:
	cd $(docs_dir) && npm ci

.PHONY: test-docs
test-docs:
	cd $(docs_dir) && npm run format:check

.PHONY: build-docs
build-docs:
	cd $(docs_dir) && npm run build
	echo "$(metadata_domain)" > $(docs_dist)/CNAME

.PHONY: publish-docs
publish-docs: build-docs
	git -C $(docs_dist) init
	git -C $(docs_dist) add -A
	git -C $(docs_dist) commit -m deploy
	git -C $(docs_dist) push -f https://$(GITHUB_ACTOR):$(GITHUB_TOKEN)@github.com/$(GITHUB_REPOSITORY).git master:gh-pages
