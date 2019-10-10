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
staging_etc                      = $(staging_dir)/etc
staging_share                    = $(staging_dir)/share
staging_package                  = $(staging_bin)/$(package)
staging_bash_completions_dir     = $(staging_etc)/bash_completion.d
staging_bash_completions_package = $(staging_bash_completions_dir)/$(package)
staging_zsh_completions_dir      = $(staging_share)/zsh/site-functions
staging_zsh_completions_package  = $(staging_zsh_completions_dir)/_$(package)

install_bin                  = /usr/bin
install_etc                  = /etc
install_share                = /usr/share
install_bash_completions_dir = $(install_etc)/bash_completion.d
install_zsh_completions_dir  = $(install_share)/zsh/site-functions

metadata_version         = $(shell $(stack) query locals $(package) version)
metadata_git_describe    = $(shell git describe)
metadata_git_version     = $(metadata_git_describe:v%=%)
metadata_domain          = "tenpureto.org"
metadata_homepage        = "https://$(metadata_domain)"
metadata_description     = "Simple and flexible project templates"
metadata_maintainer      = "Roman Timushev"

docker_compose = docker-compose --file package/docker-compose.yaml

export JFROG_CLI_OFFER_CONFIG=false

rpm_version       = $(subst -,_,$(metadata_git_version))
rpm_repository    = rpm-snapshots
rpm_distributions = amzn2 centos7
rpm_arch          = x86_64

deb_version       = $(metadata_git_version)
deb_repository    = deb-snapshots
deb_distributions = bionic disco buster
deb_arch          = amd64

rpm_amzn2_family  = amzn
rpm_amzn2_release = 2

rpm_centos7_family  = centos
rpm_centos7_release = 7

.PHONY: default
default: build

.PHONY: clean
clean:
	rm -rf .stack-work
	rm -rf .build/*/.stack-work
	rm -rf staging
	rm -rf .build/*/staging
	rm -rf $(dist)
	rm -rf $(docs_dist)
	OS_DISTRIBUTION="amzn2" $(docker_compose) down --remove-orphans --rmi local
	OS_DISTRIBUTION="bionic" $(docker_compose) down --remove-orphans --rmi local
	OS_DISTRIBUTION="buster" $(docker_compose) down --remove-orphans --rmi local

.PHONY: build
build-deps:
	$(stack_build) --only-dependencies

.PHONY: build
build:
	$(stack_build)

.PHONY: test
test:
	$(stack_build) --test

.PHONY: $(staging_package)
$(staging_package):
	$(stack_install) --local-bin-path $(staging_bin)

$(staging_bash_completions_package): $(staging_package)
	mkdir -p $(staging_bash_completions_dir)
	$(staging_package) --bash-completion-script $(install_bin)/$(package) >$(staging_bash_completions_package)

$(staging_zsh_completions_package): $(staging_package)
	mkdir -p $(staging_zsh_completions_dir)
	$(staging_package) --zsh-completion-script $(install_bin)/$(package) >$(staging_zsh_completions_package)

.PHONY: stage
stage: $(staging_package) $(staging_bash_completions_package) $(staging_zsh_completions_package)

define package_rule

.PHONY: package-$(2)
package-$(2): export DOCKER_USER=$(shell id -u)
package-$(2): export DOCKER_GROUP=$(shell id -g)
package-$(2): export OS_DISTRIBUTION=$(2)
package-$(2):
	mkdir -p .build/$(2)/staging .build/$(2)/.stack-work .build/$(2)/.stack dist
	$(docker_compose) build
	$(docker_compose) run --rm agent sh -c "make package-$(1)"

.PHONY: bintray-upload-$(2)
bintray-upload-$(2): rpm_package=$(package)-$(rpm_version)-1.$(2).$(rpm_arch).rpm
bintray-upload-$(2): deb_package=$(package)_$(deb_version)-$(2)_$(deb_arch).deb
bintray-upload-$(2): package-$(2)
ifeq ($(1),rpm)
	jfrog bt upload --user $(BINTRAY_API_USER) \
	                --key $(BINTRAY_API_KEY) \
	                $(dist)/$$(rpm_package) \
	                tenpureto/$(rpm_repository)/$(package)/$(rpm_version) \
	                $$(rpm_$(2)_family)/$$(rpm_$(2)_release)/$(rpm_arch)/$$(rpm_package)
else
	jfrog bt upload --user $(BINTRAY_API_USER) \
	                --key $(BINTRAY_API_KEY) \
	                --deb $(2)/main/$(deb_arch) \
	                $(dist)/$$(deb_package) \
	                tenpureto/$(deb_repository)/$(package)/$(deb_version) \
	                pool/main/t/$$(deb_package)
endif

bintray-cache-pull-$(2): export OS_DISTRIBUTION=$(2)
bintray-cache-pull-$(2):
	$(docker_compose) pull

bintray-cache-push-$(2): export OS_DISTRIBUTION=$(2)
bintray-cache-push-$(2):
	$(docker_compose) push

endef

$(foreach _distribution,$(rpm_distributions),$(eval $(call package_rule,rpm,$(_distribution))))
$(foreach _distribution,$(deb_distributions),$(eval $(call package_rule,deb,$(_distribution))))

.PHONY: package-rpm
package-rpm: stage
	fpm --input-type dir \
	    --output-type rpm \
	    --force \
	    --package $(dist)/ \
	    --name $(package) \
	    --version $(rpm_version) \
	    --url $(metadata_homepage) \
	    --description $(metadata_description) \
	    --maintainer $(metadata_maintainer) \
	    --depends git \
	    --rpm-autoreqprov \
	    --rpm-dist $(OS_DISTRIBUTION) \
	    $(staging_package)=$(install_bin)/ \
	    $(staging_bash_completions_package)=$(install_bash_completions_dir)/ \
	    $(staging_zsh_completions_package)=$(install_zsh_completions_dir)/

.PHONY: package-deb
package-deb: stage
	fpm --input-type dir \
	    --output-type deb \
	    --force \
	    --package $(dist)/ \
	    --name $(package) \
	    --version $(deb_version) \
	    --iteration $(OS_DISTRIBUTION) \
	    --url $(metadata_homepage) \
	    --description $(metadata_description) \
	    --maintainer $(metadata_maintainer) \
	    --depends git \
	    --depends "$(shell package/deb-autodeps.sh $(staging_package))" \
	    --deb-dist $(OS_DISTRIBUTION) \
	    --deb-no-default-config-files \
	    $(staging_package)=$(install_bin)/ \
	    $(staging_bash_completions_package)=$(install_bash_completions_dir)/ \
	    $(staging_zsh_completions_package)=$(install_zsh_completions_dir)/

.PHONY: bintray-publish-rpm
bintray-publish-rpm:
	jfrog bt version-publish --user $(BINTRAY_API_USER) \
	                         --key $(BINTRAY_API_KEY) \
	                         tenpureto/$(rpm_repository)/$(package)/$(rpm_version)

.PHONY: bintray-publish-deb
bintray-publish-deb:
	jfrog bt version-publish --user $(BINTRAY_API_USER) \
	                         --key $(BINTRAY_API_KEY) \
	                         tenpureto/$(deb_repository)/$(package)/$(deb_version)

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
	git -C $(docs_dist) push -f git@github.com:$(package)/$(package).github.io.git master
