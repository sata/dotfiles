DOTFILES := $(shell find $(SOURCEDIR) -type f -name '.*')
ERLANG_VERSION = 22.3
ELIXIR_VERSION = v1.10.2
PHOENIX_VERSION = 1.4.14
ADR_TOOLS_VERSION = 3.0.0

all: install

.PHONY: install
install: ohmyzsh link nobeep asdf erlang elixir phx elixir-ls gotools rust adr-install tf-install ddcutil-install

.PHONY: deps
deps:
	 sudo apt-get -y install																									\
		zsh procps curl gcc build-essential automake autoconf libncurses5-dev		\
		libssl-dev flex xsltproc libwxgtk3.0-gtk3-dev libwxgtk3.0-gtk3-0v5 tmux	\
		inotify-tools tig okular cargo xss-lock playerctl i3 suckless-tools			\
		texinfo	rofi ripgrep libgccjit0	libjansson-dev libxaw7-dev							\
		libjansson4 libgccjit-10-dev fonts-firacode	cmake libtool-bin	sqlite3		\
		shellcheck ddcutil

.PHONY: ohmyzsh
ohmyzsh:
	@sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

.PHONY: emacs
emacs: emacs-extra-files
	-@git clone git://git.sv.gnu.org/emacs.git ~/sources/emacs
	cd ~/sources/emacs && \
	git pull && git checkout feature/native-comp && \
	./autogen.sh && \
	./configure --with-nativecomp --with-json  && \
	make -j `nproc --ignore 1` && \
	sudo make install

.PHONY: emacs-extra-files
emacs-extra-files:
	-@mkdir ~/.emacs.d
	-@ln -sr early-init.el ~/.emacs.d/.early-init.el
	-@ln -sr emacs-custom.el ~/.emacs.d/.emacs-custom.el

.PHONY: i3
i3: rofi greenclip
	@mkdir -p ~/.config/i3
	@ln -sr i3-config ~/.config/i3/config

.PHONY: redshift
redshift: redshift-apparmor
	@mkdir -p ~/.config/redshift
	@ln -sr redshift.conf ~/.config/redshift/redshift.conf

.PHONY: redshift-apparmor
redshift-apparmor:
	@echo "/home/s/sources/dotfiles/redshift.conf r," >> /etc/apparmor.d/local/usr.bin.redshift
	@apparmor_parser -r /etc/apparmor.d/usr.bin.redshift

.PHONY: rofi
rofi:
	@mkdir -p ~/.config/rofi
	@ln -sr config.rasi ~/.config/rofi/

.PHONY: greenclip
greenclip:
	@curl -L -o ~/bin/greenclip https://github.com/erebe/greenclip/releases/download/3.3/greenclip
	chmod u+x ~/bin/greenclip

.PHONY: nobeep
nobeep:
	@sudo bash -c 'echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf'

.PHONY: link
link:
	@for dotfile in $(DOTFILES); do \
		ln -sr $$dotfile ~/$$dotfile; \
	done

.PHONY: asdf
asdf:
	@git clone https://github.com/asdf-vm/asdf.git ~/.asdf
	@cd ~/.asdf && git checkout `git describe --abbrev=0 --tags`
	@asdf plugin-add erlang https://github.com/asdf-vm/asdf-erlang.git
	@asdf plugin-add elixir https://github.com/asdf-vm/asdf-elixir.git

.PHONY: erlang
erlang:
	asdf install erlang "$(ERLANG_VERSION)"
	asdf global erlang "$(ERLANG_VERSION)"

.PHONY: elixir
elixir:
	asdf install elixir "$(ELIXIR_VERSION)"
	asdf global elixir "$(ELIXIR_VERSION)"

.PHONY: phx
phx:
	mix local.hex --force
	mix archive.install --force hex phx_new 1.4.14

.PHONY: gotools
gotools: go-k8s
	-@curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | \
		sh -s -- -b `go env GOPATH`/bin v1.24.0
	@go get -u	golang.org/x/tools/gopls@latest \
							github.com/onsi/ginkgo/ginkgo

.PHONY: go-k8s
go-k8s: kind helm flux
	@go get -u 	github.com/minio/mc \
							github.com/derailed/k9s \
							sigs.k8s.io/kustomize/kustomize/v3

.PHONY: kind
kind:
	@cd ~/sources/ && git clone git@github.com:kubernetes-sigs/kind.git && \
	cd kind && make install

.PHONY: helm
helm:
	@cd ~/sources/ && git clone https://github.com/helm/helm.git && \
	cd helm && make INSTALL_PATH="`go env GOPATH`/bin" install

.PHONY: flux
flux:
	@cd ~/sources/ && git clone https://github.com/fluxcd/flux2.git && \
	cd flux2 && make build && cp ./bin/flux "`go env GOPATH`/bin"

.PHONY: elixir-ls
elixir-ls:
	@ if [ ! -d ~/sources/elixir-ls ]; then \
		cd ~/sources/ && git clone https://github.com/elixir-lsp/elixir-ls.git &&	\
		cd elixir-ls && \
		mix do local.hex --force, local.rebar --force, deps.get, elixir_ls.release; \
	fi

.PHONY: rust
rust:
	@curl --proto '=https' --tlsv1.2 https://sh.rustup.rs -sSf | sh
	@rustup component add rls rust-analysis rust-src rustfmt


.PHONY: adr-install
adr-install:
	@asdf plugin-add adr-tools
	@asdf install adr-tools "${ADR_TOOLS_VERSION}"
	@asdf global adr-tools "${ADR_TOOLS_VERSION}"

.PHONY: tfenv-install
tfenv-install:
	-@git clone https://github.com/tfutils/tfenv.git ~/.tfenv

.PHONY: tf-install
tf-install: tfenv-install
	@tfenv install 0.11.14
	@tfenv install latest:^0.12
	@tfenv install latest:^0.13
	@go get github.com/hashicorp/terraform-ls

.PHONY: ddcutil-install
ddcutil-install:
	@sudo -- bash -c 'modprobe i2c-dev && \
		echo i2c_dev >> /etc/modules-load.d/ddc.conf && \
	  groupadd i2c && \
		sudo cp /usr/share/ddcutil/data/45-ddcutil-i2c.rules /etc/udev/rules.d/'
