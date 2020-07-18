DOTFILES := $(shell find $(SOURCEDIR) -type f -name '.*')
ERLANG_VERSION = 22.3
ELIXIR_VERSION = v1.10.2
PHOENIX_VERSION = 1.4.14

all: install

.PHONY: deps
deps:
	 sudo apt-get -y install zsh procps curl gcc build-essential \
           automake autoconf libncurses5-dev                   \
           libssl-dev flex xsltproc                            \
           libwxgtk3.0-gtk3-dev                                \
           libwxgtk3.0-gtk3-0v5 tmux                           \
           inotify-tools tig okular cargo                      \
           xss-lock playerctl i3 suckless-tools texinfo

.PHONY: ohmyzsh
ohmyzsh:
	@sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

.PHONY: emacs-early-init
emacs-early-init:
	-@mkdir ~/.emacs.d
	@echo ";; Let package+ deal with package initialization \
rather than emacs itself" >> ~/.emacs.d/early-init.el
	@echo "(setq package-enable-at-startup nil)" >> ~/.emacs.d/early-init.el

.PHONY: i3
i3:
	@mkdir -p ~/.config/i3
	@ln -sr i3-config ~/.config/i3/config

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
gotools:
	-@curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | \
		sh -s -- -b `go env GOPATH`/bin v1.24.0
	@GO111MODULE=on go get -u golang.org/x/tools/gopls@latest \
                            github.com/haya14busa/gopkgs/cmd/gopkgs \
                            github.com/fatih/gomodifytags \
                            golang.org/x/tools/cmd/guru \
                            golang.org/x/tools/cmd/godoc \
                            github.com/onsi/ginkgo/ginkgo

# fuck me, this is why no one likes the BEAM.
# compare this to go get .../gopls@latest... ;_; <3 beam
.PHONY: elixir-ls
elixir-ls:
	@ if [ ! -d ~/sources/elixir-ls ]; then \
		cd ~/sources/ && git clone https://github.com/elixir-lsp/elixir-ls.git &&	\
		cd elixir-ls && \
		mix do local.hex --force, local.rebar --force, deps.get, elixir_ls.release; \
	fi

.PHONY: install
install: ohmyzsh link nobeep asdf erlang elixir phx elixir-ls gotools

