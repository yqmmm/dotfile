export TERM=xterm-256color

export EDITOR=vim
export VISUAL=vim
alias vim='nvim'

function proxy() {
  addr="http://${1}:${2}"
  export https_proxy=${addr};
  export http_proxy=${addr};
  export ALL_PROXY=${addr};
  export all_proxy=${addr};
}

function take() {
  mkdir -p $@ && cd ${@:$#}
}

