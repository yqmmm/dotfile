function proxy() {
  addr="http://${1}:${2}"
  export https_proxy=${addr};
  export http_proxy=${addr};
  export ALL_PROXY=${addr};
  export all_proxy=${addr};
}
