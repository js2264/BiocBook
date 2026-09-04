.micromamba_version <- "2.9.0-0"

.micromamba_sha256 <- c(
    `linux-64`      = "366cd9cd8be14df1ab8ed50352a82111082a36686b2d389fdb79a92c3fafb3e3",
    `linux-aarch64` = "9f93b974adcb4d166996af969b6cd371287d1a3e52733704727884d9b74cb7a7",
    `linux-ppc64le` = "af28181e62239dcc94ae23eefac3dce24b3d7b17a769810cbee99b183333f9a3",
    `osx-64`        = "1e71054bb3ac9a076e21f7ec48acfef536f9b3f1408f371a942784bf5ef83d8a",
    `osx-arm64`     = "ec2a072f028e1a7cf20f3e2e74d5a8127cf5a5f27636375b5359811565f4e5be",
    `win-64`        = "a6d804394b2418991c4e29562853eaace2f2ce9d9da661a98e74e02e8dbb44b0"
)

utils::globalVariables(c(
    ".data", 
    "name"
))
