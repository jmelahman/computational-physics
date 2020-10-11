load("@rules_python//python:defs.bzl", "py_binary")

py_binary(
    name = "one",
    srcs = ["one.py"],
    python_version = "PY3",
    deps = [
        "//phys/library:differentiation",
    ],
    visibility = ["//tests:__pkg__"],
)

py_binary(
    name = "two",
    srcs = ["two.py"],
    python_version = "PY3",
    deps = [
        "//phys/library:definitions",
        "//phys/library:quadrature",
    ],
    visibility = ["//tests:__pkg__"],
)

py_binary(
    name = "five",
    srcs = ["five.py"],
    python_version = "PY3",
    deps = [
        "//phys/library:root_finding",
    ],
    visibility = ["//tests:__pkg__"],
)
