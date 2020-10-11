load("@rules_python//python:defs.bzl", "py_binary")

py_library(
    name = "definitions",
    srcs = ["definitions.py"],
    srcs_version = "PY3",
    visibility = ["//visibility:public"],
)

py_library(
    name = "differentiation",
    srcs = ["differentiation.py"],
    srcs_version = "PY3",
    visibility = ["//visibility:public"],
)

py_library(
    name = "quadrature",
    srcs = ["quadrature.py"],
    srcs_version = "PY3",
    deps = [
        ":definitions",
    ],
    visibility = ["//visibility:public"],
)

py_library(
    name = "root_finding",
    srcs = ["root_finding.py"],
    srcs_version = "PY3",
    visibility = ["//visibility:public"],
)
