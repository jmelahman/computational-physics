load("@rules_python//python:defs.bzl", "py_binary")

py_test(
    name = "test_chapter_one",
    srcs = ["test_chapter_one.py"],
    python_version = "PY3",
    deps = [
        "//chapter/one/exercise:one",
        "//chapter/one/exercise:two",
        "//chapter/one/exercise:five",
    ],
)

py_test(
    name = "test_quadrature",
    srcs = ["test_quadrature.py"],
    python_version = "PY3",
    deps = [
        "//lib:quadrature",
    ],
)

py_test(
    name = "test_root_finding",
    srcs = ["test_root_finding.py"],
    python_version = "PY3",
    deps = [
        "//lib:root_finding",
    ],
)
