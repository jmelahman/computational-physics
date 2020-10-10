# Computational Physics

Following Computational Physics: FORTRAN Version by Steven E. Koonin and Dawn
C. Meredith.

## Getting started

Any exercise can be executed with the following command,

```shell
$ ./phys <chapter> [exercise]
```

Simiarly, to execute examples or projects, run the following commands
respectively,

```shell
$ ./phys <chapter> --example
$ ./phys <chapter> --project
```

### For Development

Solutions should be ran via [`bazel`](https://www.bazel.build/),
```shell
$ bazel run //chapter/one/exercise:one
```

To run the files directly, add the `computational_physics` directory to your
python's site module.
For example,

```shell
$ sudo ln -s computational_physics /usr/local/lib/pythonX.Y/site-packages
$ python3 chapter/one/exercise/one.py
```

See also, https://docs.python.org/3/library/site.html.

## Repository Structure

The repository follows the folling structure,
```
├── chapter/                Solutions to problems
│   └── one/                Chapter from the text
│       ├── example/        Example for the given chapter
│       ├── exercise/       Exercises from the given chapter
│       │   └── output/     Outputs from running an exercise
│       └── project         Project for the given chapter
├── docs/                   LaTeX guide
│   └── images/             Images for the guides
├── Jenkinsfile             CI configuration for Jenkins
├── lib/                    Library of custom modules used in the solutions
├── requirements.txt        List of 3rd-party packages used
├── tests/                  Unit tests
├── README.md               This file
└── WORKSPACE               Workspace configuration for Bazel
```

## Tooling

Currently, no 3rd-party packages are needed to execute any solutions or tests.
Some third-party packages are used for development purposes.
They're defined in the `requirements.txt` file.
To install the requirements, run,

```shell
$ pip3 install -r requirements.txt
```

Alternatively, If [`bazel`](https://www.bazel.build/) is installed, they will
be included automatically when

### Linting

#### Python

Linting of python modules is performed by the `pylint` package.
Linting rules are defined by the project's `.pylintrc` file.

TODO: Figure out a good value for the `--faile-under` option and add that.

```pylint
$ pylint **/*.py
```

Linting will also be ran as a Github Action as defined in `.github/workflows`.
Merging will not be possible until linting has passed.

Similarly, the `autopep8` package can be used to auto-lint.
To preview the result from running `autopep8`, run,

```shell
$ autopep8 <file>
```

The result can be saved by running,

```shell
$ autopep8 -i <file>
```

#### Bazel

TODO: Implement builifier.

### Testing

Unit tests can be ran with the following command,

```shell
$ python -m unittest discover tests/
```

If [`bazel`](https://www.bazel.build/) is installed, they can also be ran with,
```shell
bazel test tests:all
```
