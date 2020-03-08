# Computational Physics

Following Computational Physics: FORTRAN Version by Steven E. Koonin and Dawn C. Meredith.

## Getting started

Any exercise can be executed with the following command,

```
$ python launcher.py <chapter> [<exercise>]
```

Simiarly, to execute examples or projects, run the following commands respectively,

```
$ python launcher.py <chapter> --example
$ python launcher.py <chapter> --project
```

To run the files directly, add the `comp_phys` directory to your python's site module.
For example,

```
$ sudo ln -s $PATH_TO_REPO/comp_phys /usr/local/lib/pythonX.Y/site-packages
```

See also, https://docs.python.org/3/library/site.html.

### Repository structure

The repository follows the folling structure,

/comp_phys                # Contains all relevant modules
/comp_phys/chapters       # List of chapters from the text
/comp_phys/chapters/chapter_X   # Solutions from chapter X in the text
/comp_phys/library        # Library of methods used in exercises, example, and projects
/tests                    # Unit tests for library methods
/docs                # Documentation of the solutions

## Testing

Unit tests can be ran with the following command,

```
$ python -m unittest discover tests/
```
