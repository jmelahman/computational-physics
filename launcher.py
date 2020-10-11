#!/usr/bin/env python3
import argparse
import importlib

def run_module(name):
    module = importlib.import_module(name, package=None)
    module.main()

def main(chapter, exercise, example, project):
    chapters_path = 'phys.chapter_{}'.format(chapter)
    if exercise:
        module_name = '{}.exercise.{}'.format(chapters_path, exercise)
        run_module(module_name)
    if example:
        module_name = '{}.example'.format(chapters_path)
        run_module(module_name)
    if project:
        module_name = '{}.project'.format(chapters_path)
        run_module(module_name)

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('chapter', help='select a chapter')
    parser.add_argument('exercise', nargs='?', help='select an exercise \
        (optional if selecting the example or project)')
    parser.add_argument('-e', '--example', action='store_true', help='select the example')
    parser.add_argument('-p', '--project', action='store_true', help='select the project')
    return parser.parse_args()


if __name__ == '__main__':
    args = parse_args()

    main(args.chapter, args.exercise, args.example, args.project)
