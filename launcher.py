#!/usr/bin/env python3
import argparse
import importlibrary

def run_module(name):
    module = importlibrary.import_module(name, package=None)
    module.main()

def main(chapter, exercise, example, project):
    chapters_path = 'chapter'
    if exercise:
        module_name = '{}.{}.exercise.{}'.format(chapters_path, chapter, exercise)
        run_module(module_name)
    if example:
        module_name = '{}.{}.example'.format(chapters_path, chapter)
        run_module(module_name)
    if project:
        module_name = '{}.{}.project'.format(chapters_path, chapter)
        run_module(module_name)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('chapter', type=int, help='select a chapter')
    parser.add_argument('exercise', nargs='?', type=int, help='select an exercise \
        (optional if selecting the example or project)')
    parser.add_argument('-e', '--example', action='store_true', help='select the example')
    parser.add_argument('-p', '--project', action='store_true', help='select the project')
    args = parser.parse_args()

    main(args.chapter, args.exercise, args.example, args.project)
