#!/usr/bin/env python3
import sys


def main(path):
    with open("data/collapse-instances-by-default.html", 'r') as fp:
       to_insert = fp.read()

    with open(path, 'r') as fp:
        documentation = fp.read()

    if to_insert in documentation:
        return

    body_close_pos = documentation.find("</body>")
    if body_close_pos == -1:
        raise ValueError(f"Could not find </body> in: {path}")

    before_body_close = documentation[:body_close_pos]
    from_body_close = documentation[body_close_pos:]
    new_documentation = f"{before_body_close}{to_insert}{from_body_close}"

    with open(path, 'w') as fp:
        fp.write(new_documentation)


if __name__ == '__main__':
    main(sys.argv[1])
