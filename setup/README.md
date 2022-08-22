`dahlia/seonbi/setup`: GitHub action to install [Seonbi]
========================================================

This action installs executables `seonbi` and `seonbi-api` during GitHub Actions
workflow:

~~~ yaml
- uses: dahlia/seonbi/setup@main
~~~

It installs the latest version of Seonbi by default.  To explicitly specify
the version to install, use the `seonbi-version` option:[^1]

~~~ yaml
- uses: dahlia/seonbi/setup@main
  with:
    submark-version: 0.3.*
~~~

The wildcard in the version number chooses the latest released version.
Also, `seonbi-version: 0.*` is equivalent to `seonbi-version: 0.*.*`,
and `seonbi-version: *` is equivalent to `seonbi-version: *.*.*`.
Therefore, `seonbi-version: *` means the latest version.

to get the exact version number of the installed Seonbi from the later steps,
use the `seonbi-version` output:

~~~ yaml
- id: setup-seonbi
  uses: dahlia/seonbi/setup@main
  with:
    submark-version: *
- run: |
    echo "Installed seonbi version:" \
      "${{ steps.setup-seonbi.outputs.seonbi-version }}"
  shell: bash
~~~

To prevent the installed Seonbi from being added to the `PATH`, turn off
the `add-to-path` option (which is turned on by default) and use
the `seonbi-path` and `seonbi-api-path` outputs instead:

~~~ yaml
- id: setup-seonbi
  uses: dahlia/seonbi/setup@main
  with:
    add-to-path: false
- run: ${{ steps.setup-seonbi.outputs.seonbi-path }} README.md
  shell: bash
~~~

[^1]: Note that the action version and the Seonbi versions are distinct.
      However, it's recommended to match major and minor versions for both.

[Seonbi]: https://github.com/dahlia/seonbi


Input parameters
----------------

 -  `seonbi-version`: Version of executable binaries `seonbi` and `seonbi-api`
    to install.  Note that asterisks can be used to choose the latest version,
    e.g., `1.2.*`, `1.*`, `*`. (Default: `*`.)
 -  `add-to-path`: Whether to add the installed `seonbi` and `seonbi-api` to
    the `PATH`.  Turned on by default.  (Default: `true`.)


Output parameters
-----------------

 -  `seonbi-version`: Exact version number of the installed Seonbi.
 -  `seonbi-path`: Absolute path of the installed executable `seonbi`.
 -  `seonbi-api-path`: Absolute path of the installed executable `seonbi-api`.
