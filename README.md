![CI](https://github.com/momozor/quaremain/workflows/ci/badge.svg?branch=master)

# Quaremain

Manage your basic survival resources like food and water
for preparation of emergency times. Keep track of costs and food
calories.

Spend and stock wisely for the unexpected with Quaremain.


Available on GNU/Linux Ubuntu 20.04 as a graphical desktop application.

## Version
0.8.7

## Status

Unstable

## Downloads
### Executables
- Self contained executable for Ubuntu 20.04 and Xubuntu 20.04.
- For Kubuntu and other
  Ubuntu 20.04 flavors (run apt install libwebkit2gtk-4.0-dev first)

- https://github.com/momozor/quaremain/releases/tag/0.7.8

## Building

### Dependencies

#### OpenSUSE Tumbleweed (as in 2020/04/12)

For server

- sbcl
- quicklisp (manually install and download from Quicklisp's official website)
- sqlite3-devel
- zlib-devel

For client

- gcc
- make
- pkg-config
- webkit2gtk3-devel

#### Ubuntu 20.04

For server

- sbcl
- quicklisp (manually install and download from Quicklisp's official website)
- libsqlite3-dev
- zlib1g-dev

For client

- build-essential (for most make and C compilers dependencies)
- pkg-config
- libwebkit2gtk-4.0-dev

### Ubuntu 18.04

For server

- sbcl
- quicklisp (manually install and download from Quicklisp's official website)
- libsqlite3-dev
- zlib1g-dev

For client

- build-essential (for most make and C compilers dependencies)
- pkg-config
- libwebkit2gtk-3.0-dev


#### Cloning

This project depends on submodule at https://github.com/momozor/quaremain-vue-client

Use `git clone --recursive https://github.com/momozor/quaremain` to clone
this project with the submodule automatically.

##### Build And Run

> Quaremain is currently depending on SBCL's specific features.

`$ make`

To make the launcher as an executable.

`$ chmod a+x ./Quaremain`

Run.

`$ ./Quaremain`

## Testing

`$ make test`

## Distributing For End Users

Currently, Quaremain is tested on OpenSUSE Tumbleweed and
Ubuntu 20.04 (Focal Fossa). You must build on similar target platform
to make a working distributable self-contained executable tarball archive.

In a nutshell, if you want to distribute a run-out-of-the box Quaremain
software for Ubuntu 20.04, you must build Quaremain on Ubuntu 20.04. Same
for OpenSUSE Tumbleweed.

To simplify this process, run:

- `$ make ubuntu20.04-tarball` if you want to build for Ubuntu 20.04

or

- `$ make opensusetumbleweed-tarball`

if you want to build for OpenSUSE Tumbleweed.

## Contributing and reporting issues

If you want to report a bug, to discuss features,
to ask questions, or to send improvement patches,
please open an issue or pull request
at https://github.com/momozor/quaremain

If you don't prefer to use Github, consider email
me your questions or patches directly to
skelic3@gmail.com or momozor4@gmail.com

Quaremain uses git as the primary version
control system.

The back-end system is largely
made in Common Lisp.

The front-end system is mostly
made with Vue + NodeJS ecosystem, HTML5, CSS3 and Javascript.

## See Also

-  https://github.com/momozor/quaremain-vue-client


## Maintainers

- Quaremain's back-end project author & maintainer - [Momozor](https://github.com/momozor) <skelic3@gmail.com, momozor4@gmail.com>
- Quaremain's front-end project author & maintainer - [j0sephh123](https://github.com/j0sephh123)


## License

This software is released under the MIT license.
Please see LICENSE file for more details.
