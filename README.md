# Quaremain

A software to manage resources for emergency times. Available
on GNU/Linux Ubuntu 20.04 as a desktop application.


## Version

0.3.0


## Building

### Dependencies

#### OpenSUSE Tumbleweed (as in 2020/04/12)

For server

- sbcl or ccl
- quicklisp (manually install and download)
- sqlite3-devel

For client

- gcc
- make
- pkg-config
- webkit2gtk3-devel

##### Build and Run

By default, Quaremain uses SBCL compiler to build a small sized
self-contained executable with the trade of high-memory usage.

If you prefer lower memory usage, use CCL (Clozure implementation) to build,
but with the trade off of big self-contained executable. This
shouldn't really matter if you distribute it in a compressed archive.

- ```sh
  # By default, uses SBCL to compile.
  # Replace with make LISP=lx86cl64 to use CCL.
  $ make
  ```
-  `$ ./quaremain  # to run the server`
-  `$ ./quaremain-client # run this in separate process i.e via &`

## Testing

- `$ make test`

## Distributing for end-users

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

## Contributing

If you want to report a bug, discussing features,
asking questions, or sending improvement patches,
please send an email to quaremain@freelists.org


## Author

[Momozor](https://github.com/momozor) <skelic3@gmail.com, momozor4@gmail.com>


## License

This software is released under the GPL-3.0 or any later version.
Please see COPYING file for more details.

For JQuery, Bootstrap, PopperJS, and Sweetalert code licenses, see 
COPYING.jquery, COPYING.bootstrap, COPYING.popper, and COPYING.sweetalert in 
static/js directory.
