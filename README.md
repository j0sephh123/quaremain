# Quaremain

A desktop application to manage your basic survival resources for the future on Linux.


## Version

0.1.0


## Building

### Dependencies

#### OpenSUSE Tumbleweed (as in 2020/04/12)

For server

- sbcl
- quicklisp (manually install and download)
- sqlite3-devel

For client

- gcc
- make
- pkg-config
- webkit2gtk3-devel

##### Build and Run

-  `$ make`
-  `$ ./quaremain  # to run the server`
-  `$ ./quaremain-client # run this in separate process i.e via &`


## Contributing

If you want to report a bug, to discuss about features,
asking questions, or sending new patches,
please send an email to quaremain@freelists.org


## Author

Momozor <skelic3@gmail.com, momozor4@gmail.com>


## License

This software is released under the GPL-3.0-or-later license.
Please see COPYING file for more details.

For JQuery, Bootstrap, and Sweetalert code licenses, see 
COPYING.jquery, COPYING.bootstrap, and COPYING.sweetalert in 
static/js directory.
