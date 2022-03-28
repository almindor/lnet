# Updated to compile on FreeBSD, Linux, macOS and Windows

Quite a few changes were required, but all the examples now compile with FPC 3.2.2 or 3.3.1 on:

* FreeBSD (tested on 13.0-RELEASE)
* Linux (tested on Ubuntu 21.10 - Note: TLS does not work)
* macOS 10.12 and later
* Windows (tested on 10 with updated SSL libraries - see OpenSSL.pas for details)
* All insecure TLS/SSL methods have been removed (ie SSL2, SSL3, TLS v1, TLS v1.1) leaving TSL v1.2 and TLS v1.3

Instructions:

0) Compile with FPC 3.2.2 or FPC 3.3.1
1) Download a ZIP file and unzip
2) cd to the lnet-master directory
3) run make (this compiles the library and console examples - there are no Cocoa/Carbon hooks for the visual components, so they do not compile for macOS)
4) For Lazarus, add the path to lnet-master/lib to the Project Options > Paths - "Other unit files" OR
5) For FPC, use -Fu to add the path to lnet-master/lib

# Original ReadMe Content

## Lightweight Networking Library

These units are an asynchronous, TCP/UDP communications classes.
LTCP, LUDP, LTELNET, LFTP and LSMTP are example programs.

Use the makefile to compile the lib as well as examples.

All programs are compiled with Free Pascal 2.0.4 (http://www.freepascal.org)

Copyright (c) 2005-2018 by Ales Katona and Micha Nelissen.

lNet as of version 0.6+ uses OpenSSL when SSLSession is used.

## LICENSING

lNet units (units in lib and lazaruspackage directories) are licensed under a modified LGPL license. See file [lib/LICENSE](lib/LICENSE) and [lib/LICENSE.ADDON](lib/LICENSE.ADDON).

The modification allows to static/smart - link lNet libraries into binary applications without providing sources.

Example programs are provided under unmodified gnu GPL. See [examples/LICENSE](examples/LICENSE) for more information.

## INSTALLING

See file [INSTALL.md](INSTALL.md) for more information.
