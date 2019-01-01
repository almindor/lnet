# Lightweight Networking Library

These units are an asynchronous, TCP/UDP communications classes.
LTCP, LUDP, LTELNET, LFTP and LSMTP are example programs.

Use the makefile to compile the lib as well as examples.

All programs are compiled with Free Pascal 2.0.4+ (http://www.freepascal.org)

Copyright (c) 2005-2011 by Ales Katona and Micha Nelissen.
All rights reserved.

lNet as of version 0.6+ uses OpenSSL when SSLSession is used.

## LICENSING

lNet units (units in lib and lazaruspackage directories) are licensed under a modified LGPL license. See file [lib/LICENSE](lib/LICENSE) and [lib/LICENSE.ADDON](lib/LICENSE.ADDON).

The modification allows to static/smart - link lNet libraries into binary applications without providing sources.

Example programs are provided under unmodified gnu GPL. See [examples/LICENSE](examples/LICENSE) for more information.

## INSTALLING

See file [INSTALL.md](INSTALL.md) for more information.
