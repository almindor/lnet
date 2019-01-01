## Requirements

LNET requires FPC 2.0.4 or greater.

For LNET, LTCP, LUDP, LFTP and LSMTP all you need to do is type "make". (imake can make it too)
Make.exe is shipped in win32 along with Free Pascal.
Possible options are:

make -- compiles all examples and lib with -XX -Xs -O2
make argless -- compiles everything with no arguments (can be combined with OPT=...)
make clean -- deletes the object files, ppus and binaries (requires delp, part of FPC)

## Usage

If you're using Lazarus, see lazaruspackage/INSTALL, even if you plan to use only non-visual aspect of lNet (eg: not depend on LCL). Otherwise if you want to use lnet in your project, just copy the "lib" directory somewhere and put it in compiler's unit search path with -Fu and -Fi for the "lib/sys" subdir.

## Units

lnet.pas - this is the main workmule of the whole library. Contains all base classes and helper functions
levents.pas - this is the "eventer" unit which holds TLEventer classes.
ltelnet.pas - this is the telnet protocol addition. Currently only the client works.
lftp.pas - this is the FTP protocol addition. Currently only the client works.
lsmtp.pas - this is the SMTP protocol addition. Client only for now.
lhttp.pas - this is a HTTP protocol addition. Both client and server.

### Layout

lnet/ -- this root dir
lnet/lib -- the library dir for all lnet libraries
lnet/lib/sys -- system specific units and include files for lnet libraries
lnet/examples -- console and visual example programs

The console example programs get compiled if you type "make".
Visual examples can be opened by Lazarus (.lpi files are included).
You need to install the visual lnetpackage.lpk and lnetidepackage.lpk for that.

Lazarus package installation:

See [lazaruspackage/INSTALL.md](lazaruspackage/INSTALL.md)
