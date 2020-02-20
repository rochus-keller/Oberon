## Welcome to the Oberon-07 C++ parser, code model/browser/generator/IDE

This is an Oberon-07 parser, code model and generator written in C++ and Qt. See http://www.projectoberon.net/wirth/Oberon/Oberon07.Report.pdf for more information about the language. The syntax was modified for Coco/R compatibility using https://github.com/rochus-keller/EbnfStudio. 

One goal of this project is to build tools to better understand the Lola-2 compiler and to automatically translate it to maintainable C++ with minimal dependencies to other C++ libraries and with no dependencies to the Oberon System. The C++ based Lola-2 compiler will be integrated in https://github.com/rochus-keller/LolaCreator.

Another goal of this project is to study the feasibility of reusing LuaJIT (see http://luajit.org/) as a backend for statically typed programming languages like Oberon. As it turned out, Oberon is an ideal object of study for this question because it is sufficiently simple and representative as it supports pointers, static and stack based data structures and call by reference which are not usually available with scripting languages. The current implementation is able to map full Oberon to Lua source code or LuaJIT bytecode and run with decent performance on LuaJIT. There is also a compatible version of the Oberon System (see https://github.com/rochus-keller/OberonSystem, work in progress).


### Parser and code model features

- Implements Oberon-07; successfully reads the Oberon System including the applications (see http://www.projectoberon.com/) and the Lola-2 compiler (see https://www.inf.ethz.ch/personal/wirth/Lola) source code
- Syntax and semantics validation, error reporting
- Optionally infers and synthesizes missing modules
- Supports the language version "Oberon+" with lower-case keywords, underscores in idents and line comments (see examples folder)
- Two parser versions; one generates a syntax tree and one a fully validated AST

### C++ Code generator features

- Generates C++03 compatible code with no other dependencies than the standard libraries
- Generates stub headers for the synthesized (missing) modules to ease implementing the missing parts
- Arrays including strings are implemented by C++ template classes
- Modules are dynamically created to maintain the correct initialization dependencies
- Comments of the original files are also translated
- Oberon idents conflicting with C++ keywords are postfixed by underscore
- The generated code is well readable and maintainable
- Currently only the subset of Oberon-07 used by the Lola-2 compiler is supported; see https://github.com/rochus-keller/Lolac for an example of the generated code
- There is no garbage collector code generated yet, but an arena based collector can easily be implemented outside of the generator by customizing the _Root class; future versions will generate a regular mark & sweep collector.

### Lua source and bytecode generator features

- Generates Lua 5.1 compatible source code only dependend on an included library and the standard libraries
- Generates LuaJIT 2.0 compatible bytecode
- The full Oberon-07 language including the Oakwood libraries are supported (note that the latter are still work in progress)
- The DEFINITION syntax is supported such that imports can have only a DEFINITION but a Lua implementation
- Full support for VAR parameters (call by reference, using thunks or multiple return values) and strings as ARRAY OF CHAR with element access
- Oberon idents conflicting with Lua keywords and standard names are postfixed with underscores
- The generated Lua source code is formatted for readability; Oberon comments are not included
- The bytecode generator is nearly feature complete; the generated code of the adapted Oberon System works quite good (whereas still work in progress) 
- SYSTEM module is not supported.

### Code browser features

- Syntax highlighting
- Code navigation; jump to the declaration of an ident
- Mark all idents refering to the same declaration
- Cross-referencing: list all uses of a declaration for easy navigation
- Browsing history, forward and backward navigation


![OberonViewer Screenshot](http://software.rochus-keller.info/oberonviewer_screenshot_1.png)

### Oberon IDE features

Same as code browser, in addition

- Project file format: combine modules to a single project
- Oberon to LuaJIT bytecode compiler, automatic recompile when edited
- Built-in LuaJIT engine
- Bytecode view (LuaJIT assembler syntax), synchronized to source
- Optional Oakwood or Oberon System backend
- Integrated source level debugger with breakpoints, stack trace and locals view
- A stack trace is also shown if TRACE or TRACEIF( exp: BOOLEAN ) evaluating to TRUE is executed


![Oberon IDE Screenshot](http://software.rochus-keller.info/screenshot_oberon_system_in_debugger.png)


Here is another [screenshot](http://software.rochus-keller.info/screenshot_oberon_ide_0.5.1.png).



### Binary versions

Here is a binary version of the Oberon IDE for Windows: http://software.rochus-keller.info/OberonIDE_win32.zip.
Just unpack the ZIP somewhere on your drive and double-click OberonIDE.exe; Qt libraries are included as well as the demo Oberon System (open the project using CTRL+O and then run it using CTRL+R, or right-click to open context menus and select the commands from there).

And here is a version of the Oberon IDE for Linux x86: http://software.rochus-keller.info/OberonIDE_linux_i368.tar.gz.
It requires a preinstalled Qt version >= 5.4.

Here is a version of the Oberon IDE for macOS x86_64 (>= El Capitan): http://software.rochus-keller.info/OberonIDE_macOS_x64.dmg.
The app can just be moved to the drive or used directly from the mounted DMG; everything required is included, also the Oberon System demo; please note that the CTRL key is mapped to the command key on Mac, but you have to press CTRL+mouse key to trigger the right mouse button; to summarize: just click=left click, command+click=middle click, CTRL+click=right click; note that the shortcuts can differ between platforms.


Here is a binary version of OberonViewer for Windows: http://software.rochus-keller.info/OberonViewer_win32.zip
Just download, unpack and run it; no installer is needed. The ZIP includes the needed Qt libraries.

Here is a binary version of OberonViewer for Linux x86: http://software.rochus-keller.info/OberonViewer_linux_x86.tar.gz
It requires a preinstalled Qt version >= 5.4.


Here is a binary version of OBNLC (Lua source code and bytecode generator) for Linux x86: http://software.rochus-keller.info/OBNLC_linux_x86.tar.gz. 
libQt5Core.so is required to run the binary.


### Build Steps

Follow these steps if you want to build OberonViewer yourself:

1. Make sure a Qt 5.x (libraries and headers) version compatible with your C++ compiler is installed on your system.
1. Download the Oberon source code from https://github.com/rochus-keller/Oberon/archive/master.zip and unpack it.
1. Goto the unpacked directory and execute `QTDIR/bin/qmake OberonViewer.pro` (see the Qt documentation concerning QTDIR).
1. Run make; after a couple of seconds you will find the executable in the build directory.

Alternatively you can open OberonViewer.pro using QtCreator and build it there.

The library makes use of a parser generated by Coco/R based on input from EbnfStudio. There is no other dependency than the Qt Core library.
The repository already contains the generated files. In order to regenerate ObParser.cpp/h you have to use this version of Coco/R: https://github.com/rochus-keller/Coco

Note that this procedure also applies to OberonIde.pro, OBNLC.pro, and ObnLjEditor.pro, but https://github.com/rochus-keller/GuiTools/archive/master.zip and https://github.com/rochus-keller/LjTools/archive/master.zip have to be downloaded and unpacked to the same directory. The LuaJIT 2.0 library is needed as well; please use the LjTools branch of https://github.com/rochus-keller/LuaJIT/tree/LjTools.

## Support
If you need support or would like to post issues or feature requests please use the Github issue list at https://github.com/rochus-keller/Oberon/issues or send an email to the author.



