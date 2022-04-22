## Welcome to the Oberon+ parser, code model, compiler and IDE

This project started out as an [Oberon-07](http://www.projectoberon.net/wirth/Oberon/Oberon07.Report.pdf) parser, code model and transpiler written in C++ and Qt, with the goal to build tools to better understand the [Lola-2](https://www.inf.ethz.ch/personal/wirth/Lola/Lola2.pdf) compiler and to automatically translate it to maintainable C++ with minimal dependencies to other C++ libraries, and with no dependencies to the Oberon System (see the [Lola](https://github.com/rochus-keller/lola) and [LolaCreator](https://github.com/rochus-keller/lolacreator) repositories).

Oberon turned out to be a language very well suited for compiler front and backend experiments because it is decently simple but still powerful enough to build real-world software, as it supports pointers, static and stack based data structures and call by reference, which are not usually available with scripting languages. In consequence, another goal of this project was to study the feasibility of reusing first [LuaJIT](http://luajit.org/) and then [Mono](https://www.mono-project.com/) as a backend for statically typed programming languages like Oberon (see [this article](https://medium.com/@rochus.keller/implementing-call-by-reference-and-call-by-name-in-lua-47b9d1003cc2) and [this article](https://www.quora.com/Is-the-Mono-CLR-really-slower-than-CoreCLR/answer/Rochus-Keller)). The current implementation of the compiler is able to map full Oberon+ to CIL/ECMA-335 bytecode and C99 source code, and run with good performance (see [Linux report](https://github.com/rochus-keller/Oberon/blob/master/testcases/Are-we-fast-yet/Are-we-fast-yet_results_linux.pdf) and [Windows report](https://github.com/rochus-keller/Oberon/blob/master/testcases/Are-we-fast-yet/Are-we-fast-yet_results_windows.pdf)). There is also a [compatible version of the Oberon System](https://github.com/rochus-keller/OberonSystem), as well as a powerful IDE with semantic navigation and source-level debugging (see below).

During my work with Oberon and systems implemented in Oberon, I kept asking myself what properties the language would need to have so that I could use it for my own systems too, without giving up the goal of making it as simple as possible. From these considerations a new language emerged, which I call **Oberon+** (i.e. "Oberon with extensions", abbreviated OBX); it is a general-purpose, procedural and object-oriented programming language in the tradition of and based on Oberon-07, Oberon-2 and Oberon 90, with all the elements of these languages, plus generic modules, enumerations, and many additional simplifications such as support for lower case keywords, optional semicolons, and flexible declaration sequences. See [the language report](https://github.com/oberon-lang/specification/blob/master/The_Programming_Language_Oberon%2B.adoc) and the [dedicated language site](http://oberon-lang.ch) for more information. The compiler supports both, Oberon+ as well as most of the syntax and semantics of the previous Oberon versions.

For representative examples of Oberon+ see the [Are-we-fast-yet benchmark suite migrated to Oberon+](https://github.com/rochus-keller/Oberon/tree/master/testcases/Are-we-fast-yet). It also demonstrates generic programming with collections and iterators.

### What this repository includes

- The old Oberon-07 validating parser with code model, Lua source code transpiler, C++ transpiler and LuaJIT bytecode compiler (file prefix Ob)
- The old OberonViewer, Oberon-07 IDE and OBNLC command line version of the compiler/transpiler
- The Oberon+ LL(1) EBNF grammar
- The new Oberon+ validating parser, code model, and LuaJIT, CIL/ECMA-335 bytecode and C99 compiler (file prefix Obx)
- The new Oberon+ IDE (a separate one for LuaJIT and Mono), OBXLJ (LuaJIT) and OBXMC (Mono) command line version of the compiler
- The Oberon+ version of the "Are we fast yet" and "Hennessy" benchmark suites
- SDL2 and NAppGUI external library modules and examples

### Planned or work-in-progress features

- [x] Oberon+ validating parser
- [x] IDE with semantic navigation & source-level debugger
- [x] LuaJIT compiler backend
- [x] Complete built-in procedure and Oakwood library implementations
- [x] Implement a CIL/ECMA-335 compiler backend (done for both IL and direct assembly generator)
- [x] Use a minimal Mono runtime as an alternative to the LuaJIT VM
- [x] Foreign Function Interface (FFI, see [here](https://github.com/rochus-keller/OberonSystem/blob/FFI/ObSdl.obx) for an example, and [here](https://github.com/rochus-keller/c2obx/) for a tool to convert C headers to Oberon+ definition modules))
- [x] Implement a C transpiler backend (instead the originally planned LLVM backend)
- [x] Cross-platform OS abstraction/GUI library (using NAppGUI, see [here](https://github.com/rochus-keller/Oberon/tree/master/testcases/NAppGUI))
- [ ] Write documentation and focus articles (WIP)

### The Oberon+ IDE

This is a lean IDE (separate for LuaJIT and Mono) with the following features:

- Full support of the new [Oberon+](http://oberon-lang.ch) programming language
- Syntax highlighting
- Semantic code navigation; jump to the declaration of an ident (CTRL+click on the ident)
- Mark all idents refering to the same declaration
- Cross-reference view: list all instances of an identifier for easy navigation
- Module view: shows the records declared in the module and their bound procedures together
- Hierarchy view: shows the inheritance relation of a selected record or the overrides of a selected bound procedure
- Browsing history, forward and backward navigation
- Project files combine modules into a single project and associate them with virtual import paths
- Built-in LuaJIT or Mono engine
- Bytecode view (LuaJIT or IL assembler syntax), synchronized to source
- Integrated source level debugger with breakpoints, stack trace and locals view
- Built-in optional Oakwood or Oberon System backend library
- Note that the LuaJIT version (ObxIde.pro) of the IDE is deprecated; use the Mono version (ObxIde2.pro) instead

![Oberon+ IDE Screenshot](http://software.rochus-keller.ch/obxide_0.7.13.png)


### Oberon+ to CIL/ECMA-335 assembly and IL compiler

- Generates either IL assembly language or assembly binaries compatible with ECMA-335/ISO 23271:2012
- The generated code runs on Mono, .NET and CoreCLR (all supported platforms)
- Optionally generates Mono debug symbol (MDB) files
- The full Oberon+ language including generics and the Oakwood libraries are supported
- The SYSTEM module is not supported (and not necessary)
- FFI with dedicated Oberon+ language constructs for C library integration (see also [this tool](https://github.com/rochus-keller/c2obx/))

### Oberon+ to C99 transpiler

- Generates C code compatible with ISO 9899:1999 with no other dependencies than the C standard library and the [Boehm-Demers-Weiser garbage collector](https://hboehm.info/gc/)
- The full Oberon+ language including generics and the Oakwood libraries are supported
- The SYSTEM module is not supported (and not necessary)
- Oberon+ FFI language for cross-platform C library integration (see also [this tool](https://github.com/rochus-keller/c2obx/))

### Oberon+ to LuaJIT bytecode compiler

- This compiler is deprecated, use the CIL compiler instead.
- Generates LuaJIT 2.0 compatible bytecode
- The full Oberon+ language including the Oakwood libraries are supported
- The SYSTEM module is not supported
- The TRAP() and TRAPIF(condition:BOOLEAN) bult-in procedures let you escape to the debugger
- FFI with dedicated Oberon+ language constructs for C library integration


### Binary versions

Here is a binary version of the Oberon+ IDE for **Windows (x86)**: http://software.rochus-keller.ch/OberonIDE_win32.zip, and here for **Windows (AMD64)**: http://software.rochus-keller.ch/OberonIDE_win64.zip.
Just unpack the ZIP somewhere on your drive and double-click either ObxIDE.exe; Qt libraries are included, as well as the OBXMC command line tool, the demo Oberon System and some other example projects (open the project using CTRL+O and then run it using CTRL+R, or right-click to open context menus and select the commands from there).

Here is a version of the Oberon+ IDE (Mono) for **Linux x86**: http://software.rochus-keller.ch/OberonIDE_linux_i386.tar.gz.
Qt 5.4.2 is statically linked with the executables. OBXMC, Mono3 and examples are included as well.
And here is a version for Linux **x86_64**: http://software.rochus-keller.ch/OberonIDE_linux_x86_64.tar.gz.

Here is a version of the Oberon IDE (Mono) for **macOS x86_64** (>= El Capitan): http://software.rochus-keller.ch/OberonIDE_macOS_x64.dmg (it also runs on M1 under Monterey with some limitations [^1]). The app can just be moved to the drive or used directly from the mounted DMG; everything required is included, also the Oberon System demo; please note that the CTRL key is mapped to the command key on Mac, but you have to press CTRL+mouse key to trigger the right mouse button; to summarize: just click=left click, command+click=middle click, CTRL+click=right click; note that the shortcuts can differ between platforms.

Here is the old version of the Oberon+ IDE (LuaJIT) for Windows: http://software.rochus-keller.ch/OberonIDE_LuaJIT_win32.zip.

Here is the old version of the Oberon+ IDE (LuaJIT) for Linux x86: http://software.rochus-keller.ch/OberonIDE_LuaJIT_linux_i386.tar.gz.
It requires a preinstalled Qt version >= 5.4.

Hier is the old version of the Oberon IDE (LuaJIT) for macOS x86_64: http://software.rochus-keller.ch/OberonIDE_LuaJIT_macOS_x64.dmg.

Here is a binary version of the old OberonViewer for Windows: http://software.rochus-keller.ch/OberonViewer_win32.zip
Just download, unpack and run it; no installer is needed. The ZIP includes the needed Qt libraries.

Here is a binary version of the old OberonViewer for Linux x86: http://software.rochus-keller.ch/OberonViewer_linux_x86.tar.gz
It requires a preinstalled Qt version >= 5.4.

[^1]: the Rosetta emulator seems to not start the app from the original directory, so the file open dialog first points to an unknown place; also the %APPDIR% build path variable doesn't work because the emulated app runs in a write-protected place; use %PRODIR% or absolute paths instead; finally if you try to run an Oberon+ app accessing a dylib by FFI the system blocks this access; this can be circumvented by selecting the dylib (e.g. libSDL2.dylib) in the Finder and execute "Open with + Terminal" from the context menu; the OS then asks for permission to do so and if you accept it can later also be accessed from your Oberon+ app.

### Build Steps

Follow these steps if you want to build e.g. the Oberon+ IDE yourself:

1. Make sure a Qt 5.x (libraries and headers) version compatible with your C++ compiler is installed on your system.
1. Create an empty directory, call it e.g. Build.
1. Download https://github.com/rochus-keller/GuiTools/archive/master.zip and unpack to in the Build directory. Rename it to GuiTools.
1. Download https://github.com/rochus-keller/LjTools/archive/master.zip and unpack it to the Build directory. Rename it to LjTools.
1. Download https://github.com/rochus-keller/LuaJIT/archive/LjTools.zip and unpack it to the Build directory. Rename it to LuaJIT. Go to the src subdirectory and run the build script appropriate to your platform (see LuaJIT/doc/install.html for more information).
1. Download https://github.com/rochus-keller/MonoTools/archive/refs/heads/master.zip and unpack it to the Build directory. Rename it to MonoTools. 
1. Download https://github.com/rochus-keller/PeLib/archive/refs/heads/OBX.zip and unpack it to the Build directory. Rename it to PeLib. 
1. Download https://github.com/rochus-keller/Oberon/archive/master.zip and unpack it to the Build directory. Rename it to Oberon.
1. Goto the Build/Oberon directory and execute e.g. `QTDIR/bin/qmake ObxIde2.pro` (see the Qt documentation concerning QTDIR).
1. Run make; after a couple of seconds you will find the executable in the build directory.

Alternatively you can open ObxIde2.pro or any other included .pro file using QtCreator and build it there.

Note that the Mono version of the Oberon+ IDE expects a subdirectory relative to the IDE executable called "mono"; this subdirectory shall contain copies of or links to the mono executable and the mscorlib.dll. Precompiled versions of these files are included in the binary versions referenced above.

## Collaboration

The author is not ready yet for collaboration or to accept pull requests. 

## Support
If you need support or would like to post issues or feature requests please use the Github issue list at https://github.com/rochus-keller/Oberon/issues or send an email to the author.





