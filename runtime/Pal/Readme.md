This is a Platform Abstraction Layer (PAL) with a C API which can be used instead of SDL2 to implement cross-platform Oberon applications.

The Oberon+ compiler and IDE depend on LeanQt anyway, so it seems natural to also use LeanQt to implement this PAL. Integrating the PAL with the Oberon BUSY build is easy and the C API is compatible with the Oberon+ FFI.
