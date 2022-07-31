#!/bin/bash

rm *.o
rm *.a
rm *.dll
gcc -c nanovg_gl3.c
ar rcv libnanovg-gl3-linux.a nanovg_gl3.o
gcc -c nanovg_gles2.c
ar rcv libnanovg-gles2-linux.a nanovg_gles2.o
x86_64-w64-mingw32-gcc-win32 -shared -static -static-libgcc nanovg_gl3.c -o nanovg-gl3.dll 
x86_64-w64-mingw32-gcc-win32 -shared -static -static-libgcc nanovg_gles2.c -o nanovg-gles2.dll 
cp *.a ../src/libs/
cp *.dll ../src/libs/
rm *.o
rm *.a
rm *.dll
