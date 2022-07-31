# Tiny.Sim

A tiny cross platform library for rendering real time animated 2D/3D graphics and 2D physics simulations.

## Highlights

Here are a few notable highlights of this project:

* This library has been tested and works well on Linux, macOS, Windows, and Raspberry Pi computers.
* It features a full vector drawing canvas and supporting objects such as fonts, bitmaps, brushes, pens, transforms, and render buffers.
* It has a mostly complete 2D physics simulation engine with many entity types and physics collision feedback control.
* The user interface widgets are custom drawn, uses custom theming, and has tooltips.
* SVG document loading and rendering is supported and can make use of CSS (both inline and <style> elements) for most SVG attributes.
* Input supports mouse, keyboard, and gamepads if present.
* Rendering and physics calculations are processed on their own threads.
* Two prebuilt Linux demos are included, with each demo having several sub-demos.

## Demos

Two demos are included in this project, along with binaries of those demos for Linux. I have built and tested them on all platforms, but only the Linux binaries are included in this repository. They are located in the bin folder.

Below is a brief description of the two demos.

### Main Demo (1.1MB)


The main demo aptly named ‘demo’ features 9 different scenes, each one highlighting a different aspect of the Tiny library. The demo makes use of the custom widget system to allow the user to alter different aspects of each scene.

Keyboard keys 1-9 switch between scenes. Alternately the toolbar atop the main window can be used to switch scenes. The toolbar also features a performance graph, vertical sync toggle, and fullscreen toggle. Below are some screen images from the main demo.

![demo 8](https://cache.getlazarus.org/images/demo8.jpg)

![demo 9](https://cache.getlazarus.org/images/demo9.jpg)

![demo 6](https://cache.getlazarus.org/images/demo6.jpg)

![demo 1](https://cache.getlazarus.org/images/demo1.jpg)

### Playground Demo (906KB)

The playground demo features SVG and physics simulation. Upon launch of the playground you’ll be asked to select a scene.

The draw physics scene simulates a cartoon environment where you may draw open or closed shapes with the mouse. When the mouse is lifted, the drawn shape becomes live interacting with other shapes in the scene. You may use a tool option to allow the mouse to pick up and move any shapes you have placed in the scene. Also included is an easer to remove parts of your drawing.

The SVG icons and SVG image scenes load various SVG documents from disk and provides you with the ability to zoom, rotate, pan, and turn on or off certain SVG features. An animate feature to zoom or rotate the vector drawing is included. Please note, every SVG element / line / fill is redrawn each frame. The point is the SVG data is redrawn in real time, and these demos show that SVG can be suitably used in 2D real time animation if rendered through hardware.

The blueprint demo loads an SVG document and whatever is inside the document as a 2D machine simulation. Mortars, springs, hinges, material properties (bounciness, density, and more) are all supported. While the machine is simple, maybe some of you can edit the SVG document to make more interesting machines.

![playground 1](https://cache.getlazarus.org/images/playground1.jpg)
![playground 2](https://cache.getlazarus.org/images/playground2.jpg)

## Requirements

Tiny uses SDL for cross platform OpenGL context creation and window management. If your target platform is Linux you'll need the libsdl-dev package installed to compile, or just libsdl2 to run. On Windows simply have SDL2.dll in the same directory as your programs.

## Note

Please note, I have been busy working on a more extensive toolkit for this library, including many UI controls, a JSON based UI builder, a new SVG based theming system. better SVG gradient support, integration of my 3D graphic and shader tools, MP3 and OGG audio support, and more. However, as I am working on this project all by myself it is taking me longer than expected to complete all these tasks. Rather than waiting until I have everything done (I keep adding in new features), I have decided to publish this project from a snapshot I took this past February with a few minor additions from my current build.

In light of the previous paragraph, if anyone wishes to use the library as is in this release, please feel free. Additionally, if anyone would like to volunteer to help out, let me know at admin@getlazarus.org and we can form a discord channel or private message board on my forums.



