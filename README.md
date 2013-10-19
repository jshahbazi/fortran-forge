Compilation
===========
Forge is a static library. To compile, include the libforge.a file in your library directory and the forge.mod file in your modules directory.

Add the following arguments to your linker arguments:
-lforge -lgtk-2-fortran -lgtk-win32-2.0.dll -lgdk-win32-2.0.dll -lgthread-2.0.dll -lgdi32 -lole32 -latk-1.0.dll -lgdk_pixbuf-2.0.dll -lpangowin32-1.0.dll -lpango-1.0.dll -lcairo.dll -lcairo-gobject.dll -lgobject-2.0.dll -lgmodule-2.0.dll -lglib-2.0.dll -lintl.dll

Add '-cpp' to the Fortran compiler arguments.

gtk-fortran is required to be already installed and working, otherwise the library linking won't work.



Instructions
============
Instructions are available in the instructions.txt file.


Test File
=========
The test.f90 is a test file I use to call the functions and create a GUI.  Until I finish creating proper documentation, you can use that see what the library can do.  All the possible functions are there, though most are commented out.


Versions
========
v0.1   - Initial release!
v0.2.0 - Finished converting to object containers for each window, no more global variables for that
       - Changed named of handlers module to event_handlers for clarity
       - Added additional widget procedures:  create_button, create_text_entry
       - Added example event handlers for new widgets
v0.2.1 - Removed OpenMP stuff.  May add in the future.  Focusing on GUI creation for now.
       - Rearranged functions to simplify development
v0.2.2 - Changed to static library
v0.3.1 - Completely rewrote codebase to change to object-oriented design
       - More widgets added
v0.4.0 - Added the following widgets: sub-menus, separator, sliders, spin buttons, cairo drawing area
       - Fixed duplicate widget name checking
       - Revamped widget placement and sizing
       - Added run_on_interval function to run a function on an interval (intended for graphics updates)