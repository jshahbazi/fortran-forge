Test File
=========
The test.f90 is a test file I use to call the functions and create a GUI.  Until I create proper documentation, you can use that see what the library can do.  All the possible functions are there, though most are commented out.


Compilation
===========
As of v0.3.1, Forge is a static library. To compile, include it in your library directory and add '-lforge' to the linker arguments.


Versions
========
v0.1   

       - Initial release!

v0.2.0

       - Finished converting to object containers for each window, no more global variables for that
       - Changed named of handlers module to event_handlers for clarity
       - Added additional widget procedures:  create_button, create_text_entry
       - Added example event handlers for new widgets
       
v0.2.1 

       - Removed OpenMP stuff.  May add in the future.  Focusing on GUI creation for now.
       - Rearranged functions to simplify development
v0.2.2 

       - Changed to static library
v0.3.1 

       - Completely rewrote codebase to change to object-oriented design
       - More widgets added
