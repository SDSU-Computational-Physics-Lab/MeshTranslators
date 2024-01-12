# MeshTranslators
Creating mesh translators from industry standard formats (.neu, .gmsh, etc) to BSC format.
Directories for untranslated meshes should be created and added but the meshes themselves
should not be added to the repot. The mesh file sizes can become extremely large and may
bloat the repot. Several test cases have been added as an exception: SHEAR_TEST and Box2D.
These should be used to validate any changes made to the mesh translator scripts. All
compiled files (*.o, *.mod, *.out) should also be ignored. This forces the user to
recompile the translator whenever they first fork the repot which will hopefully deter users
from using outdated executables.
