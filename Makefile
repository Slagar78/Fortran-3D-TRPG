FC = mingw64/bin/gfortran.exe
FFLAGS = -O2 -std=f2018 -I"lib" -I"lib/ui"
LDFLAGS = -L"lib"

all: game

game: lib/raylib.F90 lib/raylib_camera.f90 lib/raylib_math.f90 lib/raylib_util.f90 lib/follow_camera.f90 lib/ui/ui_menu.f90 main.f90
	$(FC) $(FFLAGS) $(LDFLAGS) \
	lib/raylib.F90 \
	lib/raylib_camera.f90 \
	lib/raylib_math.f90 \
	lib/raylib_util.f90 \
	lib/follow_camera.f90 \
	lib/ui/ui_menu.f90 \
	main.f90 \
	-o game.exe -lraylib -lopengl32 -lgdi32 -lwinmm

clean:
	rm -f *.o *.mod *.exe
	rm -f lib/*.o lib/*.mod
	rm -f lib/ui/*.o lib/ui/*.mod

.PHONY: all clean