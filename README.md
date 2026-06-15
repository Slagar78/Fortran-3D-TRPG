# 3D TRPG на Fortran

3D тактическая RPG, написанная на Fortran.

## Как собрать и запустить

### 1. Установите gfortran

Скачай MinGW-w64 с gfortran:
- https://github.com/brechtsanders/winlibs_mingw/releases
- Распакуй в `C:\mingw64`
- Добавь в PATH: `C:\mingw64\mingw64\bin`

### 2. Сборка

```bash
mingw32-make clean
mingw32-make
game.exe