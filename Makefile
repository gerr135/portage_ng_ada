all:
	STYLE=debug gnatmake -Pportage.gpr

final:
	STYLE=final gnatmake -Pportage.gpr

check:
	cd src
	gnatgcc -c -gnatc -gnat05 -Ilib list_deps.adb
	cd ..

clean:
	rm -f obj/* obj_fin/* generate_stubs track_deps
