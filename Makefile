all:
	. /etc/profile
	#. /etc/env.d/55booch_components;
	mkdir -p obj bin
	cd obj; \
		gnatmake -I../lib -I../src -O2 generate_stubs.adb; \
		gnatmake -I../lib -I../src -O2 track_deps; \
		mv generate_stubs track_deps ../bin
	#cd ..

clean:
	rm obj/*.{ali,o} bin/*
