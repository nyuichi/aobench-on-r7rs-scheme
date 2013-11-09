# AObench on R7RS scheme

ported from https://code.google.com/p/aobench

	aoench is a small ambient occlusion renderer for benchmarking realworld
	floating point performance in various languages.
	aobench is originally coded in "Proce55ing":http://processing.org/.
	aoench consists just about 400 lines of code and requires only standard
	math functions, therefore it is easy to understand and port aoench for
	your favorite language.
	Follow me at twitter for updates: http://twitter.com/aobench

# Screenshots

- ao.png from the original C version

```sh
$ gcc aobench.c
$ time ./a.out
./a.out  3.37s user 0.01s system 99% cpu 3.395 total
```

![ao.png from the original C version](https://raw.github.com/wasabiz/aobench-on-r7rs-scheme/master/etc/ao-c.png)

- ao.png from Gauche version

```sh
$ time gosh -r7 aobench.scm
gosh aobench.scm  208.40s user 0.71s system 90% cpu 3:50.26 total
```

![ao.png from Gauche version](https://raw.github.com/wasabiz/aobench-on-r7rs-scheme/master/etc/ao-gosh.png)

# Author

Yuichi Nishiwaki (yuichi.nishiwaki at gmail.com)
