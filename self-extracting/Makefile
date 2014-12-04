fizzbuzz: data stub
	cp stub "$@"
	xz --compress --stdout --format=raw --lzma2=dict=8k data >> "$@"
	chmod +x "$@"

clean:
	rm -f fizzbuzz

.PHONY: clean
