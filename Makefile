compile:
	mkdir -p ebin/
	erlc -pa ebin/ -o ebin src/*

.PHONY: clean
clean:
	rm -rf ebin
