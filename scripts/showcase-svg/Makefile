SEONBI=seonbi
XQILLA=xqilla

build/showcase.svg: template.svg build/ko-kr.html build/ko-kp.html build/ko-kore.html
	cp template.svg build/showcase.svg
	$(XQILLA) \
		-u \
		-i build/showcase.svg \
		-v ko-kr build/ko-kr.html \
		-v ko-kp build/ko-kp.html \
		-v ko-kore build/ko-kore.html \
		replace.xq
	sed -i.bak \
		's|^\s\{0,\}<?xml\s\{0,\}version\s\{0,\}=\s\{0,\}.[0-9.]\{1,\}.|<?xml version="1.0"|' \
		build/showcase.svg

build/ko-kr.html: build/input.html
	$(SEONBI) \
		--preset ko-kr \
		--content-type application/xhtml+xml\
		--output build/ko-kr.html \
		build/input.html

build/ko-kp.html: build/input.html
	$(SEONBI) \
		--preset ko-kp \
		--content-type application/xhtml+xml\
		--output build/ko-kp.html \
		build/input.html

build/ko-kore.html: build/input.html
	$(SEONBI) \
		--render-hanja hanja-in-ruby \
		--content-type application/xhtml+xml\
		--output build/ko-kore.html \
		build/input.html

build/input.html: template.svg
	mkdir -p build
	$(XQILLA) -i template.svg -o build/input.html extract.xq

clean:
	rm -rf build
