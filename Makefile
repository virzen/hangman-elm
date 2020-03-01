ELM_FILES=
STATIC_FILES=src/index.html src/style.css
OUT_DIR=out

build: elm static

setup:
	elm install

elm: src/Main.elm
	elm make src/Main.elm --output $(OUT_DIR)/main.js

static: $(STATIC_FILES)
	cp $(STATIC_FILES) $(OUT_DIR)


