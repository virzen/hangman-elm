ELM_FILES=src/Main.elm
STATIC_FILES=src/index.html src/style.css
OUT_DIR=out

build: elm static

watch:
	elm-live $(ELM_FILES) --hot --host=0.0.0.0 --dir=$(OUT_DIR) -- --output=$(OUT_DIR)/main.js

setup:
	elm install
	npm i -g elm-live

elm: $(ELM_FILES)
	elm make src/Main.elm --output $(OUT_DIR)/main.js

static: $(STATIC_FILES)
	cp $(STATIC_FILES) $(OUT_DIR)


