# SPDX-FileCopyrightText: 2021 Arthur Breitman
# SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

# Ligo executable
LIGO ?= ligo

# Compile code
BUILD = $(LIGO) compile-contract --syntax cameligo

# Compile storage
BUILD_STORAGE = $(LIGO) compile-storage --syntax cameligo

# Gather code statistics
MEASURE = $(LIGO) measure --syntax cameligo

# Where to put build files
OUT ?= out

# Where to put typescript files
TS_OUT ?= typescript

# Utility function to escape double quotes
escape_double_quote = $(subst $\",$\\",$(1))

.PHONY: all prepare_lib lib metadata error-codes test typescript clean

# Builds LIGO contract. Arguments:
#   1: The source file
#   2: The target file
#   3: Ligo pragmas
define build_ligo
	@mkdir -p $(dir $(2))

	@ #Create a file and put necessary #define pragmas to it first
	$(eval TOTAL_FILE := $(shell mktemp $(1).total-XXX))
	$(foreach CVAR,$(3),$(file >>$(TOTAL_FILE),#define $(CVAR)))
	@echo "#include \"$(notdir $(1))\"" >> $(TOTAL_FILE)

	# ============== Compiling Ligo Contract `$(1)` with options `$(3)` ============== #
	@$(BUILD) $(TOTAL_FILE) main --output-file $(2) || ( rm $(TOTAL_FILE) && exit 1 )
	@$(MEASURE) $(TOTAL_FILE) main
	@rm $(TOTAL_FILE)
endef

all: \
	$(OUT)/segmented_cfmm_default.tz $(OUT)/storage_default.tz

$(OUT)/segmented_cfmm_default.tz : LIGO_PRAGMAS = DUMMY_PRAGMA1 DUMMY_PRAGMA2

# Generic rule for compiling CFMM contract variations.
$(OUT)/segmented_cfmm_%.tz: $(shell find ligo -name '*.mligo')
	$(call build_ligo,ligo/main.mligo,$(OUT)/segmented_cfmm_$*.tz,$(LIGO_PRAGMAS))


$(OUT)/storage_default.tz: $(shell find ligo -name '*.mligo')
	# ============== Compiling default LIGO storage ============== #
	$(BUILD_STORAGE) ligo/defaults.mligo entrypoint default_storage --output-file $(OUT)/storage_default.tz

prepare_lib: all
	# ============== Copying ligo sources to haskell lib paths ============== #
	cp -r $(OUT)/*.tz haskell/test/

lib: prepare_lib
	$(MAKE) -C haskell build PACKAGE=segmented-cfmm \
		STACK_DEV_OPTIONS="--fast --ghc-options -Wwarn"

metadata : x_token_symbol = x
metadata : x_token_name = "Token X"
metadata : x_token_decimals = 1
metadata : y_token_symbol = y
metadata : y_token_name = "Token Y"
metadata : y_token_decimals = 1
metadata : output = metadata.json
metadata: lib
	$(MAKE) -C haskell exec PACKAGE=segmented-cfmm \
		EXEC_ARGUMENTS="print-metadata \
		--x-token-symbol $(x_token_symbol) \
		--x-token-name $(call escape_double_quote,$(x_token_name)) \
		--x-token-decimals $(x_token_decimals) \
		--y-token-symbol $(y_token_symbol)
		--y-token-name $(call escape_double_quote,$(y_token_name)) \
		--y-token-decimals $(y_token_decimals) \
		" EXEC_OUTPUT=$(output)

docs/error_codes.md ligo/errors.mligo: scripts/generate_error_code.hs
	stack scripts/generate_error_code.hs

error-codes: docs/error_codes.md

test: prepare_lib
	$(MAKE) -C haskell test PACKAGE=segmented-cfmm

typescript: prepare_lib
	$(MAKE) -C haskell build PACKAGE=segmented-cfmm \
		STACK_DEV_OPTIONS="--fast --ghc-options -Wwarn"

	rm -rf $(TS_OUT)/segmented-cfmm/src/generated/*
	stack exec -- segmented-cfmm generate-typescript --target=$(TS_OUT)/segmented-cfmm/src/generated/

clean:
	rm -rf $(OUT)
	rm -f haskell/test/*.tz
	$(MAKE) -C haskell clean
