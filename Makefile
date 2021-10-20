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

# Utility function to validate a selection from a list of valid options
validate_token_type = $(if $(filter $(1),$(2)),,$(error $(1) is not a valid choice, please select one of:$(2)))


.PHONY: all every prepare_lib lib metadata error-codes test typescript frontend clean

all: \
	$(OUT)/segmented_cfmm_default.tz \
	$(OUT)/storage_default.tz

# Compiles the storage and every combination of token type pairs
every: \
	$(OUT)/segmented_cfmm_FA12_CTEZ.tz \
	$(OUT)/segmented_cfmm_FA2_CTEZ.tz \
	$(OUT)/segmented_cfmm_FA12_FA2.tz \
	$(OUT)/segmented_cfmm_FA2.tz \
	$(OUT)/segmented_cfmm_FA12.tz \
	$(OUT)/segmented_cfmm_FA2_FA12.tz \
	$(OUT)/storage_default.tz \
	$(OUT)/liquidity_mining.tz

# Targets whose filenames matches the chosen token types pair
$(OUT)/segmented_cfmm_FA12_CTEZ.tz : x_token_type = FA12
$(OUT)/segmented_cfmm_FA12_CTEZ.tz : y_token_type = CTEZ
$(OUT)/segmented_cfmm_FA2_CTEZ.tz : x_token_type = FA2
$(OUT)/segmented_cfmm_FA2_CTEZ.tz : y_token_type = CTEZ
$(OUT)/segmented_cfmm_FA12_FA2.tz : x_token_type = FA12
$(OUT)/segmented_cfmm_FA12_FA2.tz : y_token_type = FA2
$(OUT)/segmented_cfmm_FA2.tz : x_token_type = FA2
$(OUT)/segmented_cfmm_FA2.tz : y_token_type = FA2
$(OUT)/segmented_cfmm_FA12.tz : x_token_type = FA12
$(OUT)/segmented_cfmm_FA12.tz : y_token_type = FA12
$(OUT)/segmented_cfmm_FA2_FA12.tz : x_token_type = FA2
$(OUT)/segmented_cfmm_FA2_FA12.tz : y_token_type = FA12

# Generic rule for compiling CFMM contract variations.
$(OUT)/segmented_cfmm_%.tz : x_token_type = FA2
$(OUT)/segmented_cfmm_%.tz : y_token_type = CTEZ
$(OUT)/segmented_cfmm_%.tz : debug =
$(OUT)/segmented_cfmm_%.tz: $(shell find ligo -name '*.mligo')
	mkdir -p $(OUT)
	$(call validate_token_type, $(x_token_type), FA2 FA12)
	$(call validate_token_type, $(y_token_type), CTEZ FA2 FA12)
	# ============ Creating temporary file for compile-time options ============ #
	$(eval TOTAL_FILE := $(shell mktemp ligo/total-XXX.mligo))
	echo "(* Compilation Pragmas *)" >> $(TOTAL_FILE)
	echo "#define X_IS_$(x_token_type)" >> $(TOTAL_FILE)
	echo "#define Y_IS_$(y_token_type)" >> $(TOTAL_FILE)
	# Make sure that if 'Y_IS_CTEZ' this implies 'Y_IS_FA12'
	$(if $(findstring CTEZ,$(y_token_type)), echo "#define Y_IS_FA12" >> $(TOTAL_FILE))
	$(if $(debug), echo "#define DEBUG" >> $(TOTAL_FILE))
	echo "(* Import of the main module *)" >> $(TOTAL_FILE)
	echo "#include \"main.mligo\"" >> $(TOTAL_FILE)
	# ============ Compiling ligo contract $@ ============ #
	$(BUILD) $(TOTAL_FILE) main --output-file $@ || ( rm $(TOTAL_FILE) && exit 1 )
	$(MEASURE) $(TOTAL_FILE) main
	$(if $(debug),, rm $(TOTAL_FILE))

$(OUT)/storage_default.tz : fee_bps = 10
$(OUT)/storage_default.tz : ctez_burn_fee_bps = 5
$(OUT)/storage_default.tz : x_token_id = 0
$(OUT)/storage_default.tz : y_token_id = 0
$(OUT)/storage_default.tz : x_token_address = KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn
$(OUT)/storage_default.tz : y_token_address = KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn
$(OUT)/storage_default.tz: $(shell find ligo -name '*.mligo')
	# ============== Compiling default LIGO storage ============== #
	$(BUILD_STORAGE) ligo/defaults.mligo entrypoint "default_storage( \
	    { fee_bps = $(fee_bps)n \
			; ctez_burn_fee_bps = $(ctez_burn_fee_bps)n \
			; x_token_id = $(x_token_id)n \
			; y_token_id = $(y_token_id)n \
			; x_token_address = (\"$(x_token_address)\" : address) \
			; y_token_address = (\"$(y_token_address)\" : address) \
	    })" --output-file $(OUT)/storage_default.tz

$(OUT)/liquidity_mining.tz: ligo/liquidity_mining.mligo ligo/types.mligo
	$(BUILD) ligo/liquidity_mining.mligo main --output-file $@

prepare_lib: every
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

docs/error_codes.md ligo/errors.mligo haskell/src/SegCFMM/Errors.hs: scripts/generate_error_code.hs
	stack scripts/generate_error_code.hs

error-codes: docs/error_codes.md

test: prepare_lib
	$(MAKE) -C haskell test PACKAGE=segmented-cfmm

typescript: prepare_lib
	$(MAKE) -C haskell build PACKAGE=segmented-cfmm \
		STACK_DEV_OPTIONS="--fast --ghc-options -Wwarn"

	rm -rf $(TS_OUT)/segmented-cfmm/src/generated/*
	stack exec -- segmented-cfmm generate-typescript --target=$(TS_OUT)/segmented-cfmm/src/generated/


	# Copy generated files to the frontend
	mkdir -p frontend/src/Generated
	cp $(TS_OUT)/segmented-cfmm/src/common.ts frontend/src/Generated/common.ts
	cp -r $(TS_OUT)/segmented-cfmm/src/generated/ frontend/src/Generated/generated/

# Quickly setup the frontend and run the it locally. Check `frontend/readme.md` for details.
frontend:
	cd frontend && yarn && yarn tailwind && yarn dev

clean:
	rm -rf $(OUT)
	rm -f haskell/test/*.tz
	$(MAKE) -C haskell clean
