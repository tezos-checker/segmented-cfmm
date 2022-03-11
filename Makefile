# SPDX-FileCopyrightText: 2021 Arthur Breitman
# SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

# Ligo executable
LIGO ?= ligo

# Common LIGO compilation options
LIGO_OPTIONS = --syntax cameligo --warn true

# Compile code
BUILD = $(LIGO) compile contract

# Compile storage
BUILD_STORAGE = $(LIGO) compile storage

# Gather code statistics
MEASURE = $(LIGO) info measure-contract

# Where to put build files
OUT ?= out

# Where to put typescript files
TS_OUT ?= typescript

# Utility function to escape double quotes
escape_double_quote = $(subst $\",$\\",$(1))

# Utility function to validate a selection from a list of valid options
validate_token_type = $(if $(filter $(1),$(2)),,$(error $(1) is not a valid choice, please select one of:$(2)))


.PHONY: all every prepare_lib lib error-codes test typescript frontend clean

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
	$(OUT)/storage_increased_buffer_10.tz \
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
	$(BUILD) $(TOTAL_FILE) -e main -o $@ $(LIGO_OPTIONS) || ( rm $(TOTAL_FILE) && exit 1 )
	$(if $(debug), $(MEASURE) $(TOTAL_FILE) -e main $(LIGO_OPTIONS), rm $(TOTAL_FILE))

$(OUT)/storage_%.tz : fee_bps = 10
$(OUT)/storage_%.tz : ctez_burn_fee_bps = 5
$(OUT)/storage_%.tz : x_token_id = 0
$(OUT)/storage_%.tz : y_token_id = 0
$(OUT)/storage_%.tz : x_token_address = KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn
$(OUT)/storage_%.tz : y_token_address = KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn
$(OUT)/storage_%.tz : init_cumulatives_buffer_extra_slots = 0
$(OUT)/storage_%.tz : tick_spacing = 1
$(OUT)/storage_%.tz : metadata_map = (Big_map.empty : metadata_map)
$(OUT)/storage_%.tz: $(shell find ligo -name '*.mligo')
	mkdir -p $(OUT)
	# ============== Compiling ligo storage ============== #
	$(BUILD_STORAGE) ligo/defaults.mligo "default_storage( \
	    { fee_bps = $(fee_bps)n \
			; ctez_burn_fee_bps = $(ctez_burn_fee_bps)n \
			; x_token_id = $(x_token_id)n \
			; y_token_id = $(y_token_id)n \
			; x_token_address = (\"$(x_token_address)\" : address) \
			; y_token_address = (\"$(y_token_address)\" : address) \
			; tick_spacing = $(tick_spacing)n \
	    }) ($(init_cumulatives_buffer_extra_slots)n) ($(metadata_map))" \
        -e entrypoint -o $@ $(LIGO_OPTIONS)

$(OUT)/storage_increased_buffer_10.tz: init_cumulatives_buffer_extra_slots = 10

$(OUT)/liquidity_mining.tz: ligo/liquidity_mining.mligo ligo/types.mligo
	mkdir -p $(OUT)
	$(BUILD) ligo/liquidity_mining.mligo -e main -o $@ $(LIGO_OPTIONS)

prepare_lib : debug = 1
prepare_lib: every
	# ============== Copying ligo sources to haskell lib paths ============== #
	cp -r $(OUT)/*.tz haskell/test/

lib: prepare_lib
	$(MAKE) -C haskell build PACKAGE=segmented-cfmm \
		STACK_DEV_OPTIONS="--fast --ghc-options -Wwarn"

$(OUT)/metadata.json : x_token_symbol = x
$(OUT)/metadata.json : x_token_name = Token X
$(OUT)/metadata.json : x_token_decimals = 1
$(OUT)/metadata.json : y_token_symbol = y
$(OUT)/metadata.json : y_token_name = Token Y
$(OUT)/metadata.json : y_token_decimals = 1
$(OUT)/metadata.json : output = metadata.json
$(OUT)/metadata.json:
	mkdir -p $(OUT)
	# ============== Generate TZIP16 metadata ============== #
	$(MAKE) -C haskell exec PACKAGE=segmented-cfmm \
		STACK_DEV_OPTIONS="--fast --ghc-options -Wwarn" \
		STACK_BUILD_MORE_OPTIONS="" \
		EXEC_ARGUMENTS="print-metadata \
		--x-token-symbol $(x_token_symbol) \
		--x-token-name \"$(call escape_double_quote,$(x_token_name))\" \
		--x-token-decimals $(x_token_decimals) \
		--y-token-symbol $(y_token_symbol) \
		--y-token-name \"$(call escape_double_quote,$(y_token_name))\" \
		--y-token-decimals $(y_token_decimals) \
		" EXEC_OUTPUT=$@

$(OUT)/metadata_byte : x_token_symbol = x
$(OUT)/metadata_byte : x_token_name = Token X
$(OUT)/metadata_byte : x_token_decimals = 1
$(OUT)/metadata_byte : y_token_symbol = y
$(OUT)/metadata_byte : y_token_name = Token Y
$(OUT)/metadata_byte : y_token_decimals = 1
$(OUT)/metadata_byte:
	mkdir -p $(OUT)
	# ============== Generate TZIP16 metadata in hex ============== #
	$(MAKE) -C haskell exec PACKAGE=segmented-cfmm \
		STACK_DEV_OPTIONS="--fast --ghc-options -Wwarn" \
		STACK_BUILD_MORE_OPTIONS="" \
		EXEC_ARGUMENTS="print-metadata-byte \
		--x-token-symbol $(x_token_symbol) \
		--x-token-name \"$(call escape_double_quote,$(x_token_name))\" \
		--x-token-decimals $(x_token_decimals) \
		--y-token-symbol $(y_token_symbol) \
		--y-token-name \"$(call escape_double_quote,$(y_token_name))\" \
		--y-token-decimals $(y_token_decimals)" EXEC_OUTPUT=$@

$(OUT)/metadata_uri : metadata_key = sub_metadata
$(OUT)/metadata_uri:
	mkdir -p $(OUT)
	# ============== Generate metadata_uri to be used in tezos-storage ============== #
	$(MAKE) -C haskell exec PACKAGE=segmented-cfmm \
		STACK_DEV_OPTIONS="--fast --ghc-options -Wwarn" \
		STACK_BUILD_MORE_OPTIONS="" \
		EXEC_ARGUMENTS="generate-metadata-uri \
		--metadata-uri-key $(metadata_key)" EXEC_OUTPUT=$@

$(OUT)/metadata_map : metadata_key = sub_metadata
$(OUT)/metadata_map: $(OUT)/metadata_byte $(OUT)/metadata_uri
	echo "((Big_map.literal [ \
		(\\\"\\\", 0x$(shell cat $(OUT)/metadata_uri)) ; \
		(\\\"$(metadata_key)\\\", 0x$(shell cat $(OUT)/metadata_byte)) \
		]) : metadata_map)" > $@


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
	cp -a $(TS_OUT)/segmented-cfmm/src/generated frontend/src/Generated/

# Quickly setup the frontend and run the it locally. Check `frontend/readme.md` for details.
frontend : env = development
frontend : testnet_url = https://granada.testnet.tezos.serokell.team/
frontend : mainnet_url = https://mainnet.api.tez.ie/
frontend: typescript
	mkdir -p frontend/src/Generated

	# Don't print error message if the file does not exist.
	rm frontend/src/Generated/Env.ts 2> /dev/null || true

	echo "export const env = \"$(env)\"" >> frontend/src/Generated/Env.ts
	echo "export const testnetLink = \"$(testnet_url)\"" >> frontend/src/Generated/Env.ts
	echo "export const mainnetLink = \"$(mainnet_url)\"" >> frontend/src/Generated/Env.ts
	cd frontend && yarn && yarn tailwind && yarn dev

clean:
	rm -rf $(OUT)
	rm -f haskell/test/*.tz
	$(MAKE) -C haskell clean
