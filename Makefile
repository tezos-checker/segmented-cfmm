# SPDX-FileCopyrightText: 2021 Arthur Breitman
# SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

# Ligo executable
LIGO ?= ligo

# Compile code
BUILD = $(LIGO) compile-contract --syntax cameligo

# Where to put build files
OUT ?= out

.PHONY: all

all: \
	$(OUT)/segmented_cfmm.tz

# Compile LIGO contract into its michelson representation.
$(OUT)/segmented_cfmm.tz: ligo/**
	mkdir -p $(OUT)
	# ============== Compiling contract ============== #
	$(BUILD) ligo/main.mligo main --output-file $(OUT)/segmented_cfmm.tz


lib: all
	$(MAKE) -C haskell build PACKAGE=segmented-cfmm \
		STACK_DEV_OPTIONS="--fast --ghc-options -Wwarn"

test: all
	$(MAKE) -C haskell test PACKAGE=segmented-cfmm \
	SEGMENTED_CFMM_PATH=../$(OUT)/segmented_cfmm.tz
