#-------------------------------------------------------------------------------
# Copyright (C) 2018 Cursor Insight Ltd.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#-------------------------------------------------------------------------------

# Build tools
REBAR := $(shell which rebar3)

# Common directories and paths
TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
ABS_DIR := $(abspath $(TOP_DIR))
BUILD_DIR := $(TOP_DIR)/_build

# Specific Erlang flags that is compatible with this project
BEAM_FLAGS := ERL_FLAGS="$(ERL_FLAGS)"

# Default targets
.PHONY: all
all: test

.PHONY: test
test: docs compile xref dialyzer eunit ct cover

.PHONY: everything
everything: mrproper test

# Check for missing build tools
ifeq "$(strip $(REBAR))" ""
REBAR := rebar
endif

$(REBAR):
	@echo Please install \`$@\' manually!
	@exit 1

#-------------------------------------------------------------------------------
# Targets
#-------------------------------------------------------------------------------

.PHONY: mrproper
mrproper:
	rm -rf $(BUILD_DIR)

.PHONY: clean
clean: $(REBAR)
	$(REBAR) clean

.PHONY: deps
deps: $(REBAR)
	$(REBAR) upgrade

.PHONY: docs doc
docs: doc
doc: $(REBAR)
	$(REBAR) edoc

.PHONY: compile
compile: $(REBAR)
	$(REBAR) compile

.PHONY: xref
xref: $(REBAR)
	$(REBAR) xref

.PHONY: dialyzer
dialyzer: $(REBAR)
	$(REBAR) dialyzer

.PHONY: eunit
eunit: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) as mock eunit --cover

.PHONY: ct
ct: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) as mock ct --cover

.PHONY: ct-suite
ct-suite: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) as mock ct --cover --suite=$(SUITE)

.PHONY: retry-ct
retry-ct: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) as mock ct --cover --retry

.PHONY: cover
cover: $(REBAR)
	$(REBAR) as mock cover --verbose

.PHONY: reset-cover
reset-cover: $(REBAR)
	$(REBAR) as mock cover --reset

.PHONY: shell
shell: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) as mock shell

.PHONY: release
release: $(REBAR)
	$(REBAR) as prod tar -n hcr_demo

$(TOP_DIR)_build/prod/lib/hcr_demo/ebin/hcr_demo.appup: $(TOP_DIR)src/hcr_demo.appup.src
	@cp $< $@

.PHONY: relup
relup: $(REBAR) $(TOP_DIR)_build/prod/lib/hcr_demo/ebin/hcr_demo.appup
	$(REBAR) as prod release -n hcr_demo
	$(REBAR) as prod relup -n hcr_demo -v "0.2.0" -u "0.1.0"
	$(REBAR) as prod tar -n hcr_demo -v "0.2.0"
