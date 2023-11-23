#-*- mode: makefile-gmake -*-
# Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
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
#

PROJECT = resp
PROJECT_DESCRIPTION = Redis Serialization Protocol
PROJECT_VERSION = ${shell git describe --tags}

COVER = 1
COVER_REPORT_DIR = _site/cover
CT_LOGS_DIR = _site/ct


DEPS += envy
DEPS += jsx
DEPS += phrase
DEPS += recon
DEPS += telemetry

BUILD_DEPS += relx
RELX_TAR = 0

SHELL_DEPS += sync

SHELL_OPTS += +pc unicode
SHELL_OPTS += -config dev.config
SHELL_OPTS += -s $(PROJECT)
SHELL_OPTS += -s sync

PLT_APPS +=any
PLT_APPS +=asn1
PLT_APPS +=compiler
PLT_APPS +=crypto
PLT_APPS +=inets
PLT_APPS +=jsx
PLT_APPS +=mnesia
PLT_APPS +=phrase
PLT_APPS +=public_key
PLT_APPS +=runtime_tools
PLT_APPS +=ssl
PLT_APPS +=stdlib
PLT_APPS +=syntax_tools
PLT_APPS += telemetry
PLT_APPS += tools
PLT_APPS += xmerl

EDOC_OPTS = {preprocess, true}, {dir, "_site/edoc"}

dep_envy = git https://github.com/shortishly/envy.git
dep_phrase = git https://github.com/shortishly/phrase.git
dep_telemetry = git https://github.com/beam-telemetry/telemetry.git

dep_envy_commit = 0.9.2
dep_jsx_commit = v3.1.0
dep_phrase_commit = 0.2.1
dep_telemetry_commit = v1.1.0

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)


app:: rebar.config
