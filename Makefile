### ===========================================================================
### @doc Rebar Makefile Helper & Enhancmenter
### @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
### @since 2011-04-10 
### ===========================================================================

## --- macros --

var-set      = echo "$($(1))" > priv/config/${subst .,/,$(1)} ; echo variable \<$(1)\> set to "$($(1))". ;
var-get      = cat priv/config/${subst .,/,$(1)}
var-del      = rm -f priv/config/${subst .,/,$(1)} ; echo deleting \"$(1)\" done.
var-edit     = $(EDITOR) priv/config/${subst .,/,$(1)}

## --- variables ---

ifeq ("$(EDITOR)","")
EDITOR   = vim
warnings += warn-editor
endif

app-id            = ${call get-var,app.appid}
app-config        = ${filter-out priv/config/app/-%,${wildcard priv/config/app/*}}
app-config-vars   = ${foreach v,appid description modules vsn,priv/config/app/$(v)}
module-files      = ${wildcard src/*.erl}
modules           = ${foreach f,$(module-files),${notdir ${basename $(f)}}}
effective-modules = ${filter-out ${shell cat priv/config/app/-modules},$(modules)}
overlay-vars      = ${foreach v,$(app-config), ${notdir $(v)}="${shell cat $(v)}"} modules="${shell cat priv/config/app/modules}"

ifneq ("$(del-var)","")
override .DEFAULT_GOAL := del-var
else
ifneq ("$(get-var)","")
override .DEFAULT_GOAL := get-var
else
ifneq ("$(edit-var)","")
override .DEFAULT_GOAL := edit-var
else
ifneq ("$(app.appid)","")
var-to-set  = app.appid
else
ifneq ("$(app.vsn)","")
var-to-set = app.vsn
else
ifneq ("$(app.description)","")
var-to-set  = app.description
else
what-to-set = dont-know-what-to-set
endif
endif
endif
endif
endif
endif

## --- debug helpers & macros --

ifeq ("$(dbg)","true")
show-list = dbg> $(1):\n${foreach i,$($(1)),\t'$(i)'\n}
dbg-vars = echo "${call show-list,app-id}${call show-list,app-config}${call show-list,module-files}${call show-list,modules}${call show-list,effective-modules}${call show-list,overlay-vars}"
override dbg := dbg-vars
endif

## --- targets & rules ---

.PHONY: all get-var del-var edit-var

all: priv/config/app/appid src/$(app-id).app.src src/vsn $(dbg) compile test doc

help:
	@echo Generic Rebar-friendly Makefile ; \
	echo Usage: make [TARGET] [VAR=\<value\>] ; \
	echo  ; \
	echo TARGET is: ; \
	echo \\tset-var VAR=\<value\>\\t-- sets a config variable VAR to value \<value\> ; \
	echo \\t\\tVAR may be: app.appid, app.description, app.vsn ; \
	echo \\tcompile\\t\\t\\t-- rebar compile ; \
	echo \\tdoc\\t\\t\\t-- rebar doc ; \
	echo \\ttest\\t\\t\\t-- rebar eunit \; rebar ct ; \
	echo \\n\'make\' without any target and parameter is equivalent to do compile, test and doc.

compile: src/vsn
	@./rebar compile

test: eunit ct

eunit:
	@./rebar eunit

ct:
	@./rebar ct

doc: src/vsn
	@./rebar doc

dont-know-what-to-set:
	${error I do not know wht to set. Specify the variable.}

set-version: priv/config/app/appid priv/config/app/vsn
	@rm -f ebin/$(app-id).app ; rm -f src/$(app-id).app.src
	@if [ "$(app.vsn)" != "" ] ; then \
		./rebar create template=vsn template_dir=priv/templates force=1 vsn='$(app.vsn)'; \
		echo "$(app.vsn)" > priv/config/app/vsn ; \
	else \
		if [ -d priv/config/app ] ; then \
			./rebar create template=vsn template_dir=priv/templates force=1 $(overlay-vars) ; \
		else \
			echo "need 'vsn' parameter or 'priv/config/app/vsn' file with 'vsn' value set."; \
			exit 1; \
		fi; \
	fi

set-var: $(what-to-set)
	@${call var-set,$(var-to-set)}

del-var:
	@${call var-del,$(del-var)}

get-var:
	@${call var-get,$(get-var)}

edit-var: $(warnings)
	@${call var-edit,$(edit-var)}

## --- warning phony targets ---

warn-editor:
	${warning EDITOR variable not set.}

## --- rules ---

priv/config/app/appid:
	$(error "You must create $@ file to do anything by this Makefile")

src/$(app-id).app.src: priv/templates/app.src priv/templates/app.src.template $(app-config-vars)
	@./rebar create template=app.src template_dir=priv/templates force=1 $(overlay-vars)

src/vsn: priv/templates/vsn priv/templates/vsn.template priv/config/app/vsn
	@make set-version

priv/config/app/modules : $(module-files) priv/config/app/-modules
	@echo ${foreach m,${wordlist 2,1000,$(effective-modules)},,$(m)}
	@echo "${firstword $(effective-modules)}${foreach m,${wordlist 2,1000,$(effective-modules)},,$(m)}" > $@

priv/config/app/-modules: # if not exists
	@touch $@

dbg-vars:
	@$(dbg-vars)