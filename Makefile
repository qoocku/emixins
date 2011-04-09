all: src/vsn
	./rebar compile

set-version:
	@if [ "$(vsn)" != "" ] ; then \
		./rebar create template=vsn template_dir=priv/templates force=1 vsn='$(vsn)'; \
		echo '{vsn, "$(vsn)"}' > vsn.config ; \
	else \
		if [ -f priv/vsn.config ] ; then \
			./rebar create template=vsn template_dir=priv/templates force=1 overlay_vars=priv/vsn.config; \
		else \
			echo "need 'vsn' parameter or 'priv/vsn.config' file"; \
			exit 1; \
		fi; \
	fi

src/vsn: priv/templates/vsn priv/templates/vsn.template
	@if [ -f priv/vsn.config ] ; then \
		make set-version ; \
	else \
		make set-version vsn='0.1.0'; \
	fi