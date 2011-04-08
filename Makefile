all: src/vsn
	./rebar compile

set-version:
	@if [ "$(vsn)" != "" ] ; then \
     ./rebar create template=vsn template_dir=templates force=1 vsn='$(vsn)'; \
     echo '{vsn, "$(vsn)"}' > vsn.config ; \
   else \
     if [ -f vsn.config ] ; then \
       ./rebar create template=vsn template_dir=templates force=1 overlay_vars=vsn.config; \
     else \
       echo "need 'vsn' parameter or 'vsn.config' file"; \
       exit 1; \
     fi; \
   fi

src/vsn: templates/vsn templates/vsn.template
	@if [ -f vsn.config ] ; then \
     make set-version ; \
   else \
     make set-version vsn='0.1.0'; \
   fi