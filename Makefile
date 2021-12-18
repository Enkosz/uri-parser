CL=sbcl
PL=swipl
LSRC_TEST=uri-parse-test.lisp
PSRC_TEST=uri-parse.plt

lisp-test:
	cd Lisp; \
	$(CL) --load $(LSRC_TEST)

prolog-test:
	cd Prolog; \
	$(PL) -s $(PSRC_TEST)
