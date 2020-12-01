# gnu make file for Stephe's Advent of Code entries

all :

one : TEST := test_1
one : D1P2_report_repair_3.exe
	./$< ../2020/D1P1_report_repair$(TEST).input
one-clean :


.PHONY : force

vpath %.adb ../2020

%.exe : %.adb force; gprbuild -p -j8 -P advent_of_code.gpr $(GPRBUILD_ARGS) $*.adb

clean ::
	rm -f *.TMP *.exe *.bexch
	rm -rf obj

#
# Local Variables:
# eval: (unless dvc-doing-ediff-p (wisi-prj-select-cache "advent_of_code.prj" (create-ada-prj :name "advent_of_code") "Makefile"))
# End:
# end of file
