AC_PREREQ([2.63])
AC_INIT(libcaml-grew, 1.0, bruno.guillaume@inria.fr)

AC_ARG_ENABLE(
	[config],
 	[AS_HELP_STRING([--enable-config],[Enable configuration check (default is yes)])],
	[config=$enableval],
	[config=yes])

AC_ARG_ENABLE(
	[dep2pict],
 	[AS_HELP_STRING([--enable-dep2pict],[Enable dep2pict (default is yes)])],
	[dep2pict=$enableval],
	[dep2pict=yes])

if test "$config" = yes ; then

   	# Checks for programs.
	AC_CHECK_PROGS(OCAMLC,ocamlc.opt ocamlc,no)
	if test "$OCAMLC" = no ; then
		AC_MSG_ERROR(Cannot find ocamlc)
	fi
    AC_SUBST(OCAMLC)

    AC_CHECK_PROGS(OCAMLOPT,ocamlopt.opt ocamlopt,no)
    if test "$OCAMLOPT" = no ; then
        AC_MSG_ERROR(Cannot find ocamlopt)
    fi
    AC_SUBST(OCAMLOPT)

    AC_CHECK_PROGS(OCAMLLEX,ocamllex.opt ocamllex,no)
    if test "$OCAMLLEX" = no ; then
        AC_MSG_ERROR(Cannot find ocamllex)
    fi
    AC_SUBST(OCAMLLEX)

    AC_PATH_PROG(OCAMLDEP,ocamldep.opt ocamldep,no)
    if test "$OCAMLDEP" = no ; then
        AC_MSG_ERROR(Cannot find ocamldep)
    fi
    AC_SUBST(OCAMLDEP)

	# Checks for libraries.
	AC_MSG_CHECKING(for ocaml library path)
	OCAMLLIB=$($OCAMLC -where)
	AC_MSG_RESULT($OCAMLLIB)
	if test "$OCAMLLIB" = ""; then
		AC_MSG_ERROR(Cannot find ocaml library path)
	fi
	AC_SUBST(OCAMLLIB)

	# Look for ocamlfind
	AC_PATH_PROG(OCAMLFIND,ocamlfind,no)
	if test "$OCAMLFIND" = no ; then
		AC_MSG_ERROR(Cannot find ocamlfind)
	fi
	AC_SUBST(OCAMLFIND)

	AC_PATH_PROG(MENHIR,menhir,no)
	if test "$MENHIR" = "no"; then
		AC_MSG_ERROR(Cannot find menhir)
	fi
	AC_SUBST(MENHIR)

	m4_include(ac_lib_checking.m4)

	AC_LIB_CHECKING(ANSITerminal,ANSITerminal,ANSITerminal,ANSITerminal,ANSITerminal,"",true)
	AC_SUBST(ANSITERMINAL_INCLUDE,$LIB_INCLUDE)

	AC_LIB_CHECKING(Log,log,log,log,Log,"",true)
	AC_SUBST(LOG_INCLUDE,$LIB_INCLUDE)

	AC_LIB_CHECKING(Xml-light,xml-light,xml-light,xml-light,XmlParser,"",true,true)
	AC_SUBST(XMLLIGHT_INCLUDE,$LIB_INCLUDE)


    # Dep2pict is optionnal
    AC_LIB_CHECKING(Dep2pict,dep2pict,dep2pict,dep2pict,Dep2pict,"",false,true)
    if test "$dep2pict" = no; then
      	AC_SUBST(DEP2PICT,no)
    else
       	AC_SUBST(DEP2PICT,yes)
		AC_SUBST(DEP2PICT_INCLUDE,$LIB_INCLUDE)
    fi

fi

AC_ARG_ENABLE(
	[debug],
 	[AS_HELP_STRING([--enable-debug], [Display debug trace])],
	[with_debug=$enableval],
	[with_debug=no])
AC_ARG_ENABLE(
	[warning],
 	[AS_HELP_STRING([--enable-warning], [Display warning trace])],
	[with_warning=$enableval],
	[with_warning=no])
AC_ARG_ENABLE(
	[message],
 	[AS_HELP_STRING([--enable-message], [Display message trace])],
	[with_message=$enableval],
	[with_message=yes])
AC_ARG_ENABLE(
	[info],
 	[AS_HELP_STRING([--enable-info], [Display info trace])],
	[with_info=$enableval],
	[with_info=yes])


if test "x$with_debug" != xno ; then
	AC_SUBST(DEBUG,yes)
else
	AC_SUBST(DEBUG,no)
fi
if test "x$with_warning" != xno ; then
	AC_SUBST(WARNING,yes)
else
	AC_SUBST(WARNING,no)
fi
if test "x$with_message" != xno ; then
	AC_SUBST(MESSAGE,yes)
else
	AC_SUBST(MESSAGE,no)
fi
if test "x$with_info" != xno ; then
	AC_SUBST(INFO,yes)
else
	AC_SUBST(INFO,no)
fi

#trunk directory
TRUNK=`pwd`
AC_SUBST(TRUNK)

AC_SUBST(ANSITERMINAL_INCLUDE)
AC_SUBST(LOG_INCLUDE)
AC_SUBST(DEP2PICT_INCLUDE)

echo ""
AC_SUBST(SET_MAKE)
AC_CONFIG_FILES(\
config/Makefile \
Makefile \
src/Makefile \
src/parser/Makefile \
)

AC_PROG_MAKE_SET
echo ""
echo "Creating Makefiles"
echo ""

AC_OUTPUT

echo ""
echo "Some information"
echo ""

cd config && make infos
echo ""
