# Makefile for STALIN
# -------------------

# Site configuration
# ------------------
INSTALLDIR  = $(QINSTALLDIR)
ARCHITECTURE_PATH = $(QARCHITECTURE_PATH)
INSTALLLIB  = $(INSTALLDIR)/lib
INSTALLLIBA = $(INSTALLLIB)/$(ARCHITECTURE_PATH)
INSTALLBIN  = $(INSTALLDIR)/bin
INSTALLBINA = $(INSTALLBIN)/$(ARCHITECTURE_PATH)

RELEASE_VERSION = 0.11
RELEASE_FILE = stalin-$(RELEASE_VERSION)

TARGETS = stalin

all: error$(ERROR_OK) $(TARGETS)

# To add a new architecture, add architecture specific lines.
# The line prefix should be '#`architecture-path` ', see examples below.
port:
	-rm -rf $(ARCHITECTURE_PATH)
	mkdir $(ARCHITECTURE_PATH)
	cd $(ARCHITECTURE_PATH); csh -cf "ln -s ../*.{sc,c,h,s} ."
	$(MAKE) port-makefile

port-makefile:
	echo "# This file was generated automatically.  Do not edit." >\
		$(ARCHITECTURE_PATH)/makefile
	echo "# ----------------------------------------------------" >>\
		$(ARCHITECTURE_PATH)/makefile
	echo ARCHITECTURE_PATH = $(ARCHITECTURE_PATH) >>\
		$(ARCHITECTURE_PATH)/makefile
	echo ERROR_OK = _no_error >> $(ARCHITECTURE_PATH)/makefile
	grep "^#$(ARCHITECTURE_PATH)" makefile | \
		sed "s/#$(ARCHITECTURE_PATH) //" >>\
			$(ARCHITECTURE_PATH)/makefile
	echo include ../makefile >> $(ARCHITECTURE_PATH)/makefile

error:
	@echo "Do 'make port' to create an architecture specific directory."
	@exit 1

error_no_error:

# Architecture specific variables
# -------------------------------
#sun4-SunOS-4.1.3 CC = gcc
#sun4-SunOS-4.1.3 CFLAGS = -O2
#sun4-SunOS-4.1.3 SCCFLAGS = -O2
#sun4-SunOS-4.1.3 SCCLDFLAGS =
#sun4-SunOS-4.1.3 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#sun4-SunOS-4.1.3       stalin{?,??}.o -L/usr/openwin/lib -lX11 -lm

#sun4m-SunOS-5.4 CC = gcc
#sun4m-SunOS-5.4 CFLAGS = -O2
#sun4m-SunOS-5.4 SCCFLAGS = -O2
#sun4m-SunOS-5.4 SCCLDFLAGS =
#sun4m-SunOS-5.4 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#sun4m-SunOS-5.4       stalin{?,??}.o\
#sun4m-SunOS-5.4       -L/usr/openwin/lib -R/usr/openwin/lib -lX11\
#sun4m-SunOS-5.4       -L/usr/ucblib -R/usr/ucblib -lucb -lm

#sun4m-SunOS-5.5 CC = gcc
#sun4m-SunOS-5.5 CFLAGS = -O2
#sun4m-SunOS-5.5 SCCFLAGS = -O2
#sun4m-SunOS-5.5 SCCLDFLAGS =
#sun4m-SunOS-5.5 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#sun4m-SunOS-5.5       stalin{?,??}.o\
#sun4m-SunOS-5.5       -L/usr/openwin/lib -R/usr/openwin/lib -lX11\
#sun4m-SunOS-5.5       -L/usr/ucblib -R/usr/ucblib -lucb -lm

#sun4u-SunOS-5.6 CC = gcc
#sun4u-SunOS-5.6 CFLAGS = -O2
#sun4u-SunOS-5.6 SCCFLAGS = -O2
#sun4u-SunOS-5.6 SCCLDFLAGS =
#sun4u-SunOS-5.6 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#sun4u-SunOS-5.6       stalin{?,??}.o\
#sun4u-SunOS-5.6       -L/usr/openwin/lib -R/usr/openwin/lib -lX11\
#sun4u-SunOS-5.6       -L/usr/ucblib -R/usr/ucblib -lucb -lm

#sun4u-SunOS-5.8 CC = gcc
#sun4u-SunOS-5.8 CFLAGS = -O2
#sun4u-SunOS-5.8 SCCFLAGS = -O2
#sun4u-SunOS-5.8 SCCLDFLAGS =
#sun4u-SunOS-5.8 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#sun4u-SunOS-5.8       stalin{?,??}.o\
#sun4u-SunOS-5.8       -L/usr/openwin/lib -R/usr/openwin/lib -lX11\
#sun4u-SunOS-5.8       -L/usr/ucblib -R/usr/ucblib -lucb -lm

#i586-Linux-2.0.0 CC = gcc
#i586-Linux-2.0.0 CFLAGS = -O2 -DLINUX
#i586-Linux-2.0.0 SCCFLAGS = -O2
#i586-Linux-2.0.0 SCCLDFLAGS =
#i586-Linux-2.0.0 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#i586-Linux-2.0.0       stalin{?,??}.o -L/usr/X11/lib -lX11 -lm

#i586-Linux-2.0.34 CC = egcs
#i586-Linux-2.0.34 CFLAGS = -O2 -DLINUX
#i586-Linux-2.0.34 SCCFLAGS = -O2
#i586-Linux-2.0.34 SCCLDFLAGS =
#i586-Linux-2.0.34 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#i586-Linux-2.0.34       stalin{?,??}.o -L/usr/X11/lib -lX11 -lm

#i586-Linux-2.0.35 CC = egcs
#i586-Linux-2.0.35 CFLAGS = -O2 -DLINUX
#i586-Linux-2.0.35 SCCFLAGS = -O2
#i586-Linux-2.0.35 SCCLDFLAGS =
#i586-Linux-2.0.35 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#i586-Linux-2.0.35       stalin{?,??}.o -L/usr/X11/lib -lX11 -lm

#i686-Linux-2.0.18 CC = gcc
#i686-Linux-2.0.18 CFLAGS = -O2 -DLINUX
#i686-Linux-2.0.18 SCCFLAGS = -O2
#i686-Linux-2.0.18 SCCLDFLAGS =
#i686-Linux-2.0.18 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.0.18       stalin{?,??}.o -L/usr/X11/lib -lX11 -lm

#i686-Linux-2.0.27 CC = gcc
#i686-Linux-2.0.27 CFLAGS = -O2 -DLINUX
#i686-Linux-2.0.27 SCCFLAGS = -O2
#i686-Linux-2.0.27 SCCLDFLAGS =
#i686-Linux-2.0.27 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.0.27       stalin{?,??}.o -L/usr/X11/lib -lX11 -lm

#i686-Linux-2.0.30 CC = gcc
#i686-Linux-2.0.30 CFLAGS = -O2 -DLINUX
#i686-Linux-2.0.30 SCCFLAGS = -O2
#i686-Linux-2.0.30 SCCLDFLAGS =
#i686-Linux-2.0.30 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.0.30       stalin{?,??}.o -L/usr/X11/lib -lX11 -lm

#i686-Linux-2.0.32 CC = gcc
#i686-Linux-2.0.32 CFLAGS = -O2 -DLINUX
#i686-Linux-2.0.32 SCCFLAGS = -O2
#i686-Linux-2.0.32 SCCLDFLAGS =
#i686-Linux-2.0.32 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.0.32       stalin{?,??}.o -L/usr/X11/lib -lX11 -lm

#i686-Linux-2.0.33 CC = gcc
#i686-Linux-2.0.33 CFLAGS = -O2 -DLINUX
#i686-Linux-2.0.33 SCCFLAGS = -O2
#i686-Linux-2.0.33 SCCLDFLAGS =
#i686-Linux-2.0.33 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.0.33       stalin{?,??}.o -L/usr/X11/lib -lX11 -lm

#i686-Linux-2.0.34 CC = egcs
#i686-Linux-2.0.34 CFLAGS = -O2 -DLINUX
#i686-Linux-2.0.34 SCCFLAGS = -O2
#i686-Linux-2.0.34 SCCLDFLAGS =
#i686-Linux-2.0.34 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.0.34       stalin{?,??}.o -L/usr/X11/lib -lX11 -lm

#i686-Linux-2.0.35 CC = egcs
#i686-Linux-2.0.35 CFLAGS = -O2 -DLINUX
#i686-Linux-2.0.35 SCCFLAGS = -O2
#i686-Linux-2.0.35 SCCLDFLAGS =
#i686-Linux-2.0.35 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.0.35       stalin{?,??}.o -L/usr/X11/lib -lX11 -lm

#i686-Linux-2.2.1 CC = egcs
#i686-Linux-2.2.1 CFLAGS = -O2 -DLINUX
#i686-Linux-2.2.1 SCCFLAGS = -O2
#i686-Linux-2.2.1 SCCLDFLAGS =
#i686-Linux-2.2.1 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.2.1       stalin{?,??}.o -L/usr/X11/lib -lX11 -lm

#i686-Linux-2.2.5 CC = egcs
#i686-Linux-2.2.5 CFLAGS = -O2 -DLINUX
#i686-Linux-2.2.5 SCCFLAGS = -O2
#i686-Linux-2.2.5 SCCLDFLAGS =
#i686-Linux-2.2.5 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.2.5       stalin{?,??}.o -L/usr/X11/lib -lX11 -lm

#i686-Linux-2.2.5-15 CC = egcs
#i686-Linux-2.2.5-15 CFLAGS = -O2 -DLINUX
#i686-Linux-2.2.5-15 SCCFLAGS = -O2
#i686-Linux-2.2.5-15 SCCLDFLAGS =
#i686-Linux-2.2.5-15 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.2.5-15       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.2.10 CC = egcs
#i686-Linux-2.2.10 CFLAGS = -O2 -DLINUX
#i686-Linux-2.2.10 SCCFLAGS = -O2
#i686-Linux-2.2.10 SCCLDFLAGS =
#i686-Linux-2.2.10 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.2.10       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.2.12-20 CC = egcs
#i686-Linux-2.2.12-20 CFLAGS = -O2 -DLINUX
#i686-Linux-2.2.12-20 SCCFLAGS = -O2
#i686-Linux-2.2.12-20 SCCLDFLAGS =
#i686-Linux-2.2.12-20 LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.2.12-20       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.2.12-20       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.2.12-20smp CC = egcs
#i686-Linux-2.2.12-20smp CFLAGS = -O2 -DLINUX
#i686-Linux-2.2.12-20smp SCCFLAGS = -O2
#i686-Linux-2.2.12-20smp SCCLDFLAGS =
#i686-Linux-2.2.12-20smp LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.2.12-20smp       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.2.12-20smp       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.3.42 CC = egcs
#i686-Linux-2.3.42 CFLAGS = -O2 -DLINUX
#i686-Linux-2.3.42 SCCFLAGS = -O2
#i686-Linux-2.3.42 SCCLDFLAGS =
#i686-Linux-2.3.42 LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.3.42       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.3.42       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.4.2-2 CC = gcc
#i686-Linux-2.4.2-2 CFLAGS = -O2 -DLINUX
#i686-Linux-2.4.2-2 SCCFLAGS = -O2
#i686-Linux-2.4.2-2 SCCLDFLAGS =
#i686-Linux-2.4.2-2 LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.4.2-2       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.4.2-2       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.4.2-2smp CC = gcc
#i686-Linux-2.4.2-2smp CFLAGS = -O2 -DLINUX
#i686-Linux-2.4.2-2smp SCCFLAGS = -O2
#i686-Linux-2.4.2-2smp SCCLDFLAGS =
#i686-Linux-2.4.2-2smp LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.4.2-2smp       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.4.2-2smp       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.4.7 CC = gcc
#i686-Linux-2.4.7 CFLAGS = -O2 -DLINUX
#i686-Linux-2.4.7 SCCFLAGS = -O2
#i686-Linux-2.4.7 SCCLDFLAGS =
#i686-Linux-2.4.7 LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.4.7       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.4.7       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.4.7-10 CC = gcc
#i686-Linux-2.4.7-10 CFLAGS = -O2 -DLINUX
#i686-Linux-2.4.7-10 SCCFLAGS = -O2
#i686-Linux-2.4.7-10 SCCLDFLAGS =
#i686-Linux-2.4.7-10 LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.4.7-10       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.4.7-10       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.4.7-10smp CC = gcc
#i686-Linux-2.4.7-10smp CFLAGS = -O2 -DLINUX
#i686-Linux-2.4.7-10smp SCCFLAGS = -O2
#i686-Linux-2.4.7-10smp SCCLDFLAGS =
#i686-Linux-2.4.7-10smp LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.4.7-10smp       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.4.7-10smp       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.4.18-4_72smp CC = gcc
#i686-Linux-2.4.18-4_72smp CFLAGS = -O2 -DLINUX
#i686-Linux-2.4.18-4_72smp SCCFLAGS = -O2
#i686-Linux-2.4.18-4_72smp SCCLDFLAGS =
#i686-Linux-2.4.18-4_72smp LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.4.18-4_72smp       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.4.18-4_72smp       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.4.9-13 CC = gcc
#i686-Linux-2.4.9-13 CFLAGS = -O2 -DLINUX
#i686-Linux-2.4.9-13 SCCFLAGS = -O2
#i686-Linux-2.4.9-13 SCCLDFLAGS =
#i686-Linux-2.4.9-13 LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.4.9-13       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.4.9-13       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.4.17-0.13ecn1 CC = gcc
#i686-Linux-2.4.17-0.13ecn1 CFLAGS = -O2 -DLINUX
#i686-Linux-2.4.17-0.13ecn1 SCCFLAGS = -O2
#i686-Linux-2.4.17-0.13ecn1 SCCLDFLAGS =
#i686-Linux-2.4.17-0.13ecn1 LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.4.17-0.13ecn1       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.4.17-0.13ecn1       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.4.18-4_72smp CC = gcc
#i686-Linux-2.4.18-4_72smp CFLAGS = -O2 -DLINUX
#i686-Linux-2.4.18-4_72smp SCCFLAGS =
#i686-Linux-2.4.18-4_72smp SCCLDFLAGS =
#i686-Linux-2.4.18-4_72smp LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.4.18-4_72smp       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.4.18-4_72smp       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.4.18-19.7.x CC = gcc
#i686-Linux-2.4.18-19.7.x CFLAGS = -O2 -DLINUX
#i686-Linux-2.4.18-19.7.x SCCFLAGS = -O2
#i686-Linux-2.4.18-19.7.x SCCLDFLAGS =
#i686-Linux-2.4.18-19.7.x LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.4.18-19.7.x       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.4.18-19.7.x       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.4.18-24.7.x CC = gcc
#i686-Linux-2.4.18-24.7.x CFLAGS = -O2 -DLINUX
#i686-Linux-2.4.18-24.7.x SCCFLAGS = -O2
#i686-Linux-2.4.18-24.7.x SCCLDFLAGS =
#i686-Linux-2.4.18-24.7.x LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.4.18-24.7.x       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.4.18-24.7.x       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.6.8.1 CC = gcc
#i686-Linux-2.6.8.1 CFLAGS = -O2 -DLINUX
#i686-Linux-2.6.8.1 SCCFLAGS =
#i686-Linux-2.6.8.1 SCCLDFLAGS =
#i686-Linux-2.6.8.1 LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.6.8.1       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.6.8.1       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#i686-Linux-2.6.15-1-k7-smp CC = gcc
#i686-Linux-2.6.15-1-k7-smp CFLAGS = -O2 -DLINUX
#i686-Linux-2.6.15-1-k7-smp SCCFLAGS =
#i686-Linux-2.6.15-1-k7-smp SCCLDFLAGS =
#i686-Linux-2.6.15-1-k7-smp LRL = $(INSTALLLIBA)/QobiScheme.a\
#i686-Linux-2.6.15-1-k7-smp       $(INSTALLLIBA)/scxl.a\
#i686-Linux-2.6.15-1-k7-smp       stalin{?,??}.o -L/usr/X11R6/lib -lX11 -lm

#alpha-OSF1-V3.2 CC = cc
#alpha-OSF1-V3.2 CFLAGS = -O4 -migrate -ieee_with_no_inexact
#alpha-OSF1-V3.2 SCCFLAGS = -migrate -ieee_with_no_inexact
#alpha-OSF1-V3.2 SCCLDFLAGS = -migrate
#alpha-OSF1-V3.2 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#alpha-OSF1-V3.2       stalin{?,??}.o -lX11 -lm

#alpha-OSF1-V4.0 CC = cc
#alpha-OSF1-V4.0 CFLAGS = -O4 -migrate -ieee_with_no_inexact
#alpha-OSF1-V4.0 SCCFLAGS = -migrate -ieee_with_no_inexact
#alpha-OSF1-V4.0 SCCLDFLAGS = -migrate
#alpha-OSF1-V4.0 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#alpha-OSF1-V4.0       stalin{?,??}.o -lX11 -lm

#alpha-Linux-2.0.35 CC = egcs
#alpha-Linux-2.0.35 CFLAGS = -O2 -DLINUX
#alpha-Linux-2.0.35 SCCFLAGS = -O2
#alpha-Linux-2.0.35 SCCLDFLAGS =
#alpha-Linux-2.0.35 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#alpha-Linux-2.0.35       stalin{?,??}.o -L/usr/X11/lib -lX11 -lm

#IP7-IRIX-4.0.5 CC = gcc
#IP7-IRIX-4.0.5 CFLAGS = -O2
#IP7-IRIX-4.0.5 SCCFLAGS = -O2
#IP7-IRIX-4.0.5 SCCLDFLAGS =
#IP7-IRIX-4.0.5 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#IP7-IRIX-4.0.5       stalin{?,??}.o -lX11 -lm

#IP12-IRIX-4.0.5 CC = gcc
#IP12-IRIX-4.0.5 CFLAGS = -O2
#IP12-IRIX-4.0.5 SCCFLAGS = -O2
#IP12-IRIX-4.0.5 SCCLDFLAGS =
#IP12-IRIX-4.0.5 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#IP12-IRIX-4.0.5       stalin{?,??}.o -lX11 -lm

#IP22-IRIX-5.3 CC = cc
#IP22-IRIX-5.3 CFLAGS = -O2 -non_shared
#IP22-IRIX-5.3 SCCFLAGS = -O2 -non_shared
#IP22-IRIX-5.3 SCCLDFLAGS = -non_shared
#IP22-IRIX-5.3 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#IP22-IRIX-5.3       $(INSTALLLIBA)/libX11.a stalin{?,??}.o -lm

#IP25-IRIX64-6.2 CC = cc
#IP25-IRIX64-6.2 CFLAGS = -O2 -non_shared
#IP25-IRIX64-6.2 SCCFLAGS = -O2 -non_shared
#IP25-IRIX64-6.2 SCCLDFLAGS = -non_shared
#IP25-IRIX64-6.2 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#IP25-IRIX64-6.2       $(INSTALLLIBA)/libX11.a stalin{?,??}.o -lm

#IP28-IRIX64-6.2 CC = cc
#IP28-IRIX64-6.2 CFLAGS = -O2 -non_shared
#IP28-IRIX64-6.2 SCCFLAGS = -O2 -non_shared
#IP28-IRIX64-6.2 SCCLDFLAGS = -non_shared
#IP28-IRIX64-6.2 LRL = $(INSTALLLIBA)/QobiScheme.a $(INSTALLLIBA)/scxl.a\
#IP28-IRIX64-6.2       $(INSTALLLIBA)/libX11.a stalin{?,??}.o -lm

arc:
	-csh -cf "rm -rf ../archive/`archive-date`"
	csh -cf "mkdir ../archive/`archive-date`"
	-csh -cf "cp makefile ~/.ob *.{sc,c,h,s} ../archive/`archive-date`/."
	-csh -cf "cp ../boot/*.{sc,c} ../boot/boot ../archive/`archive-date`/."
	-csh -cf "cp ../documentation/*.{text,tex} ../archive/`archive-date`/."
	-csh -cf "cp ../documentation/README ../archive/`archive-date`/."
	-csh -cf "cp ../documentation/install ../archive/`archive-date`/."

clean:
	-rm -f *.o core a.out

real-clean: clean
	-rm -f $(TARGETS)

install: all
	cp $(TARGETS) $(INSTALLBINA)/.

uninstall:
	cd $(INSTALLBINA); rm -rf $(TARGETS)

release:
	-rm -rf $(RELEASE_FILE) $(RELEASE_FILE).tar*
	mkdir $(RELEASE_FILE)
	mkdir $(RELEASE_FILE)/source
	mkdir $(RELEASE_FILE)/documentation
	cp makefile *.{sc,c,h,s} $(RELEASE_FILE)/source/.
	tar cvf $(RELEASE_FILE).tar ./$(RELEASE_FILE)
	compress $(RELEASE_FILE).tar
	-rm -rf $(RELEASE_FILE)
	mv $(RELEASE_FILE).tar.Z ../releases/.

publish: release
	cd ..; publish stalin $(RELEASE_VERSION)

# Compilation rules and dependencies
# ----------------------------------
STALINLIB = stalin1.o\
            stalin2.o\
            stalin3a.o\
            stalin3b.o\
            stalin4a.o\
            stalin4b.o\
            stalin4c.o\
            stalin4d.o\
            stalin5a.o\
            stalin5b.o\
            stalin5c.o\
            stalin5d.o\
            stalin5e.o

STALINLIBSC = stalin1.sc\
              stalin2.sc\
              stalin3a.sc\
              stalin3b.sc\
              stalin4a.sc\
              stalin4b.sc\
              stalin4c.sc\
              stalin4d.sc\
              stalin5a.sc\
              stalin5b.sc\
              stalin5c.sc\
              stalin5d.sc\
              stalin5e.sc

stalin1.sch: $(STALINLIBSC)
	sch stalin1 $(STALINLIBSC)

stalin1.o: stalin1.sch stalin1.sc
	scc -cc $(CC) $(SCCFLAGS) -c stalin1.sc
	-rm -f stalin1.c

stalin2.sch: $(STALINLIBSC)
	sch stalin2 $(STALINLIBSC)

stalin2.o: stalin2.sch stalin2.sc
	scc -cc $(CC) $(SCCFLAGS) -c stalin2.sc
	-rm -f stalin2.c

stalin3a.sch: $(STALINLIBSC)
	sch stalin3a $(STALINLIBSC)

stalin3a.o: stalin3a.sch stalin3a.sc
	scc -cc $(CC) $(SCCFLAGS) -c stalin3a.sc
	-rm -f stalin3a.c

stalin3b.sch: $(STALINLIBSC)
	sch stalin3b $(STALINLIBSC)

stalin3b.o: stalin3b.sch stalin3b.sc
	scc -cc $(CC) $(SCCFLAGS) -c stalin3b.sc
	-rm -f stalin3b.c

stalin4a.sch: $(STALINLIBSC)
	sch stalin4a $(STALINLIBSC)

stalin4a.o: stalin4a.sch stalin4a.sc
	scc -cc $(CC) $(SCCFLAGS) -c stalin4a.sc
	-rm -f stalin4a.c

stalin4b.sch: $(STALINLIBSC)
	sch stalin4b $(STALINLIBSC)

stalin4b.o: stalin4b.sch stalin4b.sc
	scc -cc $(CC) $(SCCFLAGS) -c stalin4b.sc
	-rm -f stalin4b.c

stalin4c.sch: $(STALINLIBSC)
	sch stalin4c $(STALINLIBSC)

stalin4c.o: stalin4c.sch stalin4c.sc
	scc -cc $(CC) $(SCCFLAGS) -c stalin4c.sc
	-rm -f stalin4c.c

stalin4d.sch: $(STALINLIBSC)
	sch stalin4d $(STALINLIBSC)

stalin4d.o: stalin4d.sch stalin4d.sc
	scc -cc $(CC) $(SCCFLAGS) -c stalin4d.sc
	-rm -f stalin4d.c

stalin5a.sch: $(STALINLIBSC)
	sch stalin5a $(STALINLIBSC)

stalin5a.o: stalin5a.sch stalin5a.sc
	scc -cc $(CC) $(SCCFLAGS) -c stalin5a.sc
	-rm -f stalin5a.c

stalin5b.sch: $(STALINLIBSC)
	sch stalin5b $(STALINLIBSC)

stalin5b.o: stalin5b.sch stalin5b.sc
	scc -cc $(CC) $(SCCFLAGS) -c stalin5b.sc
	-rm -f stalin5b.c

stalin5c.sch: $(STALINLIBSC)
	sch stalin5c $(STALINLIBSC)

stalin5c.o: stalin5c.sch stalin5c.sc
	scc -cc $(CC) $(SCCFLAGS) -c stalin5c.sc
	-rm -f stalin5c.c

stalin5d.sch: $(STALINLIBSC)
	sch stalin5d $(STALINLIBSC)

stalin5d.o: stalin5d.sch stalin5d.sc
	scc -cc $(CC) $(SCCFLAGS) -c stalin5d.sc
	-rm -f stalin5d.c

stalin5e.sch: $(STALINLIBSC)
	sch stalin5e $(STALINLIBSC)

stalin5e.o: stalin5e.sch stalin5e.sc
	scc -cc $(CC) $(SCCFLAGS) -c stalin5e.sc
	-rm -f stalin5e.c

stalin.sch: stalin.sc $(STALINLIBSC)
	sch -main stalin $(STALINLIBSC)

stalin.o: stalin.sch stalin.sc
	scc -cc $(CC) $(SCCFLAGS) -c stalin.sc
	-rm -f stalin.c

stalin: stalin.o $(STALINLIB)
	scc -cc $(CC) $(SCCLDFLAGS) -o stalin stalin.o $(LRL)
