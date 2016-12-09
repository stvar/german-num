# Copyright (C) 2016  Stefan Vargyas
# 
# This file is part of German-Num.
# 
# German-Num is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# German-Num is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with German-Num.  If not, see <http://www.gnu.org/licenses/>.

PROG := calc

GCC := gcc
CFLAGS := -Wall -Wextra -Wno-ignored-qualifiers -g -DDEBUG -DPROGRAM=${PROG}
LIBS := -lstdc++

ifeq (${GCC_MBIT},32)
CFLAGS += -m32
else ifeq (${GCC_MBIT},64)
CFLAGS += -m64
else ifneq (${GCC_MBIT},)
$(error invalid GCC_MBIT=${GCC_MBIT})
endif

default: ${PROG}

${PROG}: calc.cpp
	${GCC} ${CFLAGS} $< -o $@ ${LIBS}


