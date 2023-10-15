#ifndef _OBX_OUT_
#define _OBX_OUT_

/*
* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon+ parser/compiler library.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* This file may be used under the terms of the GNU Lesser
* General Public License version 2.1 or version 3 as published by the Free
* Software Foundation and appearing in the file LICENSE.LGPLv21 and
* LICENSE.LGPLv3 included in the packaging of this file. Please review the
* following information to ensure the GNU Lesser General Public License
* requirements will be met: https://www.gnu.org/licenses/lgpl.html and
* http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*
* Alternatively this file may be used under the terms of the Mozilla 
* Public License. If a copy of the MPL was not distributed with this
* file, You can obtain one at https://mozilla.org/MPL/2.0/.
*/

#include "OBX.Runtime.h"

extern void Out$Int(int64_t i, int32_t n);
extern void Out$Ln();
extern void Out$Char(char);
extern void Out$Open();
extern void Out$String(const struct OBX$Array$1 str);
extern void Out$Real(float x, int32_t n);
extern void Out$LongReal(double x, int32_t n);

extern void Out$init$();
extern OBX$Cmd Out$cmd$(const char*);

#endif
