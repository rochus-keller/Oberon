#ifndef OBXCILGEN_H
#define OBXCILGEN_H

/*
* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon+ parser/compiler library.
*
* The following is the license that applies to this copy of the
* library. For a license to use the library under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
*/

#include <QString>
#include <QByteArrayList>
class QIODevice;

namespace Ob
{
    class Errors;
}
namespace Obx
{
    class Model;
    class Project;
    struct Module;
    class IlEmitter;

    class CilGen
    {
    public:
        enum How { Ilasm, Fastasm, IlOnly, Pelib };
        // all true on success, false on error
        static bool translateAll(Project*, How how, bool debug, const QString& where );
        static bool translate(Module*, IlEmitter* out, bool debug, Ob::Errors* = 0 );
        static bool generateMain(IlEmitter* out, const QByteArray& thisMod,
                                 const QByteArray& callMod = QByteArray(), const QByteArray& callFunc = QByteArray());
        static bool generateMain(IlEmitter* out, const QByteArray& thisMod, const QByteArrayList& callMods );
    private:
        CilGen();
    };
}

#endif // OBXCILGEN_H
