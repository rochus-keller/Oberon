#ifndef OBXCGEN2_H
#define OBXCGEN2_H

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

    class CGen2
    {
    public:
        static bool translateAll(Project*, bool debug, const QString& where );
        static bool translate(QIODevice* header, QIODevice* body, Module*, bool debug, Ob::Errors* = 0 );
        static bool generateMain(QIODevice*, const QByteArray& callMod,
                                 const QByteArray& callFunc,
                                 const QByteArrayList& allMods );
        static bool generateMain(QIODevice*, const QByteArrayList& callMods,
                                 const QByteArrayList& allMods );
    private:
        CGen2();
    };
}

#endif // OBXCGEN2_H
