#ifndef OBXPELIBRENDERER_H
#define OBXPELIBRENDERER_H

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

#include <Oberon/ObxIlEmitter.h>

namespace DotNetPELib
{
    class PELib;
}
namespace Obx
{
    class PelibGen : public IlRenderer
    {
    public:
        PelibGen();
        ~PelibGen();

        static void printInstructionTable();

        void writeByteCode(const QByteArray& filePath );
        void writeAssembler( const QByteArray& filePath );
        void clear();
        DotNetPELib::PELib* getPelib();

        virtual void beginModule( const QByteArray& assemblyName, const QByteArray& moduleName,
                                  const QByteArrayList& imports, const QString& sourceFile, quint8 moduleKind );
        virtual void endModule();

        virtual void addMethod(const IlMethod& method );

        virtual void beginClass(const QByteArray& className, bool isPublic = true, bool byValue = false,
                         const QByteArray& superClassRef = QByteArray() );
        virtual void endClass();

        virtual void addField( const QByteArray& fieldName, // on top level or in class
                       const QByteArray& typeRef,
                       bool isPublic = true,
                       bool isStatic = false,
                       int explicitOffset = -1,
                       const QByteArray& marshalAs = QByteArray() );
    private:
        struct Imp;
        Imp* d_imp;
    };
}

#endif // OBXPELIBRENDERER_H
