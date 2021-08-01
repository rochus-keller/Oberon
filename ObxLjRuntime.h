#ifndef OBXLJRUNTIME_H
#define OBXLJRUNTIME_H

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

#include <QObject>
#include <QPair>

namespace Lua
{
class Engine2;
}

namespace Obx
{
    class Project;
    struct Module;

    class LjRuntime : public QObject
    {
    public:
        typedef QPair<Module*,QByteArray> Bytecode;
        typedef QList<Bytecode> BytecodeList;

        LjRuntime(QObject* = 0);

        Project* getPro() const { return d_pro; }
        Lua::Engine2* getLua() const { return d_lua; }

        bool compile(bool doGenerate );
        bool run();
        bool loadLibraries();
        bool loadBytecode();
        bool executeMain();
        bool restartEngine();

        QByteArray findByteCode(Module*)const;
        BytecodeList findByteCode( const QString& filePath ) const;
        bool saveBytecode(const QString& outPath, const QString& suffix = ".lua") const;
        bool hasBytecode() const { return !d_byteCode.isEmpty(); }
        bool hasBuildErrors() const { return d_buildErrors; }

        void setJitEnabled(bool);
        bool jitEnabled() const { return d_jitEnabled; }
    protected:
        void generate();
        void generate(Module* m);
        void prepareEngine();

    private:
        Project* d_pro;
        Lua::Engine2* d_lua;
        BytecodeList d_byteCode;
        bool d_jitEnabled;
        bool d_buildErrors;
    };
}

#endif // OBXLJRUNTIME_H
