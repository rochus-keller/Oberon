#ifndef _ObnLjEditor_H
#define _ObnLjEditor_H

/*
* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon parser/compiler library.
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

#include <QMainWindow>

class CodeEditor;

namespace Lua
{
class Engine2;
class BcViewer2;
class Terminal2;
class JitEngine;
}

namespace Ob
{
    class Highlighter;

    class LjEditor : public QMainWindow
    {
        Q_OBJECT

    public:
        LjEditor(QWidget *parent = 0);
        ~LjEditor();

        void loadFile( const QString& path );
        void logMessage(const QString& , bool err = false);

    protected:
        void createTerminal();
        void createDumpView();
        void createMenu();
        void closeEvent(QCloseEvent* event);
        bool checkSaved( const QString& title );
        void compile();
        void toByteCode();

    protected slots:
        void onParse();
        void onRun();
        void onRun2();
        void onNew();
        void onOpen();
        void onSave();
        void onSaveAs();
        void onCaption();
        void onGotoLnr(int);
        void onFullScreen();
        void onCursor();
        void onExportBc();
        void onExportAsm();
        void onExportLua();

    private:
        CodeEditor* d_edit;
        Lua::Engine2* d_lua;
        Lua::BcViewer2* d_bcv;
        Lua::Terminal2* d_term;
        Lua::JitEngine* d_eng;
        Highlighter* d_hl;
        QByteArray d_luaCode;
        QByteArray d_luaBc;
        QByteArray d_moduleName;
        enum { Gen1, Gen2 };
        quint8 d_useGen;
        bool d_lock;
    };
}

#endif // MAINWINDOW_H
