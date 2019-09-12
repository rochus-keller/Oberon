#ifndef _ObnLjEditor_H
#define _ObnLjEditor_H

/*
* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the JuaJIT BC Viewer application.
*
* The following is the license that applies to this copy of the
* application. For a license to use the application under conditions
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
class BcViewer;
class Terminal2;
class JitEngine;
}

namespace Ob
{

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

    protected slots:
        void onDump();
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

    private:
        CodeEditor* d_edit;
        Lua::Engine2* d_lua;
        Lua::BcViewer* d_bcv;
        Lua::Terminal2* d_term;
        Lua::JitEngine* d_eng;
        bool d_lock;
    };
}

#endif // MAINWINDOW_H
