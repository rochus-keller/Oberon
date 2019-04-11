#ifndef OBERRORS_H
#define OBERRORS_H

/*
* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon parser/code model library.
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
#include <QReadWriteLock>
#include <QHash>
#include <QSet>

namespace Ob
{
    class SynTree;

    class Errors : public QObject
    {
        // class is thread-safe
    public:
        enum Source { Lexer, Syntax, Semantics, Generator };
        struct Entry
        {
            quint32 d_line;
            quint16 d_col;
            quint16 d_source;
            QString d_msg;
            QString d_file;
            bool operator==( const Entry& rhs )const { return d_line == rhs.d_line && d_col == rhs.d_col &&
                        d_source == rhs.d_source && d_msg == rhs.d_msg; }
        };
        typedef QSet<Entry> EntryList;
        typedef QHash<QString,EntryList> EntriesByFile;

        explicit Errors(QObject *parent = 0, bool threadExclusive = false );

        void error( Source, const SynTree*, const QString& msg );
        void error( Source, const QString& file, int line, int col, const QString& msg );
        void warning( Source, const SynTree*, const QString& msg );
        void warning( Source, const QString& file, int line, int col, const QString& msg );

        bool showWarnings() const;
        void setShowWarnings(bool on);
        bool reportToConsole() const;
        void setReportToConsole(bool on);
        bool record() const;
        void setRecord(bool on);

        quint32 getErrCount() const;
        quint32 getWrnCount() const;
        EntryList getErrors(const QString& file) const;
        EntriesByFile getErrors() const;
        EntryList getWarnings(const QString& file) const;
        EntriesByFile getWarnings() const;
        quint32 getSyntaxErrCount() const { return d_numOfSyntaxErrs; }

        void clear();
        void clearFile( const QString& file );
        void clearFiles( const QStringList& files );
        void update( const Errors&, bool overwrite = false );

        static const char* sourceName(int);
    protected:
        static void log( const Entry&, bool isErr );
    private:
        mutable QReadWriteLock d_lock;
        quint32 d_numOfErrs;
        quint32 d_numOfSyntaxErrs;
        quint32 d_numOfWrns;
        EntriesByFile d_errs;
        EntriesByFile d_wrns;
        bool d_showWarnings;
        bool d_threadExclusive;
        bool d_reportToConsole;
        bool d_record;
    };

    inline uint qHash(const Errors::Entry & e, uint seed = 0) {
        return ::qHash(e.d_msg,seed) ^ ::qHash(e.d_line,seed) ^ ::qHash(e.d_col,seed) ^ ::qHash(e.d_source,seed);
    }
}

#endif // OBERRORS_H
