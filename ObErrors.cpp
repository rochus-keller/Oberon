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

#include "ObErrors.h"
#include "ObSynTree.h"
#include <QtDebug>
#include <QFileInfo>
#include <QStringList>
using namespace Ob;

Errors::Errors(QObject *parent, bool threadExclusive) :
    QObject(parent),
    d_numOfErrs(0),d_numOfWrns(0),d_showWarnings(true),d_threadExclusive(threadExclusive),
    d_reportToConsole(false),d_record(false),d_numOfSyntaxErrs(0)
{

}

bool Errors::error(Errors::Source s, const SynTree* st, const QString& msg)
{
    Q_ASSERT( st != 0 );
    return error( s, st->d_tok.d_sourcePath, st->d_tok.d_lineNr, st->d_tok.d_colNr, msg );
}

bool Errors::error(Errors::Source s, const QString& file, int line, int col, const QString& msg)
{
    if( !d_threadExclusive ) d_lock.lockForWrite();
    bool inserted = true;
    Entry e;
    e.d_col = col;
    e.d_line = line;
    e.d_msg = msg;
    e.d_source = s;
    e.d_file = file;
    if( d_record )
    {
        EntryList& l = d_errs[file];
        const int count = l.size();
        l.insert(e);
        inserted = count != l.size();
    }
    if( d_reportToConsole && inserted )
        log(e,true);
    if( inserted || !d_reportToConsole )
    {

        d_numOfErrs++;
        if( s == Syntax )
            d_numOfSyntaxErrs++;
    }
    if( !d_threadExclusive ) d_lock.unlock();
    return false;
}

void Errors::warning(Errors::Source s, const SynTree* st, const QString& msg)
{
    Q_ASSERT( st != 0 );
    warning( s, st->d_tok.d_sourcePath, st->d_tok.d_lineNr, st->d_tok.d_colNr, msg );
}

void Errors::warning(Errors::Source s, const QString& file, int line, int col, const QString& msg)
{
    if( !d_threadExclusive ) d_lock.lockForWrite();
    bool inserted = true;
    if( d_showWarnings )
    {
        Entry e;
        e.d_col = col;
        e.d_line = line;
        e.d_msg = msg;
        e.d_source = s;
        e.d_file = file;
        if( d_record )
        {
            EntryList& l = d_wrns[file];
            const int count = l.size();
            l.insert(e);
            inserted = count != l.size();
        }
        if( d_reportToConsole && inserted )
            log(e,false);
    }
    if( inserted )
        d_numOfWrns++;
    if( !d_threadExclusive ) d_lock.unlock();
}

bool Errors::showWarnings() const
{
    if( !d_threadExclusive ) d_lock.lockForRead();
    const bool res = d_showWarnings;
    if( !d_threadExclusive ) d_lock.unlock();
    return res;
}

void Errors::setShowWarnings(bool on)
{
    if( !d_threadExclusive ) d_lock.lockForWrite();
    d_showWarnings = on;
    if( !d_threadExclusive ) d_lock.unlock();
}

bool Errors::reportToConsole() const
{
    if( !d_threadExclusive ) d_lock.lockForRead();
    const bool res = d_reportToConsole;
    if( !d_threadExclusive ) d_lock.unlock();
    return res;
}

void Errors::setReportToConsole(bool on)
{
    if( !d_threadExclusive ) d_lock.lockForWrite();
    d_reportToConsole = on;
    if( !d_threadExclusive ) d_lock.unlock();
}

bool Errors::record() const
{
    if( !d_threadExclusive ) d_lock.lockForRead();
    const bool res = d_record;
    if( !d_threadExclusive ) d_lock.unlock();
    return res;
}

void Errors::setRecord(bool on)
{
    if( !d_threadExclusive ) d_lock.lockForWrite();
    d_record = on;
    if( !d_threadExclusive ) d_lock.unlock();
}

quint32 Errors::getErrCount() const
{
    if( !d_threadExclusive ) d_lock.lockForRead();
    const quint32 res = d_numOfErrs;
    if( !d_threadExclusive ) d_lock.unlock();
    return res;
}

quint32 Errors::getWrnCount() const
{
    if( !d_threadExclusive ) d_lock.lockForRead();
    const quint32 res = d_numOfWrns;
    if( !d_threadExclusive ) d_lock.unlock();
    return res;
}

Errors::EntryList Errors::getErrors(const QString& file) const
{
    EntryList res;
    if( !d_threadExclusive ) d_lock.lockForRead();
    res = d_errs.value(file);
    if( !d_threadExclusive ) d_lock.unlock();
    return res;
}

Errors::EntryList Errors::getWarnings(const QString& file) const
{
    EntryList res;
    if( !d_threadExclusive ) d_lock.lockForRead();
    res = d_wrns.value(file);
    if( !d_threadExclusive ) d_lock.unlock();
    return res;
}

Errors::EntriesByFile Errors::getWarnings() const
{
    EntriesByFile res;
    if( !d_threadExclusive ) d_lock.lockForRead();
    res = d_wrns;
    if( !d_threadExclusive ) d_lock.unlock();
    return res;
}

Errors::EntriesByFile Errors::getErrors() const
{
    EntriesByFile res;
    if( !d_threadExclusive ) d_lock.lockForRead();
    res = d_errs;
    if( !d_threadExclusive ) d_lock.unlock();
    return res;
}

void Errors::clear()
{
    if( !d_threadExclusive ) d_lock.lockForWrite();
    d_numOfErrs = 0;
    d_numOfWrns = 0;
    d_numOfSyntaxErrs = 0;
    d_errs.clear();
    d_wrns.clear();
    if( !d_threadExclusive ) d_lock.unlock();
}

void Errors::clearFile(const QString& file)
{
    if( !d_threadExclusive ) d_lock.lockForWrite();
    d_numOfErrs -= d_errs[file].size();
    d_numOfSyntaxErrs = 0;
    d_errs.remove(file);
    d_numOfWrns -= d_wrns[file].size();
    d_wrns.remove(file);
    if( !d_threadExclusive ) d_lock.unlock();
}

void Errors::clearFiles(const QStringList& files)
{
    if( !d_threadExclusive ) d_lock.lockForWrite();
    foreach( const QString& file, files )
    {
        d_numOfErrs -= d_errs[file].size();
        d_errs.remove(file);
        d_numOfWrns -= d_wrns[file].size();
        d_wrns.remove(file);
    }
    if( !d_threadExclusive ) d_lock.unlock();
}

void Errors::update(const Errors& rhs, bool overwrite)
{
    if( !d_threadExclusive ) d_lock.lockForWrite();
    if( !rhs.d_threadExclusive ) rhs.d_lock.lockForRead();

    bool doLog = d_reportToConsole && !rhs.d_reportToConsole;
    if( overwrite )
    {
        d_errs = rhs.d_errs;
        d_wrns = rhs.d_wrns;
        d_numOfSyntaxErrs = rhs.d_numOfSyntaxErrs;
        d_numOfErrs = rhs.d_numOfErrs;
        d_numOfWrns = rhs.d_numOfWrns;
    }else
    {
        EntriesByFile::const_iterator i;
        for( i = rhs.d_errs.begin(); i != rhs.d_errs.end(); ++i )
        {
            d_errs[ i.key() ] = i.value();
            if( doLog ) foreach( const Entry& e, i.value() ) log(e,true);
        }
        d_numOfErrs = 0;
        d_numOfSyntaxErrs = 0;
        for( i = d_errs.begin(); i != d_errs.end(); ++i )
            d_numOfErrs += i.value().size();
        for( i = rhs.d_wrns.begin(); i != rhs.d_wrns.end(); ++i )
        {
            d_wrns[ i.key() ] = i.value();
            if( doLog ) foreach( const Entry& e, i.value() ) log(e,false);
        }
        d_numOfWrns = 0;
        for( i = d_wrns.begin(); i != d_wrns.end(); ++i )
            d_numOfWrns += i.value().size();
    }

    if( !rhs.d_threadExclusive ) rhs.d_lock.unlock();
    if( !d_threadExclusive ) d_lock.unlock();
}

const char* Errors::sourceName(int s)
{
    switch(s)
    {
    case Lexer:
        return "Lexer";
    case Syntax:
        return "Syntax";
    case Semantics:
        return "Semantics";
    default:
        return "";
    }
}

void Errors::log(const Errors::Entry& e, bool isErr)
{
    if( isErr )
        qCritical() << QFileInfo(e.d_file).fileName() << ":" << e.d_line << ":" << e.d_col << ": error:" <<
                       e.d_msg.toUtf8().data();
    else
        qWarning() << QFileInfo(e.d_file).fileName() << ":" << e.d_line << ":" << e.d_col << ": warning:" <<
                      e.d_msg.toUtf8().data();

}
